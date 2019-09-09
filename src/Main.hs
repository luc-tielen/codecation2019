{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# LANGUAGE TupleSections, LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main ( main ) where

import Protolude
import Prelude ( String )
import Data.String ( fromString )
import Unsafe ( unsafeFromJust )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map as Map
import LLVM.Pretty
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction


type Variable = ParameterName

data AST = Func String [Variable] Expr

type Operand = Operand.Operand

data Op = Add | Subtract | Mul

data Expr
  = Var Variable
  | Value Int
  | If Expr Expr Expr
  | Equals Expr Expr
  | BinOp Op Expr Expr
  | Call Variable [Expr]

type IRState = Map Variable Operand


-- Main function, prints out IR for 3 functions
main :: IO ()
main = T.putStrLn $ ppllvm $ buildModule "fibonacci" $ mdo
  -- Hardcoded function: adds 2 integers and returns the result
  void $ function "add" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo
    void $ block `named` "entry"; do
      c <- add a b
      ret c

  -- Generate IR for fac function
  void $ buildIR facAST

  -- Generates IR for fib function:
  void $ buildIR fibAST


{-
2 example functions in this file that are converted to IR:

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-}

facAST :: AST
facAST = Func "fac" ["input"] $
  If (Var "input" `Equals` Value 0)
    (Value 1)
    (BinOp Mul
      (Var "input")
      (Call "fac" [BinOp Subtract (Var "input") (Value 1)]))

fibAST :: AST
fibAST = Func "fib" ["input"] $
  If (Var "input" `Equals` Value 0)
    (Value 1)
    (If (Var "input" `Equals` Value 1)
      (Value 1)
      (BinOp Add
        (Call "fib" [BinOp Subtract (Var "input") (Value 1)])
        (Call "fib" [BinOp Subtract (Var "input") (Value 2)])))


-- Generates the bitcode for the provided AST,
-- returns an operand which can be used in other expressions/functions
buildIR :: AST -> ModuleBuilder Operand.Operand
buildIR (Func name vars body) = mdo
  let funcName = Name.mkName name
  func <- function funcName (prepareVars vars) AST.i32 $ \args -> mdo
    _ <- block `named` "entry"
    flip evalStateT (mkState (fromString name) func args) $ do
      result <- buildExprIR body
      ret result
  pure func
  where prepareVars = map (AST.i32, )
        mkState name' func args =
          Map.fromList $ (name', func):zip vars args

-- Generates IR for an expression, returns an operand,
-- which can be used in other expressions/functions
buildExprIR :: Expr -> StateT IRState (IRBuilderT ModuleBuilder) Operand
buildExprIR = \case
  Equals l r -> do
    resL <- buildExprIR l
    resR <- buildExprIR r
    icmp P.EQ resL resR
  If c t f -> mdo
    condResult <- buildExprIR c
    condBr condResult thenBlock elseBlock

    thenBlock <- block `named` "if.then"
    trueResult <- buildExprIR t
    trueCurrentBlock <- currentBlock
    br endBlock

    elseBlock <- block `named` "if.else"
    falseResult <- buildExprIR f
    falseCurrentBlock <- currentBlock
    br endBlock

    endBlock <- block `named` "if.exit"
    phi [(trueResult, trueCurrentBlock), (falseResult, falseCurrentBlock)]
  Value v -> int32 (fromIntegral v)
  Var v -> gets $ unsafeFromJust . Map.lookup v
  BinOp op e1 e2 -> do
    res1 <- buildExprIR e1
    res2 <- buildExprIR e2
    opToIr op res1 res2
  Call name args -> do
    func <- gets $ unsafeFromJust . Map.lookup name
    args' <- traverse buildExprIR args
    let args'' = (, []) <$> args'
    call func args''

-- Finds the matching llvm instruction for an operator
opToIr :: MonadIRBuilder m => Op -> (Operand -> Operand -> m Operand)
opToIr = \case
  Add -> add
  Subtract -> sub
  Mul -> mul

