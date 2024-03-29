{-# LANGUAGE RecursiveDo, OverloadedStrings, TupleSections, LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main ( main ) where

import Protolude
import Prelude ( String )
import Data.String ( fromString )
import Unsafe ( unsafeFromJust )
import System.Directory
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map as Map
import LLVM.Pretty
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Operand ( Operand )
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction


-- Some type synonyms for readability

type Variable = ParameterName

type FuncName = String


-- We support addition, subtraction, multiplication, equality
data Op = Add | Subtract | Mul | Equals

-- The main expression type
data Expr
  = Var Variable           -- a variable reference
  | Value Int              -- a constant integer value
  | If Expr Expr Expr      -- an if expression (contains actual condition, body for then clause, body for else clause)
  | BinOp Op Expr Expr     -- a binary operator applied to 2 subexpressions
  | Call Variable [Expr]   -- a function call

-- Our main "AST" consists of a single
data Func = Func FuncName [Variable] Expr

-- During codegen we need to keep track of a mapping from variable names to operands (%0, %1, ...) in IR
type CodeGenState = Map Variable Operand


-- Main function, prints out IR for 3 functions
main :: IO ()
main = do
  let output = ppllvm $ buildModule "fibonacci" codegen
  T.putStrLn output
  doesDirectoryExist "./output" >>= \case
    True -> T.writeFile "./output/code.ll" output
    False -> pure ()

codegen :: ModuleBuilder ()
codegen = mdo
  -- Hardcoded function: adds 2 integers and returns the result
  void $ function "add" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo
    void $ block `named` "entry"; do
      c <- add a b
      ret c
  -- Generate IR for fac function:
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

facAST :: Func
facAST = Func "fac" ["input"] $
  If (BinOp Equals (Var "input") (Value 0))
    (Value 1)
    (BinOp Mul
      (Var "input")
      (Call "fac" [BinOp Subtract (Var "input") (Value 1)]))

fibAST :: Func
fibAST = Func "fib" ["input"] $
  If (BinOp Equals (Var "input") (Value 0))
    (Value 1)
    (If (BinOp Equals (Var "input") (Value 1))
      (Value 1)
      (BinOp Add
        (Call "fib" [BinOp Subtract (Var "input") (Value 1)])
        (Call "fib" [BinOp Subtract (Var "input") (Value 2)])))


-- Generates the bitcode for the provided function,
-- returns an operand which can be used in other expressions/functions
buildIR :: Func -> ModuleBuilder Operand
buildIR (Func name vars body) = mdo
  let funcName = Name.mkName name
  func <- function funcName (prepareVars vars) AST.i32 $ \args -> mdo
    _ <- block `named` "entry"
    flip runReaderT (mkState (fromString name) func args) $ do
      result <- buildExprIR body
      ret result
  pure func
  where prepareVars = map (AST.i32, )
        mkState funcName funcOperand args =
          Map.fromList $ (funcName, funcOperand) : zip vars args

-- Generates IR for an expression, returns an operand,
-- which can be used in other expressions/functions.
buildExprIR :: Expr -> ReaderT CodeGenState (IRBuilderT ModuleBuilder) Operand
buildExprIR = \case
  Value v -> int32 (fromIntegral v)
  Var v -> lookupVar v
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
  BinOp op e1 e2 -> do
    res1 <- buildExprIR e1
    res2 <- buildExprIR e2
    opToIr op res1 res2
  Call funcName args -> do
    func <- lookupVar funcName
    args' <- traverse buildExprIR args
    let args'' = (, []) <$> args'
    call func args''

-- Looks up the matching operand for a variable.
-- This assumes all variables are known in the current context.
lookupVar :: MonadReader CodeGenState m => Variable -> m Operand
lookupVar v = asks $ unsafeFromJust . Map.lookup v

-- Finds the matching LLVM instruction for an operator
opToIr :: MonadIRBuilder m => Op -> (Operand -> Operand -> m Operand)
opToIr = \case
  Add -> add
  Subtract -> sub
  Mul -> mul
  Equals -> icmp IP.EQ

