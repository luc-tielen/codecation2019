{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main ( main ) where

import Protolude
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


main :: IO ()
main = T.putStrLn $ ppllvm $ buildModule "fibonacci" $ mdo
  _ <- function "add" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo
    _ <- block `named` "entry"; do
      c <- add a b
      ret c

  buildIR fibAST


{-
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-}

{-

uint32_t fib(uint32_t input) {
  if (input == 0) {
    return 1;
  }
  if (input == 1) {
    return 1;
  }

  const res1 = fib(input - 1);
  const res2 = fib(input - 2);
  return res1 + res;
}

fib x =
  if x == 0
    then 1
    else if x == 1
           then 1
           else fib (n - 1) + fib (n - 2)
-}

type FuncName = Name.Name
type Variable = ParameterName

data AST = Func FuncName [Variable] Expr

type Operand = Operand.Operand

data Op = Add | Subtract

data Expr
  = Var Variable
  | Value Int
  | If Expr Expr Expr
  | Equals Expr Expr
  | BinOp Op Expr Expr
  --Call :: FuncName -> [Expr Operand] -> Expr Operand


fibAST :: AST
fibAST = Func "fib" ["input"] $
  If (Var "input" `Equals` Value 0)
    (Value 1)
    (If (Var "input" `Equals` Value 1)
      (Value 1)
      (BinOp Subtract (Value 2) (Value 42)))
    --  (Call "fib" []))  -- TODO implement rest of body

data IRState
  = IRState
  { varMap :: Map Variable Operand
  }

buildIR :: AST -> ModuleBuilder Operand.Operand
buildIR (Func name vars body) = mdo
  func <- function name (prepareVars vars) AST.i32 $ \args -> mdo
    _ <- block `named` "entry"
    flip evalStateT (mkState name func args) $ do
      result <- buildExprIR body
      ret result
  pure func
  where prepareVars = map (AST.i32, )
        mkState _ _ args =
          IRState $ Map.fromList $ zip vars args


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
    br endBlock

    elseBlock <- block `named` "if.else"
    falseResult <- buildExprIR f
    br endBlock

    endBlock <- block `named` "if.exit"
    phi [(trueResult, thenBlock), (falseResult, elseBlock)]
  Value v -> int32 (fromIntegral v)
  Var v ->
    gets $ unsafeFromJust . Map.lookup v . varMap
  BinOp op e1 e2 -> do
    res1 <- buildExprIR e1
    res2 <- buildExprIR e2
    opToIr op res1 res2
  --Call name args ->    _

opToIr :: MonadIRBuilder m => Op -> Operand -> Operand -> m Operand
opToIr = \case
  Add -> add
  Subtract -> sub
