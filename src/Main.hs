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

data AST = Func FuncName [Variable] (Expr ())

type Operand = Operand.Operand

data Expr ty where
  -- TODO remove return, no need for GADT
  Return :: Expr Operand -> Expr ()
  Var :: Variable -> Expr Operand
  Value :: Int -> Expr Operand
  If :: Expr Operand -> Expr Operand -> Expr Operand -> Expr Operand
  Equals :: Expr Operand -> Expr Operand -> Expr Operand


fibAST :: AST
fibAST = Func "fib" ["input"] $
  Return $ If (Var "input" `Equals` Value 0)
    (Value 1)
    (If (Var "input" `Equals` Value 1)
      (Value 1)
      (Value 2))  -- TODO implement rest of body

buildIR :: AST -> ModuleBuilder Operand.Operand
buildIR (Func name vars body) =
  function name (prepareVars vars) AST.i32 $ \args -> mdo
    _ <- block `named` "entry"
    flip evalStateT (buildArgEnv args) $ buildExprIR body
  where prepareVars = map (AST.i32, )
        buildArgEnv args = Map.fromList $ zip vars args


buildExprIR :: Expr a -> StateT (Map Variable Operand) (IRBuilderT ModuleBuilder) a
buildExprIR = \case
  Return ast ->
    ret =<< buildExprIR ast
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
  Var v -> do
    x <- gets $ Map.lookup v
    pure $ unsafeFromJust x

