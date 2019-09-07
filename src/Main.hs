{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main ( main ) where

import Protolude
import qualified Data.Text.Lazy.IO as T
import LLVM.Pretty
import qualified LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction


main :: IO ()
main = T.putStrLn $ ppllvm $ buildModule "fibonacci" $ mdo
  function "add" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo
    _ <- block `named` "entry"; do
      c <- add a b
      ret c

