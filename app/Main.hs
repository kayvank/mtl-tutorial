{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Text as T
import Trans.Transformers

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

main :: IO ()
main = do
  let r0 = eval0 Map.empty exampleExp
  print r0

  let r1 = runEval1 (eval1 Map.empty exampleExp)
  print r1

  let r2 = runEval2 (eval2 Map.empty exampleExp)
  print r2

  let r3 = runEval3 Map.empty (eval3 exampleExp)
  print r3

  let r4 = runEval4 Map.empty 0 (eval4 exampleExp)
  print r4

  r5 <- runEval5 Map.empty 0 (eval5 exampleExp)
  print r5
