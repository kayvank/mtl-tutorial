{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import MonadTransformers

prop_test :: Property
prop_test = property $ do
  doMonadTransformers === "MonadTransformers"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
