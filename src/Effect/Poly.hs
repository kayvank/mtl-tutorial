module Effect.Poly where

import Polysemy

data Log m a where
  LogInfo :: String -> Log m ()

makeSem ''Log

myBusinessFunction :: Member Log r => Integer -> Integer -> Sem r Integer
myBusinessFunction i1 i2 = do
  logInfo $ "in myBusiness Function input: (" <> show i1 <> ", " <> show i2 <> " )"
  let result = i1 + i2
  logInfo $ "in myBusiness Function result: " <> show result
  pure result
