{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module Trans.Transformers where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe
import Data.Text as T
import GHC.RTS.Flags (TickyFlags)

type Name = Text --variable-name

data Exp -- expressions
  = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value -- values
  = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
   in IntVal (i1 + i2)
eval0 env (Abs name exp) = FunVal env name exp
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
   in case val1 of
        FunVal env' n body -> eval0 (Map.insert n val2 env') body

--
-- Identity
--
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

-- eval1 :: (Monad m, MonadFail m) => Env -> Exp -> m Value
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = case Map.lookup n env of
  Nothing -> error ""
  Just x -> return x
--
-- see https://imada.sdu.dk/~rolf/Edu/DM22/F06/haskell-operatorer.pdf
-- lazy pattern, 4.4  Lazy Patterns
--
eval1 env (Plus e1 e2) = do
  ~(IntVal i1) <- eval1 env e1 --
  ~(IntVal i2) <- eval1 env e2
  return $ IntVal (i1 + i2)
eval1 env (Abs name exp) = return $ FunVal env name exp
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body

--
-- ExceptT
--
type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
  Nothing -> throwError "didnt find env"
  Just val -> return val
eval2 env (Abs name exp) = return $ FunVal env name exp
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body -> eval2 (Map.insert n val2 env') body
    _ -> throwError "type error"
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error"

--
-- ReaderT (ExceptT  ( Identity  )
--
type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = (runIdentity . runExceptT . runReaderT ev) env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError "type error"
    Just val -> return val
eval3 (Plus e1 e2) = do
  e1' <- eval3 e1
  e2' <- eval3 e2
  case (e1', e2') of
    (IntVal e1', IntVal e2') -> return $ IntVal (e1' + e2')
    _ -> throwError "type error"
eval3 (Abs name exp) = do
  env <- ask
  return $ FunVal env name exp
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval3 body)
    _ -> throwError "type error"

--
-- ReaderT (ExceptT  ( StateT (Identity )  )
--
type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env s ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) s)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = tick >> (pure $ IntVal i)
eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Just val -> pure val
    _ -> throwError "type error"
eval4 (Plus e1 e2) = do
  tick
  e1' <- eval4 e1
  e2' <- eval4 e2
  case (e1', e2') of
    (IntVal e1', IntVal e2') -> return $ IntVal (e1' + e2')
    _ -> throwError "type error"
eval4 (Abs name expr) = do
  tick
  env <- ask
  pure $ FunVal env name expr
eval4 (App e1 e2) = do
  tick
  e1' <- eval4 e1
  e2' <- eval4 e2
  case e1' of
    FunVal env' name body -> local (const (Map.insert name e2' env')) (eval4 body)
    _ -> throwError "type error"

type Eval5 a = ReaderT Env (ExceptT String (WriterT [Text] (StateT Integer IO))) a

runEval5 :: Env -> Integer -> Eval5 a -> IO ((Either String a, [Text]), Integer)
runEval5 env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = tick >> (liftIO $ print i) >> (pure $ IntVal i)
eval5 (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Just val -> pure val
    _ -> throwError $ "type error, unbounded error " <> T.unpack n
eval5 (Plus e1 e2) = do
  tick
  e1' <- eval5 e1
  e2' <- eval5 e2
  case (e1', e2') of
    (IntVal v1, IntVal v2) -> pure $ IntVal $ v1 + v2
    _ -> throwError "type error in addition"
eval5 (Abs name expr) = do
  tick
  tell [name]
  env <- ask
  pure $ FunVal env name expr
eval5 (App e1 e2) = do
  tick
  e1' <- eval5 e1
  e2' <- eval5 e2
  case e1' of
    (FunVal env' name body) -> local (const (Map.insert name e2' env')) (eval5 body)
    _ -> throwError "type error in appplication"
