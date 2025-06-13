module S (eval, plus, list, Special (..), Data (..)) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM

type Cons = (Data, Data)

type Fn = (Data -> Env Data)

data Data = Integer Integer | Char Char | Bool Bool | String String | List Cons | Nil | Function Fn | Symbol String | Special Special | Error String

data Special = If | Let | Quote

type EnvStack = [HM.HashMap String Data]

type Env = State EnvStack

eval :: Data -> Env Data
eval (List (Function f, List args)) = applyFunction f args
eval (List (Special f, List expr)) = applySpecial f expr
eval (Symbol s) = do
  env <- get
  case lookupEnv s env of
    Just v -> return v
    Nothing -> return $ Error ("Unbound symbol: " ++ s)
eval x = return x

evalList :: Cons -> Env Data
evalList (x, List xs) = fmap List $ (,) <$> eval x <*> evalList xs
evalList _ = return $ Error ""

applyFunction :: Fn -> Cons -> Env Data
applyFunction fn args =
  fn =<< evalList args

applySpecial :: Special -> Cons -> Env Data
applySpecial If (Bool p, List (then', List (else', Nil))) =
  if p then eval then' else eval else'
applySpecial _ _ = return $ Error "未実装"

-- applySpecial Let (List bindings) =

-- (let ((a b) (c d)) expr)

-- bind :: Cons -> Env () =

-- env

lookupEnv :: String -> EnvStack -> Maybe Data
lookupEnv _ [] = Nothing
lookupEnv key (frame : rest) =
  case HM.lookup key frame of
    Just val -> Just val
    Nothing -> lookupEnv key rest

putIntoEnv :: String -> Data -> Env ()
putIntoEnv key val = do
  env <- get
  case env of
    [] -> error "Environment stack is empty"
    (top : rest) -> do
      let newTop = HM.insert key val top
      put (newTop : rest)

pushEnv :: Env ()
pushEnv = modify (HM.empty :)

popEnv :: Env ()
popEnv = modify tail

-- lib

plusFn :: Data -> Env Data
plusFn (List (Integer i, List (Integer j, Nil))) = return $ Integer (i + j)
plusFn _ = return $ Error "plus Error"

plus :: Data
plus = Function plusFn

-- system lib
list :: [Data] -> Data
list [] = Nil
list [x] = List (x, Nil)
list (x : xs) = List (x, list xs)
