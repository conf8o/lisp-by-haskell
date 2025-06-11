module S (eval, plus, list, Special (..), Data (..)) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM

data Data = Integer Integer | Char Char | Bool Bool | String String | List Data Data | Nil | Fn (Data -> Env Data) | Symbol String | Special Special | Error String

data Special = If | Let | Quote

type EnvStack = [HM.HashMap String Data]

type Env = State EnvStack

eval :: Data -> Env Data
eval (List data1 data2) =
  case (data1, data2) of
    (Fn f, List x xs) -> f =<< evalList (List x xs)
    _ -> return $ Error "未実装"
eval (Symbol s) = do
  env <- get
  case lookupEnv s env of
    Just v -> return v
    Nothing -> return $ Error ("Unbound symbol: " ++ s)
eval x = return x

evalList :: Data -> Env Data
evalList Nil = return Nil
evalList (List x xs) = List <$> eval x <*> evalList xs
evalList _ = return $ Error ""

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

list :: [Data] -> Data
list [x] = List x Nil
list (x : xs) = List x $ list xs
list _ = Error "error"

plusFn :: Data -> Data
plusFn (List (Integer i) (List (Integer j) Nil)) = Integer $ i + j
plusFn _ = Error "plus Error"

plus :: Data
plus = Fn $ return . plusFn
