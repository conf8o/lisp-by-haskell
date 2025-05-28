module S (eval, plus, list, Special (..), Data (..)) where

data Special = If | Let | Quote

data Data = Integer Integer | Char Char | Bool Bool | String String | List Data Data | Nil | Fn (Data -> Data) | Symbol String | Special Special | Error String

eval :: Data -> Data
eval (List data1 data2) =
  case (data1, data2) of
    (Fn f, List x xs) -> f $ evalList (List x xs)
    _ -> Error "未実装"
eval (Symbol _) =
  Error "TODO: 環境を実装する"
eval x = x

evalList :: Data -> Data
evalList (List x Nil) = List (eval x) Nil
evalList (List x xs) = List (eval x) $ evalList xs
evalList _ = Error ""

-- lib

list :: [Data] -> Data
list [x] = List x Nil
list (x : xs) = List x $ list xs
list _ = Error "error"

plusFn :: Data -> Data
plusFn (List (Integer i) (List (Integer j) Nil)) = Integer $ i + j
plusFn _ = Error "plus Error"

plus :: Data
plus = Fn plusFn
