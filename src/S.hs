module S (eval, plus, Value (..), Special (..), Cons, Data (..)) where

data Value = Integer Integer | Char Char | Bool Bool | String String

data Special = If | Let | Quote

type Cons = (Data, Data)

data Data = Value Value | List Cons | Fn (Cons -> Data) | Symbol String | Special Special | Error String

eval :: Data -> Data
eval (List (data1, data2)) =
  case (data1, data2) of
    (Fn f, List args) -> f $ evalCons args
    (d1, d2) -> List (eval d1, eval d2)
eval (Symbol _) =
  Error "TODO: 環境を実装する"
eval x = x

evalCons :: Cons -> Cons
evalCons (x, List xs) = (eval x, List $ evalCons xs)
evalCons (x, y) = (eval x, eval y)

plus :: Cons -> Data
plus (Value (Integer i), Value (Integer j)) = Value $ Integer $ i + j
plus _ = Error "plus Error"