module Lib
  ( someFunc,
    Atom,
    Symbol,
    Special,
    Cons,
    Data,
    eval,
    evalCons,
    plus,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Atom = Integer Integer | Char Char | Bool Bool | String String | Fn (Cons -> Data) | Symbol Symbol

newtype Symbol = Special Special

data Special = If | Let | Quote

type Cons = (Data, Data)

data Data = Atom Atom | List Cons | Nil | Error String

eval :: Data -> Data
eval (Atom a) = Atom a
eval Nil = Nil
eval (Error e) = Error e
eval (List (data1, data2)) =
  case (data1, data2) of
    (Atom (Fn f), List args) -> f $ evalCons args
    (d1, d2) -> List (eval d1, eval d2)

evalCons :: Cons -> Cons
evalCons (d, Nil) = (d, Nil)
evalCons (d, Atom a) = (d, Atom a)
evalCons (d, Error e) = (d, Error e)
evalCons (d, List cons) = (eval d, List $ evalCons cons)

plus :: Cons -> Data
plus (Atom (Integer i), Atom (Integer j)) = Atom $ Integer $ i + j
plus _ = Error "plus Error"