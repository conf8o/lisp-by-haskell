data Atom = Integer Integer | Char Char | Bool Bool | String String | Fn (List -> Obj) | Symbol Symbol

data Symbol = Special Special | Key Key

type Key = String

data Special = If | Let | Quote

data List = Cons Obj Obj

data Obj = Atom Atom | List List | Nil

type Env = [(Key, Obj)]

data Evaluator = Error String | SExpr Env Obj

eval :: Evaluator -> Evaluator
eval (Error s) = Error s
eval (SExpr env (List (Cons (Atom (Fn f)) (List l)))) = SExpr env $ f l
eval (SExpr env atom) = SExpr env atom