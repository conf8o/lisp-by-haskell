data Atom = Integer Integer | Char Char | Bool Bool | String String | Fn (List Atom -> Obj) | Symbol Symbol

data Symbol = Special Special | Key Key

type Key = String

data Special = If | Let | Quote

data List a = Cons a a

data Obj = Atom Atom | List (List Obj) | Nil

type Env = [(Key, Obj)]
