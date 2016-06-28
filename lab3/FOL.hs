module FOL where

type Ft = String

-- A term in First-Order Logic
data Term = Var Ft | F Ft [Term]
  deriving (Eq)

-- An atomic formula in First-Order Logic
data Atom =
    Pred Ft [Term]
    deriving (Eq)

instance Show Term where
    show (Var x) = x
    show (F f t) = f ++ show t

instance Show Atom where
    show (Pred f t) = f ++ show t