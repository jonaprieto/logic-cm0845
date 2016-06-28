module FOL where

type Ft = String

-- A term in First-Order Logic
data Term = Var Ft | F Ft [Term]
  deriving (Show, Eq)

-- An atomic formula in First-Order Logic
data Atom =
    Pred Ft [Term]
    deriving (Show, Eq)