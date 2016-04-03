module FOL where

type Ft = Int

-- A term in First-Order Logic
data Term = Var Ft | F Ft [Term]
  deriving (Show, Eq)

-- A formula in First-Order Logic
data Formula =
    Pred Ft [Term]
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Imp Formula Formula
  | Biimp Formula Formula
  | Forall Term Formula
  | Exists Term Formula
    deriving (Show, Eq)
