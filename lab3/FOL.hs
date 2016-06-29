-- @Author: Jonathan Prieto
-- @Date:   2016-06-28 17:18:00
-- @Last Modified by:   Jonathan Prieto
-- @Last Modified time: 2016-06-28 19:13:27
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