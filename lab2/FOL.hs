-- @Author: Jonathan Prieto
-- @Date:   2016-05-27 03:24:21
-- @Last Modified by:   Jonathan Prieto
-- @Last Modified time: 2016-06-28 19:12:58

module FOL
  where

-- A term in First-Order Logic
data Term = Cons Int | Var Int | Func Int [Term]
  deriving (Show, Eq)

-- A formula in First-Order Logic
data Formula =
  Pred Int [Term]
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Imp Formula Formula
  | Biimp Formula Formula
  | Forall Term Formula
  | Exists Term Formula
    deriving (Show, Eq)