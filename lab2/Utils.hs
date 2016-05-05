-- @Author: JONATHAN STEVEN PRIETO CUBIDES
-- @Date: 2016-04-15 18:03:39
-- @Last Modified time: 2016-05-05 15:06:54

module Utils
    where

import FOL
import Data.List

isVar::Term -> Bool
isVar (Var _)           = True
isVar _                 = False

isCons::Term -> Bool
isCons (Cons _)         = True
isCons _                = False

isFunc::Term -> Bool
isFunc (Func _ _)       = True
isFunc _                = False

getInt::Term -> Int
getInt (Var x)          = x
getInt (Func x _)       = x
getInt (Cons x)         = x

getTerms::Term -> [Term]
getTerms (Func _ ts)    = ts
getTerms _              = []

-- Returns the list of variables [(Var x)] of a Formula.

getVars:: Formula -> [Term]
getVars (Forall t f)   = t:getVars f
getVars (Exists t f)   = t:getVars f
getVars (Not f)        = getVars f
getVars (And f g)      = getVars f ++ getVars g
getVars (Or f g)       = getVars f ++ getVars g
getVars (Imp f g)      = getVars f ++ getVars g
getVars (Biimp f g)    = getVars f ++ getVars g
getVars (Pred i t)     = getVars_ t

-- Returns the list of variables [(Var x)] of a list of Terms.

getVars_:: [Term] -> [Term]
getVars_ [] = []
getVars_ (x:xs) | isVar x  = x:(getVars_ xs)
                | isCons x = getVars_ xs
                | isFunc x = (getVars_ (getTerms x)) ++ (getVars_ xs)

-- Find the maximum value of the list of integers from
-- a list of terms [(Var Int)] after extract the Int field.

maxIndex::[Term] -> Int
maxIndex [] = 0
maxIndex ((Var idx):xs) = max idx (maxIndex xs)

-- Find the maximun integer for the getVars of a given formula.

boundIndex::Formula -> Int
boundIndex f = (maxIndex $ getVars f) + 1

-- Replaces all ocurrences of a Term x in a Formula by the Term y.
--   ∀x Px ≈ ∀y Py,
--   ∃x Px ≈ ∃y Py.

replace:: Term -> Term -> Formula -> Formula
replace x y (Not f)            = Not $ replace x y f
replace x y (And f g)          = And (replace x y f) (replace x y g)
replace x y (Or f g)           = Or (replace x y f) (replace x y g)
replace x y (Biimp f g)        = Biimp (replace x y f) (replace x y g)
replace x y (Imp f g)          = Imp (replace x y f) (replace x y g)
replace x y (Forall z f)       = Forall z (replace x y f)
replace x y (Exists z f)       = Exists z (replace x y f)
replace x y (Pred idx ts)      = Pred idx (replace_ x y ts)

-- Replaces all ocurrences of a Term X in a list of Term by the Term Y.
replace_:: Term -> Term -> [Term] -> [Term]
replace_ x y (t:ts)
        | isVar t && t == x     = y:rest
        | isFunc t              = newf:rest
        | otherwise             = t:rest
        where
            newterms = replace_ x y (getTerms t)
            newf     = Func (getInt t) newterms
            rest     = replace_ x y ts
replace_ x y [] = []