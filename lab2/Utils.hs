-- @Author: JONATHAN STEVEN PRIETO CUBIDES
-- @Date: 2016-04-15 18:03:39

module Utils
    where

import FOL
import Data.List

isVar :: Term -> Bool
isVar (Var _)        = True
isVar _              = False

isCons :: Term -> Bool
isCons (Cons _)      = True
isCons _             = False

isFunc :: Term -> Bool
isFunc (Func _ _)    = True
isFunc _             = False

getInt :: Term -> Int
getInt (Var x)       = x
getInt (Func x _)    = x
getInt (Cons x)      = x

getTerms :: Term -> [Term]
getTerms (Func _ ts) = ts
getTerms _           = []

-- Returns the list of variables of a Formula.
-- Note that I assure that x is Var whenever I had (Forall/Exists x ..)
-- and that should happend in all usages for Forall and Exists.

getVars :: Formula -> [Term]
getVars (Forall t f) = t:getVars f
getVars (Exists t f) = t:getVars f
getVars (Not f)      = getVars f
getVars (And f g)    = getVars f ++ getVars g
getVars (Or f g)     = getVars f ++ getVars g
getVars (Imp f g)    = getVars f ++ getVars g
getVars (Biimp f g)  = getVars f ++ getVars g
getVars (Pred i t)   = getVars_ t

-- Returns the list of variables [Var x] of a list of Terms.

getVars_ :: [Term] -> [Term]
getVars_ [] = []
getVars_ (x:xs) | isVar x  = x:getVars_ xs
                | isCons x = getVars_ xs
                | isFunc x = getVars_ (getTerms x) ++ getVars_ xs

-- All variables are unique, there are identified by their index.
-- x = Var Index.
-- The following method, find the maximum value for Index in
-- all variables of a formula.

maxIndex ::[Term] -> Int
maxIndex [] = 0
maxIndex (Var idx:xs) = max idx (maxIndex xs)


-- The following method provies a Index unused for the
-- variables in the formula. 
-- x = Var Index.
-- It helps to use new variables.

boundIndex :: Formula -> Int
boundIndex f = maxIndex (getVars f) + 1

-- The following method implements the definition 3.3.10 [van Dalen, 2013].
-- Replaces all ocurrences of a Term x in a Formula by the Term y.
--   ∀x Px ≈ ∀y Py,
--   ∃x Px ≈ ∃y Py.

replace :: Term -> Term -> Formula -> Formula
replace x y (Forall z f)
    | x == y                    = Forall z f
    | otherwise                 = Forall z $ replace x y f
replace x y (Exists z f)
    | x == y                    = Exists z f
    | otherwise                 = Exists z $ replace x y f
replace x y (Not f)             = Not $ replace x y f
replace x y (And f g)           = And (replace x y f) (replace x y g)
replace x y (Or f g)            = Or (replace x y f) (replace x y g)
replace x y (Pred idx ts)       = Pred idx $ replace_ x y ts
replace x y (Biimp f g)         = Biimp (replace x y f) (replace x y g)
replace x y (Imp f g)           = Imp (replace x y f) (replace x y g)

-- The following method implements the definition 3.3.9 [van Dalen, 2013].
-- Replaces all ocurrences of a Term X in a list of Term by the Term Y.

replace_ :: Term -> Term -> [Term] -> [Term]
replace_ x y (t:ts)
    | isVar t && t == x     = y:replace_ x y ts
    | isFunc t              = f:replace_ x y ts
    | otherwise             = t:replace_ x y ts
    where
        newterms :: [Term]
        newterms = replace_ x y $ getTerms t
        f        = Func (getInt t) newterms
replace_ x y [] = []


-- The following method implements the definition 3.3.6-7 [van Dalen, 2013].
-- Free variables of a Formula.

freeVars :: Formula -> [Term]
freeVars (Pred _ t)     = getVars_ t
freeVars (Not f)        = freeVars f
freeVars (Forall x f)   = freeVars f \\ [x]
freeVars (Exists x f)   = freeVars f \\ [x]
freeVars (And f g)      = freeVars f ++ freeVars g
freeVars (Or  f g)      = freeVars f ++ freeVars g
freeVars (Biimp  f g)   = freeVars f ++ freeVars g
freeVars (Imp  f g)     = freeVars f ++ freeVars g

-- Bound variables of a Formula.
-- Based on Definition 3.3.6 and Definition 3.3.7 [van Dalen, 2013].

boundVars :: Formula -> [Term]
boundVars (Pred _ t)  = []
boundVars (Not f)       = boundVars f
boundVars (Forall x f)  = boundVars f ++ [x]
boundVars (Exists x f)  = boundVars f ++ [x]
boundVars (And f g)     = boundVars f ++ boundVars g
boundVars (Or  f g)     = boundVars f ++ boundVars g
boundVars (Biimp  f g)  = boundVars f ++ boundVars g
boundVars (Imp  f g)    = boundVars f ++ boundVars g

-- The following method implements the theorem 3.5.2-3 [van Dalen, 2013].
-- Quantification over a variable that does not occur can be deleted.

simplifyQi :: Formula -> Formula
simplifyQi (Forall x f)
    | x `elem` freeVars f                   = Forall x f
    | otherwise                             = f
simplifyQi (Exists x f)
    | x `elem` freeVars f                   = Exists x f
    | otherwise                             = f
simplifyQi (And (Forall x f) (Forall y g))
    | x == y                                = Forall x $ And f g
    | otherwise                             = expr
    where
        expr :: Formula
        expr = And (Forall x f) (Forall y g)
simplifyQi (Or (Exists x f) (Exists y g))
    | x == y                                = Exists x $ Or f g
    | otherwise                             = expr
    where
        expr :: Formula
        expr = Or (Exists x f) (Exists y g)
simplifyQi (And (Exists x f) g)
    | x `elem` freeVars g                   = And (Exists x f) g
    | otherwise                             = Exists x $ And f g
simplifyQi (And f (Exists x g))             = simplifyQi $ And (Exists x g) f
simplifyQi (Or (Forall x f) g)
    | x `elem` freeVars g                   = Or (Forall x f) g
    | otherwise                             = Forall x $ Or f g
simplifyQi (Or f (Forall x g))              = simplifyQi $ Or (Forall x g) f
simplifyQi formula                          = formula

------------------------------------------------------------------------------
-- References

-- van Dalen, Dirk (2013). Logic and Structure. 5th ed. Springer.