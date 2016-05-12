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
getVars (Forall t f) = t : getVars f
getVars (Exists t f) = t : getVars f
getVars (Not f)      = getVars f
getVars (And f g)    = getVars f ++ getVars g
getVars (Or f g)     = getVars f ++ getVars g
getVars (Imp f g)    = getVars f ++ getVars g
getVars (Biimp f g)  = getVars f ++ getVars g
getVars (Pred _ t)   = getVarsTerm t

-- Returns the list of variables [Var x] of a list of Terms.

getVarsTerm :: [Term] -> [Term]
getVarsTerm (x : xs)
    | isVar x       = x : getVarsTerm xs
    | isCons x      = getVarsTerm xs
    | isFunc x      = getVarsTerm (getTerms x) ++ getVarsTerm xs
getVarsTerm _       = []
        
-- All variables are unique, there are identified by their index.
-- x = Var Index.
-- The following method, find the maximum value for Index in
-- all variables of a formula.

maxIndex ::[Term] -> Int
maxIndex (Var idx : xs) = max idx (maxIndex xs)
maxIndex _ = 0


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
replace x y (Not f)             = Not $ replace x y f
replace x y (And f g)           = And (replace x y f) (replace x y g)
replace x y (Or f g)            = Or (replace x y f) (replace x y g)
replace x y (Pred idx ts)       = Pred idx $ replaceTerm x y ts
replace x y (Biimp f g)         = Biimp (replace x y f) (replace x y g)
replace x y (Imp f g)           = Imp (replace x y f) (replace x y g)
replace x y (Forall z f)
    | x == y                    = Forall z f
    | otherwise                 = Forall z $ replace x y f
replace x y (Exists z f)
    | x == y                    = Exists z f
    | otherwise                 = Exists z $ replace x y f

-- The following method implements the definition 3.3.9 [van Dalen, 2013].
-- Replaces all ocurrences of a Term X in a list of Term by the Term Y.

replaceTerm :: Term -> Term -> [Term] -> [Term]
replaceTerm _ _ [] = []
replaceTerm x y (t : ts)
    | isVar t && t == x     = y : replaceTerm x y ts
    | isFunc t              = f : replaceTerm x y ts
    | otherwise             = t : replaceTerm x y ts
    where
        newterms :: [Term]
        newterms = replaceTerm x y $ getTerms t
        f        = Func (getInt t) newterms


-- This is the core of rename method in PCNF.hs.
-- It renames each bound variable based on a (index) candidate.
-- The index appropriate is given by the boundIndex method.
-- It works following a bottom-top method in the parse tree of the formula.

rectify :: Formula -> Int -> (Formula, Int)
rectify (Pred i t) start       = (Pred i t, start)
rectify (Forall x f) start     = (Forall y newf, end + 1)
    where
        midf, newf  :: Formula
        end         :: Int
        (midf, end) = rectify f start
        y           = Var end
        newf        = replace x y midf

rectify (Exists x f) start     = (Exists y newf, end + 1)
    where
        midf1, newf  :: Formula
        end         :: Int
        (midf1, end) = rectify f start
        y           = Var end
        newf        = replace x y midf1

rectify (Not f) start          = (Not newf, end)
    where
        newf        :: Formula
        end         :: Int
        (newf, end) = rectify f start

rectify (And f g) start        = (And newf1 newg, end)
    where
        newf1, newg   :: Formula
        (newg, next) = rectify g start
        (newf1, end)  = rectify f next

rectify (Or f g) start        = (Or newf newg1, end)
    where
        newf, newg1   :: Formula
        (newg1, next) = rectify g start
        (newf, end)  = rectify f next
rectify formula start = (formula, start)

-- The following method implements the definition 3.3.6-7 [van Dalen, 2013].
-- Free variables of a Formula.

freeVars :: Formula -> [Term]
freeVars (Pred _ t)     = getVarsTerm t
freeVars (Not f)        = freeVars f
freeVars (Forall x f)   = freeVars f \\ [x]
freeVars (Exists x f)   = freeVars f \\ [x]
freeVars (And f g)      = freeVars f ++ freeVars g
freeVars (Or  f g)      = freeVars f ++ freeVars g
freeVars (Biimp  f g)   = freeVars f ++ freeVars g
freeVars (Imp  f g)     = freeVars f ++ freeVars g

-- The following method implements the theorem 3.5.2-3 [van Dalen, 2013].
-- Quantification over a variable that does not occur can be deleted.

simplifyQi :: Formula -> Formula
simplifyQi (Forall x f)
    | x `elem` freeVars f                   = Forall x $ simplifyQi f
    | otherwise                             = simplifyQi f
simplifyQi (Exists x f)
    | x `elem` freeVars f                   = Exists x $ simplifyQi f
    | otherwise                             = simplifyQi f
simplifyQi (And (Forall x f) (Forall y g))
    | x == y                                = Forall x $ And f1 g1
    | otherwise                             = expr
    where
        f1, g1 :: Formula
        f1 = simplifyQi f
        g1 = simplifyQi g
        expr   :: Formula
        expr = And (Forall x f1) (Forall y g1)
simplifyQi (Or (Exists x f) (Exists y g))
    | x == y                                = Exists x $ Or f2 g2
    | otherwise                             = expr
    where
        f2, g2 :: Formula
        f2 = simplifyQi f
        g2 = simplifyQi g
        expr :: Formula
        expr = Or (Exists x f2) (Exists y g2)
simplifyQi (And (Exists x f) g)
    | x `elem` freeVars g                   = And (Exists x f3) g3
    | otherwise                             = Exists x $ And f3 g3
    where
        f3, g3 :: Formula
        f3 = simplifyQi f
        g3 = simplifyQi g
simplifyQi (And f (Exists x g))             = simplifyQi $ And (Exists x g) f
simplifyQi (Or (Forall x f) g)
    | x `elem` freeVars g                   = Or (Forall x f4) g4
    | otherwise                             = Forall x $ Or f4 g4
    where
        f4, g4 :: Formula
        f4 = simplifyQi f
        g4 = simplifyQi g
simplifyQi (Or f (Forall x g))              = simplifyQi $ Or (Forall x g) f
simplifyQi formula                          = formula

------------------------------------------------------------------------------
-- References
-- van Dalen, Dirk (2013). Logic and Structure. 5th ed. Springer.