-- @Author: Jonathan Prieto
-- @Date:   2016-06-28 11:10:10
-- @Last Modified by:   Jonathan Prieto
-- @Last Modified time: 2016-06-28 21:36:21
module Unify
    where

import           FOL

-- The type of error for unify' and unify functions.
-- When two atoms are not unifiable, they returns one
-- of these errors.
data UnifyError =
    DifferentPred
    | DifferentPredArity
    | DifferentFn
    | DifferentFnArity
    | FailRule4
    deriving (Eq, Read, Show)

-- The function unify is the implementation of Martelli-Montanari
-- algorithm. First, it evaluates the conditions over the predicates,
-- and second, if they pass, it calls the unify' function to do the rest of the
-- algorithm. If something was wrong, it returns an error of UnifyError type.
unify :: Atom -> Atom -> Either UnifyError [Atom]
unify p@(Pred f xs) q@(Pred g ys)
    | f /= g                    = Left DifferentPred
    | length xs /= length ys    = Left DifferentPredArity
    | otherwise                 = solveform
    where
        solveform = case  unify' (zip xs ys) of
                Left x    -> Left x
                Right mgu -> Right [replaceWith p mgu, replaceWith q mgu]

-- The function unify' applies the steps of the Martelli-Montanari algorithm
-- a set of rules. When it fails to find out the most general unifies,
-- it returns one of the errors of UnifyError type.
-- When it succeded, it returns a substituion, given by
-- a list of pairs of terms (variable <- term).
unify' :: [(Term, Term)] -> Either UnifyError [(Term, Term)]
unify' ((F f ts, Var x):xs)     = unify' $ xs ++ [(Var x, F f ts)]
unify' ((F f xs, F g ys):ws)
    | f /= g                    = Left DifferentFn
    | length xs /= length ys    = Left DifferentFnArity
    | otherwise                 = unify' $ zip xs ys ++ ws
unify' ((Var x, Var y):xs)
    | x == y                    = unify' xs
    | Var x `elem` vars'' xs    = unify' $ trans ++ [(Var x, Var y)]
    | otherwise                 = ans
    where
        trans = map (sub' (Var x) (Var y)) xs
        ans   = case unify' xs of
                    Left err   -> Left err
                    Right mgu  -> Right $ (Var x, Var y) : mgu
unify' ((Var x, F f ts):xs)
    | Var x `elem` vars' ts     = Left FailRule4
    | Var x `elem` vars'' xs    = unify' $ trans ++ [(Var x, F f ts)]
    | otherwise                 = ans
    where
        trans = map (sub' (Var x) (F f ts)) xs
        ans   = case unify' xs of
                    Left err  -> Left err
                    Right mgu -> Right $ (Var x, F f ts) : mgu
unify' [] = Right []

-- The function vars returns all vars from a term
vars :: Term -> [Term]
vars (Var x)    = [Var x]
vars (F _ xs)   = vars' xs

-- The function vars' returns all vars from a list of terms 
vars' :: [Term] -> [Term]
vars' (Var x : xs)  = Var x : vars' xs
vars' (F _ ts : xs) = vars' ts ++ vars' xs
vars' []            = []

-- The function vars'' returns all vars from a set of equations
-- given by a list of pairs of terms
vars'' :: [(Term, Term)]-> [Term]
vars'' = vars' . uncurry (++) . unzip

-- The function sub replaces a term x by y in all occurrencies
-- in a term
sub :: Term -> Term -> Term -> Term
sub (Var x) y (Var z)   = if x == z then y else Var z
sub (Var x) y (F f ts)  = F f (map (sub (Var x) y) ts)
sub _ _ t               = t

-- The function sub' replaces a term x by y in all occurrencies
-- in a pair of terms 
sub' :: Term -> Term -> (Term, Term) -> (Term, Term)
sub' x y (t1, t2) = (sub x y t1, sub x y t2)

-- The function replaceWith applies a substitution to an atom.
-- The substituion is given by a list of pairs of terms.
replaceWith :: Atom -> [(Term, Term)] -> Atom
replaceWith (Pred p ts) ((x, t):xs) = newts
    where
        newts'   = map (sub x t) ts
        newts    = replaceWith (Pred p newts') xs
replaceWith a [] = a