module Unify
    where

import           FOL

unify :: Atom -> Atom -> [Atom]
unify p@(Pred f xs) q@(Pred g ys)
    | condition = []
    | otherwise = solveform
    where
        condition   = f /= g || (length xs /= length ys)
        eqs         = zip xs ys
        mgu         = unify' eqs
        newp        = replaceWith p mgu
        newq        = replaceWith q mgu
        solveform   = [newp, newq]

unify' :: [(Term, Term)] -> [(Term, Term)]
unify' ((F f ts, Var x):xs) = unify' xs ++ [(Var x, F f ts)]
unify' ((F f xs, F g ys):ws)
    | condition = []
    | otherwise = unify' $ neweqs ++ ws
    where
        condition = f /= g || length xs /= length ys
        neweqs    = zip xs ys
unify' ((Var x, Var y):xs)
    | x == y    = unify' xs
    | cond      = unify' newxs
    | otherwise = (Var x, Var y) : unify' xs
    where
        cond  = Var x `elem` vars'' xs
        trans = map (sub' (Var x) (Var y)) xs
        newxs = unify' [(Var x, Var y)] ++ trans

unify' ((Var x, F f ts):xs)
    | cond1     = []
    | cond2     = unify' newxs
    | otherwise = (Var x, F f ts) : unify' xs
    where
        cond1 = Var x `elem` vars' ts
        cond2 = Var x `elem` vars'' xs
        trans = map (sub' (Var x) (F f ts)) xs
        newxs = unify' trans ++ [(Var x, F f ts)]
unify' [] = []

vars :: Term -> [Term]
vars (Var x)    = [Var x]
vars (F _ xs)   = vars' xs

vars' :: [Term] -> [Term]
vars' (Var x : xs)  = Var x : vars' xs
vars' (F _ ts : xs) = vars' ts ++ vars' xs
vars' []            = []

vars'' :: [(Term, Term)]-> [Term]
vars'' = vars' . uncurry (++) . unzip

sub :: Term -> Term -> Term -> Term
sub (Var x) y (Var z)   = if x == z then y else Var z
sub (Var x) y (F f ts)  = F f (map (sub (Var x) y) ts)
sub _ _ t               = t

sub' :: Term -> Term -> (Term, Term) -> (Term, Term)
sub' x y (t1, t2) = (sub x y t1, sub x y t2)

replaceWith :: Atom -> [(Term, Term)] -> Atom
replaceWith (Pred p ts) ((x, t):xs) = newts
    where
        newts'   = map (sub x t) ts
        newts    = replaceWith (Pred p newts') xs
replaceWith a [] = a