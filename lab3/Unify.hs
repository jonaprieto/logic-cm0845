module Unify
    where

import FOL

unify :: Atom -> Atom -> [Atom]
unify pp@(Pred p xs) qq@(Pred q ys)
    | condition = []
    | otherwise = solveform
    where
        condition   = p /= q || (length xs /= length ys)
        terms       = zip xs ys
        rules       = unify' terms
        newpp       = replaceWith pp rules
        newqq       = replaceWith qq rules
        solveform   = [newpp, newqq]

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
    | cond = unify' newxs
    | otherwise = (Var x, Var y) : unify' xs
    where
        cond  = x `elem` allVars xs
        trans = replaceVar (Var x) (Var y) xs
        newxs = unify' [(Var x, Var y)] ++ trans

unify' ((Var x, F f ts):xs)
    | cond1     = []
    | cond2     = unify' newxs
    | otherwise = (Var x, F f ts) : unify' xs
    where
        cond1 = x `elem` getVars ts
        cond2 = x `elem` allVars xs
        trans = replaceVar (Var x) (F f ts) xs
        newxs = unify' trans ++ [(Var x, F f ts)]

allVars :: [(Term, Term)]-> [String]
allVars xs = getVars $ (flat . unzip) xs
    where flat = uncurry (++)

getVars :: [Term] -> [String]
getVars (Var x : xs)    = x : getVars xs
getVars (F _ ts : xs)   = getVars ts ++ getVars xs

replaceVar :: Term -> Term -> [(Term, Term)] -> [(Term, Term)]
replaceVar x y ((z, F g ys):ws)
    | x == z    = (y, F g newys) : replaceVar x y ws
    | otherwise = []
    where
        newys = replaceVar' x y ys

replaceVar' :: Term -> Term -> [Term] -> [Term]
replaceVar' x y (Var z:ts)
    | x == Var z     = y : replaceVar' x y ts
    | otherwise      = Var z : replaceVar' x y ts
replaceVar' x y (F f xs:ts) = newf : replaceVar' x y ts
    where
        newf = F f (replaceVar' x y xs)


replaceWith :: Atom -> [(Term, Term)] -> Atom
replaceWith (Pred p ts) ((x, newx):xs) = newts
    where
        newts'   = replaceVar' x newx ts
        newts    = replaceWith (Pred p newts') xs
replaceWith a [] = a