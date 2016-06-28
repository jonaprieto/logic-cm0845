module Unify
    where

import           FOL

data UnifyError =
    DifferentPred
    | DifferentPredArity
    | DifferentFn
    | DifferentFnArity
    | FailRule4
    deriving (Eq, Read, Show)

unify :: Atom -> Atom -> Either UnifyError [Atom]
unify p@(Pred f xs) q@(Pred g ys)
    | f /= g                    = Left DifferentPred
    | length xs /= length ys    = Left DifferentPredArity
    | otherwise                 = solveform
    where
        solveform = case  unify' (zip xs ys) of
                Left x    -> Left x
                Right mgu -> Right [replaceWith p mgu, replaceWith q mgu]

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
-- unify' _ = undefined


vars :: Term -> [Term]
vars (Var x)    = [Var x]
vars (F _ xs)   = vars' xs
-- vars = undefined

vars' :: [Term] -> [Term]
vars' (Var x : xs)  = Var x : vars' xs
vars' (F _ ts : xs) = vars' ts ++ vars' xs
vars' []            = []
-- vars' = undefined

vars'' :: [(Term, Term)]-> [Term]
vars'' = vars' . uncurry (++) . unzip
-- vars'' = undefined

sub :: Term -> Term -> Term -> Term
sub (Var x) y (Var z)   = if x == z then y else Var z
sub (Var x) y (F f ts)  = F f (map (sub (Var x) y) ts)
sub _ _ t               = t
-- sub = undefined

sub' :: Term -> Term -> (Term, Term) -> (Term, Term)
sub' x y (t1, t2) = (sub x y t1, sub x y t2)
-- sub' = undefined

replaceWith :: Atom -> [(Term, Term)] -> Atom
replaceWith (Pred p ts) ((x, t):xs) = newts
    where
        newts'   = map (sub x t) ts
        newts    = replaceWith (Pred p newts') xs
replaceWith a [] = a
-- replaceWith = undefined