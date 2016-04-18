module Examples where

import PCNF

-- Some examples of formulas in First-Order Logic
-- using the data type defined in FOL.hs

-- Variables

a = Var 0
x = Var 1
y = Var 2

-- Predicates p(x), q(x), r(x,y), s

p, q, r, s :: Formula
p = Pred 3 [x]
q = Pred 4 [x]
r = Pred 5 [x,y]
s = Pred 6 []

-- Some formulas with quantifiers
-- ∀x p(x)
-- ∀x (∃y r(x,y) ∧ s)

f1, f2 :: Formula
f1 = Forall x p
f2 = Forall x (And (Exists y r) s)

f3 = (Forall x (Exists y (Or p (And q (Not r)))))
