-- @Author: Jonathan Prieto
-- @Date:   2016-06-28 18:06:43
-- @Last Modified by:   Jonathan Prieto
-- @Last Modified time: 2016-06-28 19:12:12
module Test
    where
import           Test.HUnit

import           FOL (Atom(..), Term(..))
import           Unify (unify, UnifyError(..))

x,y,z,w:: Term
x   = Var "x"
y   = Var "y"
z   = Var "z"
w   = Var "w"

ff1 = F "g" [y]
ff2 = F "f" [x, F "h" [x], y]
ff3 = F "f" [F "g" [z], w, z]

p, p' :: Atom
p    = Pred "p" [F "g" [y], F "f" [x, F "h" [x], y]]
p'   = Pred "p" [x, F "f" [F "g" [z], w, z]]
ansp = Right
    [Pred "p"
        [F "g" [Var "z"],
        F "f" [F "g" [Var "z"],F "h" [F "g" [Var "z"]],Var "z"]],
    Pred "p"
        [F "g" [Var "z"],
        F "f" [F "g" [Var "z"],F "h" [F "g" [Var "z"]],Var "z"]]]
t1  = TestCase (assertEqual "p p'" (unify p p') ansp)

q, q' :: Atom
q    = Pred "p" [F "f" [y, z, F "g" [y, z, x]]]
q'   = Pred "p" [x]
ansq = Left FailRule4
t2   = TestCase (assertEqual "q q'" (unify q q') ansq)

r, r' :: Atom
r    = Pred "p" [F "f" [z], F "g" [x]]
r'   = Pred "p" [F "f" [y], F "g" [F "g" [z]]]
ansr = Right
    [Pred "p"
        [F "f" [Var "y"],
        F "g" [F "g" [Var "y"]]],
    Pred "p"
        [F "f" [Var "y"],
        F "g" [F "g" [Var "y"]]]]

t3   = TestCase (assertEqual "r r'" (unify r r') ansr)

a, b, c, d :: Atom
a   = Pred "a" [x]
b   = Pred "b" [x]
c   = Pred "a" [x,y]
d   = Pred "a" [y]
tpred   = TestCase (
        assertEqual "dpred" (unify a b) (Left DifferentPred))
tarity  = TestCase (
        assertEqual "darity" (unify a c) (Left DifferentPredArity))

ans3 = Right [Pred "a" [Var "y"],Pred "a" [Var "y"]]
tbasic  = TestCase (
        assertEqual "basic1" (unify a d) ans3)

-- other examples taken from Ben-ari's Book, Pag. 202.
ua1 = Pred "p" [w, x, F "f" [F "g" [y]]]
ua2 = Pred "p" [y, F "f" [z], F "f" [z]]

ub1 = Pred "p" [x, F "g" [F "f" [w]], F "f" [x]]
ub2 = Pred "p" [F "f" [w], y, y]
tub = TestCase ( assertEqual "tub" (unify ub1 ub2) (Left DifferentFn))

uc1 = Pred "p" [x, F "g" [F "f" [w]], F "f" [x]]
uc2 = Pred "p" [F "f" [y], z, y]
tuc = TestCase ( assertEqual "tuc" (unify uc1 uc2) (Left FailRule4))

ud1 = Pred "p" [w, x, F "f" [F "g" [y]]]
ud2 = Pred "p" [z, F "h" [z,Var "u"],F "f" [Var "u"]]

tests = TestList [t1,t2,t3,tpred,tarity,tbasic,tub, tuc]