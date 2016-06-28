-- @Author: JONATHAN STEVEN PRIETO CUBIDES
-- @Date: 2016-04-17 13:53:04

module Test
    where
import Test.HUnit

import FOL
import Unify
-- x₀,x₁,x₂
x,y,z :: Term
x   = Var 0
y   = Var 1
z   = Var 2

-- Px₀, Px₁, Px₂
px  = Pred 0 [x]
py  = Pred 1 [y]
pz  = Pred 2 [z]

-- Qx₀, Qx₁, Qx₂
qx  = Pred 3 [x]
qy  = Pred 4 [y]
qz  = Pred 5 [z]

-- Rx₀, Rx₁, Rx₂
rx  = Pred 6 [x]
ry  = Pred 7 [y]
rz  = Pred 8 [z]

-- Test #0
-- Input:  ∀x₀ Px₀
-- Output: ∀x₁ Px₁


-- Test #1
-- Input:  ¬ ∃x₀ Px₀ → (∀×ₒ Pxₒ)
-- Output: ∀x₂ ∃x₁ (Px₂ ∧ ¬ Px₁)

-- a =
-- b =
-- r =
-- test1   = TestCase (assertEqual "f1" (unify a b) r)

-- tests  = TestList [test1]
