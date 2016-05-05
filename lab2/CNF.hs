module CNF
    where

import FOL
import Data.List

-- In this module are implemented three of the four steps to get
-- the CNF from a formula in predicate logic.
-- This implementation is based on the algorithms from
-- [Huth and Ryan, 2004] and [Ben-Ari, 2012]


-- Removes bi-implications using the logical equivalence
-- α ⇒ β ≈ (α ⇒ β) ∧ (β ⇒ α).

remBiimp :: Formula -> Formula
remBiimp (Not f)       = Not $ remBiimp f
remBiimp (And f1 f2)   = And (remBiimp f1) (remBiimp f2)
remBiimp (Or f1 f2)    = Or (remBiimp f1) (remBiimp f2)
remBiimp (Imp f1 f2)   = Imp (remBiimp f1) (remBiimp f2)
remBiimp (Biimp f1 f2) = And (Imp nf1 nf2) (Imp nf2 nf1)
  where
    nf1, nf2 :: Formula
    nf1 = remBiimp f1
    nf2 = remBiimp f2
remBiimp (Exists x f)  = Exists x (remBiimp f)
remBiimp (Forall x f)  = Forall x (remBiimp f)
remBiimp formula       = formula

-- Removes implications using the logical equivalences
-- α ⇒ β ≈ ¬ α ∨ β.

remImp :: Formula -> Formula
remImp (Not f)      = Not $ remImp f
remImp (And f1 f2)  = And (remImp f1) (remImp f2)
remImp (Or f1 f2)   = Or (remImp f1) (remImp f2)
remImp (Imp f1 f2)  = Or (Not $ remImp f1) (remImp f2)
remImp (Exists x f) = Exists x (remImp f)
remImp (Forall x f) = Forall x (remImp f)
remImp formula      = formula


-- DeMorgan laws and push operators inward using the logical equivalences
--   ¬ (α ∧ β) ≈ ¬ α ∨ ¬ β,
--   ¬ (α ∨ β) ≈ ¬ α ∧ ¬ β,
--   ¬ ¬ α     ≈ α,
--   ¬ ∀x P(x) ≈ ∃x ¬ P(x),
--   ¬ ∃x P(x) ≈ ∀x ¬ P(x).

demorgan :: Formula -> Formula
demorgan (Not (And f1 f2))  = Or (demorgan $ Not f1) (demorgan $ Not f2)
demorgan (Not (Or f1 f2))   = And (demorgan $ Not f1) (demorgan $ Not f2)
demorgan (Not (Not f))      = demorgan f
demorgan (Not (Forall x f)) = Exists x (demorgan $ Not f)
demorgan (Not (Exists x f)) = Forall x (demorgan $ Not f)
demorgan (And f1 f2)        = And (demorgan f1) (demorgan f2)
demorgan (Or f1 f2)         = Or (demorgan f1) (demorgan f2)
demorgan (Exists x f)       = Exists x (demorgan f)
demorgan (Forall x f)       = Forall x (demorgan f)
demorgan formula            = formula

-- Distributive laws
-- (a ∧ b) ∨ c = (a ∨ c) ∧ (b ∨ c)
-- a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)

dist:: Formula -> Formula
dist (Not f)            = Not $ dist f
dist (And f1 f2)        = And (dist f1) (dist f2)
dist (Or f1 f2)         = distOr f1 f2
dist (Exists x f)       = Exists x (dist f)
dist (Forall x f)       = Forall x (dist f)
dist formula            = formula

-- An auxiliar method to deal with Or case of
-- distributive law.
distOr:: Formula -> Formula -> Formula
distOr (And f1 f2) f3 = And (distOr f1 f3) (distOr f2 f3)
distOr f1 (And f2 f3) = And (distOr f1 f2) (distOr f1 f3)
distOr f1 f2          = Or f1 f2

cnf :: Formula -> Formula
cnf = dist . demorgan . remImp . remBiimp

------------------------------------------------------------------------------
-- References

-- Ben-Ari, Mordechai (2012). Mathematical Logic for Computer
-- Science. 3rd ed. Springer.

-- Huth, Michael and Ryan, Mark (2004). Logic in Computer
-- Science. Modelling and Reasoning about Systems. 2nd ed. Cambridge
-- University Press.