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
remBiimp (Not f)            = Not $ remBiimp f
remBiimp (And f g)          = And (remBiimp f) (remBiimp g)
remBiimp (Or f g)           = Or (remBiimp f) (remBiimp g)
remBiimp (Imp f g)          = Imp (remBiimp f) (remBiimp g)
remBiimp (Biimp f g)        = And (Imp nf ng) (Imp ng nf)
    where
        nf, ng :: Formula
        nf = remBiimp f
        ng = remBiimp g
remBiimp (Exists x f)       = Exists x (remBiimp f)
remBiimp (Forall x f)       = Forall x (remBiimp f)
remBiimp formula            = formula

-- Removes implications using the logical equivalences
-- α ⇒ β ≈ ¬ α ∨ β.

remImp :: Formula -> Formula
remImp (Not f)               = Not $ remImp f
remImp (And f g)             = And (remImp f) (remImp g)
remImp (Or f g)              = Or (remImp f) (remImp g)
remImp (Imp f g)             = Or (Not $ remImp f) (remImp g)
remImp (Exists x f)          = Exists x (remImp f)
remImp (Forall x f)          = Forall x (remImp f)
remImp formula               = formula


-- DeMorgan laws and push operators inward using the logical equivalences
--   ¬ (α ∧ β) ≈ ¬ α ∨ ¬ β,
--   ¬ (α ∨ β) ≈ ¬ α ∧ ¬ β,
--   ¬     ¬ α ≈ α,
--   ¬ ∀x P(x) ≈ ∃x ¬ P(x),
--   ¬ ∃x P(x) ≈ ∀x ¬ P(x).

demorgan :: Formula -> Formula
demorgan (Not (And f g))     = Or (demorgan $ Not f) (demorgan $ Not g)
demorgan (Not (Or f g))      = And (demorgan $ Not f) (demorgan $ Not g)
demorgan (Not (Not f))       = demorgan f
demorgan (Not (Forall x f))  = Exists x (demorgan $ Not f)
demorgan (Not (Exists x f))  = Forall x (demorgan $ Not f)
demorgan (Forall x f)        = Forall x (demorgan f)
demorgan (Exists x f)        = Exists x (demorgan f)
demorgan (And f g)           = And (demorgan f) (demorgan g)
demorgan (Or f g)            = Or (demorgan f) (demorgan g)
demorgan formula             = formula

-- Distributive laws
-- (a ∧ b) ∨ c = (a ∨ c) ∧ (b ∨ c)
-- a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)

dist :: Formula -> Formula
dist (Not f)                = Not $ dist f
dist (Exists x f)           = Exists x $ dist f
dist (Forall x f)           = Forall x $ dist f
dist (And f g)              = And (dist f) (dist g)
dist (Or f g)               = distOr f g
dist formula                = formula

-- An auxiliar method to deal with Or case of
-- distributive law.

distOr :: Formula -> Formula -> Formula
distOr (And f g) h          = And (distOr f h) (distOr g h)
distOr f (And g h)          = And (distOr f g) (distOr f h)
distOr f g                  = Or f g

cnf :: Formula -> Formula
cnf = dist . demorgan . remImp . remBiimp

------------------------------------------------------------------------------
-- References

-- Ben-Ari, Mordechai (2012). Mathematical Logic for Computer
-- Science. 3rd ed. Springer.

-- Huth, Michael and Ryan, Mark (2004). Logic in Computer
-- Science. Modelling and Reasoning about Systems. 2nd ed. Cambridge
-- University Press.