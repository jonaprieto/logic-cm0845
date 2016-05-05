-- @Author: JONATHAN STEVEN PRIETO CUBIDES
-- @Date: 2016-04-17 09:50:39
-- @Last Modified time: 2016-05-05 14:22:59

module PCNF
    where

import FOL
import CNF
import Utils

-- Function to get the PCNF from a given formula in FOL.

pcnf :: Formula -> Formula
pcnf = dist . extract . demorgan . remImp . remBiimp . rectify
--pcnf = demorgan . remImp . remBiimp . rectify

-- Extract the quantifiers from the inside to outside of the Formula.

extract :: Formula -> Formula
extract (Forall x f)            = (Forall x (extract f))
extract (Exists x f)            = (Exists x (extract f))
extract (Or (Forall x f) g )    = (Forall x (extract (Or f g)))
extract (Or (Exists x f) g )    = (Exists x (extract (Or f g)))
extract (And (Exists x f) g )   = (Exists x (extract (And f g)))
extract (And (Forall x f) g )   = (Forall x (extract (And f g)))
extract (Or g (Forall x f))     = (Forall x (extract (Or g f)))
extract (Or g (Exists x f))     = (Exists x (extract (Or g f)))
extract (And g (Forall x f))    = (Forall x (extract (And g f)))
extract (And g (Exists x f))    = (Exists x (extract (And g f)))
extract (And f g)               = extract_ (And (extract f) (extract g))
extract (Or f g)                = extract_ (Or (extract f) (extract g))
extract formula                 = formula

extract_ :: Formula -> Formula
extract_ (Or (Forall x f) g )    = (Forall x (extract_ (Or f g)))
extract_ (Or (Exists x f) g )    = (Exists x (extract_ (Or f g)))
extract_ (And (Exists x f) g )   = (Exists x (extract_ (And f g)))
extract_ (And (Forall x f) g )   = (Forall x (extract_ (And f g)))
extract_ (Or g (Forall x f))     = (Forall x (extract_ (Or g f)))
extract_ (Or g (Exists x f))     = (Exists x (extract_ (Or g f)))
extract_ (And g (Forall x f))    = (Forall x (extract_ (And g f)))
extract_ (And g (Exists x f))    = (Exists x (extract_ (And g f)))
extract_ formula                 = formula

-- Rectify the formula using the equivalences
-- ∀x F ≈ ∀y F[x:=y]
-- ∃x F ≈ ∃y F[x:=y]
-- where y is a free-variable in F.

rectify::Formula -> Formula
rectify f = newf
    where
        bound       = boundIndex f
        (newf, end) = rectify2 f bound

-- Auxiliar method of the `rectify` method.
-- It renames each variable (index) based on a number (index) candidate.
-- The index appropriate is given by the boundIndex method.

rectify2::Formula -> Int -> (Formula, Int)
rectify2 (Forall x f) start     = ( (Forall y newf), end + 1)
                                where
                                    midf, newf :: Formula
                                    (midf, end) = rectify2 f start
                                    y           = Var end
                                    newf        = replace x y midf

rectify2 (Exists x f) start     = ( (Exists y newf), end + 1)
                                where
                                    midf, newf :: Formula
                                    (midf, end) = rectify2 f start
                                    y           = Var end
                                    newf        = replace x y midf

rectify2 (Not f) start          = (Not newf, end)
                                where
                                    newf :: Formula
                                    (newf, end) = rectify2 f start

rectify2 (And f g) start        = ((And newf newg), end)
                                where
                                    newf, newg :: Formula
                                    (newg, next)    = rectify2 g start
                                    (newf, end)     = rectify2 f next

rectify2 (Or f g) start        = ((Or newf newg), end)
                                where
                                    newf, newg :: Formula
                                    (newg, next)    = rectify2 g start
                                    (newf, end)     = rectify2 f next

rectify2 (Imp f g) start        = ((Imp newf newg), end)
                                where
                                    newf, newg:: Formula
                                    (newg, next)    = rectify2 g start
                                    (newf, end)     = rectify2 f next

rectify2 (Biimp f g) start       = ((Biimp newf newg), end)
                                where
                                    newf, newg :: Formula
                                    (newg, next)    = rectify2 g start
                                    (newf, end)     = rectify2 f next

rectify2 (Pred idx ts) start    = ((Pred idx ts), start)