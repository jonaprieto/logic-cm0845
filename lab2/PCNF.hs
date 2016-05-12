-- @Author: JONATHAN STEVEN PRIETO CUBIDES
-- @Date: 2016-04-17 09:50:39

module PCNF
    where

import FOL
import CNF (dist, demorgan, remImp, remBiimp)
import Utils (rectify, simplifyQi, boundIndex)

-- Function to get the PCNF from a given formula in FOL.

pcnf :: Formula -> Formula
pcnf =  extract . dist . rename . demorgan . remImp . remBiimp

-- Function to get the PCNF from a given formula in FOL,
-- using theorem 3.5.2-3 [van Dalen, 2013] for simplify.

mypcnf :: Formula -> Formula
mypcnf = dist . simplifyQi . extract . rename  . demorgan . remImp . remBiimp

-- The following method generates the prenex form of formula given his CNF.
-- Extract the quantifiers from the inside to outside of the Formula.
-- Using the convention qᵢ= {∀x, ∃x} and ⊡ to denote a binary operation,
-- the defintion for the method extracts is as follows:
--
-- extract (qᵢf ⊡ g)   = qᵢ(extract (f ⊡ g))
-- extract (  f ⊡ qᵢg) = qᵢ(extract (f ⊡ g))
-- extract (qᵢf)       = qᵢ(extract f)
-- extract (f ⊡ g)     = combine (F ⊡ G)
--
-- where, F = extract f and G = extract g.
--
-- Note: We can extract with freedom the quantifiers in virtue the
-- rename method.

extract :: Formula -> Formula
extract (Forall x f)            = Forall x $ extract f
extract (Exists x f)            = Exists x $ extract f
extract (Or (Forall x f) g )    = Forall x $ extract (Or f g)
extract (Or (Exists x f) g )    = Exists x $ extract (Or f g)
extract (And (Exists x f) g )   = Exists x $ extract (And f g)
extract (And (Forall x f) g )   = Forall x $ extract (And f g)
extract (Or g (Forall x f))     = Forall x $ extract (Or g f)
extract (Or g (Exists x f))     = Exists x $ extract (Or g f)
extract (And g (Forall x f))    = Forall x $ extract (And g f)
extract (And g (Exists x f))    = Exists x $ extract (And g f)
extract (And f g)               = combineExtract $ And (extract f) (extract g)
extract (Or f g)                = combineExtract $ Or (extract f) (extract g)
extract formula                 = formula

combineExtract :: Formula -> Formula
combineExtract (Or (Forall x f) g)      = Forall x $ combineExtract (Or f g)
combineExtract (Or (Exists x f) g)      = Exists x $ combineExtract (Or f g)
combineExtract (And (Exists x f) g)     = Exists x $ combineExtract (And f g)
combineExtract (And (Forall x f) g)     = Forall x $ combineExtract (And f g)
combineExtract (Or g (Forall x f))      = Forall x $ combineExtract (Or g f)
combineExtract (Or g (Exists x f))      = Exists x $ combineExtract (Or g f)
combineExtract (And g (Forall x f))     = Forall x $ combineExtract (And g f)
combineExtract (And g (Exists x f))     = Exists x $ combineExtract (And g f)
combineExtract formula                  = formula

-- This method renames the variables of a formula using the theorem:
--   ∀x F ≈ ∀y F[x:=y]
--   ∃x F ≈ ∃y F[x:=y]
-- where y is a free-variable in F. The value of y variables is assigned
-- based on the last unused index for variables (boundIndex).
-- The outcome has all bound variables distinct, an advantage to apply
-- the theorems 3.5.2-3 [van Dalen, 2013].

rename :: Formula -> Formula
rename f = newf
    where
        newf :: Formula
        (newf, _) = rectify f (boundIndex f)