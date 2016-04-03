module PCNF where

import FOL
import CNF

-- Function to get the PCNF from a given formula in FOL.

pcnf :: Formula -> Formula
pcnf = dist . extract . demorgan . remImp . remBiimp . renameFormula

-- TODO renameFormula: Function to rename the bound variables, such that
-- there is not a variables binded by two differente quantifiers.

renameFormula :: Formula -> Formula
renameFormula f = undefined

-- TODO extract: Function to extract the quantifiers from the matrix to
-- prefix.

extract :: Formula -> Formula
extract f = undefined

-- TODO dist: Application of the distributive laws.

dist :: Formula -> Formula
dist f = undefined
