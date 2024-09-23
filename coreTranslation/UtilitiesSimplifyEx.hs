{-# LANGUAGE GADTs #-}
module UtilitiesSimplifyEx where

import ForthelDemo

-- Extracting Terms from Statements
extractTermsFromStatements :: GStatement -> GTerms
extractTermsFromStatements (GSimpleStatement term pred) = term

-- Making a Predicates from PrimClass
mkPredFromPC :: GClassNoun -> GPredicates
mkPredFromPC pc = GPosOnePredicates (GIsaPredicate (GClassNounNotion pc))

-- Making a Predicates from Adjective
mkPredFromAdj :: GAdjective -> GPredicates
mkPredFromAdj adj = GPosOnePredicates (GIsPredicate adj)

--Concatenating two Assumptions
concatAssump :: GAssumptions -> GAssumptions -> GAssumptions
concatAssump (GOneAssumptions ass) lass = GAddAssumptions ass lass 
concatAssump (GAddAssumptions ass lass1) lass2 = GAddAssumptions ass (concatAssump lass1 lass2)