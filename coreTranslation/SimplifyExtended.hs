{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-inaccessible-code #-}

module SimplifyExtended where

import ForthelDemo
import UtilitiesSimplifyEx

-- Convert an `if A, then B` to `assume A. Then B`
-- And a normal statement `A` becomes a statment with a `Then`
ifThenToAssume :: Tree c -> Tree c
ifThenToAssume t = case t of
    GStatementSection (GIfStatement antec prec) x -> GAssumptionsSection (GOneAssumptions $ GStatementAssumption antec) (GThenStatementSection prec (ifThenToAssume x))
    GStatementSection stm sec -> GThenStatementSection stm $ ifThenToAssume sec 
    _ -> composOp ifThenToAssume t

-- Convert a `let A` to `assume A`.
letToAssume :: Tree c -> Tree c
letToAssume t = case t of
    GLatexFormulaAssumption form -> GStatementAssumption (GLatexFormulaStatement form)
    GLetNamesAssumption name cn -> GNamesAssumption name cn
    _ -> composOp letToAssume t


-- DeLaTeXing and removes the extra parantheses -- not sure if this is a good step
deLatex :: Tree c -> Tree c
deLatex t = case t of
    GLatexFormulaStatement eq -> GFormulaStatement (deLatex eq)
    GLatexExpSymbTerm term -> GExpSymbTerm (deLatex term) 
    GLatexFormulaAssumption form -> GFormulaAssumption (deLatex form)
    GLatexFormulaSymbTerm form -> GFormulaSymbTerm (deLatex form)
    GLatexIndexedTerm int -> GIndexedTerm int
    GLatexVarName var -> GVarName var
    GLTFrac exp1 exp2 -> GTDiv (deLatex (GTParenth exp1)) (deLatex (GTParenth exp2))
    GLTPower exp1 exp2 -> GTExp (deLatex (GTParenth exp1)) (deLatex (GTParenth exp2))
    GLEGe -> GEGe
    GLELe -> GELe
    GLENeq -> GENeq
    GLESim -> GESim
    GLFElem lexp exp -> GFElem (deLatex lexp) (deLatex exp)
    GTParenth exp -> deLatex exp
    _ -> composOp deLatex t 



-- Splitting adjectives
splitAdj :: Tree c -> Tree c
splitAdj t = case t of
    GSimpleStatement term (GPosOnePredicates (GIsaPredicate (GClassNounNotion (GAdjClassNoun pcn adj)))) -> GAndStatement (GSimpleStatement term (mkPredFromPC pcn)) (GSimpleStatement term (mkPredFromAdj adj))
    _ -> composOp splitAdj t

-- Stack Assumptions through sections
-- So `assume A. assume B. Then C. Assume D. Assume E.` becomes `assume A and B. Then C. Assume D and assume E.`
stackAssume :: Tree c -> Tree c
stackAssume t = case t of
    GAssumptionsSection ass1 (GAssumptionsSection ass2 sec) -> GAssumptionsSection (concatAssump ass1 ass2) $ stackAssume sec
    _ -> composOp stackAssume t 

-- Splitting Ands in Statements of Assumptions,
-- (`asssume A and B` becomes `assume A and assume B`)
splitStmAnds :: Tree c -> Tree c
splitStmAnds t = case t of
    GOneAssumptions (GStatementAssumption (GAndStatement stm1 stm2)) -> GAddAssumptions (GStatementAssumption stm1) (GOneAssumptions (GStatementAssumption stm2))
    _ -> composOp splitStmAnds t

-- Changing EachTer, AllTerm, AnyTerm to EveryTerm
changeUniversals :: Tree c -> Tree c
changeUniversals t = case t of
    GEachTerm not -> GEveryTerm not
    GAllTerm not -> GEveryTerm not
    GAnyTerm not -> GEveryTerm not
    _ -> composOp changeUniversals t