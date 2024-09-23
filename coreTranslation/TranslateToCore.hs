{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TranslateToCore where

import ForthelDemo
import CoreDemo



{---------------------------------------------
Translation types:

ForthelDemo.GVar -> CoreDemo.GVar 
ForthelDemo.GInt -> CoreDemo.GInt

ForthelDemo.GVerb -> CoreDemo.GPredicate 
ForthelDemo.GPredicates -> CoreDemo.GPredicate
ForthelDemo.GPredicate -> CoreDemo.GPredicate

ForthelDemo.GStatement -> CoreDemo.GProposition
ForthelDemo.GFormula -> CoreDemo.GProposition
ForthelDemo.GEquation -> CoreDemo.GProposition

ForthelDemo.GAdjective -> CoreDemo.GModifier 

ForthelDemo.GConst -> CoreDemo.GKind 
ForthelDemo.GPrimClass -> CoreDemo.GKind
ForthelDemo.GNotion -> CoreDemo.GKind
ForthelDemo.GClassNoun -> CoreDemo.GKind

ForthelDemo.GTerms -> CoreDemo.GTerm
ForthelDemo.GExp -> CoreDemo.GTerm 
ForthelDemo.GTerm -> CoreDemo.GTerm 
ForthelDemo.GSymbTerm -> CoreDemo.GTerm 

ForthelDemo.GToplevel -> CoreDemo.GBlock

ForthelDemo.GSection -> CoreDemo.GExample

ForthelDemo.GAssumption -> CoreDemo.GAssumption

--------------------------------------------------------------}


translateVar :: ForthelDemo.GVar -> CoreDemo.GVar
translateVar var = case var of
    ForthelDemo.Gx_Var -> CoreDemo.Gx_Var
    ForthelDemo.Gy_Var -> CoreDemo.Gy_Var
    ForthelDemo.Gz_Var -> CoreDemo.Gz_Var
    ForthelDemo.Gu_Var -> CoreDemo.Gu_Var
    ForthelDemo.Ga_Var -> CoreDemo.Ga_Var
    ForthelDemo.Gb_Var -> CoreDemo.Gb_Var
    ForthelDemo.Gc_Var -> CoreDemo.Gc_Var
    ForthelDemo.Gd_Var -> CoreDemo.Gd_Var
    ForthelDemo.Gf_Var -> CoreDemo.Gf_Var
    ForthelDemo.Gg_Var -> CoreDemo.Gg_Var
    ForthelDemo.Gk_Var -> CoreDemo.Gk_Var
    ForthelDemo.Gm_Var -> CoreDemo.Gm_Var
    ForthelDemo.Gn_Var -> CoreDemo.Gn_Var
    ForthelDemo.Gp_Var -> CoreDemo.Gp_Var
    ForthelDemo.Gq_Var -> CoreDemo.Gq_Var
    ForthelDemo.Gr_Var -> CoreDemo.Gr_Var
    ForthelDemo.Gs_Var -> CoreDemo.Gs_Var
    ForthelDemo.Gt_Var -> CoreDemo.Gt_Var
    ForthelDemo.GA_Var -> CoreDemo.GA_Var
    ForthelDemo.GB_Var -> CoreDemo.GB_Var
    ForthelDemo.GC_Var -> CoreDemo.GC_Var
    ForthelDemo.GK_Var -> CoreDemo.GK_Var
    ForthelDemo.GL_Var -> CoreDemo.GL_Var
    ForthelDemo.GM_Var -> CoreDemo.GM_Var
    ForthelDemo.GS_Var -> CoreDemo.GS_Var
    ForthelDemo.GT_Var -> CoreDemo.GT_Var

translateInt :: ForthelDemo.GInt -> CoreDemo.GInt
translateInt (ForthelDemo.GInt x) = CoreDemo.GInt x

-- converge, divide missing
translateVerb :: ForthelDemo.GVerb -> CoreDemo.GPredicate
translateVerb verb = case verb of
    Gdivide_Verb term -> Gdivides (translateTerm term)
    Gbelong_Verb term -> Gbelong (translateTerm term)

-- Partial function : dividing missing 
translateAdjective :: ForthelDemo.GAdjective -> CoreDemo.GModifier
translateAdjective adj = case adj of
    Gprime_Adjective -> Gprime
    Gequal_Adjective term -> Geq (translateTerm term)
    Gless_Adjective term -> Gless (translateTerm term)
    Ggreater_Adjective term -> Ggreater (translateTerm term)
    Grational_Adjective -> Grational
    Godd_Adjective -> Godd
    Geven_Adjective -> Geven
    Gnegative_Adjective -> Gneg
    Gnonnegative_Adjective -> Gnonnegative
    Gpositive_Adjective -> Gpositive
    Gless_or_equal_Adjective term -> Gleq (translateTerm term)
    Ggreater_or_equal_Adjective term -> Ggeq (translateTerm term)

translatePrimClass :: ForthelDemo.GPrimClass -> CoreDemo.GKind
translatePrimClass primClass = case primClass of
    ForthelDemo.Gset_PrimClass -> Gset
    ForthelDemo.Ginteger_PrimClass -> Ginteger
    ForthelDemo.Gnumber_PrimClass -> Gnumber

-- Partial Function
translateEquation :: ForthelDemo.GEquation -> CoreDemo.GProposition
translateEquation equation = case equation of
    GEBinary eqsign exp1 exp2 -> case eqsign of
        GEEq -> GTermPred (translateExp exp1) (GModPred (Geq (translateExp exp2)))
        GEGe -> GTermPred (translateExp exp1) (GModPred (Ggeq (translateExp exp2)))
        GELe -> GTermPred (translateExp exp1) (GModPred (Gleq (translateExp exp2)))
        GELt -> GTermPred (translateExp exp1) (GModPred (Gless (translateExp exp2)))
        GENeq -> GTermPred (translateExp exp1) (GModPred (Gneq (translateExp exp2)))
        GEGt -> GTermPred (translateExp exp1) (GModPred (Ggreater (translateExp exp2)))

-- TApp missing
translateExp :: ForthelDemo.GExp -> CoreDemo.GTerm
translateExp exp = case exp of
    GTPlus exp1 exp2 -> Gplus (translateExp exp1) (translateExp exp2)
    GTMinus exp1 exp2 -> Gminus (translateExp exp1) (translateExp exp2)
    GTTimes exp1 exp2 -> Gtimes (translateExp exp1) (translateExp exp2)
    GTDiv exp1 exp2 -> Gdiv (translateExp exp1) (translateExp exp2)
    GTExp exp1 exp2 -> Gexp (translateExp exp1) (translateExp exp2)
    GTNeg exp -> GnegT (translateExp exp)
    GTVar var -> GVarTerm (translateVar var)
    GTNumber int -> GIntTerm (translateInt int)

translateConst :: ForthelDemo.GConst -> CoreDemo.GKind
translateConst const = case const of
    GN_Const -> GModKind Gnatural Gnumber
    GZ_Const -> Ginteger
    GQ_Const -> GModKind Grational Gnumber
    GR_Const -> GModKind Greal Gnumber


translateSymbTerm :: ForthelDemo.GSymbTerm -> CoreDemo.GTerm
translateSymbTerm symbTerm = case symbTerm of
    GFormulaSymbTerm form -> GPropTerm (translateFormula form)
    GExpSymbTerm exp -> translateExp exp

-- Partial Function
translateTerm :: ForthelDemo.GTerm -> CoreDemo.GTerm
translateTerm term = case term of
    GSymbolicTerm symbTerm -> translateSymbTerm symbTerm
    --GEveryTerm notion -> 
    --GSomeTerm notion ->
    --GNoTerm notion -> 

-- Partial Function
translateFormula :: ForthelDemo.GFormula -> CoreDemo.GProposition
translateFormula formula = case formula of
    GFEquation eqn ->  translateEquation eqn
    GFElem (GListExp [exp1]) (GTConst const) -> GTermPred (GVarTerm CoreDemo.GA_Var) (GKindPred (translateConst const))
    -- GFElem lexp exp

translateToplevel :: ForthelDemo.GToplevel -> CoreDemo.GBlock
translateToplevel toplevel = case toplevel of
    GSectionToplevel header sec -> GExmBl (translateSection sec)


-- Simplification in the extended language guarantees that these two are the only possibilities
translateSection :: ForthelDemo.GSection -> CoreDemo.GExample
translateSection section = case section of
    GAssumptionsSection assums (GThenStatementSection stm GEmptySection) -> GAssExm (translateAssumptions assums) (translateStatement stm)
    GThenStatementSection stm GEmptySection -> GAssExm GBAssumption $ translateStatement stm


translateAssumptions :: ForthelDemo.GAssumptions -> CoreDemo.GLAssumption
translateAssumptions assumptions = case assumptions of
    GOneAssumptions assump -> GCAssumption (translateAssumption assump) GBAssumption
    GAddAssumptions assump lassump -> GCAssumption (translateAssumption assump) (translateAssumptions lassump)

-- Partial
translateAssumption :: ForthelDemo.GAssumption -> CoreDemo.GAssumption
translateAssumption assumption = case assumption of
    GStatementAssumption stm -> GPropAssump $ translateStatement stm
    GFormulaAssumption form -> GPropAssump $ translateFormula form


translateTerms :: ForthelDemo.GTerms -> CoreDemo.GTerm
translateTerms terms = case terms of
    GOneTerms term -> translateTerm term
    GAddTerms term lterm -> GAndTerm (translateTerm term) (translateTerms lterm)

-- Partial
translateStatement :: ForthelDemo.GStatement -> CoreDemo.GProposition
translateStatement statement = case statement of
    GAndStatement stm1 stm2 -> GAndProp (translateStatement stm1) (translateStatement stm2)
    GOrStatement stm1 stm2 -> GOrProp (translateStatement stm1) (translateStatement stm2)
    GIfStatement stm1 stm2 -> GIfProp (translateStatement stm1) (translateStatement stm2)
    GIffStatement stm1 stm2 -> GIffProp (translateStatement stm1) (translateStatement stm2)
    GForStatement terms stm -> GQuantProp (translateTerms terms) (translateStatement stm)
    GFormulaStatement form -> translateFormula form
    GSimpleStatement term pred -> GTermPred (translateTerms term) (translatePredicates pred)
    -- GExistStatement terms stm etc.  


translatePredicates :: ForthelDemo.GPredicates -> CoreDemo.GPredicate
translatePredicates predicates = case predicates of
    GPosOnePredicates pred -> translatePredicate pred
    GNegOnePredicates pred -> GNegPred (translatePredicate pred)
    GPosAddPredicates pred lpred -> GAndPred (translatePredicate pred) (translatePredicates lpred)
    GNegAddPredicates pred lpred -> GAndPred (GNegPred (translatePredicate pred)) (translatePredicates lpred)

-- Partial Function
translatePredicate :: ForthelDemo.GPredicate -> CoreDemo.GPredicate
translatePredicate pred = case pred of
    GDoesPredicate verb -> translateVerb verb
    GIsPredicate adj -> GModPred (translateAdjective adj)
    GIsaPredicate notion -> GKindPred (translateNotion notion)
    -- GIsaPredicate primClass etc.

-- Partial Function
translateNotion :: ForthelDemo.GNotion -> CoreDemo.GKind
translateNotion notion = case notion of
    GClassNounNotion classNoun -> translateClassNoun classNoun

    -- GClassNounName
-- Partial
translateClassNoun :: ForthelDemo.GClassNoun -> CoreDemo.GKind
translateClassNoun classNoun = case classNoun of
    GPrimClassNoun primClass -> translatePrimClass primClass
    GAdjClassNoun cn adj -> GModKind (translateAdjective adj) (translateClassNoun cn)
    
