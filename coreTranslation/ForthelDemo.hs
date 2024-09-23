{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module ForthelDemo where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GAdjective = Tree GAdjective_
data GAdjective_
type GAssumption = Tree GAssumption_
data GAssumption_
type GAssumptions = Tree GAssumptions_
data GAssumptions_
type GClassNoun = Tree GClassNoun_
data GClassNoun_
type GConst = Tree GConst_
data GConst_
type GConstant = Tree GConstant_
data GConstant_
type GDefiniteNoun = Tree GDefiniteNoun_
data GDefiniteNoun_
type GDefinition = Tree GDefinition_
data GDefinition_
type GEqsign = Tree GEqsign_
data GEqsign_
type GEquation = Tree GEquation_
data GEquation_
type GExp = Tree GExp_
data GExp_
type GFormula = Tree GFormula_
data GFormula_
type GFunction = Tree GFunction_
data GFunction_
type GHeader = Tree GHeader_
data GHeader_
type GListExp = Tree GListExp_
data GListExp_
type GListName = Tree GListName_
data GListName_
type GName = Tree GName_
data GName_
type GNotion = Tree GNotion_
data GNotion_
type GNotions = Tree GNotions_
data GNotions_
type GPredicate = Tree GPredicate_
data GPredicate_
type GPredicates = Tree GPredicates_
data GPredicates_
type GPrimClass = Tree GPrimClass_
data GPrimClass_
type GSection = Tree GSection_
data GSection_
type GStatement = Tree GStatement_
data GStatement_
type GSymbTerm = Tree GSymbTerm_
data GSymbTerm_
type GSynonym = Tree GSynonym_
data GSynonym_
type GTerm = Tree GTerm_
data GTerm_
type GTermSymb = Tree GTermSymb_
data GTermSymb_
type GTerms = Tree GTerms_
data GTerms_
type GToplevel = Tree GToplevel_
data GToplevel_
type GVar = Tree GVar_
data GVar_
type GVerb = Tree GVerb_
data GVerb_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  Gdividing_Adjective :: GTerm -> Tree GAdjective_
  Gequal_Adjective :: GTerm -> Tree GAdjective_
  Geven_Adjective :: Tree GAdjective_
  Ggeneral_A :: Tree GAdjective_
  Ggreater_Adjective :: GTerm -> Tree GAdjective_
  Ggreater_or_equal_Adjective :: GTerm -> Tree GAdjective_
  Gless_Adjective :: GTerm -> Tree GAdjective_
  Gless_or_equal_Adjective :: GTerm -> Tree GAdjective_
  Glinear_A :: Tree GAdjective_
  Gnegative_Adjective :: Tree GAdjective_
  Gnonnegative_Adjective :: Tree GAdjective_
  Godd_Adjective :: Tree GAdjective_
  Gpositive_Adjective :: Tree GAdjective_
  Gprime_Adjective :: Tree GAdjective_
  Grational_Adjective :: Tree GAdjective_
  Greal_Adjective :: Tree GAdjective_
  GFormulaAssumption :: GFormula -> Tree GAssumption_
  GLatexFormulaAssumption :: GFormula -> Tree GAssumption_
  GLetNamesAssumption :: GListName -> GClassNoun -> Tree GAssumption_
  GNamesAssumption :: GListName -> GClassNoun -> Tree GAssumption_
  GStatementAssumption :: GStatement -> Tree GAssumption_
  GAddAssumptions :: GAssumption -> GAssumptions -> Tree GAssumptions_
  GOneAssumptions :: GAssumption -> Tree GAssumptions_
  GAdjClassNoun :: GClassNoun -> GAdjective -> Tree GClassNoun_
  GPrimClassNoun :: GPrimClass -> Tree GClassNoun_
  GRelClassNoun :: GClassNoun -> GPredicates -> Tree GClassNoun_
  GStatClassNoun :: GClassNoun -> GStatement -> Tree GClassNoun_
  GN_Const :: Tree GConst_
  GQ_Const :: Tree GConst_
  GR_Const :: Tree GConst_
  GZ_Const :: Tree GConst_
  Gcontradiction_Constant :: Tree GConstant_
  Gcontrary_Constant :: Tree GConstant_
  Gthesis_Constant :: Tree GConstant_
  Gorder_DefiniteNoun :: GTerm -> Tree GDefiniteNoun_
  Gzero_DefiniteNoun :: Tree GDefiniteNoun_
  GFunctionDefinition :: GDefiniteNoun -> GTerm -> Tree GDefinition_
  GFunctionIsEqualDefinition :: GDefiniteNoun -> GTerm -> Tree GDefinition_
  GNotionDefinition :: GNotion -> GNotion -> Tree GDefinition_
  GPredicateDefinition :: GPredicate -> GListName -> GStatement -> Tree GDefinition_
  GEEq :: Tree GEqsign_
  GEGe :: Tree GEqsign_
  GEGt :: Tree GEqsign_
  GELe :: Tree GEqsign_
  GELt :: Tree GEqsign_
  GENeq :: Tree GEqsign_
  GESim :: Tree GEqsign_
  GLEGe :: Tree GEqsign_
  GLELe :: Tree GEqsign_
  GLENeq :: Tree GEqsign_
  GLESim :: Tree GEqsign_
  GEBinary :: GEqsign -> GExp -> GExp -> Tree GEquation_
  GEChain :: GEqsign -> GExp -> GEquation -> Tree GEquation_
  GLTAbsolute :: GExp -> Tree GExp_
  GLTComprehension :: GExp -> GExp -> GFormula -> Tree GExp_
  GLTFrac :: GExp -> GExp -> Tree GExp_
  GLTNegative :: GExp -> Tree GExp_
  GLTPositive :: GExp -> Tree GExp_
  GLTPower :: GExp -> GExp -> Tree GExp_
  GLTextbfExp :: GExp -> Tree GExp_
  GTApp :: GFunction -> GListExp -> Tree GExp_
  GTConst :: GConst -> Tree GExp_
  GTDiv :: GExp -> GExp -> Tree GExp_
  GTExp :: GExp -> GExp -> Tree GExp_
  GTMinus :: GExp -> GExp -> Tree GExp_
  GTNeg :: GExp -> Tree GExp_
  GTNumber :: GInt -> Tree GExp_
  GTParenth :: GExp -> Tree GExp_
  GTPlus :: GExp -> GExp -> Tree GExp_
  GTTimes :: GExp -> GExp -> Tree GExp_
  GTVar :: GVar -> Tree GExp_
  GFElem :: GListExp -> GExp -> Tree GFormula_
  GFEquation :: GEquation -> Tree GFormula_
  GLFElem :: GListExp -> GExp -> Tree GFormula_
  GFDerivative :: GFunction -> Tree GFunction_
  GFVar :: GVar -> Tree GFunction_
  Gex_Header :: Tree GHeader_
  GnoHeader :: Tree GHeader_
  GListExp :: [GExp] -> Tree GListExp_
  GListName :: [GName] -> Tree GListName_
  GLatexVarName :: GVar -> Tree GName_
  GVarName :: GVar -> Tree GName_
  GClassNounNamesNotion :: GClassNoun -> GListName -> Tree GNotion_
  GClassNounNotion :: GClassNoun -> Tree GNotion_
  Ggeneral_linear_group_Notion :: GTermSymb -> GTermSymb -> Tree GNotion_
  GAddNotions :: GNotion -> GNotions -> Tree GNotions_
  GOneNotions :: GNotion -> Tree GNotions_
  GDoesPredicate :: GVerb -> Tree GPredicate_
  GHasNoPredicate :: GNotion -> Tree GPredicate_
  GHasPredicate :: GNotion -> Tree GPredicate_
  GIsPredicate :: GAdjective -> Tree GPredicate_
  GIsaPredicate :: GNotion -> Tree GPredicate_
  GNegAddPredicates :: GPredicate -> GPredicates -> Tree GPredicates_
  GNegOnePredicates :: GPredicate -> Tree GPredicates_
  GPosAddPredicates :: GPredicate -> GPredicates -> Tree GPredicates_
  GPosOnePredicates :: GPredicate -> Tree GPredicates_
  Gelement_PrimClass :: GTerm -> Tree GPrimClass_
  Gfunction_PrimClass :: GTerm -> GTerm -> Tree GPrimClass_
  Ggroup_N :: Tree GPrimClass_
  Ginteger_PrimClass :: Tree GPrimClass_
  Gnumber_PrimClass :: Tree GPrimClass_
  Gorder_PrimClass :: Tree GPrimClass_
  Gset_PrimClass :: Tree GPrimClass_
  GAssumptionsSection :: GAssumptions -> GSection -> Tree GSection_
  GDefinitionSection :: GDefinition -> GSection -> Tree GSection_
  GEmptySection :: Tree GSection_
  GStatementSection :: GStatement -> GSection -> Tree GSection_
  GThenStatementSection :: GStatement -> GSection -> Tree GSection_
  GAndStatement :: GStatement -> GStatement -> Tree GStatement_
  GForStatement :: GTerms -> GStatement -> Tree GStatement_
  GFormulaStatement :: GFormula -> Tree GStatement_
  GIfStatement :: GStatement -> GStatement -> Tree GStatement_
  GIffStatement :: GStatement -> GStatement -> Tree GStatement_
  GLatexFormulaStatement :: GFormula -> Tree GStatement_
  GOrStatement :: GStatement -> GStatement -> Tree GStatement_
  GSimpleStatement :: GTerms -> GPredicates -> Tree GStatement_
  GThereIsNoStatement :: GNotion -> Tree GStatement_
  GThereIsStatement :: GNotions -> Tree GStatement_
  GWeHaveConstStatement :: GConstant -> Tree GStatement_
  GWeHaveSymbStatement :: GSymbTerm -> Tree GStatement_
  GExpSymbTerm :: GExp -> Tree GSymbTerm_
  GFormulaSymbTerm :: GFormula -> Tree GSymbTerm_
  GIndexedTerm :: GInt -> Tree GSymbTerm_
  GLatexExpSymbTerm :: GExp -> Tree GSymbTerm_
  GLatexFormulaSymbTerm :: GFormula -> Tree GSymbTerm_
  GLatexIndexedTerm :: GInt -> Tree GSymbTerm_
  GFunctionSynonym :: GSymbTerm -> GTerm -> Tree GSynonym_
  GNotionSynonym :: GNotion -> GPrimClass -> Tree GSynonym_
  GPredicateSynonym :: GPredicate -> GListName -> GStatement -> Tree GSynonym_
  GAllTerm :: GNotion -> Tree GTerm_
  GAnyTerm :: GNotion -> Tree GTerm_
  GDefinitePlNounTerm :: GDefiniteNoun -> Tree GTerm_
  GDefiniteSgNounTerm :: GDefiniteNoun -> Tree GTerm_
  GEachTerm :: GNotion -> Tree GTerm_
  GEveryTerm :: GNotion -> Tree GTerm_
  GNoTerm :: GNotion -> Tree GTerm_
  GSomeTerm :: GNotion -> Tree GTerm_
  GSymbolicTerm :: GSymbTerm -> Tree GTerm_
  GApposTermSymb :: GPrimClass -> GName -> Tree GTermSymb_
  GMkTermSymb :: GTerm -> GSymbTerm -> Tree GTermSymb_
  GAddTerms :: GTerm -> GTerms -> Tree GTerms_
  GOneTerms :: GTerm -> Tree GTerms_
  GSectionToplevel :: GHeader -> GSection -> Tree GToplevel_
  GA_Var :: Tree GVar_
  GB_Var :: Tree GVar_
  GC_Var :: Tree GVar_
  GK_Var :: Tree GVar_
  GL_Var :: Tree GVar_
  GM_Var :: Tree GVar_
  GS_Var :: Tree GVar_
  GT_Var :: Tree GVar_
  Ga_Var :: Tree GVar_
  Gb_Var :: Tree GVar_
  Gc_Var :: Tree GVar_
  Gd_Var :: Tree GVar_
  Gf_Var :: Tree GVar_
  Gg_Var :: Tree GVar_
  Gk_Var :: Tree GVar_
  Gm_Var :: Tree GVar_
  Gn_Var :: Tree GVar_
  Gp_Var :: Tree GVar_
  Gq_Var :: Tree GVar_
  Gr_Var :: Tree GVar_
  Gs_Var :: Tree GVar_
  Gt_Var :: Tree GVar_
  Gu_Var :: Tree GVar_
  Gx_Var :: Tree GVar_
  Gy_Var :: Tree GVar_
  Gz_Var :: Tree GVar_
  Gbelong_Verb :: GTerm -> Tree GVerb_
  Gconverge_Verb :: Tree GVerb_
  Gdivide_Verb :: GTerm -> Tree GVerb_
  Gjoin_Verb :: GTerm -> GTerm -> Tree GVerb_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (Gdividing_Adjective x1,Gdividing_Adjective y1) -> and [ x1 == y1 ]
    (Gequal_Adjective x1,Gequal_Adjective y1) -> and [ x1 == y1 ]
    (Geven_Adjective,Geven_Adjective) -> and [ ]
    (Ggeneral_A,Ggeneral_A) -> and [ ]
    (Ggreater_Adjective x1,Ggreater_Adjective y1) -> and [ x1 == y1 ]
    (Ggreater_or_equal_Adjective x1,Ggreater_or_equal_Adjective y1) -> and [ x1 == y1 ]
    (Gless_Adjective x1,Gless_Adjective y1) -> and [ x1 == y1 ]
    (Gless_or_equal_Adjective x1,Gless_or_equal_Adjective y1) -> and [ x1 == y1 ]
    (Glinear_A,Glinear_A) -> and [ ]
    (Gnegative_Adjective,Gnegative_Adjective) -> and [ ]
    (Gnonnegative_Adjective,Gnonnegative_Adjective) -> and [ ]
    (Godd_Adjective,Godd_Adjective) -> and [ ]
    (Gpositive_Adjective,Gpositive_Adjective) -> and [ ]
    (Gprime_Adjective,Gprime_Adjective) -> and [ ]
    (Grational_Adjective,Grational_Adjective) -> and [ ]
    (Greal_Adjective,Greal_Adjective) -> and [ ]
    (GFormulaAssumption x1,GFormulaAssumption y1) -> and [ x1 == y1 ]
    (GLatexFormulaAssumption x1,GLatexFormulaAssumption y1) -> and [ x1 == y1 ]
    (GLetNamesAssumption x1 x2,GLetNamesAssumption y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNamesAssumption x1 x2,GNamesAssumption y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStatementAssumption x1,GStatementAssumption y1) -> and [ x1 == y1 ]
    (GAddAssumptions x1 x2,GAddAssumptions y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOneAssumptions x1,GOneAssumptions y1) -> and [ x1 == y1 ]
    (GAdjClassNoun x1 x2,GAdjClassNoun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPrimClassNoun x1,GPrimClassNoun y1) -> and [ x1 == y1 ]
    (GRelClassNoun x1 x2,GRelClassNoun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStatClassNoun x1 x2,GStatClassNoun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GN_Const,GN_Const) -> and [ ]
    (GQ_Const,GQ_Const) -> and [ ]
    (GR_Const,GR_Const) -> and [ ]
    (GZ_Const,GZ_Const) -> and [ ]
    (Gcontradiction_Constant,Gcontradiction_Constant) -> and [ ]
    (Gcontrary_Constant,Gcontrary_Constant) -> and [ ]
    (Gthesis_Constant,Gthesis_Constant) -> and [ ]
    (Gorder_DefiniteNoun x1,Gorder_DefiniteNoun y1) -> and [ x1 == y1 ]
    (Gzero_DefiniteNoun,Gzero_DefiniteNoun) -> and [ ]
    (GFunctionDefinition x1 x2,GFunctionDefinition y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFunctionIsEqualDefinition x1 x2,GFunctionIsEqualDefinition y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotionDefinition x1 x2,GNotionDefinition y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredicateDefinition x1 x2 x3,GPredicateDefinition y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GEEq,GEEq) -> and [ ]
    (GEGe,GEGe) -> and [ ]
    (GEGt,GEGt) -> and [ ]
    (GELe,GELe) -> and [ ]
    (GELt,GELt) -> and [ ]
    (GENeq,GENeq) -> and [ ]
    (GESim,GESim) -> and [ ]
    (GLEGe,GLEGe) -> and [ ]
    (GLELe,GLELe) -> and [ ]
    (GLENeq,GLENeq) -> and [ ]
    (GLESim,GLESim) -> and [ ]
    (GEBinary x1 x2 x3,GEBinary y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GEChain x1 x2 x3,GEChain y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GLTAbsolute x1,GLTAbsolute y1) -> and [ x1 == y1 ]
    (GLTComprehension x1 x2 x3,GLTComprehension y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GLTFrac x1 x2,GLTFrac y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLTNegative x1,GLTNegative y1) -> and [ x1 == y1 ]
    (GLTPositive x1,GLTPositive y1) -> and [ x1 == y1 ]
    (GLTPower x1 x2,GLTPower y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLTextbfExp x1,GLTextbfExp y1) -> and [ x1 == y1 ]
    (GTApp x1 x2,GTApp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTConst x1,GTConst y1) -> and [ x1 == y1 ]
    (GTDiv x1 x2,GTDiv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTExp x1 x2,GTExp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTMinus x1 x2,GTMinus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTNeg x1,GTNeg y1) -> and [ x1 == y1 ]
    (GTNumber x1,GTNumber y1) -> and [ x1 == y1 ]
    (GTParenth x1,GTParenth y1) -> and [ x1 == y1 ]
    (GTPlus x1 x2,GTPlus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTTimes x1 x2,GTTimes y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTVar x1,GTVar y1) -> and [ x1 == y1 ]
    (GFElem x1 x2,GFElem y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFEquation x1,GFEquation y1) -> and [ x1 == y1 ]
    (GLFElem x1 x2,GLFElem y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFDerivative x1,GFDerivative y1) -> and [ x1 == y1 ]
    (GFVar x1,GFVar y1) -> and [ x1 == y1 ]
    (Gex_Header,Gex_Header) -> and [ ]
    (GnoHeader,GnoHeader) -> and [ ]
    (GListExp x1,GListExp y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListName x1,GListName y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GLatexVarName x1,GLatexVarName y1) -> and [ x1 == y1 ]
    (GVarName x1,GVarName y1) -> and [ x1 == y1 ]
    (GClassNounNamesNotion x1 x2,GClassNounNamesNotion y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GClassNounNotion x1,GClassNounNotion y1) -> and [ x1 == y1 ]
    (Ggeneral_linear_group_Notion x1 x2,Ggeneral_linear_group_Notion y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAddNotions x1 x2,GAddNotions y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOneNotions x1,GOneNotions y1) -> and [ x1 == y1 ]
    (GDoesPredicate x1,GDoesPredicate y1) -> and [ x1 == y1 ]
    (GHasNoPredicate x1,GHasNoPredicate y1) -> and [ x1 == y1 ]
    (GHasPredicate x1,GHasPredicate y1) -> and [ x1 == y1 ]
    (GIsPredicate x1,GIsPredicate y1) -> and [ x1 == y1 ]
    (GIsaPredicate x1,GIsaPredicate y1) -> and [ x1 == y1 ]
    (GNegAddPredicates x1 x2,GNegAddPredicates y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNegOnePredicates x1,GNegOnePredicates y1) -> and [ x1 == y1 ]
    (GPosAddPredicates x1 x2,GPosAddPredicates y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPosOnePredicates x1,GPosOnePredicates y1) -> and [ x1 == y1 ]
    (Gelement_PrimClass x1,Gelement_PrimClass y1) -> and [ x1 == y1 ]
    (Gfunction_PrimClass x1 x2,Gfunction_PrimClass y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Ggroup_N,Ggroup_N) -> and [ ]
    (Ginteger_PrimClass,Ginteger_PrimClass) -> and [ ]
    (Gnumber_PrimClass,Gnumber_PrimClass) -> and [ ]
    (Gorder_PrimClass,Gorder_PrimClass) -> and [ ]
    (Gset_PrimClass,Gset_PrimClass) -> and [ ]
    (GAssumptionsSection x1 x2,GAssumptionsSection y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDefinitionSection x1 x2,GDefinitionSection y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEmptySection,GEmptySection) -> and [ ]
    (GStatementSection x1 x2,GStatementSection y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GThenStatementSection x1 x2,GThenStatementSection y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAndStatement x1 x2,GAndStatement y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GForStatement x1 x2,GForStatement y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GFormulaStatement x1,GFormulaStatement y1) -> and [ x1 == y1 ]
    (GIfStatement x1 x2,GIfStatement y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIffStatement x1 x2,GIffStatement y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLatexFormulaStatement x1,GLatexFormulaStatement y1) -> and [ x1 == y1 ]
    (GOrStatement x1 x2,GOrStatement y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSimpleStatement x1 x2,GSimpleStatement y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GThereIsNoStatement x1,GThereIsNoStatement y1) -> and [ x1 == y1 ]
    (GThereIsStatement x1,GThereIsStatement y1) -> and [ x1 == y1 ]
    (GWeHaveConstStatement x1,GWeHaveConstStatement y1) -> and [ x1 == y1 ]
    (GWeHaveSymbStatement x1,GWeHaveSymbStatement y1) -> and [ x1 == y1 ]
    (GExpSymbTerm x1,GExpSymbTerm y1) -> and [ x1 == y1 ]
    (GFormulaSymbTerm x1,GFormulaSymbTerm y1) -> and [ x1 == y1 ]
    (GIndexedTerm x1,GIndexedTerm y1) -> and [ x1 == y1 ]
    (GLatexExpSymbTerm x1,GLatexExpSymbTerm y1) -> and [ x1 == y1 ]
    (GLatexFormulaSymbTerm x1,GLatexFormulaSymbTerm y1) -> and [ x1 == y1 ]
    (GLatexIndexedTerm x1,GLatexIndexedTerm y1) -> and [ x1 == y1 ]
    (GFunctionSynonym x1 x2,GFunctionSynonym y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotionSynonym x1 x2,GNotionSynonym y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredicateSynonym x1 x2 x3,GPredicateSynonym y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GAllTerm x1,GAllTerm y1) -> and [ x1 == y1 ]
    (GAnyTerm x1,GAnyTerm y1) -> and [ x1 == y1 ]
    (GDefinitePlNounTerm x1,GDefinitePlNounTerm y1) -> and [ x1 == y1 ]
    (GDefiniteSgNounTerm x1,GDefiniteSgNounTerm y1) -> and [ x1 == y1 ]
    (GEachTerm x1,GEachTerm y1) -> and [ x1 == y1 ]
    (GEveryTerm x1,GEveryTerm y1) -> and [ x1 == y1 ]
    (GNoTerm x1,GNoTerm y1) -> and [ x1 == y1 ]
    (GSomeTerm x1,GSomeTerm y1) -> and [ x1 == y1 ]
    (GSymbolicTerm x1,GSymbolicTerm y1) -> and [ x1 == y1 ]
    (GApposTermSymb x1 x2,GApposTermSymb y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMkTermSymb x1 x2,GMkTermSymb y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAddTerms x1 x2,GAddTerms y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOneTerms x1,GOneTerms y1) -> and [ x1 == y1 ]
    (GSectionToplevel x1 x2,GSectionToplevel y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GA_Var,GA_Var) -> and [ ]
    (GB_Var,GB_Var) -> and [ ]
    (GC_Var,GC_Var) -> and [ ]
    (GK_Var,GK_Var) -> and [ ]
    (GL_Var,GL_Var) -> and [ ]
    (GM_Var,GM_Var) -> and [ ]
    (GS_Var,GS_Var) -> and [ ]
    (GT_Var,GT_Var) -> and [ ]
    (Ga_Var,Ga_Var) -> and [ ]
    (Gb_Var,Gb_Var) -> and [ ]
    (Gc_Var,Gc_Var) -> and [ ]
    (Gd_Var,Gd_Var) -> and [ ]
    (Gf_Var,Gf_Var) -> and [ ]
    (Gg_Var,Gg_Var) -> and [ ]
    (Gk_Var,Gk_Var) -> and [ ]
    (Gm_Var,Gm_Var) -> and [ ]
    (Gn_Var,Gn_Var) -> and [ ]
    (Gp_Var,Gp_Var) -> and [ ]
    (Gq_Var,Gq_Var) -> and [ ]
    (Gr_Var,Gr_Var) -> and [ ]
    (Gs_Var,Gs_Var) -> and [ ]
    (Gt_Var,Gt_Var) -> and [ ]
    (Gu_Var,Gu_Var) -> and [ ]
    (Gx_Var,Gx_Var) -> and [ ]
    (Gy_Var,Gy_Var) -> and [ ]
    (Gz_Var,Gz_Var) -> and [ ]
    (Gbelong_Verb x1,Gbelong_Verb y1) -> and [ x1 == y1 ]
    (Gconverge_Verb,Gconverge_Verb) -> and [ ]
    (Gdivide_Verb x1,Gdivide_Verb y1) -> and [ x1 == y1 ]
    (Gjoin_Verb x1 x2,Gjoin_Verb y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAdjective where
  gf (Gdividing_Adjective x1) = mkApp (mkCId "dividing_Adjective") [gf x1]
  gf (Gequal_Adjective x1) = mkApp (mkCId "equal_Adjective") [gf x1]
  gf Geven_Adjective = mkApp (mkCId "even_Adjective") []
  gf Ggeneral_A = mkApp (mkCId "general_A") []
  gf (Ggreater_Adjective x1) = mkApp (mkCId "greater_Adjective") [gf x1]
  gf (Ggreater_or_equal_Adjective x1) = mkApp (mkCId "greater_or_equal_Adjective") [gf x1]
  gf (Gless_Adjective x1) = mkApp (mkCId "less_Adjective") [gf x1]
  gf (Gless_or_equal_Adjective x1) = mkApp (mkCId "less_or_equal_Adjective") [gf x1]
  gf Glinear_A = mkApp (mkCId "linear_A") []
  gf Gnegative_Adjective = mkApp (mkCId "negative_Adjective") []
  gf Gnonnegative_Adjective = mkApp (mkCId "nonnegative_Adjective") []
  gf Godd_Adjective = mkApp (mkCId "odd_Adjective") []
  gf Gpositive_Adjective = mkApp (mkCId "positive_Adjective") []
  gf Gprime_Adjective = mkApp (mkCId "prime_Adjective") []
  gf Grational_Adjective = mkApp (mkCId "rational_Adjective") []
  gf Greal_Adjective = mkApp (mkCId "real_Adjective") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "dividing_Adjective" -> Gdividing_Adjective (fg x1)
      Just (i,[x1]) | i == mkCId "equal_Adjective" -> Gequal_Adjective (fg x1)
      Just (i,[]) | i == mkCId "even_Adjective" -> Geven_Adjective 
      Just (i,[]) | i == mkCId "general_A" -> Ggeneral_A 
      Just (i,[x1]) | i == mkCId "greater_Adjective" -> Ggreater_Adjective (fg x1)
      Just (i,[x1]) | i == mkCId "greater_or_equal_Adjective" -> Ggreater_or_equal_Adjective (fg x1)
      Just (i,[x1]) | i == mkCId "less_Adjective" -> Gless_Adjective (fg x1)
      Just (i,[x1]) | i == mkCId "less_or_equal_Adjective" -> Gless_or_equal_Adjective (fg x1)
      Just (i,[]) | i == mkCId "linear_A" -> Glinear_A 
      Just (i,[]) | i == mkCId "negative_Adjective" -> Gnegative_Adjective 
      Just (i,[]) | i == mkCId "nonnegative_Adjective" -> Gnonnegative_Adjective 
      Just (i,[]) | i == mkCId "odd_Adjective" -> Godd_Adjective 
      Just (i,[]) | i == mkCId "positive_Adjective" -> Gpositive_Adjective 
      Just (i,[]) | i == mkCId "prime_Adjective" -> Gprime_Adjective 
      Just (i,[]) | i == mkCId "rational_Adjective" -> Grational_Adjective 
      Just (i,[]) | i == mkCId "real_Adjective" -> Greal_Adjective 


      _ -> error ("no Adjective " ++ show t)

instance Gf GAssumption where
  gf (GFormulaAssumption x1) = mkApp (mkCId "FormulaAssumption") [gf x1]
  gf (GLatexFormulaAssumption x1) = mkApp (mkCId "LatexFormulaAssumption") [gf x1]
  gf (GLetNamesAssumption x1 x2) = mkApp (mkCId "LetNamesAssumption") [gf x1, gf x2]
  gf (GNamesAssumption x1 x2) = mkApp (mkCId "NamesAssumption") [gf x1, gf x2]
  gf (GStatementAssumption x1) = mkApp (mkCId "StatementAssumption") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "FormulaAssumption" -> GFormulaAssumption (fg x1)
      Just (i,[x1]) | i == mkCId "LatexFormulaAssumption" -> GLatexFormulaAssumption (fg x1)
      Just (i,[x1,x2]) | i == mkCId "LetNamesAssumption" -> GLetNamesAssumption (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NamesAssumption" -> GNamesAssumption (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "StatementAssumption" -> GStatementAssumption (fg x1)


      _ -> error ("no Assumption " ++ show t)

instance Gf GAssumptions where
  gf (GAddAssumptions x1 x2) = mkApp (mkCId "AddAssumptions") [gf x1, gf x2]
  gf (GOneAssumptions x1) = mkApp (mkCId "OneAssumptions") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AddAssumptions" -> GAddAssumptions (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OneAssumptions" -> GOneAssumptions (fg x1)


      _ -> error ("no Assumptions " ++ show t)

instance Gf GClassNoun where
  gf (GAdjClassNoun x1 x2) = mkApp (mkCId "AdjClassNoun") [gf x1, gf x2]
  gf (GPrimClassNoun x1) = mkApp (mkCId "PrimClassNoun") [gf x1]
  gf (GRelClassNoun x1 x2) = mkApp (mkCId "RelClassNoun") [gf x1, gf x2]
  gf (GStatClassNoun x1 x2) = mkApp (mkCId "StatClassNoun") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjClassNoun" -> GAdjClassNoun (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PrimClassNoun" -> GPrimClassNoun (fg x1)
      Just (i,[x1,x2]) | i == mkCId "RelClassNoun" -> GRelClassNoun (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "StatClassNoun" -> GStatClassNoun (fg x1) (fg x2)


      _ -> error ("no ClassNoun " ++ show t)

instance Gf GConst where
  gf GN_Const = mkApp (mkCId "N_Const") []
  gf GQ_Const = mkApp (mkCId "Q_Const") []
  gf GR_Const = mkApp (mkCId "R_Const") []
  gf GZ_Const = mkApp (mkCId "Z_Const") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "N_Const" -> GN_Const 
      Just (i,[]) | i == mkCId "Q_Const" -> GQ_Const 
      Just (i,[]) | i == mkCId "R_Const" -> GR_Const 
      Just (i,[]) | i == mkCId "Z_Const" -> GZ_Const 


      _ -> error ("no Const " ++ show t)

instance Gf GConstant where
  gf Gcontradiction_Constant = mkApp (mkCId "contradiction_Constant") []
  gf Gcontrary_Constant = mkApp (mkCId "contrary_Constant") []
  gf Gthesis_Constant = mkApp (mkCId "thesis_Constant") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "contradiction_Constant" -> Gcontradiction_Constant 
      Just (i,[]) | i == mkCId "contrary_Constant" -> Gcontrary_Constant 
      Just (i,[]) | i == mkCId "thesis_Constant" -> Gthesis_Constant 


      _ -> error ("no Constant " ++ show t)

instance Gf GDefiniteNoun where
  gf (Gorder_DefiniteNoun x1) = mkApp (mkCId "order_DefiniteNoun") [gf x1]
  gf Gzero_DefiniteNoun = mkApp (mkCId "zero_DefiniteNoun") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "order_DefiniteNoun" -> Gorder_DefiniteNoun (fg x1)
      Just (i,[]) | i == mkCId "zero_DefiniteNoun" -> Gzero_DefiniteNoun 


      _ -> error ("no DefiniteNoun " ++ show t)

instance Gf GDefinition where
  gf (GFunctionDefinition x1 x2) = mkApp (mkCId "FunctionDefinition") [gf x1, gf x2]
  gf (GFunctionIsEqualDefinition x1 x2) = mkApp (mkCId "FunctionIsEqualDefinition") [gf x1, gf x2]
  gf (GNotionDefinition x1 x2) = mkApp (mkCId "NotionDefinition") [gf x1, gf x2]
  gf (GPredicateDefinition x1 x2 x3) = mkApp (mkCId "PredicateDefinition") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "FunctionDefinition" -> GFunctionDefinition (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "FunctionIsEqualDefinition" -> GFunctionIsEqualDefinition (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NotionDefinition" -> GNotionDefinition (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "PredicateDefinition" -> GPredicateDefinition (fg x1) (fg x2) (fg x3)


      _ -> error ("no Definition " ++ show t)

instance Gf GEqsign where
  gf GEEq = mkApp (mkCId "EEq") []
  gf GEGe = mkApp (mkCId "EGe") []
  gf GEGt = mkApp (mkCId "EGt") []
  gf GELe = mkApp (mkCId "ELe") []
  gf GELt = mkApp (mkCId "ELt") []
  gf GENeq = mkApp (mkCId "ENeq") []
  gf GESim = mkApp (mkCId "ESim") []
  gf GLEGe = mkApp (mkCId "LEGe") []
  gf GLELe = mkApp (mkCId "LELe") []
  gf GLENeq = mkApp (mkCId "LENeq") []
  gf GLESim = mkApp (mkCId "LESim") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EEq" -> GEEq 
      Just (i,[]) | i == mkCId "EGe" -> GEGe 
      Just (i,[]) | i == mkCId "EGt" -> GEGt 
      Just (i,[]) | i == mkCId "ELe" -> GELe 
      Just (i,[]) | i == mkCId "ELt" -> GELt 
      Just (i,[]) | i == mkCId "ENeq" -> GENeq 
      Just (i,[]) | i == mkCId "ESim" -> GESim 
      Just (i,[]) | i == mkCId "LEGe" -> GLEGe 
      Just (i,[]) | i == mkCId "LELe" -> GLELe 
      Just (i,[]) | i == mkCId "LENeq" -> GLENeq 
      Just (i,[]) | i == mkCId "LESim" -> GLESim 


      _ -> error ("no Eqsign " ++ show t)

instance Gf GEquation where
  gf (GEBinary x1 x2 x3) = mkApp (mkCId "EBinary") [gf x1, gf x2, gf x3]
  gf (GEChain x1 x2 x3) = mkApp (mkCId "EChain") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "EBinary" -> GEBinary (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "EChain" -> GEChain (fg x1) (fg x2) (fg x3)


      _ -> error ("no Equation " ++ show t)

instance Gf GExp where
  gf (GLTAbsolute x1) = mkApp (mkCId "LTAbsolute") [gf x1]
  gf (GLTComprehension x1 x2 x3) = mkApp (mkCId "LTComprehension") [gf x1, gf x2, gf x3]
  gf (GLTFrac x1 x2) = mkApp (mkCId "LTFrac") [gf x1, gf x2]
  gf (GLTNegative x1) = mkApp (mkCId "LTNegative") [gf x1]
  gf (GLTPositive x1) = mkApp (mkCId "LTPositive") [gf x1]
  gf (GLTPower x1 x2) = mkApp (mkCId "LTPower") [gf x1, gf x2]
  gf (GLTextbfExp x1) = mkApp (mkCId "LTextbfExp") [gf x1]
  gf (GTApp x1 x2) = mkApp (mkCId "TApp") [gf x1, gf x2]
  gf (GTConst x1) = mkApp (mkCId "TConst") [gf x1]
  gf (GTDiv x1 x2) = mkApp (mkCId "TDiv") [gf x1, gf x2]
  gf (GTExp x1 x2) = mkApp (mkCId "TExp") [gf x1, gf x2]
  gf (GTMinus x1 x2) = mkApp (mkCId "TMinus") [gf x1, gf x2]
  gf (GTNeg x1) = mkApp (mkCId "TNeg") [gf x1]
  gf (GTNumber x1) = mkApp (mkCId "TNumber") [gf x1]
  gf (GTParenth x1) = mkApp (mkCId "TParenth") [gf x1]
  gf (GTPlus x1 x2) = mkApp (mkCId "TPlus") [gf x1, gf x2]
  gf (GTTimes x1 x2) = mkApp (mkCId "TTimes") [gf x1, gf x2]
  gf (GTVar x1) = mkApp (mkCId "TVar") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "LTAbsolute" -> GLTAbsolute (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "LTComprehension" -> GLTComprehension (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "LTFrac" -> GLTFrac (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "LTNegative" -> GLTNegative (fg x1)
      Just (i,[x1]) | i == mkCId "LTPositive" -> GLTPositive (fg x1)
      Just (i,[x1,x2]) | i == mkCId "LTPower" -> GLTPower (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "LTextbfExp" -> GLTextbfExp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "TApp" -> GTApp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "TConst" -> GTConst (fg x1)
      Just (i,[x1,x2]) | i == mkCId "TDiv" -> GTDiv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "TExp" -> GTExp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "TMinus" -> GTMinus (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "TNeg" -> GTNeg (fg x1)
      Just (i,[x1]) | i == mkCId "TNumber" -> GTNumber (fg x1)
      Just (i,[x1]) | i == mkCId "TParenth" -> GTParenth (fg x1)
      Just (i,[x1,x2]) | i == mkCId "TPlus" -> GTPlus (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "TTimes" -> GTTimes (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "TVar" -> GTVar (fg x1)


      _ -> error ("no Exp " ++ show t)

instance Gf GFormula where
  gf (GFElem x1 x2) = mkApp (mkCId "FElem") [gf x1, gf x2]
  gf (GFEquation x1) = mkApp (mkCId "FEquation") [gf x1]
  gf (GLFElem x1 x2) = mkApp (mkCId "LFElem") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "FElem" -> GFElem (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "FEquation" -> GFEquation (fg x1)
      Just (i,[x1,x2]) | i == mkCId "LFElem" -> GLFElem (fg x1) (fg x2)


      _ -> error ("no Formula " ++ show t)

instance Gf GFunction where
  gf (GFDerivative x1) = mkApp (mkCId "FDerivative") [gf x1]
  gf (GFVar x1) = mkApp (mkCId "FVar") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "FDerivative" -> GFDerivative (fg x1)
      Just (i,[x1]) | i == mkCId "FVar" -> GFVar (fg x1)


      _ -> error ("no Function " ++ show t)

instance Gf GHeader where
  gf Gex_Header = mkApp (mkCId "ex_Header") []
  gf GnoHeader = mkApp (mkCId "noHeader") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "ex_Header" -> Gex_Header 
      Just (i,[]) | i == mkCId "noHeader" -> GnoHeader 


      _ -> error ("no Header " ++ show t)

instance Gf GListExp where
  gf (GListExp [x1]) = mkApp (mkCId "BaseExp") [gf x1]
  gf (GListExp (x:xs)) = mkApp (mkCId "ConsExp") [gf x, gf (GListExp xs)]
  fg t =
    GListExp (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseExp" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsExp" -> fg x1 : fgs x2


      _ -> error ("no ListExp " ++ show t)

instance Gf GListName where
  gf (GListName [x1]) = mkApp (mkCId "BaseName") [gf x1]
  gf (GListName (x:xs)) = mkApp (mkCId "ConsName") [gf x, gf (GListName xs)]
  fg t =
    GListName (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseName" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsName" -> fg x1 : fgs x2


      _ -> error ("no ListName " ++ show t)

instance Gf GName where
  gf (GLatexVarName x1) = mkApp (mkCId "LatexVarName") [gf x1]
  gf (GVarName x1) = mkApp (mkCId "VarName") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "LatexVarName" -> GLatexVarName (fg x1)
      Just (i,[x1]) | i == mkCId "VarName" -> GVarName (fg x1)


      _ -> error ("no Name " ++ show t)

instance Gf GNotion where
  gf (GClassNounNamesNotion x1 x2) = mkApp (mkCId "ClassNounNamesNotion") [gf x1, gf x2]
  gf (GClassNounNotion x1) = mkApp (mkCId "ClassNounNotion") [gf x1]
  gf (Ggeneral_linear_group_Notion x1 x2) = mkApp (mkCId "general_linear_group_Notion") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ClassNounNamesNotion" -> GClassNounNamesNotion (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ClassNounNotion" -> GClassNounNotion (fg x1)
      Just (i,[x1,x2]) | i == mkCId "general_linear_group_Notion" -> Ggeneral_linear_group_Notion (fg x1) (fg x2)


      _ -> error ("no Notion " ++ show t)

instance Gf GNotions where
  gf (GAddNotions x1 x2) = mkApp (mkCId "AddNotions") [gf x1, gf x2]
  gf (GOneNotions x1) = mkApp (mkCId "OneNotions") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AddNotions" -> GAddNotions (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OneNotions" -> GOneNotions (fg x1)


      _ -> error ("no Notions " ++ show t)

instance Gf GPredicate where
  gf (GDoesPredicate x1) = mkApp (mkCId "DoesPredicate") [gf x1]
  gf (GHasNoPredicate x1) = mkApp (mkCId "HasNoPredicate") [gf x1]
  gf (GHasPredicate x1) = mkApp (mkCId "HasPredicate") [gf x1]
  gf (GIsPredicate x1) = mkApp (mkCId "IsPredicate") [gf x1]
  gf (GIsaPredicate x1) = mkApp (mkCId "IsaPredicate") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "DoesPredicate" -> GDoesPredicate (fg x1)
      Just (i,[x1]) | i == mkCId "HasNoPredicate" -> GHasNoPredicate (fg x1)
      Just (i,[x1]) | i == mkCId "HasPredicate" -> GHasPredicate (fg x1)
      Just (i,[x1]) | i == mkCId "IsPredicate" -> GIsPredicate (fg x1)
      Just (i,[x1]) | i == mkCId "IsaPredicate" -> GIsaPredicate (fg x1)


      _ -> error ("no Predicate " ++ show t)

instance Gf GPredicates where
  gf (GNegAddPredicates x1 x2) = mkApp (mkCId "NegAddPredicates") [gf x1, gf x2]
  gf (GNegOnePredicates x1) = mkApp (mkCId "NegOnePredicates") [gf x1]
  gf (GPosAddPredicates x1 x2) = mkApp (mkCId "PosAddPredicates") [gf x1, gf x2]
  gf (GPosOnePredicates x1) = mkApp (mkCId "PosOnePredicates") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "NegAddPredicates" -> GNegAddPredicates (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NegOnePredicates" -> GNegOnePredicates (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PosAddPredicates" -> GPosAddPredicates (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PosOnePredicates" -> GPosOnePredicates (fg x1)


      _ -> error ("no Predicates " ++ show t)

instance Gf GPrimClass where
  gf (Gelement_PrimClass x1) = mkApp (mkCId "element_PrimClass") [gf x1]
  gf (Gfunction_PrimClass x1 x2) = mkApp (mkCId "function_PrimClass") [gf x1, gf x2]
  gf Ggroup_N = mkApp (mkCId "group_N") []
  gf Ginteger_PrimClass = mkApp (mkCId "integer_PrimClass") []
  gf Gnumber_PrimClass = mkApp (mkCId "number_PrimClass") []
  gf Gorder_PrimClass = mkApp (mkCId "order_PrimClass") []
  gf Gset_PrimClass = mkApp (mkCId "set_PrimClass") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "element_PrimClass" -> Gelement_PrimClass (fg x1)
      Just (i,[x1,x2]) | i == mkCId "function_PrimClass" -> Gfunction_PrimClass (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "group_N" -> Ggroup_N 
      Just (i,[]) | i == mkCId "integer_PrimClass" -> Ginteger_PrimClass 
      Just (i,[]) | i == mkCId "number_PrimClass" -> Gnumber_PrimClass 
      Just (i,[]) | i == mkCId "order_PrimClass" -> Gorder_PrimClass 
      Just (i,[]) | i == mkCId "set_PrimClass" -> Gset_PrimClass 


      _ -> error ("no PrimClass " ++ show t)

instance Gf GSection where
  gf (GAssumptionsSection x1 x2) = mkApp (mkCId "AssumptionsSection") [gf x1, gf x2]
  gf (GDefinitionSection x1 x2) = mkApp (mkCId "DefinitionSection") [gf x1, gf x2]
  gf GEmptySection = mkApp (mkCId "EmptySection") []
  gf (GStatementSection x1 x2) = mkApp (mkCId "StatementSection") [gf x1, gf x2]
  gf (GThenStatementSection x1 x2) = mkApp (mkCId "ThenStatementSection") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AssumptionsSection" -> GAssumptionsSection (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "DefinitionSection" -> GDefinitionSection (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "EmptySection" -> GEmptySection 
      Just (i,[x1,x2]) | i == mkCId "StatementSection" -> GStatementSection (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ThenStatementSection" -> GThenStatementSection (fg x1) (fg x2)


      _ -> error ("no Section " ++ show t)

instance Gf GStatement where
  gf (GAndStatement x1 x2) = mkApp (mkCId "AndStatement") [gf x1, gf x2]
  gf (GForStatement x1 x2) = mkApp (mkCId "ForStatement") [gf x1, gf x2]
  gf (GFormulaStatement x1) = mkApp (mkCId "FormulaStatement") [gf x1]
  gf (GIfStatement x1 x2) = mkApp (mkCId "IfStatement") [gf x1, gf x2]
  gf (GIffStatement x1 x2) = mkApp (mkCId "IffStatement") [gf x1, gf x2]
  gf (GLatexFormulaStatement x1) = mkApp (mkCId "LatexFormulaStatement") [gf x1]
  gf (GOrStatement x1 x2) = mkApp (mkCId "OrStatement") [gf x1, gf x2]
  gf (GSimpleStatement x1 x2) = mkApp (mkCId "SimpleStatement") [gf x1, gf x2]
  gf (GThereIsNoStatement x1) = mkApp (mkCId "ThereIsNoStatement") [gf x1]
  gf (GThereIsStatement x1) = mkApp (mkCId "ThereIsStatement") [gf x1]
  gf (GWeHaveConstStatement x1) = mkApp (mkCId "WeHaveConstStatement") [gf x1]
  gf (GWeHaveSymbStatement x1) = mkApp (mkCId "WeHaveSymbStatement") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AndStatement" -> GAndStatement (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ForStatement" -> GForStatement (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "FormulaStatement" -> GFormulaStatement (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IfStatement" -> GIfStatement (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IffStatement" -> GIffStatement (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "LatexFormulaStatement" -> GLatexFormulaStatement (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OrStatement" -> GOrStatement (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SimpleStatement" -> GSimpleStatement (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ThereIsNoStatement" -> GThereIsNoStatement (fg x1)
      Just (i,[x1]) | i == mkCId "ThereIsStatement" -> GThereIsStatement (fg x1)
      Just (i,[x1]) | i == mkCId "WeHaveConstStatement" -> GWeHaveConstStatement (fg x1)
      Just (i,[x1]) | i == mkCId "WeHaveSymbStatement" -> GWeHaveSymbStatement (fg x1)


      _ -> error ("no Statement " ++ show t)

instance Gf GSymbTerm where
  gf (GExpSymbTerm x1) = mkApp (mkCId "ExpSymbTerm") [gf x1]
  gf (GFormulaSymbTerm x1) = mkApp (mkCId "FormulaSymbTerm") [gf x1]
  gf (GIndexedTerm x1) = mkApp (mkCId "IndexedTerm") [gf x1]
  gf (GLatexExpSymbTerm x1) = mkApp (mkCId "LatexExpSymbTerm") [gf x1]
  gf (GLatexFormulaSymbTerm x1) = mkApp (mkCId "LatexFormulaSymbTerm") [gf x1]
  gf (GLatexIndexedTerm x1) = mkApp (mkCId "LatexIndexedTerm") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ExpSymbTerm" -> GExpSymbTerm (fg x1)
      Just (i,[x1]) | i == mkCId "FormulaSymbTerm" -> GFormulaSymbTerm (fg x1)
      Just (i,[x1]) | i == mkCId "IndexedTerm" -> GIndexedTerm (fg x1)
      Just (i,[x1]) | i == mkCId "LatexExpSymbTerm" -> GLatexExpSymbTerm (fg x1)
      Just (i,[x1]) | i == mkCId "LatexFormulaSymbTerm" -> GLatexFormulaSymbTerm (fg x1)
      Just (i,[x1]) | i == mkCId "LatexIndexedTerm" -> GLatexIndexedTerm (fg x1)


      _ -> error ("no SymbTerm " ++ show t)

instance Gf GSynonym where
  gf (GFunctionSynonym x1 x2) = mkApp (mkCId "FunctionSynonym") [gf x1, gf x2]
  gf (GNotionSynonym x1 x2) = mkApp (mkCId "NotionSynonym") [gf x1, gf x2]
  gf (GPredicateSynonym x1 x2 x3) = mkApp (mkCId "PredicateSynonym") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "FunctionSynonym" -> GFunctionSynonym (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "NotionSynonym" -> GNotionSynonym (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "PredicateSynonym" -> GPredicateSynonym (fg x1) (fg x2) (fg x3)


      _ -> error ("no Synonym " ++ show t)

instance Gf GTerm where
  gf (GAllTerm x1) = mkApp (mkCId "AllTerm") [gf x1]
  gf (GAnyTerm x1) = mkApp (mkCId "AnyTerm") [gf x1]
  gf (GDefinitePlNounTerm x1) = mkApp (mkCId "DefinitePlNounTerm") [gf x1]
  gf (GDefiniteSgNounTerm x1) = mkApp (mkCId "DefiniteSgNounTerm") [gf x1]
  gf (GEachTerm x1) = mkApp (mkCId "EachTerm") [gf x1]
  gf (GEveryTerm x1) = mkApp (mkCId "EveryTerm") [gf x1]
  gf (GNoTerm x1) = mkApp (mkCId "NoTerm") [gf x1]
  gf (GSomeTerm x1) = mkApp (mkCId "SomeTerm") [gf x1]
  gf (GSymbolicTerm x1) = mkApp (mkCId "SymbolicTerm") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AllTerm" -> GAllTerm (fg x1)
      Just (i,[x1]) | i == mkCId "AnyTerm" -> GAnyTerm (fg x1)
      Just (i,[x1]) | i == mkCId "DefinitePlNounTerm" -> GDefinitePlNounTerm (fg x1)
      Just (i,[x1]) | i == mkCId "DefiniteSgNounTerm" -> GDefiniteSgNounTerm (fg x1)
      Just (i,[x1]) | i == mkCId "EachTerm" -> GEachTerm (fg x1)
      Just (i,[x1]) | i == mkCId "EveryTerm" -> GEveryTerm (fg x1)
      Just (i,[x1]) | i == mkCId "NoTerm" -> GNoTerm (fg x1)
      Just (i,[x1]) | i == mkCId "SomeTerm" -> GSomeTerm (fg x1)
      Just (i,[x1]) | i == mkCId "SymbolicTerm" -> GSymbolicTerm (fg x1)


      _ -> error ("no Term " ++ show t)

instance Gf GTermSymb where
  gf (GApposTermSymb x1 x2) = mkApp (mkCId "ApposTermSymb") [gf x1, gf x2]
  gf (GMkTermSymb x1 x2) = mkApp (mkCId "MkTermSymb") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ApposTermSymb" -> GApposTermSymb (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MkTermSymb" -> GMkTermSymb (fg x1) (fg x2)


      _ -> error ("no TermSymb " ++ show t)

instance Gf GTerms where
  gf (GAddTerms x1 x2) = mkApp (mkCId "AddTerms") [gf x1, gf x2]
  gf (GOneTerms x1) = mkApp (mkCId "OneTerms") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AddTerms" -> GAddTerms (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OneTerms" -> GOneTerms (fg x1)


      _ -> error ("no Terms " ++ show t)

instance Gf GToplevel where
  gf (GSectionToplevel x1 x2) = mkApp (mkCId "SectionToplevel") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "SectionToplevel" -> GSectionToplevel (fg x1) (fg x2)


      _ -> error ("no Toplevel " ++ show t)

instance Gf GVar where
  gf GA_Var = mkApp (mkCId "A_Var") []
  gf GB_Var = mkApp (mkCId "B_Var") []
  gf GC_Var = mkApp (mkCId "C_Var") []
  gf GK_Var = mkApp (mkCId "K_Var") []
  gf GL_Var = mkApp (mkCId "L_Var") []
  gf GM_Var = mkApp (mkCId "M_Var") []
  gf GS_Var = mkApp (mkCId "S_Var") []
  gf GT_Var = mkApp (mkCId "T_Var") []
  gf Ga_Var = mkApp (mkCId "a_Var") []
  gf Gb_Var = mkApp (mkCId "b_Var") []
  gf Gc_Var = mkApp (mkCId "c_Var") []
  gf Gd_Var = mkApp (mkCId "d_Var") []
  gf Gf_Var = mkApp (mkCId "f_Var") []
  gf Gg_Var = mkApp (mkCId "g_Var") []
  gf Gk_Var = mkApp (mkCId "k_Var") []
  gf Gm_Var = mkApp (mkCId "m_Var") []
  gf Gn_Var = mkApp (mkCId "n_Var") []
  gf Gp_Var = mkApp (mkCId "p_Var") []
  gf Gq_Var = mkApp (mkCId "q_Var") []
  gf Gr_Var = mkApp (mkCId "r_Var") []
  gf Gs_Var = mkApp (mkCId "s_Var") []
  gf Gt_Var = mkApp (mkCId "t_Var") []
  gf Gu_Var = mkApp (mkCId "u_Var") []
  gf Gx_Var = mkApp (mkCId "x_Var") []
  gf Gy_Var = mkApp (mkCId "y_Var") []
  gf Gz_Var = mkApp (mkCId "z_Var") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "A_Var" -> GA_Var 
      Just (i,[]) | i == mkCId "B_Var" -> GB_Var 
      Just (i,[]) | i == mkCId "C_Var" -> GC_Var 
      Just (i,[]) | i == mkCId "K_Var" -> GK_Var 
      Just (i,[]) | i == mkCId "L_Var" -> GL_Var 
      Just (i,[]) | i == mkCId "M_Var" -> GM_Var 
      Just (i,[]) | i == mkCId "S_Var" -> GS_Var 
      Just (i,[]) | i == mkCId "T_Var" -> GT_Var 
      Just (i,[]) | i == mkCId "a_Var" -> Ga_Var 
      Just (i,[]) | i == mkCId "b_Var" -> Gb_Var 
      Just (i,[]) | i == mkCId "c_Var" -> Gc_Var 
      Just (i,[]) | i == mkCId "d_Var" -> Gd_Var 
      Just (i,[]) | i == mkCId "f_Var" -> Gf_Var 
      Just (i,[]) | i == mkCId "g_Var" -> Gg_Var 
      Just (i,[]) | i == mkCId "k_Var" -> Gk_Var 
      Just (i,[]) | i == mkCId "m_Var" -> Gm_Var 
      Just (i,[]) | i == mkCId "n_Var" -> Gn_Var 
      Just (i,[]) | i == mkCId "p_Var" -> Gp_Var 
      Just (i,[]) | i == mkCId "q_Var" -> Gq_Var 
      Just (i,[]) | i == mkCId "r_Var" -> Gr_Var 
      Just (i,[]) | i == mkCId "s_Var" -> Gs_Var 
      Just (i,[]) | i == mkCId "t_Var" -> Gt_Var 
      Just (i,[]) | i == mkCId "u_Var" -> Gu_Var 
      Just (i,[]) | i == mkCId "x_Var" -> Gx_Var 
      Just (i,[]) | i == mkCId "y_Var" -> Gy_Var 
      Just (i,[]) | i == mkCId "z_Var" -> Gz_Var 


      _ -> error ("no Var " ++ show t)

instance Gf GVerb where
  gf (Gbelong_Verb x1) = mkApp (mkCId "belong_Verb") [gf x1]
  gf Gconverge_Verb = mkApp (mkCId "converge_Verb") []
  gf (Gdivide_Verb x1) = mkApp (mkCId "divide_Verb") [gf x1]
  gf (Gjoin_Verb x1 x2) = mkApp (mkCId "join_Verb") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "belong_Verb" -> Gbelong_Verb (fg x1)
      Just (i,[]) | i == mkCId "converge_Verb" -> Gconverge_Verb 
      Just (i,[x1]) | i == mkCId "divide_Verb" -> Gdivide_Verb (fg x1)
      Just (i,[x1,x2]) | i == mkCId "join_Verb" -> Gjoin_Verb (fg x1) (fg x2)


      _ -> error ("no Verb " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    Gdividing_Adjective x1 -> r Gdividing_Adjective `a` f x1
    Gequal_Adjective x1 -> r Gequal_Adjective `a` f x1
    Ggreater_Adjective x1 -> r Ggreater_Adjective `a` f x1
    Ggreater_or_equal_Adjective x1 -> r Ggreater_or_equal_Adjective `a` f x1
    Gless_Adjective x1 -> r Gless_Adjective `a` f x1
    Gless_or_equal_Adjective x1 -> r Gless_or_equal_Adjective `a` f x1
    GFormulaAssumption x1 -> r GFormulaAssumption `a` f x1
    GLatexFormulaAssumption x1 -> r GLatexFormulaAssumption `a` f x1
    GLetNamesAssumption x1 x2 -> r GLetNamesAssumption `a` f x1 `a` f x2
    GNamesAssumption x1 x2 -> r GNamesAssumption `a` f x1 `a` f x2
    GStatementAssumption x1 -> r GStatementAssumption `a` f x1
    GAddAssumptions x1 x2 -> r GAddAssumptions `a` f x1 `a` f x2
    GOneAssumptions x1 -> r GOneAssumptions `a` f x1
    GAdjClassNoun x1 x2 -> r GAdjClassNoun `a` f x1 `a` f x2
    GPrimClassNoun x1 -> r GPrimClassNoun `a` f x1
    GRelClassNoun x1 x2 -> r GRelClassNoun `a` f x1 `a` f x2
    GStatClassNoun x1 x2 -> r GStatClassNoun `a` f x1 `a` f x2
    Gorder_DefiniteNoun x1 -> r Gorder_DefiniteNoun `a` f x1
    GFunctionDefinition x1 x2 -> r GFunctionDefinition `a` f x1 `a` f x2
    GFunctionIsEqualDefinition x1 x2 -> r GFunctionIsEqualDefinition `a` f x1 `a` f x2
    GNotionDefinition x1 x2 -> r GNotionDefinition `a` f x1 `a` f x2
    GPredicateDefinition x1 x2 x3 -> r GPredicateDefinition `a` f x1 `a` f x2 `a` f x3
    GEBinary x1 x2 x3 -> r GEBinary `a` f x1 `a` f x2 `a` f x3
    GEChain x1 x2 x3 -> r GEChain `a` f x1 `a` f x2 `a` f x3
    GLTAbsolute x1 -> r GLTAbsolute `a` f x1
    GLTComprehension x1 x2 x3 -> r GLTComprehension `a` f x1 `a` f x2 `a` f x3
    GLTFrac x1 x2 -> r GLTFrac `a` f x1 `a` f x2
    GLTNegative x1 -> r GLTNegative `a` f x1
    GLTPositive x1 -> r GLTPositive `a` f x1
    GLTPower x1 x2 -> r GLTPower `a` f x1 `a` f x2
    GLTextbfExp x1 -> r GLTextbfExp `a` f x1
    GTApp x1 x2 -> r GTApp `a` f x1 `a` f x2
    GTConst x1 -> r GTConst `a` f x1
    GTDiv x1 x2 -> r GTDiv `a` f x1 `a` f x2
    GTExp x1 x2 -> r GTExp `a` f x1 `a` f x2
    GTMinus x1 x2 -> r GTMinus `a` f x1 `a` f x2
    GTNeg x1 -> r GTNeg `a` f x1
    GTNumber x1 -> r GTNumber `a` f x1
    GTParenth x1 -> r GTParenth `a` f x1
    GTPlus x1 x2 -> r GTPlus `a` f x1 `a` f x2
    GTTimes x1 x2 -> r GTTimes `a` f x1 `a` f x2
    GTVar x1 -> r GTVar `a` f x1
    GFElem x1 x2 -> r GFElem `a` f x1 `a` f x2
    GFEquation x1 -> r GFEquation `a` f x1
    GLFElem x1 x2 -> r GLFElem `a` f x1 `a` f x2
    GFDerivative x1 -> r GFDerivative `a` f x1
    GFVar x1 -> r GFVar `a` f x1
    GLatexVarName x1 -> r GLatexVarName `a` f x1
    GVarName x1 -> r GVarName `a` f x1
    GClassNounNamesNotion x1 x2 -> r GClassNounNamesNotion `a` f x1 `a` f x2
    GClassNounNotion x1 -> r GClassNounNotion `a` f x1
    Ggeneral_linear_group_Notion x1 x2 -> r Ggeneral_linear_group_Notion `a` f x1 `a` f x2
    GAddNotions x1 x2 -> r GAddNotions `a` f x1 `a` f x2
    GOneNotions x1 -> r GOneNotions `a` f x1
    GDoesPredicate x1 -> r GDoesPredicate `a` f x1
    GHasNoPredicate x1 -> r GHasNoPredicate `a` f x1
    GHasPredicate x1 -> r GHasPredicate `a` f x1
    GIsPredicate x1 -> r GIsPredicate `a` f x1
    GIsaPredicate x1 -> r GIsaPredicate `a` f x1
    GNegAddPredicates x1 x2 -> r GNegAddPredicates `a` f x1 `a` f x2
    GNegOnePredicates x1 -> r GNegOnePredicates `a` f x1
    GPosAddPredicates x1 x2 -> r GPosAddPredicates `a` f x1 `a` f x2
    GPosOnePredicates x1 -> r GPosOnePredicates `a` f x1
    Gelement_PrimClass x1 -> r Gelement_PrimClass `a` f x1
    Gfunction_PrimClass x1 x2 -> r Gfunction_PrimClass `a` f x1 `a` f x2
    GAssumptionsSection x1 x2 -> r GAssumptionsSection `a` f x1 `a` f x2
    GDefinitionSection x1 x2 -> r GDefinitionSection `a` f x1 `a` f x2
    GStatementSection x1 x2 -> r GStatementSection `a` f x1 `a` f x2
    GThenStatementSection x1 x2 -> r GThenStatementSection `a` f x1 `a` f x2
    GAndStatement x1 x2 -> r GAndStatement `a` f x1 `a` f x2
    GForStatement x1 x2 -> r GForStatement `a` f x1 `a` f x2
    GFormulaStatement x1 -> r GFormulaStatement `a` f x1
    GIfStatement x1 x2 -> r GIfStatement `a` f x1 `a` f x2
    GIffStatement x1 x2 -> r GIffStatement `a` f x1 `a` f x2
    GLatexFormulaStatement x1 -> r GLatexFormulaStatement `a` f x1
    GOrStatement x1 x2 -> r GOrStatement `a` f x1 `a` f x2
    GSimpleStatement x1 x2 -> r GSimpleStatement `a` f x1 `a` f x2
    GThereIsNoStatement x1 -> r GThereIsNoStatement `a` f x1
    GThereIsStatement x1 -> r GThereIsStatement `a` f x1
    GWeHaveConstStatement x1 -> r GWeHaveConstStatement `a` f x1
    GWeHaveSymbStatement x1 -> r GWeHaveSymbStatement `a` f x1
    GExpSymbTerm x1 -> r GExpSymbTerm `a` f x1
    GFormulaSymbTerm x1 -> r GFormulaSymbTerm `a` f x1
    GIndexedTerm x1 -> r GIndexedTerm `a` f x1
    GLatexExpSymbTerm x1 -> r GLatexExpSymbTerm `a` f x1
    GLatexFormulaSymbTerm x1 -> r GLatexFormulaSymbTerm `a` f x1
    GLatexIndexedTerm x1 -> r GLatexIndexedTerm `a` f x1
    GFunctionSynonym x1 x2 -> r GFunctionSynonym `a` f x1 `a` f x2
    GNotionSynonym x1 x2 -> r GNotionSynonym `a` f x1 `a` f x2
    GPredicateSynonym x1 x2 x3 -> r GPredicateSynonym `a` f x1 `a` f x2 `a` f x3
    GAllTerm x1 -> r GAllTerm `a` f x1
    GAnyTerm x1 -> r GAnyTerm `a` f x1
    GDefinitePlNounTerm x1 -> r GDefinitePlNounTerm `a` f x1
    GDefiniteSgNounTerm x1 -> r GDefiniteSgNounTerm `a` f x1
    GEachTerm x1 -> r GEachTerm `a` f x1
    GEveryTerm x1 -> r GEveryTerm `a` f x1
    GNoTerm x1 -> r GNoTerm `a` f x1
    GSomeTerm x1 -> r GSomeTerm `a` f x1
    GSymbolicTerm x1 -> r GSymbolicTerm `a` f x1
    GApposTermSymb x1 x2 -> r GApposTermSymb `a` f x1 `a` f x2
    GMkTermSymb x1 x2 -> r GMkTermSymb `a` f x1 `a` f x2
    GAddTerms x1 x2 -> r GAddTerms `a` f x1 `a` f x2
    GOneTerms x1 -> r GOneTerms `a` f x1
    GSectionToplevel x1 x2 -> r GSectionToplevel `a` f x1 `a` f x2
    Gbelong_Verb x1 -> r Gbelong_Verb `a` f x1
    Gdivide_Verb x1 -> r Gdivide_Verb `a` f x1
    Gjoin_Verb x1 x2 -> r Gjoin_Verb `a` f x1 `a` f x2
    GListExp x1 -> r GListExp `a` foldr (a . a (r (:)) . f) (r []) x1
    GListName x1 -> r GListName `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
