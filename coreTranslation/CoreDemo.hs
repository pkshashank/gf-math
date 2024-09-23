{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module CoreDemo where

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

type GAssumption = Tree GAssumption_
data GAssumption_
type GBlock = Tree GBlock_
data GBlock_
type GExample = Tree GExample_
data GExample_
type GKind = Tree GKind_
data GKind_
type GLAssumption = Tree GLAssumption_
data GLAssumption_
type GLKind = Tree GLKind_
data GLKind_
type GLVar = Tree GLVar_
data GLVar_
type GModifier = Tree GModifier_
data GModifier_
type GPredicate = Tree GPredicate_
data GPredicate_
type GProposition = Tree GProposition_
data GProposition_
type GQuant = Tree GQuant_
data GQuant_
type GTerm = Tree GTerm_
data GTerm_
type GVar = Tree GVar_
data GVar_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GPropAssump :: GProposition -> Tree GAssumption_
  GExmBl :: GExample -> Tree GBlock_
  GAssExm :: GLAssumption -> GProposition -> Tree GExample_
  GModKind :: GModifier -> GKind -> Tree GKind_
  Ginteger :: Tree GKind_
  Gnumber :: Tree GKind_
  Gset :: Tree GKind_
  GBAssumption :: Tree GLAssumption_
  GCAssumption :: GAssumption -> GLAssumption -> Tree GLAssumption_
  GBKind :: Tree GLKind_
  GCKind :: GKind -> GLKind -> Tree GLKind_
  GBVar :: Tree GLVar_
  GCVar :: GVar -> GLVar -> Tree GLVar_
  Geq :: GTerm -> Tree GModifier_
  Geven :: Tree GModifier_
  Ggeq :: GTerm -> Tree GModifier_
  Ggreater :: GTerm -> Tree GModifier_
  Gleq :: GTerm -> Tree GModifier_
  Gless :: GTerm -> Tree GModifier_
  Gnatural :: Tree GModifier_
  Gneg :: Tree GModifier_
  Gneq :: GTerm -> Tree GModifier_
  Gnonnegative :: Tree GModifier_
  Godd :: Tree GModifier_
  Gpositive :: Tree GModifier_
  Gprime :: Tree GModifier_
  Grational :: Tree GModifier_
  Greal :: Tree GModifier_
  GAndPred :: GPredicate -> GPredicate -> Tree GPredicate_
  GKindPred :: GKind -> Tree GPredicate_
  GModPred :: GModifier -> Tree GPredicate_
  GNegPred :: GPredicate -> Tree GPredicate_
  GOrPred :: GPredicate -> GPredicate -> Tree GPredicate_
  Gbelong :: GTerm -> Tree GPredicate_
  Gdivides :: GTerm -> Tree GPredicate_
  GAndProp :: GProposition -> GProposition -> Tree GProposition_
  GIfProp :: GProposition -> GProposition -> Tree GProposition_
  GIffProp :: GProposition -> GProposition -> Tree GProposition_
  GNotProp :: GProposition -> Tree GProposition_
  GOrProp :: GProposition -> GProposition -> Tree GProposition_
  GQuantProp :: GTerm -> GProposition -> Tree GProposition_
  GTermPred :: GTerm -> GPredicate -> Tree GProposition_
  GAll :: Tree GQuant_
  GNo :: Tree GQuant_
  GSome :: Tree GQuant_
  GAndTerm :: GTerm -> GTerm -> Tree GTerm_
  GIntTerm :: GInt -> Tree GTerm_
  GOrTerm :: GTerm -> GTerm -> Tree GTerm_
  GPropTerm :: GProposition -> Tree GTerm_
  GQuantKindPropTerm :: GQuant -> GLKind -> GProposition -> Tree GTerm_
  GQuantKindTerm :: GQuant -> GLKind -> GLVar -> Tree GTerm_
  GVarTerm :: GVar -> Tree GTerm_
  Gdiv :: GTerm -> GTerm -> Tree GTerm_
  Gexp :: GTerm -> GTerm -> Tree GTerm_
  Gminus :: GTerm -> GTerm -> Tree GTerm_
  GnegT :: GTerm -> Tree GTerm_
  Gplus :: GTerm -> GTerm -> Tree GTerm_
  Gtimes :: GTerm -> GTerm -> Tree GTerm_
  GA_Var :: Tree GVar_
  GB_Var :: Tree GVar_
  GC_Var :: Tree GVar_
  GIntVar :: GInt -> Tree GVar_
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
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GPropAssump x1,GPropAssump y1) -> and [ x1 == y1 ]
    (GExmBl x1,GExmBl y1) -> and [ x1 == y1 ]
    (GAssExm x1 x2,GAssExm y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GModKind x1 x2,GModKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Ginteger,Ginteger) -> and [ ]
    (Gnumber,Gnumber) -> and [ ]
    (Gset,Gset) -> and [ ]
    (GBAssumption,GBAssumption) -> and [ ]
    (GCAssumption x1 x2,GCAssumption y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GBKind,GBKind) -> and [ ]
    (GCKind x1 x2,GCKind y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GBVar,GBVar) -> and [ ]
    (GCVar x1 x2,GCVar y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Geq x1,Geq y1) -> and [ x1 == y1 ]
    (Geven,Geven) -> and [ ]
    (Ggeq x1,Ggeq y1) -> and [ x1 == y1 ]
    (Ggreater x1,Ggreater y1) -> and [ x1 == y1 ]
    (Gleq x1,Gleq y1) -> and [ x1 == y1 ]
    (Gless x1,Gless y1) -> and [ x1 == y1 ]
    (Gnatural,Gnatural) -> and [ ]
    (Gneg,Gneg) -> and [ ]
    (Gneq x1,Gneq y1) -> and [ x1 == y1 ]
    (Gnonnegative,Gnonnegative) -> and [ ]
    (Godd,Godd) -> and [ ]
    (Gpositive,Gpositive) -> and [ ]
    (Gprime,Gprime) -> and [ ]
    (Grational,Grational) -> and [ ]
    (Greal,Greal) -> and [ ]
    (GAndPred x1 x2,GAndPred y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GKindPred x1,GKindPred y1) -> and [ x1 == y1 ]
    (GModPred x1,GModPred y1) -> and [ x1 == y1 ]
    (GNegPred x1,GNegPred y1) -> and [ x1 == y1 ]
    (GOrPred x1 x2,GOrPred y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gbelong x1,Gbelong y1) -> and [ x1 == y1 ]
    (Gdivides x1,Gdivides y1) -> and [ x1 == y1 ]
    (GAndProp x1 x2,GAndProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIfProp x1 x2,GIfProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIffProp x1 x2,GIffProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNotProp x1,GNotProp y1) -> and [ x1 == y1 ]
    (GOrProp x1 x2,GOrProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GQuantProp x1 x2,GQuantProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GTermPred x1 x2,GTermPred y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GAll,GAll) -> and [ ]
    (GNo,GNo) -> and [ ]
    (GSome,GSome) -> and [ ]
    (GAndTerm x1 x2,GAndTerm y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIntTerm x1,GIntTerm y1) -> and [ x1 == y1 ]
    (GOrTerm x1 x2,GOrTerm y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPropTerm x1,GPropTerm y1) -> and [ x1 == y1 ]
    (GQuantKindPropTerm x1 x2 x3,GQuantKindPropTerm y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GQuantKindTerm x1 x2 x3,GQuantKindTerm y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GVarTerm x1,GVarTerm y1) -> and [ x1 == y1 ]
    (Gdiv x1 x2,Gdiv y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gexp x1 x2,Gexp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gminus x1 x2,Gminus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GnegT x1,GnegT y1) -> and [ x1 == y1 ]
    (Gplus x1 x2,Gplus y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gtimes x1 x2,Gtimes y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GA_Var,GA_Var) -> and [ ]
    (GB_Var,GB_Var) -> and [ ]
    (GC_Var,GC_Var) -> and [ ]
    (GIntVar x1,GIntVar y1) -> and [ x1 == y1 ]
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
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAssumption where
  gf (GPropAssump x1) = mkApp (mkCId "PropAssump") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "PropAssump" -> GPropAssump (fg x1)


      _ -> error ("no Assumption " ++ show t)

instance Gf GBlock where
  gf (GExmBl x1) = mkApp (mkCId "ExmBl") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ExmBl" -> GExmBl (fg x1)


      _ -> error ("no Block " ++ show t)

instance Gf GExample where
  gf (GAssExm x1 x2) = mkApp (mkCId "AssExm") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AssExm" -> GAssExm (fg x1) (fg x2)


      _ -> error ("no Example " ++ show t)

instance Gf GKind where
  gf (GModKind x1 x2) = mkApp (mkCId "ModKind") [gf x1, gf x2]
  gf Ginteger = mkApp (mkCId "integer") []
  gf Gnumber = mkApp (mkCId "number") []
  gf Gset = mkApp (mkCId "set") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ModKind" -> GModKind (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "integer" -> Ginteger 
      Just (i,[]) | i == mkCId "number" -> Gnumber 
      Just (i,[]) | i == mkCId "set" -> Gset 


      _ -> error ("no Kind " ++ show t)

instance Gf GLAssumption where
  gf GBAssumption = mkApp (mkCId "BAssumption") []
  gf (GCAssumption x1 x2) = mkApp (mkCId "CAssumption") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "BAssumption" -> GBAssumption 
      Just (i,[x1,x2]) | i == mkCId "CAssumption" -> GCAssumption (fg x1) (fg x2)


      _ -> error ("no LAssumption " ++ show t)

instance Gf GLKind where
  gf GBKind = mkApp (mkCId "BKind") []
  gf (GCKind x1 x2) = mkApp (mkCId "CKind") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "BKind" -> GBKind 
      Just (i,[x1,x2]) | i == mkCId "CKind" -> GCKind (fg x1) (fg x2)


      _ -> error ("no LKind " ++ show t)

instance Gf GLVar where
  gf GBVar = mkApp (mkCId "BVar") []
  gf (GCVar x1 x2) = mkApp (mkCId "CVar") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "BVar" -> GBVar 
      Just (i,[x1,x2]) | i == mkCId "CVar" -> GCVar (fg x1) (fg x2)


      _ -> error ("no LVar " ++ show t)

instance Gf GModifier where
  gf (Geq x1) = mkApp (mkCId "eq") [gf x1]
  gf Geven = mkApp (mkCId "even") []
  gf (Ggeq x1) = mkApp (mkCId "geq") [gf x1]
  gf (Ggreater x1) = mkApp (mkCId "greater") [gf x1]
  gf (Gleq x1) = mkApp (mkCId "leq") [gf x1]
  gf (Gless x1) = mkApp (mkCId "less") [gf x1]
  gf Gnatural = mkApp (mkCId "natural") []
  gf Gneg = mkApp (mkCId "neg") []
  gf (Gneq x1) = mkApp (mkCId "neq") [gf x1]
  gf Gnonnegative = mkApp (mkCId "nonnegative") []
  gf Godd = mkApp (mkCId "odd") []
  gf Gpositive = mkApp (mkCId "positive") []
  gf Gprime = mkApp (mkCId "prime") []
  gf Grational = mkApp (mkCId "rational") []
  gf Greal = mkApp (mkCId "real") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "eq" -> Geq (fg x1)
      Just (i,[]) | i == mkCId "even" -> Geven 
      Just (i,[x1]) | i == mkCId "geq" -> Ggeq (fg x1)
      Just (i,[x1]) | i == mkCId "greater" -> Ggreater (fg x1)
      Just (i,[x1]) | i == mkCId "leq" -> Gleq (fg x1)
      Just (i,[x1]) | i == mkCId "less" -> Gless (fg x1)
      Just (i,[]) | i == mkCId "natural" -> Gnatural 
      Just (i,[]) | i == mkCId "neg" -> Gneg 
      Just (i,[x1]) | i == mkCId "neq" -> Gneq (fg x1)
      Just (i,[]) | i == mkCId "nonnegative" -> Gnonnegative 
      Just (i,[]) | i == mkCId "odd" -> Godd 
      Just (i,[]) | i == mkCId "positive" -> Gpositive 
      Just (i,[]) | i == mkCId "prime" -> Gprime 
      Just (i,[]) | i == mkCId "rational" -> Grational 
      Just (i,[]) | i == mkCId "real" -> Greal 


      _ -> error ("no Modifier " ++ show t)

instance Gf GPredicate where
  gf (GAndPred x1 x2) = mkApp (mkCId "AndPred") [gf x1, gf x2]
  gf (GKindPred x1) = mkApp (mkCId "KindPred") [gf x1]
  gf (GModPred x1) = mkApp (mkCId "ModPred") [gf x1]
  gf (GNegPred x1) = mkApp (mkCId "NegPred") [gf x1]
  gf (GOrPred x1 x2) = mkApp (mkCId "OrPred") [gf x1, gf x2]
  gf (Gbelong x1) = mkApp (mkCId "belong") [gf x1]
  gf (Gdivides x1) = mkApp (mkCId "divides") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AndPred" -> GAndPred (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "KindPred" -> GKindPred (fg x1)
      Just (i,[x1]) | i == mkCId "ModPred" -> GModPred (fg x1)
      Just (i,[x1]) | i == mkCId "NegPred" -> GNegPred (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OrPred" -> GOrPred (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "belong" -> Gbelong (fg x1)
      Just (i,[x1]) | i == mkCId "divides" -> Gdivides (fg x1)


      _ -> error ("no Predicate " ++ show t)

instance Gf GProposition where
  gf (GAndProp x1 x2) = mkApp (mkCId "AndProp") [gf x1, gf x2]
  gf (GIfProp x1 x2) = mkApp (mkCId "IfProp") [gf x1, gf x2]
  gf (GIffProp x1 x2) = mkApp (mkCId "IffProp") [gf x1, gf x2]
  gf (GNotProp x1) = mkApp (mkCId "NotProp") [gf x1]
  gf (GOrProp x1 x2) = mkApp (mkCId "OrProp") [gf x1, gf x2]
  gf (GQuantProp x1 x2) = mkApp (mkCId "QuantProp") [gf x1, gf x2]
  gf (GTermPred x1 x2) = mkApp (mkCId "TermPred") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AndProp" -> GAndProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IfProp" -> GIfProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IffProp" -> GIffProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NotProp" -> GNotProp (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OrProp" -> GOrProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuantProp" -> GQuantProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "TermPred" -> GTermPred (fg x1) (fg x2)


      _ -> error ("no Proposition " ++ show t)

instance Gf GQuant where
  gf GAll = mkApp (mkCId "All") []
  gf GNo = mkApp (mkCId "No") []
  gf GSome = mkApp (mkCId "Some") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "All" -> GAll 
      Just (i,[]) | i == mkCId "No" -> GNo 
      Just (i,[]) | i == mkCId "Some" -> GSome 


      _ -> error ("no Quant " ++ show t)

instance Gf GTerm where
  gf (GAndTerm x1 x2) = mkApp (mkCId "AndTerm") [gf x1, gf x2]
  gf (GIntTerm x1) = mkApp (mkCId "IntTerm") [gf x1]
  gf (GOrTerm x1 x2) = mkApp (mkCId "OrTerm") [gf x1, gf x2]
  gf (GPropTerm x1) = mkApp (mkCId "PropTerm") [gf x1]
  gf (GQuantKindPropTerm x1 x2 x3) = mkApp (mkCId "QuantKindPropTerm") [gf x1, gf x2, gf x3]
  gf (GQuantKindTerm x1 x2 x3) = mkApp (mkCId "QuantKindTerm") [gf x1, gf x2, gf x3]
  gf (GVarTerm x1) = mkApp (mkCId "VarTerm") [gf x1]
  gf (Gdiv x1 x2) = mkApp (mkCId "div") [gf x1, gf x2]
  gf (Gexp x1 x2) = mkApp (mkCId "exp") [gf x1, gf x2]
  gf (Gminus x1 x2) = mkApp (mkCId "minus") [gf x1, gf x2]
  gf (GnegT x1) = mkApp (mkCId "negT") [gf x1]
  gf (Gplus x1 x2) = mkApp (mkCId "plus") [gf x1, gf x2]
  gf (Gtimes x1 x2) = mkApp (mkCId "times") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AndTerm" -> GAndTerm (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IntTerm" -> GIntTerm (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OrTerm" -> GOrTerm (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PropTerm" -> GPropTerm (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "QuantKindPropTerm" -> GQuantKindPropTerm (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "QuantKindTerm" -> GQuantKindTerm (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "VarTerm" -> GVarTerm (fg x1)
      Just (i,[x1,x2]) | i == mkCId "div" -> Gdiv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "exp" -> Gexp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "minus" -> Gminus (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "negT" -> GnegT (fg x1)
      Just (i,[x1,x2]) | i == mkCId "plus" -> Gplus (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "times" -> Gtimes (fg x1) (fg x2)


      _ -> error ("no Term " ++ show t)

instance Gf GVar where
  gf GA_Var = mkApp (mkCId "A_Var") []
  gf GB_Var = mkApp (mkCId "B_Var") []
  gf GC_Var = mkApp (mkCId "C_Var") []
  gf (GIntVar x1) = mkApp (mkCId "IntVar") [gf x1]
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
      Just (i,[x1]) | i == mkCId "IntVar" -> GIntVar (fg x1)
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


instance Compos Tree where
  compos r a f t = case t of
    GPropAssump x1 -> r GPropAssump `a` f x1
    GExmBl x1 -> r GExmBl `a` f x1
    GAssExm x1 x2 -> r GAssExm `a` f x1 `a` f x2
    GModKind x1 x2 -> r GModKind `a` f x1 `a` f x2
    GCAssumption x1 x2 -> r GCAssumption `a` f x1 `a` f x2
    GCKind x1 x2 -> r GCKind `a` f x1 `a` f x2
    GCVar x1 x2 -> r GCVar `a` f x1 `a` f x2
    Geq x1 -> r Geq `a` f x1
    Ggeq x1 -> r Ggeq `a` f x1
    Ggreater x1 -> r Ggreater `a` f x1
    Gleq x1 -> r Gleq `a` f x1
    Gless x1 -> r Gless `a` f x1
    Gneq x1 -> r Gneq `a` f x1
    GAndPred x1 x2 -> r GAndPred `a` f x1 `a` f x2
    GKindPred x1 -> r GKindPred `a` f x1
    GModPred x1 -> r GModPred `a` f x1
    GNegPred x1 -> r GNegPred `a` f x1
    GOrPred x1 x2 -> r GOrPred `a` f x1 `a` f x2
    Gbelong x1 -> r Gbelong `a` f x1
    Gdivides x1 -> r Gdivides `a` f x1
    GAndProp x1 x2 -> r GAndProp `a` f x1 `a` f x2
    GIfProp x1 x2 -> r GIfProp `a` f x1 `a` f x2
    GIffProp x1 x2 -> r GIffProp `a` f x1 `a` f x2
    GNotProp x1 -> r GNotProp `a` f x1
    GOrProp x1 x2 -> r GOrProp `a` f x1 `a` f x2
    GQuantProp x1 x2 -> r GQuantProp `a` f x1 `a` f x2
    GTermPred x1 x2 -> r GTermPred `a` f x1 `a` f x2
    GAndTerm x1 x2 -> r GAndTerm `a` f x1 `a` f x2
    GIntTerm x1 -> r GIntTerm `a` f x1
    GOrTerm x1 x2 -> r GOrTerm `a` f x1 `a` f x2
    GPropTerm x1 -> r GPropTerm `a` f x1
    GQuantKindPropTerm x1 x2 x3 -> r GQuantKindPropTerm `a` f x1 `a` f x2 `a` f x3
    GQuantKindTerm x1 x2 x3 -> r GQuantKindTerm `a` f x1 `a` f x2 `a` f x3
    GVarTerm x1 -> r GVarTerm `a` f x1
    Gdiv x1 x2 -> r Gdiv `a` f x1 `a` f x2
    Gexp x1 x2 -> r Gexp `a` f x1 `a` f x2
    Gminus x1 x2 -> r Gminus `a` f x1 `a` f x2
    GnegT x1 -> r GnegT `a` f x1
    Gplus x1 x2 -> r Gplus `a` f x1 `a` f x2
    Gtimes x1 x2 -> r Gtimes `a` f x1 `a` f x2
    GIntVar x1 -> r GIntVar `a` f x1
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
