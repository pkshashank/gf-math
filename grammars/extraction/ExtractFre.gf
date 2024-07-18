--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractFre of ExtractFreAbs =
  MorphoDictFre,
  MathWordsFre, -- built by add_language.py
  StructuralFre

** open
  SyntaxFre,
  SymbolicFre,
  (E=ExtendFre),
  (P=ParadigmsFre),
  (R=ResFre),
  (M=MakeStructuralFre)
  in {

lincat
  Term = Utt ;

lin
  NPTerm np = mkUtt np ;
  APTerm ap = mkUtt ap ;
  AdvTerm adv = mkUtt adv ;

  UseN n = mkCN n ;
  AdjCN ap cn = mkCN ap cn ;
  CompoundN x y = E.CompoundN x y ; 
  IntCompoundCN i x = prefixCN (i.s ++ hyphen) x ;
  NameCompoundCN n x = prefixCN (mkUtt (mkNP n)).s x ;
  NounIntCN cn i = mkCN cn (symb i) ;
  NounPrepCN cn adv = mkCN cn adv ;
  NounGenCN cn np = mkCN cn (mkAdv possess_Prep np) ;

  DefCN cn = mkNP the_Det cn ;
  DefPluralCN cn = mkNP thePl_Det cn ;
  IndefCN cn = mkNP a_Det cn ;
  IndefPluralCN cn = mkNP aPl_Det cn ;
  BareCN cn = mkNP (M.mkDet "") cn ;

  PositA a = mkAP a ;
  AdAP ad ap = mkAP ad ap ;

  PrepNP prep np = mkAdv prep np ;

oper
  -- not used for parsing, but as replacements of Int-functions when used as lin terms, as a work-around to GF type checking
  nounStrCN : CN -> Str -> CN = \cn, s -> mkCN cn (symb s) ;
  strCompoundCN : Str -> CN -> CN = \s, cn -> prefixCN (s ++ hyphen) cn ;

  prefixCN : Str -> CN -> CN = \s, cn ->
    cn ** {s = \\n => s ++ cn.s ! n} ;

  hyphen : Str = Predef.BIND ++ "-" ++ Predef.BIND ;

}