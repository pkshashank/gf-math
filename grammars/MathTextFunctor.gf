incomplete concrete MathTextFunctor of MathText = MathWikidata, Prop, TermLatex **

open
  Syntax,
  Extend,
  Grammar,
  Symbolic,
  Formal,
  Prelude

in {

lincat
  Paragraph = Text ;
  Definition = S ;
  Condition = {s : Agreement => Cl} ;
  Hypothesis = Text ;
  [Hypothesis] = Text ;
  [Variable] = NP ;

oper
  Agreement : PType = {} ;
  refPron : Agreement -> Pron = \_ -> it_Pron ;
  kindAgr : CN -> Agreement = \_ -> <> ;

  genRP : CN -> RP = \cn -> GenRP singularNum cn ;

  thenDef_Adv : Adv = then_Adv ;

lin
  ParDefinition hs d = mkText hs (mkText d) ;
  ParStatement hs p = mkText hs (mkText (mkS thenDef_Adv p.s)) ;

  DefIsA a b = mkS (mkCl (mkNP a_Det a) b) ;
  DefIsASuch a b c = mkS (mkCl (mkNP a_Det a) (mkCN b (mkRS (mkRCl (c.s ! kindAgr b))))) ;
  DefIsAIf a b c = mkS (mkCl (mkNP a_Det b) (mkCN a (Syntax.mkAdv if_Subj (mkS (c.s ! kindAgr b))))) ;
  DefWhose a b f c = mkS (mkCl (mkNP a_Det a) (mkCN b (mkRS (mkRCl (genRP f.v) (mkVP c))))) ;

  CondIsA b = {s = \\a => mkCl (mkNP (refPron a)) b} ;
  CondPred1 p = {s = \\a => mkCl (mkNP (refPron a)) p} ;
  CondItsFun1 f p = {s = \\a => mkCl (mkNP (refPron a) f.v) p} ;

  HypTyping xs k = mkText (ImpP3 xs (mkVP k)) ;
  
  BaseHypothesis = emptyText ;
  ConsHypothesis h hs = mkText h hs ; 

  BaseVariable x = symb (mkSymb (mathEnv x)) ;
  ConsVariable x xs = mkNP and_Conj (symb (mkSymb (mathEnv x))) xs ;

-- using Wikidata --- the categories should be decided there
  KindQN qn = qn ;
  Fun1QN qn = mkFun1 (mkUtt qn).s qn possess_Prep ; --- leads to different function names in langs

-- using Term
  PEquation e = {s = symb (mkSymb (mathEnv e.s)) ; c = False} ;
  ITerm t = {s = symb (mkSymb (mathEnv (top t))) ; isSymbolic = True} ;

oper
  mathEnv : Str -> Str = \s ->
    "$" ++ s ++ "$" ;
    -- change to s if you don't want dollars

}