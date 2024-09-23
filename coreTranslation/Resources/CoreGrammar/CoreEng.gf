concrete CoreEng of Core = Utilities ** open Prelude in{
    lincat
        Predicate = SS ;
        Proposition = SS ;
        Term = SS ;
        Kind = SS ;
        LKind = SS ;
        Modifier = SS ;
        Var = SS ;
        LVar = SS ;
        Quant = SS ;

    -- L commands
    lin
        BVar = ss "" ;
        CVar = infixSS "" ;
        BKind = ss "" ;
        CKind = infixSS "" ;
    
    lin
    --- For functions
        NotProp = prefixSS "s!" ;
        AndProp = connective "s&" ; 
        OrProp = connective "s|" ;
        IfProp  = connective "s->"  ;
        IffProp  = connective "s<->"  ;
        QuantProp qnt = cc2 (prefixSS "for" qnt) ;
        TermPred term pred = {s = pred.s ++ paren term.s} ;

        AndPred = connective "p&" ;
        OrPred = connective "p|" ;
        NegPred = prefixSS "p!" ;
        ModPred = id SS ;
        KindPred = id SS ;

        IntTerm = id SS ;
        AndTerm = connective "t&" ;
        OrTerm = connective "t|" ;
        VarTerm = id SS ;
        QuantKindTerm = cc3 ;
        QuantKindPropTerm = cc3 ;
        PropTerm = id SS ;

        ModKind = cc2 ;

        

        IntVar int = parenss (prefixSS "x" int) ; 

        All = ss "all" ;
        No = ss "no" ;
        Some = ss "some" ;
    
   lincat
        Block = SS ;
        Example = SS ;
        Assumption = SS ;
        LAssumption = SS ;
    lin
        BAssumption = ss "" ;
        CAssumption ass lass = cc3 ass (ss ".") lass ;

    lin 
        PropAssump = id SS ;

        AssExm lass prop = {s = "ex ." ++ lass.s ++ ":" ++ prop.s} ;

        ExmBl = id SS ;

}