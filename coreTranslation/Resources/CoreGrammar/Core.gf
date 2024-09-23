abstract Core = {
    flags
        startcat = Block ;
    cat
        Predicate ;
        Proposition ;
        Term ;
        Kind ;
        LKind ; -- {0} ;
        Modifier ;
        Var ;
        LVar ; -- {0} ;
        Quant ;

    -- List functions
    fun
        BVar : LVar ;
        CVar : Var -> LVar -> LVar ;
        BKind : LKind ;
        CKind : Kind -> LKind -> LKind ;
        
        NotProp : Proposition -> Proposition ;
        AndProp, OrProp, IffProp, IfProp : Proposition -> Proposition -> Proposition ; -- And S,T,U

        QuantProp : Term -> Proposition -> Proposition ; -- for every integer, S
        TermPred : Term -> Predicate -> Proposition ; -- every integer is odd

        AndPred, OrPred : Predicate -> Predicate -> Predicate ;  -- is even and odd
        NegPred : Predicate -> Predicate ;  -- is not even
        ModPred : Modifier -> Predicate ; -- is greater than 2
        KindPred : Kind -> Predicate ; -- is integer

        IntTerm : Int -> Term ;
        AndTerm, OrTerm : Term -> Term -> Term ;  -- every odd number x and every integer y
        VarTerm : Var -> Term ; -- x
        QuantKindTerm : Quant -> LKind -> LVar -> Term ; -- no (odd integer) x
        QuantKindPropTerm : Quant -> LKind -> Proposition -> Term ; -- no (odd integer) x > 0
        PropTerm : Proposition -> Term ; -- x > 0 in x > 0 is odd
        
        
        ModKind : Modifier -> Kind -> Kind ; -- even integer

        

        IntVar : Int -> Var ;

        All, No, Some : Quant ;

    cat
        Block ;
        Example ;
        Assumption ;
        LAssumption ;-- {0} ;

    -- List categories
    fun
        BAssumption : LAssumption;
        CAssumption : Assumption -> LAssumption -> LAssumption ;

    fun
        PropAssump : Proposition -> Assumption ; 
    
        AssExm : LAssumption -> Proposition -> Example ;

        ExmBl : Example -> Block ; 

}
