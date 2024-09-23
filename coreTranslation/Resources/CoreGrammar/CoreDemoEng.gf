concrete CoreDemoEng of CoreDemo = CoreEng ** open Prelude in {
    lin
        rational = ss "rational" ;
        odd = ss "odd" ;
        real = ss "real" ;
        even = ss "even" ;
        positive = ss "positive" ;
        nonnegative = ss "nonnegative" ;
        neg = ss "neg" ;
        prime = ss "prime" ;
        natural = ss "natural" ;

        greater = prefixSS ">" ;
        less = prefixSS "<" ;
        eq = prefixSS "=" ;
        leq = prefixSS "<=" ;
        geq = prefixSS ">=" ;
        neq = prefixSS "/=" ;

        set = ss "set" ;
        integer = ss "integer" ;
        number = ss "number" ;
        
        divides = prefixSS "divides" ;
        belong = prefixSS "belongs" ;

        plus t1 t2 = parenss (infixSS "+" t1 t2) ;
        minus t1 t2 = parenss (infixSS "-" t1 t2) ;
        times t1 t2 = parenss (infixSS "*" t1 t2) ;
        div t1 t2 = parenss (infixSS "/" t1 t2) ;
        exp t1 t2 = parenss (infixSS "^" t1 t2) ;
        negT t = parenss (prefixSS "-" t) ;

    lin
        x_Var = ss "x" ;
        y_Var = ss "y" ;
        z_Var = ss "z" ;
        u_Var = ss "u" ;
        a_Var = ss "a" ;
        b_Var = ss "b" ;
        c_Var = ss "c" ;
        d_Var = ss "d" ;
        f_Var = ss "f" ;
        g_Var = ss "g" ;
        k_Var = ss "k" ;
        m_Var = ss "m" ;
        n_Var = ss "n" ;
        p_Var = ss "p" ;
        q_Var = ss "q" ;
        r_Var = ss "r" ;
        s_Var = ss "s" ;
        t_Var = ss "t" ;
        A_Var = ss "A" ;
        B_Var = ss "B" ;
        C_Var = ss "C" ;
        K_Var = ss "K" ;
        L_Var = ss "L" ;
        M_Var = ss "M" ;
        S_Var = ss "S" ;
        T_Var = ss "T" ;
}
