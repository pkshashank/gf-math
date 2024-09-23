abstract CoreDemo = Core ** {
    flags
        startcat = Block ;
    fun

        rational : Modifier ;
        odd : Modifier ;
        real : Modifier ;
        even : Modifier ;
        positive : Modifier ;
        nonnegative : Modifier ;
        neg : Modifier ;
        prime : Modifier ;
        natural : Modifier ;

        greater : Term -> Modifier ;
        less : Term -> Modifier ;
        eq : Term -> Modifier ;
        leq : Term -> Modifier ;
        geq : Term -> Modifier ;
        neq : Term -> Modifier ;


        set : Kind ;
        integer : Kind ;
        number : Kind ;

        
        divides : Term -> Predicate ;
        belong : Term -> Predicate ;

        plus : Term -> Term -> Term ;
        minus : Term -> Term -> Term ;
        times : Term -> Term -> Term ;
        div : Term -> Term -> Term ;
        exp : Term -> Term -> Term ;
        negT : Term -> Term ;
        
    fun
        x_Var, y_Var, z_Var, u_Var : Var ; 
        a_Var, b_Var, c_Var, d_Var : Var ;
        f_Var, g_Var : Var ;
        k_Var, m_Var, n_Var, p_Var : Var ;
        q_Var, r_Var : Var ;
        s_Var, t_Var : Var ;
        A_Var, B_Var, C_Var, K_Var : Var ;
        L_Var, M_Var, S_Var, T_Var : Var ;

}
