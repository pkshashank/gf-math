resource Utilities = open Prelude in {
    oper
        connective : Str -> SS -> SS -> SS = \s, s1, s2 -> infixSS s s1 s2 ;
}