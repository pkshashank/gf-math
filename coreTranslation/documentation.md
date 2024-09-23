# Translation Steps

If-then kind of statements become assume-then statements.
Let statements also become assume-statements.


--------------------------------
# Progress

Basic input output and tree manipulation tests work now.

-------------------------------
# Further Tasks To Do.

Textual Simplifications.
EveryTerm, EachTerm, AllTerm, AnyTerm becomes EveryTerm
SomeTerm, and NoTerm remain the same

All extra parenthesis should be removed

-------------------------------------
# Changes needed in the grammar.

1. **Conjunction of terms with assume have to syntactic trees:**

ForthelDemo> p -cat=Assumption "assume m , n are odd integers"
NamesAssumption (ConsName (VarName m_Var) (BaseName (VarName n_Var))) (AdjClassNoun (PrimClassNoun integer_PrimClass) odd_Adjective)

ForthelDemo> p -cat=Assumption "assume m is an odd integer"
NamesAssumption (BaseName (VarName m_Var)) (AdjClassNoun (PrimClassNoun integer_PrimClass) odd_Adjective)
StatementAssumption (SimpleStatement (OneTerms (SymbolicTerm (ExpSymbTerm (TVar m_Var)))) (PosOnePredicates (IsaPredicate (ClassNounNotion (AdjClassNoun (PrimClassNoun integer_PrimClass) odd_Adjective)))))



