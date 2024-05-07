-- mathematical terms as they appear in "normal" mathematical text

abstract Term = {

cat
  Equation ;
  Term ;
  [Term] {1} ;
  Variable ;
  Function ;

fun
  EEq, ENeq, ELt, EGt, ELe, EGe, ESim : Term -> Term -> Equation ;

  TPlus, TMinus, TTimes, TDiv, TExp : Term -> Term -> Term ;
  TNeg : Term -> Term ;
  TApp : Function -> [Term] -> Term ;

  TVariable : Variable -> Term ;
  TNumber : Float -> Term ;

  FVariable : Variable -> Function ;
  FDerivative : Function -> Function ;


  x_Variable, y_Variable, z_Variable, u_Variable : Variable ; 
  a_Variable, b_Variable, c_Variable, d_Variable : Variable ;
  f_Variable, g_Variable : Variable ;

}