
MODULE test_RK4_three EXPORTS Main;


(* ode      : y''' - 6y'' + 11y' - 6y = x^2;
   bc       : y = y' = y'' = 1 at x = -2;
   interval : [-2,-1]
   soln     : y(x) = 2*exp(x)/exp(-2) 
                   - 5/4*exp(2.0D0*x)/exp(-4) 
                   + 13/27*exp(3.0D0*x)/exp(-6)
                   - 85.0D0/108.0D0 - 11.0D0*x/18.0D0 - pow(x,2.0D0)/6.0D0;

   remarks  : exact and numerical solutions match well;  confirmed by
              Maple plot of exact solution;
  *)

IMPORT ode_v1 AS Ode;
IMPORT SimonH_Log_v01 AS Log;
IMPORT Fatal_Error_v0 AS FE;
FROM Math IMPORT exp, pow;

CONST
  order = 3;
  num_intervals = 10;

TYPE
  one_d_array = REF ARRAY OF LONGREAL;

VAR
  (* arrays of indy and dep vars, i.e., solution : *)
  xx := NEW(one_d_array, num_intervals+1);
  yy := NEW(one_d_array, num_intervals+1);

  init_conds := NEW(one_d_array, order+1);

  (* x \in [a,b] : *)
  a, b, x : LONGREAL;



(*<T> The given ode is
  $$ y''' - 6y'' + 11y' - 6y = x^2;. $$
  *)
PROCEDURE F (         x : LONGREAL;
             READONLY y : REF ARRAY OF LONGREAL) : LONGREAL =
BEGIN
  RETURN 6.0D0*y[2] - 11.0D0*y[1] + 6.0D0*y[0] + pow(x,2.0D0);
END F;


BEGIN
  
  (* Set initial conditions : *)
  init_conds[1] := 1.0D0;
  init_conds[2] := 1.0D0;
  init_conds[3] := 1.0D0;

  (* Set solution interval : *)
  a := -2.0D0;  b := -1.0D0;  

  (* Call integrator : *)
  TRY
    Ode.RK4(order, a, b, num_intervals, F, init_conds, xx, yy);
  EXCEPT
    | Ode.F_Sys_Error => FE.F_Error("RK4 failed --- F_Sys_Error.");
  END;  

  (* Loop over interval : *)
  FOR i := 0 TO num_intervals DO

    (* Output numerical solution : *)
    Log.Put_LReal(yy[i]);  Log.Put_Text("  ");

    (* Compute interval value corresponding to "i" : *)
    x := a + FLOAT(i,LONGREAL)*(b - a)/FLOAT(num_intervals,LONGREAL);

    (* Output exact solution : *)
    Log.Put_LReal(  2.0D0*exp(x)/exp(-2.0D0) 
                  - 5.0D0*exp(2.0D0*x)/(4.0D0*exp(-4.0D0)) 
                  + 13.0D0*exp(3.0D0*x)/(27.0D0*exp(-6.0D0))
                  - 85.0D0/108.0D0 - 11.0D0*x/18.0D0 
                  - pow(x,2.0D0)/6.0D0);
    Log.Put_Ln();
  END
END test_RK4_three.

