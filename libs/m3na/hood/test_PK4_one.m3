
MODULE test_RK4_one EXPORTS Main;


(*  ode      : y'' = exp(2x)*sin(x) - 2y + 2y'
    bc       : y(0) = -0.4,  y'(0) = -0.6;
    solution : 0.2exp(2.0*x)(sin(x) - 2.0*cos(x));
    interval : [0,1]
    soln     : compared to Table 5.12, Burden, Faires and Reynolds, "Numerical
               Analysis", 2nd ed.;  also exact solution and numerical approx.
               agree to several s.f.;
  *)

FROM Math IMPORT exp, sin, cos;
IMPORT ode_v1 AS ODE;
IMPORT SimonH_Log_v01 AS Log;
IMPORT Fatal_Error_v0 AS FE;

CONST
  order = 2;
  num_intervals = 10;

TYPE
  one_d_array = REF ARRAY OF LONGREAL;

VAR
  (* arrays of indy and dep vars, i.e., solution : *)
  xx := NEW(one_d_array, num_intervals+1);
  yy := NEW(one_d_array, num_intervals+1);

  init_conds := NEW(one_d_array, order+1);



(*<T> The given ode is
  $$ y'' = exp(2x)sin(x) - 2y + 2y'. $$
  *)
PROCEDURE F (         x : LONGREAL;
             READONLY y : REF ARRAY OF LONGREAL) : LONGREAL =
BEGIN
  RETURN exp(2.0D0*x)*sin(x) - 2.0D0*y[0] + 2.0D0*y[1];
END F;


BEGIN

  (* Apply initial conditions : *)
  init_conds[1] := -0.4D0;
  init_conds[2] := -0.6D0;

  (* Call integrator : *)
  TRY
    ODE.RK4(order, 0.0D0, 1.0D0, num_intervals, F, init_conds, xx, yy);
  EXCEPT
    | ODE.F_Sys_Error => FE.F_Error("RK4 failed --- F_Sys_Error.");
  END;  

  (* Output results --- compare to exact solution : *)
  TRY
    Log.Put_Text("\n\n x             y-exact       y-numerical \n\n");
    FOR i := 0 TO num_intervals DO
      Log.Put_LReal(xx[i]);
      Log.Put_Text("  ");
      Log.Put_LReal(0.2D0*exp(2.0D0*xx[i])*(sin(xx[i]) - 2.0D0*cos(xx[i])));
      Log.Put_Text("  ");
      Log.Put_LReal(yy[i]);  
      Log.Put_Ln();
    END;
    Log.Put_Text("\n\n");
  EXCEPT
    | Log.Cannot_Put => FE.F_Error("Output of results failed!");
  END;
END test_RK4_one.

