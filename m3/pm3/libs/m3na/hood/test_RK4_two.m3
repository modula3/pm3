
MODULE test_RK4_two EXPORTS Main;


(* ode      :  y''' - 6y'' + 11y' - 6y = x^2;
   bc       :  y = y' = y'' = 0 at x = 0;
   interval : [0,1]
   soln     :  y(x) = exp(x) - exp(2.0D0*x)/4.0D0 + exp(3.0D0*x)/27.0D0
                   - 85.0D0/108.0D0 - 11.0D0*x/18.0D0 - pow(x,2.0D0)/6.0D0;

   remarks  : exact and numerical solutions match well;  confirmed by
              Maple plot of exact solution;
  *)

IMPORT ode_v1 AS Ode;
IMPORT SimonH_Log_v01 AS Log;
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
						(***[0..num_intervals] ***)

  init_conds := NEW(one_d_array, order+1);
                                      (***ARRAY [1..order] OF LONGREAL;***)

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
  init_conds[1] := 0.0D0;
  init_conds[2] := 0.0D0;
  init_conds[3] := 0.0D0;

  (* Set solution interval : *)
  b := 1.0D0;  a := 0.0D0;

  (* Solve : *)
  Ode.RK4(order, a, b, num_intervals, F, init_conds, xx, yy);

  (* Loop over interval : *)
  FOR i := 0 TO num_intervals DO

    (* Output numerical solution : *)
    Log.Put_LReal(yy[i]);  Log.Put_Text("  ");

    (* Compute interval value corresponding to "i" : *)
    x := FLOAT(i,LONGREAL)*(b - a)/FLOAT(num_intervals,LONGREAL);

    (* Output exact solution : *)
    Log.Put_LReal(   exp(x) - exp(2.0D0*x)/4.0D0 + exp(3.0D0*x)/27.0D0
                   - 85.0D0/108.0D0 - 11.0D0*x/18.0D0 - pow(x,2.0D0)/6.0D0);
    Log.Put_Ln();
  END
END test_RK4_two.

