
MODULE test_RK4_five EXPORTS Main;


(* ode      : y^{V} - y' = x^2;
   bc       : y = y' = y'' = y''' = y^{IV} = 0 at x = 0;
   interval : [0,1]
   soln     : y(x) = -x^3/3 + exp(x)/2 - sin(x) - exp(-x)/2;

   remarks  : numerical solution and exact analytical solution agree
              to 3 s.f.;  confirmed by Maple plot of exact analytical 
              soln;
  *)

IMPORT ode_v1 AS Ode;
IMPORT SimonH_Log_v01 AS Log;
FROM Math IMPORT pow, sin, exp;

CONST
  order = 5;
  num_intervals = 30;

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
  RETURN y[1] - pow(x,2.0D0);
END F;



BEGIN
  
  (* Set initial conditions : *)
  init_conds[1] := 0.0D0;
  init_conds[2] := 0.0D0;
  init_conds[3] := 0.0D0;
  init_conds[4] := 0.0D0;
  init_conds[5] := 0.0D0;

  (* Set solution interval : *)
  a := 0.0D0;  b := 1.0D0;  

  (* Solve : *)
  Ode.RK4(order, a, b, num_intervals, F, init_conds, xx, yy);

  (* Loop over interval : *)
  FOR i := 0 TO num_intervals DO

    (* Output numerical solution : *)
    Log.Put_LReal(yy[i]);  Log.Put_Text("  ");

    (* Compute interval value corresponding to "i" : *)
    x := a + FLOAT(i,LONGREAL)*(b - a)/FLOAT(num_intervals,LONGREAL);

    (* Output exact solution : *)
    Log.Put_LReal( - pow(x,3.0D0)/3.0D0 + exp(x)/2.0D0 - sin(x)
                   - exp(-x)/2.0D0);
    Log.Put_Ln();
  END
END test_RK4_five.

