
MODULE test_RK4_four EXPORTS Main;


(* ode      : y^{IV} + y'' = x^3;
   bc       : y = y' = y'' = y''' = 0 at x = 0;
   interval : [0,2]
   soln     : y(x) = -x^3 + x^5/20 + 6x - 6sin(x);

   remarks  : numerical solution and exact analytical solution agree
              to 3 s.f.;  confirmed by Maple plot of exact analytical 
              soln;;
  *)

IMPORT ode_v1 AS Ode;
IMPORT SimonH_Log_v01 AS Log;
FROM Math IMPORT pow, sin;

CONST
  order = 4;
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
  $$ y^{IV} + y'' = x^3 $$
  *)
PROCEDURE F (         x : LONGREAL;
             READONLY y : REF ARRAY OF LONGREAL) : LONGREAL =
BEGIN
  RETURN pow(x,3.0D0) - y[2];
END F;



BEGIN
  
  (* Set initial conditions : *)
  init_conds[1] := 0.0D0;
  init_conds[2] := 0.0D0;
  init_conds[3] := 0.0D0;

  (* Set solution interval : *)
  a := 0.0D0;  b := 2.0D0;  

  (* Solve : *)
  Ode.RK4(order, a, b, num_intervals, F, init_conds, xx, yy);

  (* Loop over interval : *)
  FOR i := 0 TO num_intervals DO

    (* Output numerical solution : *)
    Log.Put_LReal(yy[i]);  Log.Put_Text("  ");

    (* Compute interval value corresponding to "i" : *)
    x := a + FLOAT(i,LONGREAL)*(b - a)/FLOAT(num_intervals,LONGREAL);

    (* Output exact solution : *)
    Log.Put_LReal( - pow(x,3.0D0) + pow(x,5.0D0)/20.0D0 + 6.0D0*x 
                   - 6.0D0*sin(x));
    Log.Put_Ln();
  END
END test_RK4_four.

