
(*****************************************************************************)
(*****************************************************************************)

MODULE test_relax_two EXPORTS Main;

(*<T>
  This for $y'' = (1/8)(32 + 2x^3 - yy')$, with mixed boundary conditions
  boundary conditions $y + y'= 3$ at k\_a and $y + y' = 167/9$ at k\_b;  
  the exact solution is $y x^2 + 16/x$, same as for Test 2.
 *)

IMPORT 
    Fatal_Error_v0 AS FE,
    SimonH_Log_v01 AS Log,
    Lin_Alg_v03 AS LA,
    ode_v1 AS ODE;

FROM Math IMPORT pow;

CONST
  (* Constant parameters : *)
  debug           = TRUE;
  tolerance       = 0.001D0;
  iteration_limit = 10;

  num_pts  = 20;
  num_eqns = 2;
  num_bca  = 1;

  k_a = 0;
  k_b = num_pts - 1;

  x_a = 1.0D0;
  x_b = 3.0D0;


TYPE
  x_type       = REF ARRAY OF LONGREAL;
  y_type       = REF ARRAY OF ARRAY OF LONGREAL;
  y_scale_type = REF ARRAY OF LONGREAL;


VAR
  x       := NEW(x_type, num_pts);
  Y       := NEW(y_type, num_eqns, num_pts);
  y_scale := NEW(y_scale_type, num_eqns);      

  delta_x : LONGREAL;


PROCEDURE Compute_F(READONLY k : INTEGER;
                    READONLY x : REF ARRAY OF LONGREAL;
                    READONLY Y : REF ARRAY OF ARRAY OF LONGREAL;
                    VAR      F : REF ARRAY OF LONGREAL) 
RAISES {} =
BEGIN
  IF k = k_a THEN
      F[FIRST(F^)]   := Y[0][0] + Y[1][0] - 3.0D0;
      F[FIRST(F^)+1] := 0.0D0;
  ELSIF k = k_b + 1 THEN
      F[FIRST(F^)]   := Y[0][num_pts-1] + Y[1][num_pts-1] - 167.0D0/9.0D0;
      F[FIRST(F^)+1] := 0.0D0;
  ELSE (* interior : *) 
      F[FIRST(F^)] := Y[0][k] - Y[0][k-1] 
                              - 0.5D0*delta_x*(Y[1][k] + Y[1][k-1]);
      F[FIRST(F^)+1] := Y[1][k] - Y[1][k-1] 
                      - (delta_x/8.0D0)
                       *(   32.0D0 + 2.0D0*pow(0.5D0*(x[k] + x[k-1]),3.0D0) 
                          - 0.25D0*(Y[0][k] + Y[0][k-1])
                                  *(Y[1][k] + Y[1][k-1]));
  END;
END Compute_F;

PROCEDURE Compute_S(READONLY k : INTEGER;
                    READONLY x : REF ARRAY OF LONGREAL;
                    READONLY Y : REF ARRAY OF ARRAY OF LONGREAL;
                    VAR      S : REF ARRAY OF ARRAY OF LONGREAL) 
RAISES {} =
BEGIN
  WITH N = num_eqns DO
    IF k = k_a THEN
        S[FIRST(S^)][0] := 1.0D0;
        S[FIRST(S^)][1] := 1.0D0;
        S[FIRST(S^)][N+0] := 0.0D0;
        S[FIRST(S^)][N+1] := 0.0D0;
        S[FIRST(S^)+1][0] := 0.0D0;
        S[FIRST(S^)+1][1] := 0.0D0;
        S[FIRST(S^)+1][N+0] := 0.0D0;
        S[FIRST(S^)+1][N+1] := 0.0D0;
    ELSIF k = k_b+1 THEN
        S[FIRST(S^)][0] := 1.0D0;
        S[FIRST(S^)][1] := 1.0D0;
        S[FIRST(S^)][N+0] := 0.0D0;
        S[FIRST(S^)][N+1] := 0.0D0;
        S[FIRST(S^)+1][0] := 0.0D0;
        S[FIRST(S^)+1][1] := 0.0D0;
        S[FIRST(S^)+1][N+0] := 0.0D0;
        S[FIRST(S^)+1][N+1] := 0.0D0;
    ELSE 
        S[FIRST(S^)][0]     := -1.0D0;
        S[FIRST(S^)][1]     := -0.5D0*delta_x;
        S[FIRST(S^)][N+0]   :=  1.0D0;
        S[FIRST(S^)][N+1]   := -0.5D0*delta_x;
        S[FIRST(S^)+1][0]   :=  (delta_x/32.0D0)*(Y[1][k] + Y[1][k-1]);
        S[FIRST(S^)+1][1]   := -1.0D0 + (delta_x/32.0D0)*(Y[0][k] + Y[0][k-1]);
        S[FIRST(S^)+1][N+0] :=  (delta_x/32.0D0)*(Y[1][k] + Y[1][k-1]);
        S[FIRST(S^)+1][N+1] :=  1.0D0 + (delta_x/32.0D0)*(Y[0][k] + Y[0][k-1]);
    END;
  END;
END Compute_S;

BEGIN

(* INITIALISATION : *)

  (* Non-constant parameters : *)
  delta_x := (x_b - x_a)/FLOAT(num_pts - 1, LONGREAL);  

  (* Set up grid : *)
  FOR k := k_a TO k_b DO
      x[k] := x_a + delta_x*FLOAT(k - k_a, LONGREAL);
  END;

  (*<T> Initial guess;  second order equation, num\_eqns = 2;  exact solution 
     is $y_0 = y = x^2 + 16/x$ (and therefore, $y_1 = y' = 2x - 16/x^2$);  
     therefore set this, but with some systematic drift : *)

  FOR k := k_a TO k_b DO
      Y[0][k] := x[k]*x[k] + 16.0D0/x[k] + 1.0D0;     
      Y[1][k] := 2.0D0*x[k] - 16.0D0/(x[k]*x[k]) + 0.2D0;
  END;                               

  (* Scale of each dependent variable --- helps convergence test : *)
  y_scale[0] := 15.0D0;
  y_scale[1] := 3.0D0;


(* MAIN BIT : *)

  ODE.BVP_OrdN_FD(iteration_limit, num_eqns, num_pts, num_bca, 
                  k_a, k_b, 
                  tolerance, debug,
                  x, y_scale, 
                  Compute_F, Compute_S, LA.Square2,
                  Y);


(* RESULTS : *)
  
   (* Output zero'th derivative along grid : *)
   TRY
     Log.Put_Text("\n\n The zero'th derivative along the grid together with"
                  & " the exact solution : \n\n");
     FOR k := k_a TO k_b DO
         Log.Put_LReal(x[k]);     Log.Put_Text("   ");
         Log.Put_LReal(Y[0][k]);  
         Log.Put_LReal(pow(x[k],2.0D0) + 16.0D0/x[k]); 
         Log.Put_Ln();
     END;
   EXCEPT
     | Log.Cannot_Put => FE.F_Error("Cannot put results ... :-( ");
   END;

END test_relax_two.

