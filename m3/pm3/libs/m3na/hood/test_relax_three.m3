
(*****************************************************************************)
(*****************************************************************************)

MODULE test_relax_three EXPORTS Main;

(*<T>
  This for $y''' = (1/8)(6x^2 - (y')^2 - yy'')$, with simple boundary 
  conditions boundary conditions $y = 17$ and $y' = -14$ at $x_a = 1$ and  
  $y = 43/3$ at $x_b = 3$;  
  the exact solution is $y = x^2 + 16/x$, same as for Test 2.
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
  iteration_limit = 25;

  num_pts  = 20;
  num_eqns = 3;
  num_bca  = 2;

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
      F[FIRST(F^)]   := Y[0][0] - 17.0D0;
      F[FIRST(F^)+1] := Y[1][0] + 14.0D0;
      F[FIRST(F^)+1] := 0.0D0;
  ELSIF k = k_b + 1 THEN
      F[FIRST(F^)]   := Y[0][num_pts-1] - 43.0D0/3.0D0;
      F[FIRST(F^)+1] := 0.0D0;
      F[FIRST(F^)+1] := 0.0D0;
  ELSE (* interior : *) 
      F[FIRST(F^)] := Y[0][k] - Y[0][k-1] 
                              - 0.5D0*delta_x*(Y[1][k] + Y[1][k-1]);

      F[FIRST(F^)+1] := Y[1][k] - Y[1][k-1] 
                              - 0.5D0*delta_x*(Y[2][k] + Y[2][k-1]);

      F[FIRST(F^)+2] := Y[2][k] - Y[2][k-1] 
                      - (delta_x/8.0D0)
                       *(   6.0D0*pow(0.5D0*(x[k] + x[k-1]),2.0D0) 
                          - 0.25D0*(Y[1][k] + Y[1][k-1])
                                  *(Y[1][k] + Y[1][k-1])
                          - 0.25D0*(Y[0][k] + Y[0][k-1])
                                  *(Y[2][k] + Y[2][k-1]));
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
        S[FIRST(S^)][1] := 0.0D0;
        S[FIRST(S^)][2] := 0.0D0;

        S[FIRST(S^)][N+0] := 0.0D0;
        S[FIRST(S^)][N+1] := 0.0D0;
        S[FIRST(S^)][N+2] := 0.0D0;

        S[FIRST(S^)+1][0] := 0.0D0;
        S[FIRST(S^)+1][1] := 1.0D0;
        S[FIRST(S^)+1][2] := 0.0D0;

        S[FIRST(S^)+1][N+0] := 0.0D0;
        S[FIRST(S^)+1][N+1] := 0.0D0;
        S[FIRST(S^)+1][N+2] := 0.0D0;

        S[FIRST(S^)+2][0] := 0.0D0;
        S[FIRST(S^)+2][1] := 0.0D0;
        S[FIRST(S^)+2][2] := 0.0D0;

        S[FIRST(S^)+2][N+0] := 0.0D0;
        S[FIRST(S^)+2][N+1] := 0.0D0;
        S[FIRST(S^)+2][N+2] := 0.0D0;

    ELSIF k = k_b+1 THEN
        S[FIRST(S^)][0] := 1.0D0;
        S[FIRST(S^)][1] := 0.0D0;
        S[FIRST(S^)][2] := 0.0D0;

        S[FIRST(S^)][N+0] := 0.0D0;
        S[FIRST(S^)][N+1] := 0.0D0;
        S[FIRST(S^)][N+2] := 0.0D0;

        S[FIRST(S^)+1][0] := 0.0D0;
        S[FIRST(S^)+1][1] := 0.0D0;
        S[FIRST(S^)+1][2] := 0.0D0;

        S[FIRST(S^)+1][N+0] := 0.0D0;
        S[FIRST(S^)+1][N+1] := 0.0D0;
        S[FIRST(S^)+1][N+2] := 0.0D0;

        S[FIRST(S^)+2][0] := 0.0D0;
        S[FIRST(S^)+2][1] := 0.0D0;
        S[FIRST(S^)+2][2] := 0.0D0;

        S[FIRST(S^)+2][N+0] := 0.0D0;
        S[FIRST(S^)+2][N+1] := 0.0D0;
        S[FIRST(S^)+2][N+2] := 0.0D0;
    ELSE 
        S[FIRST(S^)][0]     := -1.0D0;
        S[FIRST(S^)][1]     := -0.5D0*delta_x;
        S[FIRST(S^)][2]     :=  0.0D0;

        S[FIRST(S^)][N+0]   :=  1.0D0;
        S[FIRST(S^)][N+1]   := -0.5D0*delta_x;
        S[FIRST(S^)][N+2]   :=  0.0D0;

        S[FIRST(S^)+1][0]     :=  0.0D0;
        S[FIRST(S^)+1][1]     := -1.0D0; 
        S[FIRST(S^)+1][2]     := -0.5D0*delta_x;

        S[FIRST(S^)+1][N+0]   :=  0.0D0;
        S[FIRST(S^)+1][N+1]   :=  1.0D0;
        S[FIRST(S^)+1][N+2]   := -0.5D0*delta_x;

        S[FIRST(S^)+2][0]   :=   (delta_x/32.0D0)*(Y[2][k] + Y[2][k-1]);
        S[FIRST(S^)+2][1]   :=   (delta_x/32.0D0)*(Y[1][k] + Y[1][k-1]); 
        S[FIRST(S^)+2][2]   := - 1.0D0 
                               + (delta_x/32.0D0)*(Y[0][k] + Y[0][k-1]); 

        S[FIRST(S^)+2][N+0] :=         (delta_x/32.0D0)*(Y[2][k] + Y[2][k-1]);
        S[FIRST(S^)+2][N+1] :=         (delta_x/32.0D0)*(Y[1][k] + Y[1][k-1]);
        S[FIRST(S^)+2][N+2] := 1.0D0 + (delta_x/32.0D0)*(Y[0][k] + Y[0][k-1]);
    END;
  END;
END Compute_S;

BEGIN

(* INITIALISATION : *)

  (* Non-constant parameters : *)
  delta_x      := (x_b - x_a)/FLOAT(num_pts - 1, LONGREAL);  

  (* Set up grid : *)
  FOR k := k_a TO k_b DO
      x[k] := x_a + delta_x*FLOAT(k - k_a, LONGREAL);
  END;

  (*<T> Initial ``guess'';  second order equation, num\_eqns = 2;  exact 
     solution is $y_0 = y = x^2 + 16/x$ (and therefore, 
     $y_1 = y' = 2x - 16/x^2$, $y_2 = y'' = 2 + 32/x^3$);  therefore could
     use this, perhaps with some systematic drift, for testing;  however,
     that's too easy!  instead extrapolate between $y(x_a)$ and $y(x_b)$
     for $y$, take $y'$ to be equal to $y'(x_a)$ throughout and guess 
     wildly at $y''$ : *)
  FOR k := k_a TO k_b DO
      Y[0][k] := 17.0D0 + (43.0D0/3.0D0 - 17.0D0)*((x[k] - x_a) / (x_b - x_a));
      Y[1][k] := - 14.0D0;
      Y[2][k] := 0.0D0; 
  END;                               

  (* Scale of each dependent variable --- helps convergence test : *)
  y_scale[0] := 15.0D0;
  y_scale[1] := 3.0D0;
  y_scale[2] := 10.0D0;


(* MAIN BIT : *)
  TRY
    ODE.BVP_OrdN_FD(iteration_limit, num_eqns, num_pts, num_bca, 
                    k_a, k_b, 
                    tolerance, debug,
                    x, y_scale, 
                    Compute_F, Compute_S, LA.Square2,
                    Y);
  EXCEPT
    | ODE.Too_Many_Iterations => FE.F_Error("Too many iterations --- guess" &
                                            " this ain't gonna converge...");
  END;


(* RESULTS : *)
  
   (* Output zero'th derivative along grid : *)
   TRY
     Log.Put_Text("\n\n And the results are (compare numerical and exact" &
                  "solutions) : ");
     Log.Put_Text("\n\n x             y : \n\n");
     FOR k := k_a TO k_b DO
         Log.Put_LReal(x[k]);     Log.Put_Text("   ");
         Log.Put_LReal(Y[0][k]);  
         Log.Put_LReal(pow(x[k],2.0D0) + 16.0D0/x[k]); 
         Log.Put_Ln();
     END;
     Log.Put_Text("\n\n");
   EXCEPT
     | Log.Cannot_Put => FE.F_Error("Cannot put results ... :-( ");
   END;
END test_relax_three.

