
(*<T>*************************************************************************)
(*<T>*************************************************************************)

MODULE ode_bvp_relax_v1 EXPORTS ode_v1;

IMPORT Lin_Alg_v03 AS LA;
IMPORT SimonH_Log_v01 AS Log;
IMPORT Fatal_Error_v0 AS FE;

IMPORT Wr;

TYPE 
  Writer = Wr.T;

  e_type = REF ARRAY OF LONGREAL;
  E_type = REF ARRAY OF ARRAY OF LONGREAL;
  F_type = REF ARRAY OF LONGREAL;
  S_type = REF ARRAY OF ARRAY OF LONGREAL;

(*<T>*************************************************************************)
(*<T>OFF-LINE DIAGNOSTICS :                                                  *)

(*<T>Diagnose $x$, the grid on which the given ode is integrated :           *)
PROCEDURE Log_x(READONLY wr : Writer;
                READONLY x  : REF ARRAY OF LONGREAL) 
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n This is the matrix 'x' " &
               "from the relaxed ode-bvp solver;\n\n", wr);

  FOR i := FIRST(x^) TO LAST(x^) DO
      Log.Put_LReal(x[i], wr);  Log.Put_Text("   ", wr);
      Log.Put_Ln(wr);
  END;
  
END Log_x;

(*<T>Diagnose $Y$, the array containing each derivative, $0$th -- $N$th, at
     each grid-point : *)
PROCEDURE Log_Y(READONLY wr : Writer;
                READONLY Y  : REF ARRAY OF ARRAY OF LONGREAL)
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n This is the matrix 'Y' " &
               "from the relaxed ode-bvp solver;\n\n", wr);

  FOR i := FIRST(Y^) TO LAST(Y^) DO
    FOR j := FIRST(Y[0]) TO LAST(Y[0]) DO
      Log.Put_LReal(Y[i][j], wr);  Log.Put_Text("   ", wr);
    END;
    Log.Put_Ln(wr);
  END;
END Log_Y;

(*<T>Diagnose the corrections, $\Delta Y$, to each element in $Y$, from each
     iteration : *)
PROCEDURE Log_Corrections(READONLY wr          : Writer;
                          READONLY corrections : REF ARRAY OF LONGREAL)
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n These are the corrections " &
               "from the relaxed ode-bvp solver;\n\n", wr);
  FOR i := FIRST(corrections^) TO LAST(corrections^) DO
      Log.Put_LReal(corrections[i], wr);
      Log.Put_Ln(wr);
  END;
END Log_Corrections;

(*<T>Diagnose the matrix $\E$ in $\E\Delta\y = \e$ : *)
PROCEDURE Log_E(READONLY wr : Writer;
                READONLY E  : REF ARRAY OF ARRAY OF LONGREAL)
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n This is the matrix 'E' " &
               "from the relaxed ode-bvp solver;\n\n", wr);

  FOR i := FIRST(E^) TO LAST(E^) DO
      FOR j:= FIRST(E[0]) TO LAST(E[0]) DO
          Log.Put_LReal(E[i][j], wr);  Log.Put_Text(" ", wr);
      END;
      Log.Put_Ln(wr);
  END;
END Log_E;

(*<T>Diagnose the vector $\e$ in $\E\Delta\y = \e$ :*)
PROCEDURE Log_e(READONLY wr : Writer;
                READONLY e  : REF ARRAY OF LONGREAL) 
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n This is the vector 'e' " &
               "from the relaxed ode-bvp solver;\n\n", wr);

  FOR i := FIRST(e^) TO LAST(e^) DO
     Log.Put_LReal(e[i], wr);  Log.Put_Ln(wr);
  END;
END Log_e;

(*<T>*************************************************************************)
(*<T> ON-LINE DIAGNOSTICS : *)

(*<T>Output iteration reached to `stdio' so user has some idea how far
     we've got : *)
PROCEDURE Put_Iteration(READONLY iteration : INTEGER)
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n Iteration : ");
  Log.Put_Int(iteration);
  Log.Put_Ln();
END Put_Iteration;

(*<T>Output the maximum error in each $y^{[n]}, y = 1\ldots n$, on the grid,
     to `stdio', so the user has some idea whether the routine is converging
     --- in particular, is the maximum error in $y^{[0]}$ converging below
     the set `tolerance'? *)
PROCEDURE Put_max_delta_Y(max_delta_Y : REF ARRAY OF LONGREAL)
RAISES {Log.Cannot_Put} =
BEGIN
  Log.Put_Text("\n\n The maximum error in y_vec over the interval is : \n\n");
  FOR i := FIRST(max_delta_Y^) TO LAST(max_delta_Y^) DO
      Log.Put_LReal(max_delta_Y[i]);
      Log.Put_Ln();
  END;
END Put_max_delta_Y;

(*<T>*************************************************************************)

(*<T>Construct $\E$, from $\E\Delta\y = \e$, from the user-given (via 
     ``Compute\_S'') partial-derivatives of the system of odes and 
     boundary-conditions : *)
PROCEDURE Construct_E(READONLY k_a, k_b, num_eqs, num_pts,
                               num_bca   : INTEGER;
                      READONLY x         : REF ARRAY OF LONGREAL;
                      READONLY Y         : REF ARRAY OF ARRAY OF LONGREAL;
                               Compute_S : S_proc_type;
                      VAR      E         : REF ARRAY OF ARRAY OF LONGREAL)
RAISES {} =
VAR S := NEW(S_type, num_eqs, 2*num_eqs);
BEGIN WITH N = num_eqs, N_a = num_bca DO

  (* First boundary, 'a' : *)
  Compute_S(k_a, x, Y, S);
  FOR j := 0 TO N_a - 1 DO
      FOR n := 0 TO N - 1 DO 
          E[j][n] := S[j][n];
      END;
  END;

  (*<T> Interior : *)
  FOR k:= k_a + 1 TO k_b DO
      Compute_S(k, x, Y, S);
      FOR j:= 0 TO N - 1 DO
          FOR n := 0 TO 2*N - 1 DO
              E[N_a + (k-k_a-1)*N + j][(k-k_a-1)*N + n] := S[j][n];
          END;
      END;
  END;

  (*<T> Second/Last boundary, 'b' : *)
  Compute_S(k_b+1, x, Y, S);
  FOR j := 0 TO N - N_a - 1 DO	                     (*<T> $N_b = (N - N_a)$ *)
      FOR n := 0 TO N - 1 DO
          E[N_a + (num_pts-1)*N + j][(num_pts-1)*N + n] := S[j][n]; 
      END;                                        
  END;

END; END Construct_E; (*<T> WITH, PROCEDURE *)

(*<T>*************************************************************************)

(*<T>Construct $\e$, from $\E\Delta\y = \e$, from the user-given (via 
     ``Compute\_F'') FDEs representing the system of odes and 
     boundary-conditions : *)
PROCEDURE Construct_e(READONLY k_a, k_b, num_pts, num_eqs,  
                               num_bca   : INTEGER;
                      READONLY x         : REF ARRAY OF LONGREAL;
                      READONLY Y         : REF ARRAY OF ARRAY OF LONGREAL;
                               Compute_F : F_proc_type;
                      VAR      e         : REF ARRAY OF LONGREAL) 
RAISES {} =
VAR F := NEW(F_type, num_eqs); 
BEGIN WITH N = num_eqs, K = num_pts, N_a = num_bca DO

  (*<T> First boundary : *)
  Compute_F(k_a, x, Y, F);
  FOR j := 0 TO N_a - 1 DO
      e[j] := -F[j];
  END;

  (*<T> Interior : *)
  FOR k := k_a + 1 TO k_b DO
      Compute_F(k, x, Y, F);
      FOR j := 0 TO N - 1 DO
          e[N_a + (k-k_a-1)*N + j] := -F[j];
      END;
  END;

  (*<T> Second boundary : *)
  Compute_F(k_b+1, x, Y, F);
  FOR j := 0 TO N - N_a - 1 DO
      e[N_a + (K-1)*N + j] := -F[j];
  END;
END; END  Construct_e;

(*<T>*************************************************************************)

(*<T> Solve a $N$'th order ordinary differential equation (ode) by 
      representing it as a system of $N$ first-order odes and using a 
      finite-difference/relaxation algorithm : *)
PROCEDURE BVP_OrdN_FD(         iteration_limit,
                               num_eqs, num_pts, num_bca, 
                               k_a, k_b       : INTEGER;
                               tolerance      : LONGREAL;
                               debug          : BOOLEAN;
                      READONLY x              : REF ARRAY OF LONGREAL;
                      READONLY y_scale        : REF ARRAY OF LONGREAL;
                               Compute_F      : F_proc_type;
                               Compute_S      : S_proc_type;
                               LA_solve       : LA_solve_type;
                      VAR      Y              : REF ARRAY OF ARRAY OF LONGREAL)
RAISES {Too_Many_Iterations} =

TYPE
  delta_Y_type     = REF ARRAY OF LONGREAL;
  max_delta_Y_type = REF ARRAY OF LONGREAL;

VAR
  iteration : INTEGER := 0;
  x_lwr, Y_log, E_log, e_log, corr_lwr : Writer;


  e           := NEW(e_type, num_eqs*num_pts);
  E           := NEW(E_type, num_eqs*num_pts, num_eqs*num_pts);
  delta_Y     := NEW(delta_Y_type, num_eqs*num_pts);
  max_delta_Y := NEW(max_delta_Y_type, num_eqs);

BEGIN

  (* Get those debug-diagnostics initialised : *)
  TRY
    Y_log := Log.Create_Stream("ode-Y.dlog");
    x_lwr := Log.Create_Stream("ode-x.dlog");
    E_log := Log.Create_Stream("ode-E.dlog");
    e_log := Log.Create_Stream("ode-e.dlog");
    corr_lwr := Log.Create_Stream("ode-corr.dlog");
  EXCEPT
    | Log.Name_Is_Nil   => FE.F_Error("Cannot create log-streams with" &
                                      " 'nil' as name ... THE END!");
    | Log.Cannot_Create => FE.F_Error("Unable to open some debug-logging." &
                                      "THE END!");
  END;

  TRY
    IF debug THEN Log_x(x_lwr, x) END;

    (*<T> Have been supplied with initial guess for ${\bfmit Y}$, i.e.,
      {\bfmit y} $\forall k$;  iterate through successive relaxations
      to hopefully converge to the `right' answer : *)
    LOOP (*<T> Main loop : *)

      (*<T> Log ${\bfmit Y}$, i.e., ${\bfmit y}_k \forall k$ : *)
      IF debug THEN Log_Y(Y_log, Y) END;

      (*<T> Construct and log ${\bfmit E}$ : *)
      Construct_E(k_a, k_b, num_eqs, num_pts, num_bca, x, Y, Compute_S, E);  
      IF debug THEN Log_E(E_log, E) END;

      (*<T> Construct and log ${\bfmit e}$ : *)
      Construct_e(k_a, k_b, num_pts, num_eqs, num_bca, x, Y, Compute_F, e);  
      IF debug THEN Log_e(e_log, e) END;

      (*<T> Solve ${\bfmit E}\Delta{\bfmit y} = {\bfmit e}$ for 
        $\Delta {\bfmit Y}$, i.e. for this iteration's corrections 
        to ${\bfmit Y}$; log corrections : *)
      TRY
        LA_solve(num_eqs*num_pts, E, e, TRUE, delta_Y);  
      EXCEPT
        | Log.Cannot_Close,
          Log.Cannot_Create,
          Log.Name_Is_Nil    => 
            Log.Put_Text("\n\n WARNING : " &
                         "problem with Lin Alg logging ...\n\n");
      END;

      IF debug THEN Log_Corrections(corr_lwr, delta_Y) END;

      (*<T> Apply corrections : *)
      FOR k := k_a TO k_b DO
          FOR n := 0 TO num_eqs - 1 DO
              Y[n][k] := Y[n][k] + delta_Y[(k-k_a)*num_eqs + n]
          END;
      END;

      (*<T> Search over integration domain and compute largest correction for 
        each $y^{[n]}$ : *)
      FOR n := 0 TO num_eqs - 1 DO 
          max_delta_Y[n] := 0.0D0;
          FOR k := k_a TO k_b DO
              IF (   ABS(delta_Y[(k-k_a)*num_eqs + n]) 
                   > ABS(max_delta_Y[n])) THEN 
                  max_delta_Y[n] := delta_Y[(k-k_a)*num_eqs + n]; 
              END
          END;
      END;

      (*<T> Output iteration and max-corrections : *)
      Put_Iteration(iteration);
      Put_max_delta_Y(max_delta_Y);      

      (*<T> Reached solution yet? *)
      IF (ABS(max_delta_Y[FIRST(max_delta_Y^)]/y_scale[1]) <= tolerance) THEN 
      EXIT END;                            (* \TODO{what about derivatives?} *)

      (*<T> How many iterations have we done? : *)
      INC(iteration);
      IF (iteration > iteration_limit) THEN  
          RAISE Too_Many_Iterations                  (* Probably diverging. *)
      END;
    END; (*<T> : Main LOOP. *)
  EXCEPT
    | Log.Cannot_Put =>    FE.F_Error("Unable to put debug-logging " & 
                                      "THE END!");
    | LA.No_Unique_Soln => FE.F_Error("No unique solution of 'E' " &
                                      "THE END!");
  END; 

  (* Tidy up : *)
  TRY
    Log.Close_Stream(Y_log);  Log.Close_Stream(x_lwr);
    Log.Close_Stream(E_log);  Log.Close_Stream(e_log);  
    Log.Close_Stream(corr_lwr);
  EXCEPT
    | Log.Cannot_Close => Log.Put_Text("\n\n WARNING : " &
                                       "Unable to close debug-logs ...\n\n");
  END;

  (*<T> If converged then $\Y$ is returned;  otherwise an exception has
    been raised\dots *)

END BVP_OrdN_FD;

(*<T>*************************************************************************)
(*<T>*************************************************************************)

BEGIN
(*<T>Make fresh pot of coffee. *)
END ode_bvp_relax_v1.

(*<T>*************************************************************************)
(*<T>*************************************************************************)

