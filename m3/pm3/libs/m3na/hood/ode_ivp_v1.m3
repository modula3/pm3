
(*****************************************************************************)
(*****************************************************************************)


MODULE ode_ivp_v1 EXPORTS ode_v1;

(*<T> Vector-valued function;  gives the 'i'th' element of the vector;
   if $i \not= \hbox{order}$ then $F_sys(i,x) = y_i(x)$, else
   $F_Sys = y_{\rm order} = F(x, y, y', y'', ...)$ :  *)

PROCEDURE F_Sys (         i     : INTEGER;
                          order : INTEGER;
                          x     : LONGREAL;
                 READONLY y     : REF ARRAY OF LONGREAL;
                          F     : ivp_given_ode_proc_type) : LONGREAL 
RAISES {F_Sys_Error} = 
VAR result : LONGREAL;
BEGIN
  IF (i = order-1) THEN result := F(x, y);
  ELSE
      IF ((i + 1 <= LAST(y^)) AND (i + 1 >= FIRST(y^))) THEN 
        result := y[i+1];
      ELSE
        RAISE F_Sys_Error;
      END;
  END;
  RETURN result;
END F_Sys;


(*****************************************************************************)


(* Algorithm taken from 
     \ANI{
         "Numerical Analysis", by Burden, Faires and Reynolds, 
         pub. Prindle, Weber and Schmidt, 2nd ed., 1981.  (1st ed., 1978),
         Section 5.8.  
       }    
   *)

PROCEDURE RK4(         order      : INTEGER;
                       a,b        : LONGREAL;
                       J          : INTEGER;
                       F          : ivp_given_ode_proc_type;
              READONLY init_conds : REF ARRAY OF LONGREAL;  
                                                  (* ARRAY [0..order-1] *)
              VAR      x          : REF ARRAY OF LONGREAL;  (* ARRAY [0..J] *)
              VAR      y          : REF ARRAY OF LONGREAL;  (* ARRAY [0..J] *)
             )
          RAISES {F_Sys_Error} = 

TYPE
    (*<T>An N'th-order differential equation is represented as N first-order
       equations:

         $$ y^(N) = F(x, y, y', y'', y''', ...), $$
   
       is equivalent to

         $$\eqalign{
              y_0' &= y_1 \cr
              y_1' &= y_2 \cr
              y_2' &= y_3 \cr
              .    & . \cr
              y_{N-1}' &= F(x, y_0, y_1, y_2, y_3, ..., y_{N-1}); \cr
           }$$

       for each equation there are [0..J] points at which finite-difference
       values are taken;  therefore, 

         $$ ysys[j,n] \hbox{is} y_n(x_j). $$
  
       *)

    ysys_type = REF ARRAY OF ARRAY OF LONGREAL;  
    k_type    = REF ARRAY OF LONGREAL;  
    t_type    = REF ARRAY OF LONGREAL;
VAR 
    (* interval size : *)
    h : LONGREAL;  

    (* coeffs for R-K scheme : *)
    k1, k2, k3, k4 := NEW(k_type, order);

    temp_array := NEW(t_type, order);

    (*<T> 2-d array for the $y_n$ at $x_j$;  ARRAY [0..J],[0..order] : *)    
    ysys := NEW(ysys_type, J+1, order);

BEGIN
  TRY
    (* Compute step : *)
    h := (b - a)/FLOAT(J,LONGREAL);
          
    (* Use initial conditions : *)
    x[0] := a;
    FOR n := 1 TO order DO ysys[0,n-1] := init_conds[n]; END;

    (* From a to b : *)
    FOR j := 1 TO J DO  (* i.e., FOR j := FIRST(ysys) + 1 TO LAST(ysys) DO *)

        (* Iterate through the system of equations for each of k1, k2, k3 
           and k4 in turn : *)
        FOR n := 0 TO order-1 DO temp_array[n] := ysys[j-1][n]; END;
        FOR i := 0 TO order-1 DO
            k1[i] := h * F_Sys(i, order, 
                               x[j - 1], temp_array,
                               F);
        END;

        FOR n := 0 TO order-1 DO 
            temp_array[n] := ysys[j-1][n] + k1[n]/2.0D0; END;
        FOR i := 0 TO order-1 DO
          k2[i] := h 
                 * F_Sys(i, order,
                         x[j - 1] + h/2.0D0, temp_array,
                         F);
        END;

        FOR n := 0 TO order-1 DO 
            temp_array[n] := ysys[j-1][n] + k2[n]/2.0D0; END;
        FOR i := 0 TO order-1 DO
          k3[i] := h 
                 * F_Sys(i, order,
                         x[j - 1] + h/2.0D0, temp_array,
                         F);
        END;

        FOR n := 0 TO order-1 DO temp_array[n] := ysys[j-1][n] + k3[n] END;
        FOR i := 0 TO order-1 DO
          k4[i] := h 
                 * F_Sys(i, order, 
                         x[j - 1] + h, temp_array, 
                         F);
        END;

        (* Compute y1, ..., y_{max_order} at next point in interval : *)
        FOR n := 0 TO order-1 DO
          ysys[j, n] := ysys[j-1, n] 
                      + (   (k1[n] + (2.0D0 * k2[n]) + (2.0D0 * k3[n]) + k4[n])
                          / 6.0D0);
        END;

        (* compute next value of independent variable : *)
        x[j] := a + (FLOAT(j,LONGREAL) * h);

        (* extract soln : *)
        y[j] := ysys[j,0]

    END; (* FOR j := 1 TO J DO *)
        
   (*<T> solution at $x_0$ has been missed : *)
   y[0] := ysys[0,0];  

  EXCEPT 
      (* All EXCEPTIONS currently propogated to caller ... *)
  END;
END RK4;


(*****************************************************************************)


BEGIN
END ode_ivp_v1.

(*****************************************************************************)
(*****************************************************************************)


