INTERFACE xUtils;
(*Copyright (c) 1996, Harry George
  
Abstract: Private interface to na's utilities

12/28/95  Harry George    Initial version
1/22/96   Harry George    Converted to m3na project
*)

VAR verbosity:[0..3];

EXCEPTION Error(Err);

TYPE Err= {bad_size       (*e.g., vector sizes mismatched*)
          ,b1_too_small    (*in tridiagonal, rewrite for n-1 eqns*)
          ,divide_by_zero  (*x/0 condition detected*)
          ,need_more_data  (*e.g., more data points in statistics*)
          ,not_bracketed   (*give x1,x2 do not bracket root*)
          ,not_converging  (*e.g., eps or maxiter too small*)
          ,not_implemented (*it's just a stub*)
          ,out_of_range    (*parm is out of range*)
         };

         
PROCEDURE debug(level:[0..3]; ftn,str:TEXT);

PROCEDURE err(ftn:TEXT;code:Err; errmsg:TEXT:=NIL) RAISES {Error};

END xUtils.

