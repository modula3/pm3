MODULE xReal64;
(*Copyright (c) 1996, m3na project
  
Abstract: Real64 numbers

2/17/96  Harry George    Initial version
*)
FROM xUtils IMPORT Error,Err;

CONST Module = "xReal64.";
(*==========================*)
(*--------------------------*)
<*INLINE*>
PROCEDURE sgn(a:REAL64):REAL64=
BEGIN
  IF a >=Zero THEN RETURN One ELSE RETURN -One; END;
END sgn;


(*==========================*)
BEGIN
END xReal64.
