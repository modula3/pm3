MODULE xVect;
(*
Abstract:

6/6/87    hgeorge
          Initial version.

2/11/89   hgeorge
          To work with generic matrices.

11/20/94  Harry George
          Converted to Modula3 dynamic arrays.

12/18/95  Harry George
          ...and back to fully instantiated for REAL32.

1/27/96   Harry George
          Converted to OO format, and REAL64          

2/17/96   Harry George   Converted from OO to ADT format
*)
FROM xUtils IMPORT Error,Err;
IMPORT Wr,TextWr,Fmt,xReal64 AS R;
FROM xReal64 IMPORT REAL64;

CONST Module = "xVect";

(*-----------------*)
PROCEDURE new(  n:CARDINAL):Vector =
BEGIN
  RETURN NEW(Vector,n);
END new;
(*-----------------*)
PROCEDURE copy(  v:Vector):Vector =
VAR
  n:=NUMBER(v^);
  tmp:=NEW(Vector,n);
BEGIN
  tmp^:=v^;
  RETURN tmp;
END copy;
(*-----------------*)
PROCEDURE lex( 
               str:TEXT):Vector RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END lex;
(*-----------------*)
PROCEDURE fmt( 
            v:Vector; 
            style:=Fmt.Style.Fix;
            prec:=2):TEXT= 
CONST width = 12;
VAR
  wr:=TextWr.New();
  n:=NUMBER(v^); n1:=0; nn:=n-1;
BEGIN
  Wr.PutText(wr,"V" & Fmt.Int(n) & "{");
  FOR i:=n1 TO nn DO
    Wr.PutText(wr,Fmt.Pad(R.fmt(v[i],style,prec),width)); 
    IF i#nn THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);
END fmt;

(*-----------------*)
PROCEDURE Zero( 
                v:Vector)=
(*set all zeros*)
VAR
  n:=NUMBER(v^); n1:=0; nn:=n-1;  
BEGIN
  FOR i:=n1 TO nn DO
    v[i]:=R.Zero;
  END;
END Zero;

(*-----------------*)
PROCEDURE abs( 
                v:Vector):REAL64=
(*returns |v|*)
VAR
  n:=NUMBER(v^); n1:=0; nn:=n-1;
  sum:REAL64;
BEGIN
  sum:=R.Zero;
  FOR i:=n1 TO nn DO
    sum:=sum+v[i]*v[i];
  END;
  RETURN R.sqrt(sum);
END abs;


(*-----------------*)
PROCEDURE add( 
                 v1,v2:Vector):Vector RAISES {Error}=
(*v1:=v1+v2*)
VAR
  n:=NUMBER(v1^); n1:=0; nn:=n-1;
  tmp:=NEW(Vector,n); 
BEGIN
  IF NUMBER(v2^) #n THEN
    RAISE Error(Err.bad_size);
  END;
  FOR i:=n1 TO nn DO
    tmp[i]:=v1[i]+v2[i];
  END;
  RETURN tmp;
END add;

(*-----------------*)
PROCEDURE sub( 
               v1,v2:Vector):Vector RAISES {Error}=
(*v1:=v1-v2*)
VAR
  n:=NUMBER(v1^); n1:=0; nn:=n-1;  
  tmp:=NEW(Vector,n);
BEGIN
  IF NUMBER(v2^) #n THEN
    RAISE Error(Err.bad_size);
  END;
  FOR i:=n1 TO nn DO
    tmp[i]:=v1[i]-v2[i];
  END;
  RETURN tmp;
END sub;


(*-----------------*)
PROCEDURE scale( 
                 v:Vector; factor:REAL64)=
(*scale v by factor*)
VAR
  n:=NUMBER(v^); n1:=0; nn:=n-1;  
BEGIN
  FOR i:=n1 TO nn DO
    v[i]:=v[i]*factor;
  END;
END scale;


(*-----------------*)
PROCEDURE dot( 
                v1,v2:Vector):REAL64 RAISES {Error}=
(*return dot product of v1 and v2*)
VAR
  n:=NUMBER(v1^); n1:=0; nn:=n-1;  
  sum:REAL64;
BEGIN
  IF NUMBER(v2^)#n THEN
    RAISE Error(Err.bad_size);
  END;

  sum:=R.Zero;
  FOR i:=n1 TO nn DO
    sum:=v1[i]*v2[i];
  END;
  RETURN sum;
END dot;

(*-----------------*)
PROCEDURE cross( 
                v1,v2:Vector):Vector RAISES {Error}=
(*return cross product of v1 and v2*)
VAR
  n:=NUMBER(v1^); n1:=0; nn:=n-1;  
  tmp:=NEW(Vector,n);
BEGIN
  RAISE Error(Err.not_implemented);
END cross;


(*-----------------*)
BEGIN
END xVect.
