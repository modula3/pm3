MODULE xMat;
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
          Converted to OO format and REAL64          

2/17/96   Harry George   ...and back to ADT format
*)

FROM xUtils IMPORT Error,Err;
FROM xVect IMPORT Vector;
IMPORT Wr,TextWr,Fmt,xReal64 AS R;

CONST Module = "xMat";

(*-----------------*)
PROCEDURE new( 
               m,n:CARDINAL):Matrix =
BEGIN
  RETURN NEW(Matrix,m,n);
END new;
(*-----------------*)
PROCEDURE copy( 
                mat:Matrix):Matrix =
VAR
  m:=NUMBER(mat^); n:=NUMBER(mat[0]);
  tmp:=NEW(Matrix,m,n);
BEGIN
  tmp^:=mat^;
  RETURN tmp;
END copy;
(*-----------------*)
PROCEDURE lex( 
               str:TEXT):Matrix RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END lex;
(*-----------------*)
PROCEDURE fmt( 
            mat:Matrix; 
            style:=Fmt.Style.Fix;
            prec:=2):TEXT= 
CONST width = 12;
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]);  n1:=0; nn:=n-1;
  wr:=TextWr.New();
BEGIN
  Wr.PutText(wr,"M" & Fmt.Int(m) & "x" & Fmt.Int(n) & "{\n");
  FOR i:=m1 TO mm DO
    Wr.PutText(wr,"V" & Fmt.Int(n) & "{");
    FOR j:= n1 TO nn DO
      Wr.PutText(wr,Fmt.Pad(R.fmt(mat[i,j],style,prec),width));
      IF j#nn THEN Wr.PutText(wr,", "); END;
    END;
    Wr.PutText(wr,"}");
    IF i#mm THEN Wr.PutText(wr,","); END;
    Wr.PutText(wr,"\n");
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);
END fmt;

(*-----------------*)
PROCEDURE Zero( 
                mat:Matrix)=
(*set all zeros*)
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]); n1:=0; nn:=n-1;
BEGIN
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      mat[i,j]:=R.Zero;
    END;
  END;
END Zero;
(*-----------------*)
PROCEDURE One( 
               mat:Matrix) =
(*set all zeros except diagonal to 1's*)
CONST ftn = "Midentity";
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]); n1:=0; nn:=n-1;
BEGIN
  IF m # n THEN
    RAISE Error(Err.bad_size);
  END;
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      mat[i,j]:=R.Zero;
    END;
  END;
  FOR i:=m1 TO mm DO
    mat[i,i]:=R.One;
  END;
END One;



(*----------------*)
PROCEDURE add( 
               mat1,mat2:Matrix):Matrix RAISES {Error} =
(*return mat1+mat2*)
(*each is mxn*)
CONST ftn = Module & "add";
VAR
  m:=NUMBER(mat1^);   m1:=0; mm:=m-1;
  n:=NUMBER(mat1[0]); n1:=0; nn:=n-1;
  tmp:Matrix;
BEGIN
  IF NUMBER(mat2^)#m OR NUMBER(mat2[0])#n THEN
    RAISE Error(Err.bad_size);
  END;

  tmp:=NEW(Matrix,m,n);
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      tmp[i,j]:= mat1[i,j] + mat2[i,j];
    END;
  END;
  RETURN tmp;
END add;
(*----------------*)
PROCEDURE sub( 
               mat1,mat2:Matrix):Matrix RAISES {Error} =
(*return mat1-mat2*)
(*each is mxn*)
CONST ftn = Module & "sub";
VAR
  m:=NUMBER(mat1^);   m1:=0; mm:=m-1;
  n:=NUMBER(mat1[0]); n1:=0; nn:=n-1;
  tmp:Matrix;
BEGIN
  IF NUMBER(mat2^)#m OR NUMBER(mat2[0])#n THEN
    RAISE Error(Err.bad_size);
  END;

  tmp:=NEW(Matrix,m,n);
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      tmp[i,j]:= mat1[i,j] - mat2[i,j];
    END;
  END;
  RETURN tmp;
END sub;

(*-----------------*)
PROCEDURE mul( 
               mat1,mat2:Matrix):Matrix RAISES {Error}=
(*return mat1*mat2*)
(* mat1:mxn  mat2:nxp  return:mxp*)
CONST ftn = "mul";
VAR
  m:=NUMBER(mat1^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat1[0]); n1:=0; nn:=n-1;
  p:=NUMBER(mat2[0]); p1:=0; pp:=p-1;
  tmp:Matrix;
  
BEGIN
  IF NUMBER(mat2^)#n THEN
    RAISE Error(Err.bad_size);
  END;
  tmp:=NEW(Matrix,m,p);
  FOR i:=m1 TO mm DO
    FOR j:=p1 TO pp DO
      tmp[i,j]:=R.Zero;
      FOR k:=n1 TO nn DO
        tmp[i,j]:=tmp[i,j] + mat1[i,k] * mat2[k,j];
      END;
    END;
  END;
  RETURN tmp;
END mul;
(*-----------------*)
(*----------------*)
PROCEDURE mulV( 
               A:Matrix; b:Vector):Vector RAISES {Error} =
(*return c, in A x b=c*)
(*A:mxn, b:nx1, return:mx1*)

CONST ftn = Module & "mulV";
VAR
  tmp:Matrix;
  m:=NUMBER(A^);   m1:=0; mm:=m-1;
  n:=NUMBER(A[0]); n1:=0; nn:=n-1;
  c:=NEW(Vector,m);
BEGIN
  IF NUMBER(b^)#n THEN
    RAISE Error(Err.bad_size);
  END;
  
  FOR i:=m1 TO mm DO
    c[i]:=R.Zero;
    FOR j:=n1 TO nn DO
      c[i]:=c[i]+b[j]*A[i,j];
    END;
  END;
  RETURN c;
END mulV;
(*-----------------*)
PROCEDURE transpose( 
                     mat:Matrix):Matrix =
CONST ftn = Module & "mTranspose";
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]); n1:=0; nn:=n-1;
  tmp:Matrix;
BEGIN
  tmp:=NEW(Matrix,n,m);
  FOR i:=n1 TO nn DO
    FOR j:=m1 TO mm DO
      tmp[i,j]:=mat[j,i];
    END;
  END;
  RETURN tmp;
END transpose;
(*-----------------*)
BEGIN
END xMat.
