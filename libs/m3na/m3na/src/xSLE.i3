INTERFACE xSLE;
(*Copyright (c) 1996, m3na project
  
Abstract: Simultaneous Linear Equations.

1/27/96  Harry George    Initial version, from prev work
*)
FROM xUtils IMPORT Error;
IMPORT xInteger AS I,
       xReal64 AS R,
       xMat AS M,
       xVect AS V;
FROM xReal64 IMPORT REAL64;
(*==========================*)
(* Triangluar Matrices      *)
(*==========================*)
(*A triangular matrix A is of the form:
| a11 a12 a13 a14
| 0   a22 a23 a24
| 0   0   a33 a34
| 0   0   0   a44

A x = b can be solved for x by back substitution
*)
PROCEDURE backsub(A:M.Matrix;
                  x,b:R.Array
                  ) RAISES {Error};
                  
(*==========================*)
(* Tridiagonal Matrices     *)
(*==========================*)
(*A tridiagonal matrix A has diagonals a,b,c:
|  b1 c1  0    ...
|  a2 b2 c2    ...
|   0 a3 b3 c3 ...
|              ...
|                 aN-1 bN-1 cN-1
|                      aN   bN
*)

PROCEDURE householder(A:M.Matrix);   (*nxn*)
(*Convert A to tridiagonal form (destroying original A)*)

PROCEDURE matrix_to_arrays(A:M.Matrix;        (*nxn tridiagonal*)
                           VAR a,b,c:R.Array  (*array form*)
                          ) RAISES {Error};         

PROCEDURE tridiag(a,b,c,r:R.Array;
                    VAR u:R.Array) RAISES {Error};
(*Solve for u in A*u=r, where A is given as a,b,c*)

(*==========================*)
(* nxn Matrices             *)
(*==========================*)
(*A general nxn real matrix A is of the form
| a11 a12 a13
| a21 a22 a23
| a31 a32 a33XS

A x = b can be solved for x by Gaussian Elimination and
backsubstitution
*)
PROCEDURE GaussElim(A:  M.Matrix;
                    x,b:V.Vector;
                    pivot:BOOLEAN:=TRUE
                    ) RAISES {Error};
(*Generally, we need to pivot to assure division by the largest
coeff.  However, sometimes we already know the matrix is in
the correct form and can avoid pivoting.  In that case, set
pivot:=FALSE
*)

(*==========================*)
(* LU Factoring             *)
(*==========================*)
(*-----------------*)
PROCEDURE LUfactor(A      :M.Matrix;
                   index  :I.Array;
               VAR d      :INTEGER) RAISES ANY;
(*Factor A into Lower/Upper portions 
Destroys A's values. 
A is real nxn
index is integer nx1
return value "d" is used for backsub and det
*)
(*-----------------*)
PROCEDURE LUbacksub(A     :M.Matrix;
	            B     :R.Array;
                    index :I.Array) RAISES ANY;
(*After LUfactor on A, solves A dot X = B.  
X is returned in B.  B's values are destroyed.
A is real nxn
B is real nx1
index is integer nx1
*)
(*-----------------*)
PROCEDURE LUinverse(A,B  :M.Matrix;
                    index:I.Array) RAISES ANY;
(*
Inverse of A goes to B
Must have done LUfactor on A first
Destroys A's values. 
A is real nxn 
B is real nxn
index is integer nx1
*)
(*-----------------*)
PROCEDURE LUdet(A:M.Matrix;
                d:INTEGER):REAL64 RAISES ANY;
(*after LUfactor on A and no backsubs,
returns determinant
"d" is the parity marker from LUdecomp
*)

(*==========================*)
(* QR Factoring             *)
(*==========================*)

(*=============================*)
(* Singular Value Decomposition*)
(*=============================*)
(*In the problem A*x=b, we can decompose to A = U*W*V^T.
Then x=V*diag(W)*U^T*b.  There are also others things which
can be solved once we have U,V,W.
*)

PROCEDURE svd_golub(
           A:M.Matrix;         (*mxn matrix*)
           b:V.Vector;         (*nx1 col matrix*)
           rhs:CARDINAL;       (*number of right hand sides*)
           matU:BOOLEAN;       (*make U in the decomposition*)
           matV:BOOLEAN;       (*make V in the decomposition*)
           VAR U,V,W:M.Matrix  (*decomposition products*)
           ) RAISES {Error};
(*Do SVD via Golub and Reinsch *)
                   
PROCEDURE svd_chan(
           A:M.Matrix;         (*mxn matrix*)
           b:V.Vector;         (*nx1 col matrix*)
           rhs:CARDINAL;       (*number of right hand sides*)
           matU:BOOLEAN;       (*make U in the decomposition*)
           matV:BOOLEAN;       (*make V in the decomposition*)
           VAR U,V,W:M.Matrix  (*decomposition products*)
           ) RAISES {Error};
(*Do SVD via T. Chan's ACM algorithm 581*)

PROCEDURE svd_solve(U,V,W:M.Matrix; (*decomposition*)
                    b:V.Vector;     (*rightside*)
                    VAR x:V.Vector    (*result*)
                   ) RAISES {Error};            
(*==========================*)
END xSLE.
