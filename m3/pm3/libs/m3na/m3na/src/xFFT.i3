INTERFACE xFFT;
(*Copyright (c) 1996, m3na project
  
Abstract: Fast Fourier Transforms (FFT's)

3/18/96  Warren Smith    Initial version
*)

IMPORT xComplex AS C;
(*==========================*)
(**************************************
Reorders array a[0..n-1] so that element[i] is
swapped with element[reverse-bit-order[i]].
Two successive calls are identity.
The algorithm below runs in O(n) time and is based on implementing a
reverse-binary counter. The forward counter f increments 0..n/2-1 step 2
and if r, 0<=r<n/2 is the bit-reverse of f,
we swap(f+1,n/2+r) with no test needed and we do
swap(f,r) if r>f and also swap(n-1-f,n-1-r) if f,r both <n/2.
***************************************************)

PROCEDURE ReOrder (VAR a: C.Array);




(***************************************
 direction = +1 for inverse FFT, -1 for forward.  (See FFT defn
in module.)

 a[] overwritten by transform. 
NOTE: You must call ReOrder(a) before calling this routine,
because this routine assumes it has re-ordered a[]s as its input.
To do an FFT of some data, therefore, we would call
  ReOrder(data); FFTwithWrongOrderedInput(data);
I have separated the routines this way because I want to be able to
avoid the ReOrder when computing convolutions and correlations.
****************************************)
PROCEDURE FFTwithWrongOrderedInput(
              VAR a: C.Array;
              direction: LONGREAL);



(*----------------------------*)
PROCEDURE Test();
(*perform assertions*)
(*==========================*)
END xFFT.
