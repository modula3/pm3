INTERFACE  CRX;
(* Parser Generation *)

IMPORT Wr ;

PROCEDURE Generate() ;
(* Generates the target compiler (parser). *)

PROCEDURE WriteStatistics(wr : Wr.T) ;
(* Writes statistics about compilation *)

END CRX.

