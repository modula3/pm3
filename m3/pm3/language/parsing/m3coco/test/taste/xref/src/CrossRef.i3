INTERFACE CrossRef ;
(* Create cross reference list of identifiers
   P.D. Terry, Rhodes University, January 1992
   Release for use with COCO/R system - uses FileIO *)

IMPORT Wr ;

TYPE
  Table <: ROOT ;

VAR
  global : Table ;

PROCEDURE Create(VAR table : Table) ;
  (* Initialise a new (empty) Table *)

PROCEDURE Add(VAR table : Table ; Name : TEXT ;
              Reference : CARDINAL ; Defining : BOOLEAN) ;
  (* Add Name to Table with given Reference, specifying whether this is a
     Defining (as opposed to an applied occurrence) *)

PROCEDURE List(table : Table ; wr : Wr.T) ;
  (* List out cross reference Table on output device *)

END CrossRef.
