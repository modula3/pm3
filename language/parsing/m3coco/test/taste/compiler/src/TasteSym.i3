INTERFACE TasteSym ;

TYPE
  ObjKind = { Undef, Var, Proc } ;
  VarType = { Undef, Int, Bool } ;

TYPE
  Obj      = OBJECT
               name    : TEXT ;      (* name of the object *)
               kind    : ObjKind ;   (* the kind of object *)
               vtype   : VarType ;   (* type (undef for procedures) *)
               adr     : CARDINAL ;  (* address in memory *)
               level   : CARDINAL ;  (* nesting level of declaration for vars
                                        next free address in scope for procs *)
               next    : Obj
             END ;

VAR
  undefObj : Obj ;                   (* object node for erroneous symbols *)
  curLevel : CARDINAL ;              (* nesting level of current scope *)

PROCEDURE EnterScope() ;

PROCEDURE LeaveScope() ;

PROCEDURE DataSpace() : CARDINAL ;

PROCEDURE New(name: TEXT ; kind : ObjKind) : Obj ;

PROCEDURE Find(name: TEXT) : Obj ;

END TasteSym.
