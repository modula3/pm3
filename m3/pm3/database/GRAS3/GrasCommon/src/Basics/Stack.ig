GENERIC INTERFACE Stack(ElemType);

(***************************************************************************)
(* This module implements a stack of elements of ElemType.  It requires
   ElemType to export a type T *)
(***************************************************************************)
(** Created by:  Reiner Gombert                                            *)


(***************************************************************************)

CONST Brand = "( Stack " & ElemType.Brand & ")";


EXCEPTION
  Undefined;                     (* The stack was not initialized with
                                    Init. *)
  Full;
  Empty;


TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      (**********************************************************************)
      (**                                                                   *)
      (**                      Organization                                 *)
      (**                                                                   *)
      (**********************************************************************)

      init (): T;
            (* Initialize a stack created by a NEW(T) command.  After
               initialization, the stack is empty *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Queries                                      *)
      (**                                                                   *)
      (**********************************************************************)

      isEmpty (): BOOLEAN RAISES {Undefined};
               (* Yields TRUE if the stack is empty. *)

      isFull (): BOOLEAN RAISES {Undefined};
              (* Yields TRUE if the stack is full. *)

      depth (): CARDINAL RAISES {Undefined};
            (* Yields the size of the stack *)

      (**********************************************************************)
      (**                                                                   *)
      (**                      Element Queries                              *)
      (**                                                                   *)
      (**********************************************************************)

      top (): ElemType.T RAISES {Undefined,Empty};
           (* Yields the top element of the stack *)

      pop (): ElemType.T RAISES {Undefined,Empty};
           (* Yields the top element of the stack, removes it from the
              top *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Insert                                       *)
      (**                                                                   *)
      (**********************************************************************)

      push (READONLY Data: ElemType.T) RAISES {Undefined,Full};
            (* Insert Data at the top of the stack. *)

      (**********************************************************************)
      (**                                                                   *)
      (**                      Delete                                       *)
      (**                                                                   *)
      (**********************************************************************)

      clear () RAISES {Undefined}
            (* the stack is cleared *)


    END;

END Stack.
