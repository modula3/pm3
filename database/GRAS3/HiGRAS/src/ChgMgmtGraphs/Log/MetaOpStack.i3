INTERFACE MetaOpStack;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:32:59  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/11/20 12:20:46  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/09/23 08:34:54  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

(* A MetaOpStack stores cardinal numbers to protocol the application of
   meta commands such as und/redo and backstep/forstep.  MetaOpStacks are
   needed to implement the operatoins backstaep and forstep of
   ChgMgmtGraph.  Though MetaOpStacks will typically only store few
   cardinals they might in principle grow indefinitly long.  Hence, there
   is neither a Full exception nor a check if the stack is full.

   This is the abstract base class of PersistentMetaOpStack and
   VolatileMetaOpStack. *)

IMPORT Access;
IMPORT AtomList;

TYPE

  T =
    (* ABSTRACT *) OBJECT
    METHODS
      push (x: CARDINAL) RAISES {Access.Locked, InternalError} := NIL;
            (* Push x on the stack. *)
      pop (): CARDINAL RAISES {Empty, Access.Locked, InternalError} := NIL;
           (* Take the top element from the stack and return its value. *)
      clear () RAISES {Access.Locked, InternalError} := NIL;
             (* Remove all elements from the stack. *)

      isEmpty (): BOOLEAN RAISES {Access.Locked, InternalError} := NIL;
               (* Test if stack is empty. *)
    END;

EXCEPTION
  Empty;
  InternalError(AtomList.T);
  
END MetaOpStack.
