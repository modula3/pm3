INTERFACE Delta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.3  1998/05/19 10:17:44  roland
    Support for log-groups implemented.

    Revision 1.2  1997/05/30 07:54:01  roland
    Backward loop added to deltas to efficiently implement copying of
    backward deltas.

    Revision 1.1  1997/04/23 13:32:31  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:20:41  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/20 13:58:33  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:07  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A Delta.T is a sequence of graph changing commands.  Commands can be
   added to a delta but never removed.  The commands of a delta can be read
   in a loop.  The cost of a delta is the number of commands stored.

   Delta.T is the virtual base class for PersistentDelta.T and
   VolatileDelta.T. *)

IMPORT GraphCommand, Access;
IMPORT AtomList;

TYPE
  T =
    (* ABSTRACT *) OBJECT
    METHODS
      addCommand (READONLY c: GraphCommand.T) RAISES {Access.Locked, Error} := NIL;
                  (* Add c to the end of the delta. *)

      costs (): CARDINAL RAISES {Access.Locked, Error} := NIL;
             (* Return the costs of a delta *)

      loop () RAISES {Access.Locked, Error} := NIL;
            (* Initialize a loop to read all commands of a delta. *)

      getNextCommand (VAR c: GraphCommand.T; VAR ok: BOOLEAN)
                     RAISES {Error, Access.Locked} := NIL;
                      (* Return the next element in the sequence and step
                         forward in the loop.  If there is no next command,
                         ok will be FALSE, otherwise TRUE. *)

      reverseLoop () RAISES {Access.Locked, Error} := NIL;
            (* Initialize a loop to read all commands of a delta in reverse
               order. *)

      getPrevCommand (VAR c: GraphCommand.T; VAR ok: BOOLEAN)
                     RAISES {Error, Access.Locked} := NIL;
                      (* Return the previous element in the sequence and step
                         backward in the loop.  If there is no previous command,
                         ok will be FALSE, otherwise TRUE. *)

END;

EXCEPTION
  Error(AtomList.T); (* Reading a command failed *)
END Delta.
