INTERFACE LogContexts;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/12/02 17:56:37  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

(* The ChgMgmt-Layer introduces two contexts in which events may appear

   LinearReplay is active whenever Undo/Redo operations are initiated

   Replay is active for all user-recovery commands, i.e.  undo/redo redoIth,
   backstep, and forstep


   To receive only events that are not raised within user-recovery put
   'Replay' in the triggers inhibiting context set.

   To receive events that are raised as a consequence of a non-linear
   user-recovery command, put 'Replay' in the triggers permitting and
   'LinearReplay' in its inhibiting context set etc.

   *)

TYPE Context = {Replay, LinearReplay};

CONST
  Names = ARRAY Context OF TEXT{"Replay", "LinearReplay"};
  (* You can query the ContextSet interface with these names *)

PROCEDURE Set (unit: CARDINAL; c: Context);
PROCEDURE Clear (unit: CARDINAL; c: Context);
  (* These procedures call RuleEngine.ActivateContext and
     RuleEngine.DeactivateContext respectively.  They are used by GRAS to
     notify the RuleEngine about context changes and should never be called
     by client programs directly. *)

END LogContexts.
