INTERFACE DeltaInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:33:31  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/23 08:35:14  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT FilePos, Type;

TYPE
  State = {Open, Closed};
    (* State.Open => commands are still appended to the deltas.
       State.Close => a checkpoint was set and the delta will not be
       changed. *)

  SingleDelta = RECORD
                  costs: CARDINAL;
                  (* start, end and current position in a command stream *)
                  start, end, current: FilePos.T;
                END;

CONST
  Forward = TRUE;
  Backward = FALSE;

  (* ByteSize is twice the size of SingleDelta plus state *)
  ByteSize = 1 + 2 * (BYTESIZE(CARDINAL) + 3*FilePos.ByteSize);
  
TYPE
  T = RECORD
        state: State;
        info : ARRAY BOOLEAN OF SingleDelta;
      END;

(* Conversion routines to and from byte arrays *)
PROCEDURE ToByteArray (READONLY el : T; VAR ba: Type.ByteArray);

PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                         VAR      el : T);

END DeltaInfo.
