MODULE TriggerMap;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:21  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT IntIntTbl;

VAR
  LocalToRemote, RemoteToLocal: IntIntTbl.T;
  Access                      : MUTEX;

PROCEDURE Bind (local, remote: CARDINAL) =
  BEGIN
    LOCK Access DO
      EVAL LocalToRemote.put(local, remote);
      EVAL RemoteToLocal.put(remote, local);
    END;
  END Bind;

PROCEDURE RemoveWithLocal (local: CARDINAL) =
  VAR remote, loc: INTEGER;
  BEGIN
    LOCK Access DO
      IF LocalToRemote.delete(local, remote) THEN
        EVAL RemoteToLocal.delete(remote, loc);
      END;
    END;
  END RemoveWithLocal;

PROCEDURE RemoveWithRemote (remote: CARDINAL) =
  VAR local, rem: INTEGER;
  BEGIN
    LOCK Access DO
      IF RemoteToLocal.delete(remote, local) THEN
        EVAL LocalToRemote.delete(local, rem);
      END;
    END;
  END RemoveWithRemote;

PROCEDURE GetLocal (remote: CARDINAL; VAR local: CARDINAL): BOOLEAN =
  VAR loc: INTEGER;
  BEGIN
    LOCK Access DO
      IF RemoteToLocal.get(remote, loc) THEN
        local := loc;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END
  END GetLocal;

PROCEDURE GetRemote (local: CARDINAL; VAR remote: CARDINAL): BOOLEAN =
  VAR rem: INTEGER;
  BEGIN
    LOCK Access DO
      IF LocalToRemote.get(local, rem) THEN
        remote := rem;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
  END GetRemote;

BEGIN
  LocalToRemote := NEW(IntIntTbl.Default).init();
  RemoteToLocal := NEW(IntIntTbl.Default).init();
  Access := NEW(MUTEX);
END TriggerMap.
