MODULE PersistentMetaOpStack;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:30:36  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:27  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/11/20 12:21:18  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/09/23 08:35:31  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT MetaOpStack AS Super;
IMPORT FilePos, Pathname, Access, PageFile, VirtualResource, CardStream;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        stream: CardStream.T;
        start : FilePos.T;
      OVERRIDES
        open    := Open;
        close   := Close;
        push    := Push;
        pop     := Pop;
        isEmpty := IsEmpty;
        clear   := Clear;
      END;

PROCEDURE Open (s       : T;
                resource: VirtualResource.T;
                path    : Pathname.T;
                access  : Access.Mode;
                new     : BOOLEAN            ): T
  RAISES {Access.Denied, Access.Locked, PageFile.NoAccess, Super.InternalError} =
  BEGIN
    TRY
      s.stream :=
        NEW(CardStream.T).open(resource, path, access, new, forward := TRUE);
      s.stream.getFirstPosition(s.start);
    EXCEPT
      CardStream.DirectionMismatch =>
        RAISE Super.InternalError(
                ErrorSupport.Create("PersistentMetaOpStack.Open",
                                    "CardStream.DirectionMismatch"));
    | CardStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentMetaOpStack.Open",
                                       "CardStream.InternalError", info));
    END;
    RETURN s;
  END Open;

PROCEDURE Close (s: T) RAISES {Super.InternalError} =
  BEGIN
    TRY
      s.stream.close();
    EXCEPT
    | CardStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentMetaOpStack.Close",
                                       "CardStream.InternalError", info));
    END;
  END Close;

PROCEDURE Push (s: T; x: CARDINAL)
  RAISES {Access.Locked, Super.InternalError} =
  BEGIN
    TRY
      s.stream.write(x, overwrite := TRUE);
    EXCEPT
    | CardStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentMetaOpStack.Push",
                                       "CardStream.InternalError", info));
    END;
  END Push;

PROCEDURE Pop (s: T): CARDINAL
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    res: CARDINAL  := 0;
    pos: FilePos.T;
  BEGIN
    TRY
      s.stream.getPosition(pos);
      IF pos = s.start THEN
        RAISE Super.Empty;
      ELSE
        s.stream.backward();
        s.stream.read(res);
        (* go to new top *)
        s.stream.backward();
      END;
    EXCEPT
      CardStream.EOS, CardStream.ElementError =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentMetaOpStack.Pop",
                  "CardStream.EOS, CardStream.ElementError"));
    | CardStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentMetaOpStack.Pop",
                                       "CardStream.InternalError", info));
    END;
    RETURN res;
  END Pop;

PROCEDURE Clear (s: T) RAISES {Access.Locked, Super.InternalError} =
  BEGIN
    TRY
      s.stream.setPosition(s.start);
    EXCEPT
    | CardStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentMetaOpStack.Clear",
                                       "CardStream.InternalError", info));
    END;
  END Clear;

PROCEDURE IsEmpty (s: T): BOOLEAN
  RAISES {Access.Locked, Super.InternalError} =
  VAR pos: FilePos.T;
  BEGIN
    TRY
      s.stream.getPosition(pos);
    EXCEPT
    | CardStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentMetaOpStack.IsEmpty",
                                       "CardStream.InternalError", info));
    END;
    RETURN pos = s.start;
  END IsEmpty;

BEGIN
END PersistentMetaOpStack.
