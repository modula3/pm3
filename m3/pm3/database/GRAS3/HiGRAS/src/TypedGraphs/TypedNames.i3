INTERFACE TypedNames;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.3  1998/03/18 12:13:29  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.2  1998/03/17 14:14:34  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/05/01 13:23:26  roland
    TypedGraph layer adapted to graph boundary crossing edges.

*)
(***************************************************************************)

IMPORT Pathname, TextCursorSet, Access, ChgMgmtGraphPool;
IMPORT AtomList;
IMPORT ChgMgmtNames AS Super;

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      login (pool: ChgMgmtGraphPool.T; collection: Pathname.T)
             RAISES {InternalError, Access.Locked};

      insertScheme (name: Pathname.T; local: BOOLEAN)
                    RAISES {Access.Locked, InternalError};
                    (* Insertion of a new scheme with name 'name'. *)

      setVersion (name: Pathname.T; local: BOOLEAN; version: CARDINAL)
                  RAISES {Access.Locked, Unknown, InternalError};
                  (* Sets a version number for name, which might be a graph
                     or a scheme. *)

      getVersion (name: Pathname.T; local: BOOLEAN): CARDINAL
                  RAISES {Access.Locked, Unknown, InternalError};
                  (* Read the version number of name *)

      connectToScheme (grname: Pathname.T;
                       sname : Pathname.T;
                       local : BOOLEAN      := FALSE)
                       RAISES {Access.Locked, Unknown, InternalError};
                       (* Graph grname gets scheme sname as its scheme *)

      existsScheme (sname: Pathname.T; local: BOOLEAN): BOOLEAN
                    RAISES {Access.Locked, InternalError};
                    (* Returns TRUE, if the scheme with name name exists *)

      hasScheme (gname: Pathname.T; local: BOOLEAN): BOOLEAN
                 RAISES {Access.Locked, Unknown, InternalError};
                 (* Returns TRUE, iff graph gname has a scheme *)

      getScheme (gname: Pathname.T; local: BOOLEAN): Pathname.T
                 RAISES {Access.Locked, Unknown, InternalError};
                 (* Returns the scheme of graph gname *)

      getGraphsWithScheme (sname: Pathname.T; local: BOOLEAN):
                           TextCursorSet.T
                           RAISES {Access.Locked, Unknown, InternalError};
                           (* Find all graphs that have sname as scheme *)

      getSchemes (local: BOOLEAN): TextCursorSet.T
                  RAISES {Access.Locked, InternalError};
                  (* Return all schemes *)
    END;

EXCEPTION
  InternalError(AtomList.T);
  Unknown;

END TypedNames.
