MODULE TypedNames;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.4  1998/03/18 12:13:30  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.3  1998/03/17 14:14:35  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/07/21 10:48:12  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/05/01 13:23:28  roland
    TypedGraph layer adapted to graph boundary crossing edges.

*)
(***************************************************************************)

IMPORT Pathname, TextCursorSet;
IMPORT ChgMgmtNames AS Super;
IMPORT ErrorSupport, Names, Text, Access, ChgMgmtGraphPool;

CONST
  (* Collections *)
  SchemesCollName = "Schemes";

  (* Relations *)
  ToSchemeName = "GraphToScheme";

  (* Attributes *)
  VersionName = "SchemeVersion";


REVEAL
  T = Public BRANDED OBJECT
        (* These variables hold the ids of collections, attributes, and
           realtions *)
        schemes : CARDINAL;
        toScheme: CARDINAL;
        version : CARDINAL;
      OVERRIDES
        login               := Login;
        insertScheme        := InsertScheme;
        setVersion          := SetVersion;
        getVersion          := GetVersion;
        connectToScheme     := ConnectToScheme;
        existsScheme        := ExistsScheme;
        hasScheme           := HasScheme;
        getScheme           := GetScheme;
        getGraphsWithScheme := GetGraphsWithScheme;
        getSchemes          := GetSchemes;
      END;

CONST
  (* Constant attribute values *)
  NullText = "\000\000\000\000";

PROCEDURE TextToInt (t: TEXT): INTEGER =
  VAR
    b: ARRAY [0 .. 3] OF CHAR;
    i: INTEGER                := 0;
  BEGIN
    Text.SetChars(b, Text.Sub(t, 0, 4));
    i := ORD(b[3]);
    FOR k := 2 TO 0 BY -1 DO i := i * 256 + ORD(b[k]) END;
    RETURN i;
  END TextToInt;

PROCEDURE IntToText (x: INTEGER): TEXT =
  VAR b: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    FOR k := 0 TO 2 DO b[k] := VAL(x MOD 256, CHAR); x := x DIV 256; END;
    b[3] := VAL(x, CHAR);
    RETURN Text.FromChars(b);
  END IntToText;

(* Everything we use here will be declared at Login.  If the application
   does not login, every call to Names will fail anyway. *)
<* FATAL Names.Undeclared *>

PROCEDURE Login (names         : T;
                 pool          : ChgMgmtGraphPool.T;
                 collectionname: Pathname.T          )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      Super.T.login(names, pool, collectionname);
      (* Get Ids for collections and relations *)
      names.schemes := Super.T.declareCollection(names, SchemesCollName);
      names.toScheme := Super.T.declareRelation(names, ToSchemeName);
      names.version := Super.T.declareAttribute(names, VersionName);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.Login", "ChgMgmtNames.InternalError", info));
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.Login", "Names.InternalError", info));
    END;
  END Login;


PROCEDURE InsertScheme (names: T; name: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  <* FATAL Names.Unknown *>
  BEGIN
    TRY
      Super.T.insert(names, name, local, collection := names.schemes);
      Super.T.setAttribute(names, name, local, names.version, NullText);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.InsertGraph", "Names.InternalError", info));
    END;
  END InsertScheme;

PROCEDURE SetVersion (names  : T;
                      name   : Pathname.T;
                      local  : BOOLEAN;
                      version: CARDINAL    )
  RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      Super.T.setAttribute(
        names, name, local, names.version, IntToText(version));
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.SetVersion", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END SetVersion;

PROCEDURE GetVersion (names: T; name: Pathname.T; local: BOOLEAN): CARDINAL
  RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      RETURN
        TextToInt(Super.T.getAttribute(names, name, local, names.version));
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.GetVersion", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetVersion;

PROCEDURE ConnectToScheme (names : T;
                           grname: Pathname.T;
                           sname : Pathname.T;
                           local : BOOLEAN      := FALSE)
  RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      Super.T.insertInRelation(names, grname, sname, local, names.toScheme);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedNames.ConnectToScheme",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END ConnectToScheme;

PROCEDURE ExistsScheme (names: T; sname: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.contained(names, sname, local, names.schemes);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.ExistsScheme", "Names.InternalError", info));
    END;
  END ExistsScheme;

PROCEDURE HasScheme (names: T; gname: Pathname.T; local: BOOLEAN): BOOLEAN
  RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      RETURN
        NOT Super.T.targets(names, gname, local, names.toScheme).isEmpty();
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.HasScheme", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END HasScheme;

PROCEDURE GetScheme (names: T; gname: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, Unknown, InternalError} =
  VAR
    ok   : BOOLEAN;
    sname: TEXT;
  BEGIN
    TRY
      sname :=
        Super.T.targets(
          names, gname, local, names.toScheme).extractAnyElement(ok);
      IF NOT ok THEN RAISE Unknown END;
      RETURN sname;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.GetScheme", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetScheme;

PROCEDURE GetGraphsWithScheme (names: T; sname: Pathname.T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.sources(names, sname, local, names.toScheme);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedNames.GetGraphsWithScheme",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetGraphsWithScheme;

PROCEDURE GetSchemes (names: T; local: BOOLEAN): TextCursorSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN
        Super.T.getAllEntries(names, local, collection := names.schemes);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedNames.GetSchemes", "Names.InternalError", info));
    END;
  END GetSchemes;

BEGIN
END TypedNames.
