MODULE GrasParams;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:51  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:41  hosking
    Import of GRAS3 1.1

    Revision 1.4  1997/06/20 14:33:59  roland
    Bugfix: ServerId-Parameter must be assigned to serverid.

    Revision 1.3  1997/05/05 10:55:46  roland
    Error message added.

    Revision 1.2  1997/03/25 17:07:34  roland
    GrasServerId-Parameter added.

    Revision 1.1  1997/02/28 13:28:27  roland
    A utility module for reading GRAS3 configuration options from
    command-line or environment variables.

*)
(***************************************************************************)

IMPORT Env, ParseParams, Pathname, Stdio, Config, IO, Scan, FloatMode, Lex;

VAR
  graskey: ARRAY Param OF TEXT := DefaultKeys;
  grasenv: ARRAY Param OF TEXT := DefaultEnv;

PROCEDURE SetKeyword (param: Param; key: TEXT) =
  BEGIN
    graskey[param] := key;
  END SetKeyword;

PROCEDURE SetEnvName (param: Param; name: TEXT) =
  BEGIN
    grasenv[param] := name;
  END SetEnvName;

PROCEDURE ParseComandLine (VAR root      : TEXT;
                           VAR rootValid : BOOLEAN;
                           VAR cacheSize : CARDINAL;
                           VAR serverid  : TEXT;
                           VAR nameserver: TEXT;
                               quiet     : BOOLEAN    := FALSE) =

  VAR pp := NEW(ParseParams.T).init(Stdio.stderr);

  PROCEDURE ParseError (err: TEXT) =
    BEGIN
      IF NOT quiet THEN
        IO.Put(Pathname.Last(pp.arg[0]) & ": " & err & "\n", Stdio.stderr);
      END;
    END ParseError;

  PROCEDURE Cardinal (t: TEXT; VAR val: CARDINAL): BOOLEAN =
    VAR v: INTEGER;
    BEGIN
      TRY
        v := Scan.Int(t);
      EXCEPT
        Lex.Error, FloatMode.Trap => RETURN FALSE;
      END;
      IF v >= FIRST(CARDINAL) AND v <= LAST(CARDINAL) THEN
        val := v;
        RETURN TRUE;
      END;
      RETURN FALSE;
    END Cardinal;

  VAR cacheText: TEXT;
  BEGIN
    root := NIL;
    rootValid := FALSE;
    cacheSize := Config.DefaultCacheSize;
    nameserver := Config.DefaultNameServer;

    TRY
      root := Env.Get(grasenv[Param.RootPath]);
      IF pp.keywordPresent(graskey[Param.RootPath]) THEN
        root := pp.getNext();
      END;
    EXCEPT
      ParseParams.Error =>
        ParseError(
          "Parameter '" & graskey[Param.RootPath] & "' requires an argument.");
    END;
    IF root # NIL THEN
      rootValid := TRUE;
    ELSE
        ParseError("Root path not set. You must either use command line\n" &
          "switch '" & graskey[Param.RootPath] & "' or environment variable '" &
          grasenv[Param.RootPath] & "' to specify it.\n");
    END;

    TRY
      cacheText := Env.Get(grasenv[Param.CacheSize]);
      IF cacheText # NIL AND NOT Cardinal(cacheText, cacheSize) THEN
        ParseError("Environment variable '" & graskey[Param.CacheSize]
                     & "' does not contain a valid cache-size.");
      END;
      IF pp.keywordPresent(graskey[Param.CacheSize]) THEN
        cacheText := pp.getNext();
      END;
      IF cacheText # NIL AND NOT Cardinal(cacheText, cacheSize) THEN
        ParseError("Parameter argument for key '" & graskey[Param.CacheSize]
                     & "' is not a valid cache-size.");
      END;
    EXCEPT
      ParseParams.Error =>
        ParseError("Parameter '" & graskey[Param.CacheSize]
                     & "' requires a numeric argument.");
    END;

    TRY
      serverid := Env.Get(grasenv[Param.ServerId]);
      IF pp.keywordPresent(graskey[Param.ServerId]) THEN
        serverid := pp.getNext();
      END;
    EXCEPT
      ParseParams.Error =>
        ParseError("Parameter '" & graskey[Param.ServerId]
                     & "' requires an argument.");
    END;

    TRY
      nameserver := Env.Get(grasenv[Param.NameServer]);
      IF pp.keywordPresent(graskey[Param.NameServer]) THEN
        nameserver := pp.getNext();
      END;
    EXCEPT
      ParseParams.Error =>
        ParseError("Parameter '" & graskey[Param.NameServer]
                     & "' requires an argument.");
    END;
  END ParseComandLine;

BEGIN
END GrasParams.
