(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Unit.m3                                               *)
(* Last modified on Thu Sep 22 11:53:02 PDT 1994 by kalsow     *)
(*      modified on Fri May 28 15:34:11 PDT 1993 by muller     *)

MODULE Unit;

IMPORT Rd, FileRd, Text, Thread, TextTextTbl, Pathname, OSError;
IMPORT M3Compiler, M3Path, Msg, Utils, FS, File, Wr;

CONST
  suffix = ARRAY [0..3] OF M3Path.Kind {
    M3Path.Kind.M3, M3Path.Kind.MG, M3Path.Kind.I3, M3Path.Kind.IG
  };

TYPE
  Node = REF RECORD
    next : Node;
    dir  : TEXT;
    map  : TextTextTbl.T;
  END;

VAR
  path: Node := NIL;

PROCEDURE ResetPath () =
  BEGIN
    path := NIL;
  END ResetPath;

PROCEDURE PushDir (name : TEXT) =
  BEGIN
    (* Msg.Debug ("Unit.PushDir (", name, ")", Wr.EOL); *)
    path := NEW (Node, next := path, dir := name, map := NIL);
  END PushDir;

PROCEDURE PushTable (name: TEXT) =
  BEGIN
    (* Msg.Debug ("Unit.PushTable (", name, ")", Wr.EOL); *)
    path := NEW (Node, next := path, dir := name, map := ReadTable (name));
  END PushTable;

PROCEDURE Open (name: TEXT; interface,generic:BOOLEAN): M3Compiler.SourceFile =
  VAR file: M3Compiler.SourceFile;  n := 0;
  BEGIN
    IF (interface) THEN INC (n,2); END;
    IF (generic)   THEN INC (n);   END;
    file.name := M3Path.Join (NIL, name, suffix[n], host := TRUE);
    SearchPath (file);
    RETURN file;
  END Open;

PROCEDURE Find (name: TEXT;  interface, generic: BOOLEAN): TEXT =
  VAR file: M3Compiler.SourceFile;  n := 0;
  BEGIN
    IF (interface) THEN INC (n,2); END;
    IF (generic)   THEN INC (n);   END;
    file.name := M3Path.Join (NIL, name, suffix[n], host := TRUE);
    SearchPath (file);
    IF (file.contents = NIL) THEN RETURN NIL; END;
    Utils.CloseReader (file.contents, file.name);
    RETURN file.name;
  END Find;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE ReadTable (file: TEXT): TextTextTbl.T =
  <*FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted*>
  VAR tbl := NEW (TextTextTbl.Default).init (32);
  VAR rd: Rd.T;  dir, unit, other: TEXT;
  BEGIN
    TRY
      rd := FileRd.Open (file);
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, "unable to open import table: ", file);
    END;

    dir := NIL;
    WHILE NOT Rd.EOF (rd) DO
      unit := Rd.GetLine (rd);
      (* Msg.Debug ("  Rd.GetLine -> \"", unit, "\"", Wr.EOL); *)
      IF Text.GetChar (unit, 0) = '@' THEN
        dir := Text.Sub (unit, 1, LAST (CARDINAL));
        (* Msg.Debug ("  dir := ", dir, Wr.EOL); *)
      ELSIF tbl.get (unit, other) THEN
        Msg.FatalError (NIL,
                     "duplicate unit "& unit &" in import table; present in ", 
                     other, " and ", dir);
      ELSE
        EVAL tbl.put (unit, dir);
      END;
    END;

    TRY
      Rd.Close (rd);
    EXCEPT Rd.Failure =>
      Msg.FatalError (NIL, "unable to close import table: ", file);
    END;

    RETURN tbl;
  END ReadTable;

PROCEDURE SearchPath (VAR file: M3Compiler.SourceFile) =
  VAR n: Node;  fullname, dir: TEXT;
  BEGIN
    file.contents := NIL;
    Msg.Debug ("--Unit.SearchPath (", file.name, ")", Wr.EOL);
    IF Text.Empty (file.name) THEN RETURN; END;

    IF Pathname.Absolute (file.name) THEN
      Msg.Debug ("  absolute path...", Wr.EOL);
      IF IsReadable (file.name, file.contents) THEN RETURN; END;

    ELSE
      (* try the search path... *)
      n := path;
      WHILE (n # NIL) DO
        IF (n.map = NIL) THEN
          Msg.Debug ("  try dir ", n.dir, Wr.EOL);
	  fullname := M3Path.Join (n.dir, file.name,
                                   M3Path.Kind.Unknown, host := TRUE);
          IF IsReadable (fullname, file.contents) THEN
            Msg.Verbose ("  resolve: ", file.name, " -> ", fullname);
            file.name := fullname;
            RETURN;
          END;
        ELSIF n.map.get (file.name, dir) THEN
          IF (dir = NIL)
            THEN fullname := file.name;
            ELSE fullname := M3Path.Join (dir, file.name,
                                          M3Path.Kind.Unknown, host := TRUE);
          END;
          Msg.Debug ("  try file ", fullname, Wr.EOL);
          IF IsReadable (fullname, file.contents) THEN
            Msg.Verbose ("  resolve: ", file.name, " -> ", fullname);
            file.name := fullname;
            RETURN;
          END;
        END;
        n := n.next;
      END;
    END;

    (* failed *)
    Msg.Verbose ("  cannot find: ", file.name);
  END SearchPath;

PROCEDURE IsReadable (file: TEXT;  VAR(*OUT*) rd: File.T): BOOLEAN =
  BEGIN
    TRY
      rd := FS.OpenFileReadonly (file);
      RETURN TRUE;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END IsReadable;

BEGIN
END Unit.
