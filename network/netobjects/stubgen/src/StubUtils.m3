(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Mon Feb 28 16:48:21 PST 1994 by wobber     *)
(*      modified on Mon May 17 13:37:22 PDT 1993 by mjordan    *)
(*      modified on Wed Feb 10 09:51:33 PST 1993 by owicki     *)

MODULE StubUtils;

IMPORT Atom, Fmt, Thread, Stdio, Protocol, Wr;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE Message(text: TEXT) =
  BEGIN
    Wr.PutText(stubchatter, "stubgen: " & text & "\n")
  END Message;

PROCEDURE SetPerfMon(flag: BOOLEAN) =
  BEGIN
    perfMon := flag;
  END SetPerfMon;

PROCEDURE FileName(typeName: Atom.T): TEXT =
  BEGIN
    RETURN Atom.ToText(typeName) & "_v" & Fmt.Int(Protocol.Version);
  END FileName;

BEGIN
  stubchatter := Stdio.stderr;
END StubUtils.


