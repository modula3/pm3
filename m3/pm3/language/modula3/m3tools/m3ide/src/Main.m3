(*  This file is part of m3ide, a simple development environment for M3    *)
(*  Copyright (C) 1995 Michel Dagenais                                     *)
(*                                                                         *)
(*  This program is free software; you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation; either version 2 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program; if not, write to the Free Software            *)
(*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.              *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

(* This program awaits commands on stdin, fetches arguments according
   to the command received, executes the command and returns the error
   text and result text. The arguments and results are sent as a ten
   character ASCII integer length, a newline and the value of the specified
   length. The commands accepted correspond to the procedures available
   in Ide.i3. *)

MODULE Main;

IMPORT Text, Stdio, Lex, Wr, Rd, Ide, Fmt, Thread, FloatMode,
       TextF, M3Config;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>
<*FATAL FloatMode.Trap*>
<*FATAL Rd.EndOfFile*>
<*FATAL Rd.Failure*>

PROCEDURE ReplaceChar(in: TEXT; from, to: CHAR): TEXT =
  VAR
    len := Text.Length(in);
    out := TextF.New(len);
  BEGIN
    FOR i := 0 TO len - 1 DO
      IF in[i] = from THEN out[i] := to; ELSE out[i] := in[i] END;
    END;
    RETURN out;
  END ReplaceChar;

PROCEDURE GetFileArg(rd: Rd.T): TEXT =
  BEGIN
    IF convert THEN RETURN ReplaceChar(GetArg(rd), extSL, intSL);
    ELSE RETURN GetArg(rd);
    END;
  END GetFileArg;

PROCEDURE SendFileResult(text: TEXT) =
  BEGIN
    IF convert THEN SendResult(ReplaceChar(text, intSL, extSL));
    ELSE SendResult(text);
    END;
  END SendFileResult;
  
PROCEDURE SendResult(text: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout,Fmt.Pad(Fmt.Int(Text.Length(text)),10) & "\n");
    Wr.PutText(Stdio.stdout,text);
    Wr.Flush(Stdio.stdout);
  END SendResult;

<*FATAL Lex.Error*>

PROCEDURE GetArg(rd: Rd.T): TEXT =
  VAR
    length: INTEGER;
    value: TEXT;
  BEGIN
    Lex.Skip(rd);
    length := Lex.Int(rd);
    value := Rd.GetLine(rd); (* Skip to end of line *)
    value := Rd.GetText(rd,length);
    RETURN value;
  END GetArg;

PROCEDURE Interact() =
  VAR
    command, arg1, arg2, arg3, arg4, arg5, arg6: TEXT;
    result: TEXT;
    arrayResult: REF ARRAY OF TEXT;
    error: TEXT;
  BEGIN
    Wr.PutText(Stdio.stdout,
        "Started (if you see this, you are not within emacs as you should)\n");
    Wr.Flush(Stdio.stdout);
    LOOP
      command := Rd.GetLine(Stdio.stdin);
      IF Text.Equal(command,"showForm") THEN
        arg1 := GetFileArg(Stdio.stdin);
        arg2 := GetArg(Stdio.stdin);
        result := Ide.ShowForm(arg1,arg2);
        SendResult("\n");
        SendResult(result);
      ELSIF Text.Equal(command,"pprint") THEN
        arg1 := GetArg(Stdio.stdin);
        result := Ide.PPrint(arg1,error);
        SendResult(error);
        SendResult(result);
      ELSIF Text.Equal(command,"pkgPrefix") THEN
        arg1 := GetFileArg(Stdio.stdin);
        result := Ide.PackagePrefix(arg1);
        SendResult("\n");
        SendFileResult(result);
      ELSIF Text.Equal(command,"pkgM3makefile") THEN
        arg1 := GetFileArg(Stdio.stdin);
        result := Ide.PackageM3makefile(arg1);
        SendResult("\n");
        SendFileResult(result);
      ELSIF Text.Equal(command,"pkgProgram") THEN
        arg1 := GetFileArg(Stdio.stdin);
        arg2 := GetFileArg(Stdio.stdin);
        result := Ide.PackageProgram(arg1,arg2);
        SendResult("\n");
        SendFileResult(result);
      ELSIF Text.Equal(command,"buildDir") THEN
        arg1 := GetFileArg(Stdio.stdin);
        result := Ide.BuildDir(arg1);
        SendResult("\n");
        SendFileResult(result);
      ELSIF Text.Equal(command,"buildPath") THEN
        arg1 := GetFileArg(Stdio.stdin);
        arg2 := GetFileArg(Stdio.stdin);
        result := Ide.BuildPath(arg1,arg2);
        SendResult("\n");
        SendFileResult(result);
      ELSIF Text.Equal(command,"sourcePaths") THEN
        arg1 := GetFileArg(Stdio.stdin);
        arg2 := GetFileArg(Stdio.stdin);
        arg3 := GetArg(Stdio.stdin);
        arg4 := GetArg(Stdio.stdin);
        arg5 := GetArg(Stdio.stdin);
        arg6 := GetArg(Stdio.stdin);

        arrayResult := Ide.SourcePaths(arg1,arg2);
        result := arg3;
        FOR i := 0 TO LAST(arrayResult^) DO
          result := result & arg5 & arrayResult[i] & arg6;
        END;
        result := result & arg4;
        SendResult("\n");
        SendFileResult(result);
      ELSIF Text.Equal(command,"exit") THEN
        EXIT;
      ELSIF Text.Equal(command,"") THEN
        (* ignore *)
      ELSIF Debug THEN
        Wr.PutText(debugFile,"Unknown command " & command & "\n");
      END;
    END;
  END Interact;

CONST
  Debug = FALSE;

VAR
  intSL := M3Config.PATH_SEP[0];
  extSL := '/';
  convert := intSL # extSL;
  debugFile: Wr.T;

BEGIN
  Interact();
END Main.

