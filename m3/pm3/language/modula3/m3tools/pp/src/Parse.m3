(* Copyright (C) 1990, Digital Equipment Corporation.             *)
(* All rights reserved.                                           *)
(* See the file COPYRIGHT for a full description.                 *)


(* Last modified on Fri May 26 08:35:28 PDT 1995 by kalsow                 *)
(*      modified on Tue Jun 23 10:42:31 PDT 1992 by schilit@xerox.com      *)
(*      modified on Thu Jul 25 20:16:14 PDT 1991 by stolfi                 *)
(*      modified on Mon Apr 22 17:42:17 1991 by nichols@xerox.com          *)
(*      modified on Tue Apr 10 23:47:30 1990 by muller                     *)


UNSAFE MODULE Parse;

IMPORT NewFormatter AS Formatter;
IMPORT Ctypes, FBE, M3toC;

VAR fontInfo: FontInfo;
(* we must keep the traced font references here, in a Modula-3 global
   variable, so that the collector can update them as it moves
   objects around.  **** NOTE: this module is non-reentrant! **** *)

(* Do various setups, then call C init routine. *)
PROCEDURE Init (         inputFile      : TEXT;
                         output         : Formatter.T;
                READONLY info           : Options;
                         calledFromEmacs: BOOLEAN      ) RAISES {FBE.Failed} =
  VAR
    infile : Ctypes.char_star := NIL;
    fbe : FBE.T := Formatter.UnderlyingFBE(output);
    spaceWidth: REAL;
  BEGIN
    fontInfo.bf  := fbe.GetFont(info.bodyFont);
    fontInfo.kf  := fbe.GetFont(info.keywordFont);
    fontInfo.bif := fbe.GetFont(info.builtinIDFont);
    fontInfo.pf  := fbe.GetFont(info.procNameFont);
    fontInfo.cf  := fbe.GetFont(info.commentFont);
    fontInfo.fcf := fbe.GetFont(info.fixedCommentFont);
    fontInfo.ff  := fbe.GetFont(info.fixedFont);
    spaceWidth   := fbe.CharWidth(' ', fontInfo.bf);
    IF inputFile # NIL THEN infile := M3toC.TtoS(inputFile); END;
    initParser(
      infile      := infile,
      output      := output,
      emacs       := ORD (calledFromEmacs),
      lowerCase   := ORD (info.lowerCase),
      fonts       := fontInfo,
      offset      := FLOAT (info.offset * spaceWidth, LONGREAL),
      commentColumn := FLOAT (info.commentColumn * spaceWidth, LONGREAL),
      style       := ORD (info.style),
      alignDecls  := ORD (info.alignDecls),
      breakType   := ORD (info.breakType),
      follow (* comBreakNLs *) := ORD(info.follow),
      callSpace   := ORD (info.callSpace),
      charWidth   := CharWidth,
      flush       := Formatter.Flush,
      setFont     := Formatter.SetFont,
      putChar     := Formatter.PutChar,
      break       := Break,
      newLine     := NewLine,
      unitedBreak := UnitedBreak,
      group       := Formatter.Group,
      begin       := Begin,
      align       := Align,
      noAlign     := Formatter.NoAlign,
      col         := Col,
      end         := Formatter.End
    );
  END Init;

(*------------------------------------------------------------ C wrappers ---*)
(* This code is a bit kludgy.  Since C insists on promoting floats to
   doubles, we must provide some conversion wrappers. *)

(* Called from the C code. *)
PROCEDURE CharWidth(f: Formatter.T; font: FBE.Font; c: CHAR): LONGREAL =
  VAR fbe := Formatter.UnderlyingFBE(f);
  BEGIN
    RETURN FLOAT (fbe.CharWidth(c, font), LONGREAL);
  END CharWidth;

PROCEDURE Break (t: Formatter.T;
                 offset: LONGREAL;
                 type := Formatter.BreakType.OptimalBreak;
                 freshLine: INTEGER) RAISES {FBE.Failed} =
  BEGIN
    Formatter.Break (t, FLOAT (offset, REAL), type, freshLine # 0);
  END Break;

PROCEDURE NewLine (t: Formatter.T;
                   offset: LONGREAL;
                   freshLine: INTEGER) RAISES {FBE.Failed} =
  BEGIN
    Formatter.NewLine (t, FLOAT (offset, REAL), freshLine # 0);
  END NewLine;

PROCEDURE UnitedBreak (t: Formatter.T;
                       offset: LONGREAL;
                       freshLine: INTEGER) RAISES {FBE.Failed} =
  BEGIN
    Formatter.UnitedBreak (t, FLOAT (offset, REAL), freshLine # 0);
  END UnitedBreak;

PROCEDURE Begin (t: Formatter.T;
                 offset: LONGREAL;
                 width: LONGREAL) RAISES {FBE.Failed} =
  BEGIN
    Formatter.Begin (t, FLOAT (offset, REAL), FLOAT (width, REAL));
  END Begin;

PROCEDURE Align (t:          Formatter.T;
                 columns:    CARDINAL;
                 tryOneLine: INTEGER;
                 tryAlign:   INTEGER) RAISES {FBE.Failed} =
  BEGIN
    IF (tryAlign # 0)
      THEN Formatter.Align (t, columns, tryOneLine # 0, 0.0, NIL);
      ELSE Formatter.Align (t, columns, tryOneLine # 0, 0.0, falsePred);
    END;
  END Align;

VAR falsePred := NEW(Formatter.AlignPred, pred := FalseProc);

PROCEDURE FalseProc (
    <*UNUSED*> arg: REFANY;
    <*UNUSED*> column: CARDINAL;
    <*UNUSED*> maxWidth: REAL;
    <*UNUSED*> width: REAL
  ): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END FalseProc;

PROCEDURE Col (t: Formatter.T;
               column: LONGREAL;
               relative: INTEGER;
               space: LONGREAL) RAISES {FBE.Failed} =
  BEGIN
    Formatter.Col (t, FLOAT (column, REAL), relative # 0, FLOAT (space, REAL));
  END Col;


BEGIN
END Parse.
