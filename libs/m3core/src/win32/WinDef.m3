(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Oct  3 11:59:48 PDT 1995 by najork   *)
(*      modified on Tue Nov  8 16:21:09 PST 1994 by kalsow   *)
(*      modified on Tue May 25 12:01:34 PDT 1993 by harrison *)

(*
 * Here are Modula-3 procedures providing the functionality of C macros.
 *)

MODULE WinDef;

IMPORT Word, Ctypes;

TYPE
  dummy = RECORD unused: Ctypes.int; END;

REVEAL
  HWND         = UNTRACED BRANDED "HWND" REF dummy;
  HHOOK        = UNTRACED BRANDED "HHOOK" REF dummy;
  (** HGDIOBJ      = UNTRACED BRANDED "HGDIOBJ" REF dummy; **)
  HACCEL       = UNTRACED BRANDED "HACCEL" REF dummy;
  HBITMAP      = UNTRACED BRANDED "HBITMAP" REF dummy;
  HBRUSH       = UNTRACED BRANDED "HBRUSH" REF dummy;
  HDC          = UNTRACED BRANDED "HDC" REF dummy;
  HGLRC        = UNTRACED BRANDED "HGLRC" REF dummy;
  HDESK        = UNTRACED BRANDED "HDESK" REF dummy;
  HENHMETAFILE = UNTRACED BRANDED "HENHMETAFILE" REF dummy;
  HMF          = UNTRACED BRANDED "HMF" REF dummy;
  HEMF         = UNTRACED BRANDED "HEMF" REF dummy;
  HFONT        = UNTRACED BRANDED "HFONT" REF dummy;
  HICON        = UNTRACED BRANDED "HICON" REF dummy;
  HMENU        = UNTRACED BRANDED "HMENU" REF dummy;
  HMETAFILE    = UNTRACED BRANDED "HMETAFILE" REF dummy;
  HINSTANCE    = UNTRACED BRANDED "HINSTANCE" REF dummy;
  HPALETTE     = UNTRACED BRANDED "HPALETTE" REF dummy;
  HPEN         = UNTRACED BRANDED "HPEN" REF dummy;
  HRGN         = UNTRACED BRANDED "HRGN" REF dummy;
  HRSRC        = UNTRACED BRANDED "HRSRC" REF dummy;
  HSTR         = UNTRACED BRANDED "HSTR" REF dummy;
  HWINSTA      = UNTRACED BRANDED "HWINSTA" REF dummy;
  HKL          = UNTRACED BRANDED "HKL" REF dummy;

<* INLINE *>
PROCEDURE MAKEWORD (a, b: BYTE): WORD =
  BEGIN
    RETURN Word.Or(a, Word.Shift(b, 8));
  END MAKEWORD;

<* INLINE *>
PROCEDURE MAKELONG (a, b: WORD): LONG =
  BEGIN
    RETURN Word.Or(a, Word.Shift(b, 16));
  END MAKELONG;

<* INLINE *>
PROCEDURE LOWORD (l: LONG): WORD =
  BEGIN
    RETURN Word.And(l, 16_ffff);
  END LOWORD;

<* INLINE *>
PROCEDURE HIWORD (l: LONG): WORD =
  BEGIN
    RETURN Word.And(Word.Shift(l, -16), 16_ffff);
  END HIWORD;

<* INLINE *>
PROCEDURE LOBYTE (w: WORD): BYTE =
  BEGIN
    RETURN Word.And(w, 16_ff)
  END LOBYTE;

<* INLINE *>
PROCEDURE HIBYTE (w: WORD): BYTE =
  BEGIN
    RETURN Word.And(Word.Shift(w, -8), 16_ff);
  END HIBYTE;

BEGIN
END WinDef.
