(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: Xct.i3							*)
(* Last modified on Fri May  7 16:28:51 PDT 1993 by mjordan     *) 
(*      modified on Sat Feb 24 02:18:21 1990 by muller		*)
(*      modified on Tue Feb 13 10:43:34 1990 by jerome		*)

INTERFACE Xct;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	The Compound Text Functions Library			*)
(*	contains:						*)
(*								*)
(*			../include/Xmu/Xct.h			*)
(*==============================================================*)

(*
 *
 * Copyright 1988 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 * The interfaces described by this header file are for miscellaneous utilities
 * and are not part of the Xlib standard.
 *)

(*======================================================================
 * $XConsortium: Xct.h,v 1.3 89/11/21 12:03:47 rws Exp $
 *======================================================================*)

FROM  Ctypes	IMPORT char_star, int, unsigned_long;

CONST
  Version	= 1;

TYPE
  String	= char_star;
  Enumeration   = int;
  Int = int;

TYPE
  HDirection	= Enumeration;
CONST
  Unspecified	= 0;
  LeftToRight	= 1;
  RightToLeft	= 2;


TYPE
  Flags   	= unsigned_long;

(* These are bits in Xct.Flags. *)

CONST
  SingleSetSegments	= 16_0001;
   (* This means that returned segments should contain characters from only
    * one set (C0, C1, GL, GR).  When this is requested, Xct.Segment is never
    * returned, instead Xct.C0Segment, Xct.C1Segment, Xct.GlSegment, and
    * Xct.GRSegment are returned.  C0 and C1 segments are always returned as
    * singleton characters.
    *)

  ProvideExtensions	= 16_0002;
   (* This means that if the Compound Text string is from a higher version
    * than this code is implemented to, then syntactically correct but unknown
    * control sequences should be returned as Xct.Extension items.  If this
    * flag is not set, and the Compound Text string version indicates that
    * extensions cannot be ignored, then each unknown control sequence will be
    * reported as an Xct.Error.
    *)

  AcceptC0Extensions	= 16_0004;
   (* This means that if the Compound Text string is from a higher version
    * than this code is implemented to, then unknown C0 characters should be
    * treated as if they were legal, and returned as C0 characters (regardless
    * of how Xct.ProvideExtensions is set).  If this flag is not set, then all
    * unknown C0 characters are treated according to Xct.ProvideExtensions.
    *)

  AcceptC1Extensions	= 16_0008;
   (* This means that if the Compound Text string is from a higher version
    * than this code is implemented to, then unknown C0 characters should be
    * treated as if they were legal, and returned as C0 characters (regardless
    * of how Xct.ProvideExtensions is set).  If this flag is not set, then all
    * unknown C0 characters are treated according to Xct.ProvideExtensions.
    *)

  HideDirection		= 16_0010;
   (* This means that horizontal direction changes should be reported as
    * Xct.Horizontal items. If this flag is not set, then direction changes are
    * not returned as items, but the current direction is still maintained and
    * reported for other items.
    *)

  FreeString		= 16_0020;
   (* This means that Xct.Free should free the Compound Text string (that was
    * passed to Xct.Create.  If this flag is not set, the string is not freed.
    *)

  ShiftMultiGRToGL	= 16_0040;
   (* Translate GR segments on-the-fly into GL segments for the GR sets:
    * GB2312.1980-1, JISX0208.1983-1, and KSC5601.1987-1.
    *)

(* This is the return type for Xct.NextItem. *)

TYPE
  Result	= Enumeration;
CONST
  Segment	= 0;	(* used when Xct.SingleSetSegments is not requested *)
  C0Segment	= 1;	(* used when Xct.SingleSetSegments is requested *)
  GLSegment	= 2;	(* used when Xct.SingleSetSegments is requested *)
  C1Segment	= 3;	(* used when Xct.SingleSetSegments is requested *)
  GRSegment	= 4;	(* used when Xct.SingleSetSegments is requested *)
  ExtendedSegment = 5;	(* an extended segment *)
  Extension	= 6;	(* used when Xct.ProvideExtensions is requested *)
  Horizontal	= 7;	(* horizontal direction or depth change *)
  EndOfText	= 8;	(* end of text string *)
  Error		= 9;	(* syntactic or semantic error *)


TYPE
  Data = RECORD  
    total_string: String;	(* as given to Xct.Create *)
    total_length: Int;		(* as given to Xct.Create *)
    flags: Flags;		(* as given to Xct.Create *)
    version: Int;		(* indicates the version of the CT spec
				 * the  string was produced from *)
    can_ignore_exts: Int; 	(* non-zero if ignoring extensions is
				 * acceptable, else zero *)
    item: String;		(* item returned from Xct.NextItem *)
    item_length: Int;		(* length of item in bytes *)
    char_size: Int;		(* number of bytes per character in
				 * item, with zero meaning variable *)
    encoding: char_star;		(* Encoding name for item *)
    horizontal: HDirection;	(* direction of item *)
    horz_depth: Int;		(* current direction nesting depth *)
    GL: char_star;		(* "{I} F" string for current GL *)
    GL_encoding: char_star;	(* Encoding name for current GL *)
    GL_set_size: Int;		(* 94 or 96 *)
    GL_char_size: Int;		(* number of bytes per GL character *)
    GR: char_star;		(* "{I} F" string for current GR *)
    GR_encoding: char_star;	(* Encoding name for current GR *)
    GR_set_size: Int;		(* 94 or 96 *)
    GR_char_size: Int;		(* number of bytes per GR character *)
    GLGR_encoding: char_star;	(* Encoding name for current GL+GR,
				 * if known *)
    priv: ADDRESS		(* private to parser, don't peek *)
  END;

(* these are the external routines *)

<* EXTERNAL XctCreate *>
   PROCEDURE Create (string: String; length: Int; flags: Flags): Data;

(* parses the next item from the CT string *)
<* EXTERNAL XctNextItem *>
   PROCEDURE NextItem (data: Data): Result;

(* frees all data associated with the Data structure *)
<* EXTERNAL XctFree *>
   PROCEDURE Free (data: Data);

(* resets the Data structure to reparse the CT string from the beginning *)
<* EXTERNAL XctReset *>
   PROCEDURE Reset (data: Data);

END Xct.
