(*
   FBE.i3
   The backend of Formatter.i3.
   David Nichols, Xerox PARC
   July, 1991

   $Id$
*)
(* Copyright (c) 1991 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works based
   upon this software are permitted.  Any distribution of this software or
   derivative works must comply with all applicable United States export
   control laws.  This software is made available AS IS, and Xerox Corporation
   makes no warranty about the software, its performance or its conformity to
   any specification. *)

(* This interface is for a back end to Formatter.m3.  The idea is to allow the
   formatter to produce output using multiple fonts, etc.  Possible targets
   would be PostScript, Tioga, and ATK. *)

INTERFACE FBE;

IMPORT ExceptionArg;

EXCEPTION Failed(Failure);       (* when something goes wrong *)
TYPE
  Failure = ExceptionArg.T BRANDED OBJECT END;
  (* Failed because of Wr.Failure: *)
  WrFailure = Failure BRANDED OBJECT
                wrRef: REFANY;   (* the refany from Wr.Failure *)
              END;

TYPE
  Font = OBJECT END;
  T = OBJECT
      METHODS
        GetFont   (fontName: TEXT): Font RAISES {Failed}; (* lookup a font *)
        PageWidth (): REAL;      (* get width of page *)
        TextWidth (t: TEXT; font: Font): REAL; (* width of t in font *)
        CharWidth (c: CHAR; font: Font): REAL; (* width of c in font *)

        NewLine () RAISES {Failed}; (* go to next line *)
        Goto    (pos: REAL) RAISES {Failed}; (* go to x position on line *)
        GetPos  (): REAL;        (* find out x position *)
        PutText (t: TEXT; font: Font) RAISES {Failed}; (* output some text *)
        PutChar (c: CHAR; font: Font) RAISES {Failed}; (* output one char *)
        Flush   () RAISES {Failed}; (* flush pending output *)
        Close   () RAISES {Failed}; (* final cleanup *)
      END;

CONST
  MaxWidth = 1.0E20;             (* because LAST(REAL) doesn't work yet *)
  DefaultFont: Font = NIL;

END FBE.
