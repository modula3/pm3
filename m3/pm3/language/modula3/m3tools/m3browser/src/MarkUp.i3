(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec  8 09:53:21 PST 1994 by kalsow                   *)

INTERFACE MarkUp;

IMPORT Buf, Wx;

PROCEDURE Annotate (buf: Buf.T;  wx: Wx.T);
(* copy the Modula-3 source in "buf" to "wx" adding HTML annotations. *)

END MarkUp.
