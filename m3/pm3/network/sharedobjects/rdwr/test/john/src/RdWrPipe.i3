(*
 * RdWrPipe.i3 -- a reader/writer pair that a directly connected together.
 * Copyright (C) Blair MacIntyre 1995
 * Author          : Blair MacIntyre
 * Created On      : Mon Feb 20 17:43:14 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Feb 25 13:23:31 1995
 * Update Count    : 6
 * Status          : Unknown, Use with caution!
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:47  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 01:20:26  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.1.1.1  2001/12/02 00:29:10  wagner
 * Blair MacIntyre's rdwr library
 *
 * Revision 1.1.1.1  1996/03/03 19:05:03  bm
 * Imported Sources
 *
 * 
 * HISTORY
 *)

(* The FileRdWr module sets up and returns a Rd/Wr pair which are
   linked together. Anything written to the writer is immediately
   available to the reader.  *)

INTERFACE RdWrPipe;

IMPORT Rd, Wr;

CONST
  (* the default size of the shared buffer *)
  BufferSize = 1024;

PROCEDURE New(VAR rd: Rd.T; VAR wr: Wr.T; buff_size: CARDINAL :=
  BufferSize; nm : TEXT := NIL);
(* Returns a read and writer which are connected together. *)

PROCEDURE ResetRdCounter(rd: Rd.T);
(* Reset the cur, lo and hi pointers, to allow this to read more
   characters than LAST(CARDINAL).  Should be called periodically. *)

PROCEDURE ResetWrCounter(wr: Wr.T);
(* Reset the cur, lo and hi pointers, to allow this to write more
   characters than LAST(CARDINAL).  Should be called periodically. *)

END RdWrPipe.
