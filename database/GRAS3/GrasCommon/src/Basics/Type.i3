INTERFACE Type;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.3  1996/02/09 16:06:58  rbnix
    	Types are now packed to be more reliable.

# Revision 1.2  1993/10/08  13:59:19  pk
# Triple inserted.
#
# Revision 1.1  1993/09/29  23:55:47  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Int32;

CONST
  MaxByte	= 16_FF;			(* max byte value	*)
  MaxShort	= 16_FFFF;			(* max two byte value	*)
  MaxTriple	= 16_FFFFFF;			(* max three byte value *)
  MaxWord	= 16_FFFFFFFF;			(* max four byte value	*)


TYPE
  Byte		= BITS  8 FOR [0 .. MaxByte];	(* a byte		*)
  Short		= BITS 16 FOR [0 .. MaxShort];	(* two bytes		*)
  Triple	= BITS 24 FOR [0 .. MaxTriple];	(* three byte subrange	*)
  Word		= Int32.T;			(* four bytes		*)

  ByteArray	= ARRAY OF Byte;

END Type.
