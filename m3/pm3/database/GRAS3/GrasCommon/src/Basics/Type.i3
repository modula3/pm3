INTERFACE Type;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

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

IMPORT Word AS Unsigned;


CONST
  MaxByte	= 255;					(* max byte value	*)
  MaxShort	= 65535;				(* max two byte value	*)
  MaxTriple	= 16777215;				(* max three byte value *)
  MaxWord	= LAST(Unsigned.T);			(* max four byte value	*)


TYPE
  Byte		= BITS  8 FOR [0 .. MaxByte];		(* a byte		*)
  Short		= BITS 16 FOR [0 .. MaxShort];		(* two bytes		*)
  Triple	= BITS 24 FOR [0 .. MaxTriple];		(* three byte subrange	*)
  Word		= BITS 32 FOR Unsigned.T;		(* four bytes		*)

  ByteArray	= ARRAY OF Byte;

END Type.
