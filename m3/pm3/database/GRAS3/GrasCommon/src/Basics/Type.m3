MODULE Type;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.3  1996/08/06 16:22:50  roland
    Merge of PAGESERVER and main branch.

# Revision 1.2  1993/10/08  13:59:49  pk
# Error corrected.
#
# Revision 1.1  1993/09/29  23:55:48  pk
# Initial revision
#
*)
(***************************************************************************)

BEGIN
  <* ASSERT ((BITSIZE(Byte) = 8) AND (BITSIZE(Short) = 16)
  AND (BITSIZE(Word) = 32) AND (BYTESIZE(Byte) = 1) AND
  (BYTESIZE(Short) = 2) AND (BYTESIZE(Word) = 4)) *>
END Type.
