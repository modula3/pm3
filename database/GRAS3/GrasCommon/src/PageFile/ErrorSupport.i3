INTERFACE ErrorSupport;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.3  1996/11/20 12:19:52  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:26:04  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/06/13 12:42:51  rbnix
    	Error handling improved using new module ErrorSupport.

*)
(***************************************************************************)

(*
 | --- ErrorSupport -------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

IMPORT OSError;
IMPORT AtomList;

PROCEDURE Fmt		(         code		:OSError.Code) :TEXT;

PROCEDURE Create(proc, exception: TEXT): AtomList.T;
  (* Creates a new AtomList.T with two memebers proc and exception *)
PROCEDURE Propagate(proc, exception: TEXT; info: AtomList.T): AtomList.T;
  (* Prepends info with proc and exception *)
PROCEDURE ToText (info: AtomList.T): TEXT;
  
END ErrorSupport.
