INTERFACE Access;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.4  1996/11/14 14:12:07  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.3  1996/03/01 12:56:39  rbnix
    	Format function(s) added.

    Revision 1.2  1996/02/21 13:44:54  rbnix
    	Desciptive parameter TEXT added to exception Access.Invalid.

    Revision 1.1  1996/02/09 16:03:01  rbnix
    	First version of common used scheduler parts.

*)
(***************************************************************************)

(*
 | --- Access -------------------------------------------------------------
  This type collection module contains open modes for ressources.
 | ------------------------------------------------------------------------
 *)

EXCEPTION
  Locked;					(* currently not available *)
  Invalid (TEXT);				(* wrong use		   *)
  Denied (TEXT);                                (* conlicting access modes *)


CONST
  WriteModes            = ModeSet {Mode.ReadWriteShared,
                                   Mode.ReadWriteExclusive};

TYPE
  Mode			= {ReadOnlyShared,
                           ReadWriteShared,
                           ReadWriteExclusive};
  ModeSet		= SET OF Mode;

  Kind			= {Data,
                           Log};


PROCEDURE FmtMode	(         mode		:Mode) :TEXT;

PROCEDURE FmtKind	(         kind		:Kind) :TEXT;
  

END Access.
