INTERFACE NullMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:04:39  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- NullMedia ---------------------------------------------------------
 * This specialized media is like /dev/null. It represents a media usable
 * by the cache but without visible functionality otherwhere, e.g. no
 * data is transfered to or from the media's storage.
 * ------------------------------------------------------------------------
 *)

IMPORT PageMedia AS Super;

TYPE
  T                     <: Public;

  Public		= Super.T OBJECT
    END;

END NullMedia.


