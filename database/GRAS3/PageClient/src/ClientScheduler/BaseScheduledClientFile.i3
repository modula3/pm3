INTERFACE BaseScheduledClientFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.6  1998/01/21 14:11:04  roland
    Method baseName now in public interface.
    Files can now be opened as read-only in read-write-exclusive resources.

    Revision 1.5  1996/11/18 17:51:32  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.4  1996/10/29 14:59:38  rbnix
    	New method getTransactionNumber added.

    Revision 1.3  1996/03/11 17:16:05  rbnix
    	Method close moved from public to internal interface.

    	Method getBaseName added.

    Revision 1.2  1996/02/29 09:34:51  rbnix
    	Release of resources added.

    Revision 1.1  1996/02/09 16:46:30  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)


(*
 | --- BaseScheduledClientFile --------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Transaction, Access;
IMPORT
  AtomList, Pathname;

TYPE
  T			<: Public;

  Public		= OBJECT
    METHODS
      getBaseName	() :Pathname.T;
      getTransactionLevel() :Transaction.Level;
      getTransactionNumber () :CARDINAL;
      getAccessMode	() :Access.Mode;
    END;
  
EXCEPTION
  FatalError(AtomList.T);
  
END BaseScheduledClientFile.
