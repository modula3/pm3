INTERFACE NameService;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.3  1996/11/21 15:18:36  roland
    Atoms for NetObj.Error defined.

    Revision 1.2  1996/02/21 13:48:04  rbnix
    	Parameter of function ExportEntry swapped to use default values.

    Revision 1.1  1996/02/09 16:43:46  rbnix
    	First version of name service for network objects added.

*)
(***************************************************************************)

(*
 | --- NameService --------------------------------------------------------
 This functional module implements some name services to export and import
 communication ports.
 | ------------------------------------------------------------------------
 *)
IMPORT
  Thread, NetObj, Atom,
  EntryPort;


PROCEDURE ExportEntry	(         port		:EntryPort.T;
                                  nameServer	:TEXT
						:= NIL)
			RAISES {Thread.Alerted, NetObj.Error, NetObj.Invalid};

PROCEDURE ImportEntry	(         nameServer	:TEXT
						:= NIL) :EntryPort.T
			RAISES {Thread.Alerted, NetObj.Error, NetObj.Invalid};


  (* NameService.ImportEntry raises NetObj.Error with EntryNotFound
     if it can't find the EntryServer. If ExportEntry is called with
     a non NIL port, it tests if an entry exists. If it exists, it raises
     NetObj.Error(EntryExists). If not, it installs the entry port. If
     ExportEntry is called with port = NIL it deletes the EntryServer from
     the NameService. *)
VAR
  EntryNotFound: Atom.T;
  (* CONST EntryNotFound = "EntryNotFound"; *)
  EntryExists  : Atom.T;
  (* CONST EntryExist    = "EntryExists"; *)

END NameService.
