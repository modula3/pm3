MODULE NameService;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.6  1996/11/21 15:18:38  roland
    Atoms for NetObj.Error defined.

    Revision 1.5  1996/10/30 08:36:12  roland
    Special case for shutdown in ExportEntry.

    Revision 1.4  1996/10/08 13:08:32  roland
    Only one entry is allowed in a network. Error handling improved.
    Changes to EntryServer undone.

    Revision 1.3  1996/03/01 13:36:34  rbnix
    	Bug fixed: returning NIL entry port recognized.

    Revision 1.2  1996/02/21 13:48:07  rbnix
    	Parameter of function ExportEntry swapped to use default values.

    Revision 1.1  1996/02/09 16:43:48  rbnix
    	First version of name service for network objects added.

*)
(***************************************************************************)
IMPORT
  Uugid, Fmt,
  Atom, AtomList,
  Thread, NetObj,
  EntryPort;


(*
 | --- support stuff ------------------------------------------------------
 *)
PROCEDURE EntryName	() :TEXT =
  CONST		
    baseName		="GRAS-3";
  BEGIN
    RETURN (baseName & "." & Fmt.Int (Uugid.getuid ()));
  END EntryName;


PROCEDURE ServerLocation(         nameServer	:TEXT) : NetObj.Address 
			RAISES {Thread.Alerted, NetObj.Error, NetObj.Invalid} =
  BEGIN
    IF nameServer # NIL THEN
      RETURN NetObj.Locate (nameServer);
    ELSE
      RETURN NIL;
    END;
  END ServerLocation;

(*
 | --- public stuff -------------------------------------------------------
 *)
PROCEDURE ExportEntry	(         port		:EntryPort.T;
                                  nameServer	:TEXT
						:= NIL)
			RAISES {Thread.Alerted, NetObj.Error, NetObj.Invalid} =
  BEGIN
    IF port = NIL OR NetObj.Import (EntryName (),
                                     ServerLocation (nameServer)) = NIL THEN
      NetObj.Export (EntryName (), port, ServerLocation (nameServer))
    ELSE
      RAISE NetObj.Error(NEW(AtomList.T,
                             head := EntryExists,
                             tail := NIL));
    END;
  END ExportEntry;
  

PROCEDURE ImportEntry	(         nameServer	:TEXT
						:= NIL)
			:EntryPort.T
			RAISES {Thread.Alerted, NetObj.Error, NetObj.Invalid} =
  VAR
    entry		:EntryPort.T;
  BEGIN
    entry := NetObj.Import (EntryName (), ServerLocation (nameServer));
    IF entry = NIL THEN
      RAISE NetObj.Error (NEW(AtomList.T,
                             head := EntryNotFound,
                             tail := NIL));
    END;

    RETURN entry;
  END ImportEntry;

BEGIN
  EntryNotFound := Atom.FromText("EntryNotFound");
  EntryExists := Atom.FromText("EntryExists");
END NameService.
