INTERFACE LocalResourceSystem;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1998/02/10 16:36:28  roland
    LocalResourceSystem offers procedures to manage the local part of
    resources.

*)
(***************************************************************************)

IMPORT Pathname;
IMPORT PageFile;

PROCEDURE DeleteLocalResource (baseName: Pathname.T)
  RAISES {PageFile.NoAccess};

PROCEDURE CopyLocalResource (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess};

PROCEDURE RenameLocalResource (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess};

PROCEDURE ExistsLocalResource (baseName: Pathname.T): BOOLEAN
  RAISES {PageFile.NoAccess};


END LocalResourceSystem.
