INTERFACE PageFileSystem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.3  1997/03/26 11:19:25  roland
    New procedure MakePath creates a directory and additionally all
    necessary ancestors.

    Revision 1.2  1996/08/06 16:26:09  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/25 11:49:21  rbnix
    	First version of module PageFileSystem added.

*)
(***************************************************************************)
(*
 | --- PageFileSystem -----------------------------------------------------
 This abstract data type module provides some basic functionality on files.
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname, TextSeq,
  PageFile;


(* file support *)
PROCEDURE DeleteFile		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess};

PROCEDURE CopyFile		(         sourceName,
                                          destName	:Pathname.T)
				RAISES {PageFile.NoAccess};

PROCEDURE RenameFile		(         oldName,
                                          newName	:Pathname.T)
				RAISES {PageFile.NoAccess};

PROCEDURE ExistsFile		(         name		:Pathname.T)
				:BOOLEAN;

PROCEDURE FileSize		(         name		:Pathname.T)
				:CARDINAL
				RAISES {PageFile.NoAccess};
  (*
    Returns the file size as number of data pages.
  *)


(* directory support *)
PROCEDURE MakeDir		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess};
  (* Try to create a directory 'name'. Parents have to exist *)

PROCEDURE MakePath		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess};
  (* Try to create a directory 'name'. If parents of name do not exists,
     try to create them. *)

PROCEDURE RemoveDir		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess};

PROCEDURE ExistsDir		(         name		:Pathname.T)
				:BOOLEAN;


PROCEDURE GetFileNames		(         dirName	:Pathname.T)
				:TextSeq.T 
				RAISES {PageFile.NoAccess};

PROCEDURE GetDirNames		(         dirName	:Pathname.T)
				:TextSeq.T 
				RAISES {PageFile.NoAccess};


END PageFileSystem.
