INTERFACE BaseServerScheduler;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.5  1997/06/13 11:44:22  rbnix
    	Parameter omitLastComponent for functions CreateResourcePath
    	and DestroyResourcePath added. Resource paths are unified and
    	build with Config.GetRootPrefix.

    Revision 1.4  1997/06/10 12:54:51  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.3  1996/08/22 14:16:34  rbnix
    	Support for complex resource names added. Resource names are
    	structured like file paths and are directly mapped to the disk
    	file system. Neccessary path components are supervised and
    	constructed transparently to the user.

    Revision 1.2  1996/08/06 16:32:40  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/08/01 17:40:16  rbnix
    	New functions MakeResourcePath and MakeResourceName added to
    	standardize the construction of the path.

    Revision 1.1.2.1  1996/07/11 11:06:09  rbnix
    	Parameters of procedure WaitAccess enhanced by lock table.

    Revision 1.1  1996/02/26 17:59:35  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- BaseServerScheduler ------------------------------------------------
 This abstract object module coodinates waiting clients until their
 request possibly can be served or has to be aborted due to determined
 deadlock. 

 The function WaitAccess temporarily suspends atomic execution of the
 current thread preserved via page cache!
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  PageFile,
  Access, PageLock,
  ServedClient,
  ServerLockTable;


PROCEDURE WaitAccess	(         client	:ServedClient.T;
                                  locks		:ServerLockTable.T;
                                  file          :Pathname.T;
                                  pageNo        :CARDINAL;
                                  lock          :PageLock.ServerMode)
			RAISES {Access.Locked};

PROCEDURE SignalAccess	();


PROCEDURE MakeResourcePath
			(         resourceName	:Pathname.T;
                                  temporary     :BOOLEAN)
			:Pathname.Arcs
			RAISES {PageFile.NoAccess};

PROCEDURE MakeResourceName
			(         resourceName	:Pathname.T;
                                  temporary     :BOOLEAN)
			:Pathname.T
			RAISES {PageFile.NoAccess};
  (*
    MakeResourcePath/MakeResourceName return a pathname in components
    (arcs) or at whole consisting of a (prefix) for temporary or persistent
    root part and the (appended) specified resource name.
  *)

  
PROCEDURE CreateResourcePath
			(         resourceName		:Pathname.T;
                                  temporary		:BOOLEAN;
                                  omitLastComponent	:BOOLEAN
							:= FALSE)
			RAISES {PageFile.NoAccess};

PROCEDURE DestroyResourcePath
			(         resourceName		:Pathname.T;
                                  temporary		:BOOLEAN;
                                  omitLastComponent	:BOOLEAN
							:= FALSE)
			RAISES {PageFile.NoAccess};
  (*
    Create/DestroyResourcePath makes and removes the path for an
    resource. The path contains all directories from the baseName
    relative to the root path maybe omitting the last path component of 
    baseName (usefull when handling the resource at whole).
  *)

END BaseServerScheduler.
