INTERFACE InternalBaseScheduledServerResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.6  1997/06/13 11:46:26  rbnix
    	Adapted to unified resource paths of module
    	BaseServerScheduler.

    Revision 1.5  1997/06/10 12:54:58  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.4  1996/08/06 16:32:46  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.3  1996/07/30 09:40:13  rbnix
    	New method makeFileName added.

    Revision 1.3.2.2  1996/07/23 14:19:24  rbnix
    	Bug fixed: handling of access mode adjusted. The access mode
    	is now determined by the actual first active client rather
    	than the very first client.

    Revision 1.3.2.1  1996/07/11 11:06:13  rbnix
    	Parameters of procedure WaitAccess enhanced by lock table.

    Revision 1.3  1996/03/01 13:02:48  rbnix
    	Method getBaseName moved from private to public parts of
    	subsystem interfaces.

    Revision 1.2  1996/02/28 11:03:17  rbnix
    	File and resource pathes are now related to a root path
    	via Config. Errors related to creation of are submittet to
    	clients via exception PageFile.NoAccess.

    Revision 1.1  1996/02/26 17:59:40  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- InternalBaseScheduledServerResource --------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  PageFile,
  Access, PageLock,
  ServedClient,
  BaseScheduledServerResource, ServerLockTable;

REVEAL
  BaseScheduledServerResource.T <: Internal;

TYPE
  Internal			= BaseScheduledServerResource.Public OBJECT
    METHODS
      init		(         baseName	:Pathname.T;
                                  new		:BOOLEAN)
			:BaseScheduledServerResource.T
			RAISES {PageFile.NoAccess};
      cleanUp           ();

      setAccessMode	(         access	:Access.Mode);
      getAccessMode	() :Access.Mode;

      getPath		() :Pathname.Arcs;
      makeFileName	(         baseName	:Pathname.T;
                                  temporary	:BOOLEAN)
			:Pathname.T
			RAISES {PageFile.NoAccess};

      waitAccess	(         client	:ServedClient.T;
                                  locks		:ServerLockTable.T;
                                  file          :Pathname.T;
                                  pageNo        :CARDINAL;
                                  lock          :PageLock.ServerMode)
			RAISES {Access.Locked};
    END;

END InternalBaseScheduledServerResource.
