INTERFACE ITCFile;

(***************************************************************************)
(* ITCFiles are PoolFiles with an additional cache for index tree accesses
   attached. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:45  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:28  hosking
    Import of GRAS3 1.1

    Revision 1.3  1998/01/21 12:34:52  roland
    New method baseName to determine filename.

    Revision 1.2  1997/04/24 14:31:26  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:24:57  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.6  1996/11/20 12:22:01  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.5  1996/08/06 16:25:28  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.2  1996/07/25 08:53:30  rbnix
    	Methods insertCacheEntry, removeCacheEntry, findCacheEntry,
    	changeCacheEntry moved from ITCFile into new interface
    	InternalITCFile.

    	BEWARE: usage of this methods is *** CURRENTLY NOT *** correct
    	in multiuser mode because of ignorance of changes in database
    	from other clients.

    Revision 1.4.2.1  1996/04/29 13:38:03  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.4  1994/03/30  17:28:12  pk
# Adaptions for new Files subsystem.
#
# Revision 1.3  1993/11/03  18:25:26  pk
# New naming convention Base and Super introduced.
#
# Revision 1.2  1993/10/26  19:48:17  pk
# Naming of methods and procedures updated.
#
# Revision 1.1  1993/08/17  12:51:07  pk
# Abstract data type for index tree cache files.
#
*)
(***************************************************************************)

IMPORT VirtualPage, PageFile, VirtualResource;
IMPORT DataPage, Access;
IMPORT AtomList, Pathname;

TYPE
  T				<: Public;

  Public			= <*TRANSIENT*> ROOT OBJECT
    METHODS
      open (resource : VirtualResource.T;
            fileName : Pathname.T;
            mode     : Access.Mode;
            kind     : Access.Kind;
            new      : BOOLEAN;
            local    : BOOLEAN           ): T
            RAISES {Access.Denied, PageFile.NoAccess};
            (* Open a (new) VirtualFile and initialize it as an ITCFile.
               Depending on 'local' a
               VirtualLocalFile or VirtualRemoteFile is used. *)

      close		() RAISES {InternalError};

      baseName(): Pathname.T;
      
      getPage		(         pageNo	:CARDINAL)
			:VirtualPage.T;

      copyPage                  (         oldPageNo,
                                          newPageNo	:CARDINAL)
				RAISES {Access.Locked, InternalError};
      (*
        copy the contents of the original (oldPageNo) to the page page with
        number newPageNo
      *)
      
      splitPage			(         oldpno,
                                          newpno1,
                                          newpno2	:CARDINAL;
                                 VAR      oldPage	:DataPage.T;
                                 VAR      newPage1,
                                          newPage2      :DataPage.T)
	                        RAISES {Access.Locked, InternalError};
      
      mergePage			(         newpno,
                                          oldpno1,
                                          oldpno2	:CARDINAL;
                                 VAR      newPage	:DataPage.T;
                                 VAR      oldPage1,
                                          oldPage2      :DataPage.T)
				RAISES {Access.Locked, InternalError};
    END;

EXCEPTION
  InternalError(AtomList.T);
  
END ITCFile.
