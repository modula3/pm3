INTERFACE InternalITCFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:25:18  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.2  1996/08/06 16:25:42  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/25 08:53:39  rbnix
    	Methods insertCacheEntry, removeCacheEntry, findCacheEntry,
    	changeCacheEntry moved from ITCFile into new interface
    	InternalITCFile.

    	BEWARE: usage of this methods is *** CURRENTLY NOT *** correct
    	in multiuser mode because of ignorance of changes in database
    	from other clients.

*)
(***************************************************************************)
IMPORT
  ITCFile;

REVEAL
  ITCFile.T			<: Internal;

TYPE
  Internal			= ITCFile.Public OBJECT
    METHODS
      insertCacheEntry		(         indexTree	:CARDINAL;
                                          key1, key2    :CARDINAL;
                                          location      :CARDINAL;
                                          depth1, depth2:CARDINAL);
      (*
        Insert an entry into the cache.
      *)


      removeCacheEntry		(         indexTree	:CARDINAL;
                                          pageNo	:CARDINAL);
      (*
        Remove any references to page pageNo in index tree indexTree from
        the cache.
      *) 


      findCacheEntry		(         indexTree     :CARDINAL;
                                          key1, key2    :CARDINAL;
                                 VAR      location      :CARDINAL;
                                 VAR      depth1, depth2:CARDINAL)
				:BOOLEAN;
      (*
        Retrieve an entry from the cache.  The method returns TRUE if an
        entry was found, and FALSE otherwise.  Note that only the relevant
        parts of the keys (regarding the depths given on InsertEntry) are
        compared.
      *)


      changeCacheEntry		(         indexTree	:CARDINAL;
                                          oldPageNo,
                                          newPageNo	:CARDINAL);
      (*
        Change any hash entries regarding page oldPageNo in index tree
        indexTree to newPageNo.
      *)
    END;


END InternalITCFile.
