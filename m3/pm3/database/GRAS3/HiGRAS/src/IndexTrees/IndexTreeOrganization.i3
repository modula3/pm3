INTERFACE IndexTreeOrganization;

(***************************************************************************)
(* This module provides some basic functionality to deal with index
   trees. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:25:13  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.6  1996/11/20 12:22:10  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.5  1996/08/06 16:25:39  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.2  1996/07/24 09:18:50  rbnix
    	Error handling adjusted: internal errors are now guarded by
    	assertions rather than exceptions. This should simplify
    	locating errors.

    Revision 1.4.2.1  1996/04/29 13:38:13  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.4  1994/03/30  17:28:19  pk
# Adaptions for new Files subsystem.
#
# Revision 1.3  1993/11/03  21:01:49  pk
# Parameter type for splitKey changed from [0 .. 1] to [1 .. 2].
#
# Revision 1.2  1993/11/03  20:56:42  pk
# Parameter type for splitKey changed from CARDINAL to [0 .. 1].
#
# Revision 1.1  1993/08/17  12:52:53  pk
# Functional support module for organizing the system index trees.
#
*)
(***************************************************************************)

IMPORT AtomList;
IMPORT
  Access,
  ITCFile, IndexPage, DataPage;


CONST
  SystemPageNo = 0;

  DataPagePriority  = 1;
  IndexPagePriority = 2;


PROCEDURE SplitDataPage (    file                  : ITCFile.T;
                             dataPageNo            : CARDINAL;
                             indexTree             : CARDINAL;
                             splitKey              : [1 .. 2];
                         VAR newPage1No, newPage2No: CARDINAL;
                         VAR oldPage               : DataPage.T;
                         VAR newPage1, newPage2    : DataPage.T  )
  RAISES {Access.Locked, WrongPageReference, DepthExhausted, InternalError};
  (* Split the data page dataPageNo in index tree indexTree according to to
     key splitKey.  The new pages and their numbers are returned.  Note
     that the old page is removed from the file.  The pointer to the old
     page is valid only until a subsequent call to split or merge. *)


PROCEDURE MergeDataPage (    file                  : ITCFile.T;
                             oldPage1No, oldPage2No: CARDINAL;
                             indexTree             : CARDINAL;
                             splitKey              : [1 .. 2];
                         VAR newPageNo             : CARDINAL;
                         VAR oldPage1, oldPage2    : DataPage.T;
                         VAR newPage               : DataPage.T  )
  RAISES {Access.Locked, WrongPageReference, InternalError, PageBufferFault};
  (* Merge the data pages oldPage1No and oldPage2No in index tree
     indexTree.  The pages have to be brothers splitted by splitKey.  The
     new page and its number is returned.  Note that the old pages are
     removed from the file.  The pointers to the old pages are valid only
     until a subsequent call to split or merge. *)


PROCEDURE GetFatherAndBrotherEntry (    file      : ITCFile.T;
                                        indexTree : CARDINAL;
                                        key1, key2: CARDINAL;
                                        depth     : CARDINAL;
                                    VAR father    : IndexPage.Entry;
                                    VAR brother   : IndexPage.Entry  )
  RAISES {Access.Locked, DepthExhausted, InternalError};
  (* For a given key pair, return the father and brother entries for that
     page. *)

EXCEPTION
  WrongPageReference;
  DepthExhausted;
  PageBufferFault;
  InternalError(AtomList.T);
  
END IndexTreeOrganization.
