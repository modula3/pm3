INTERFACE ITPFile;

(***************************************************************************)
(* ITPFiles are ITCFiles which operate on pages organized in index trees.
   Neither the page access methods from PoolFiles nor the cache methods
   from ITCFiles may be used on ITPFiles. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.3  1997/09/24 13:22:32  roland
    A method for preorder traversal of index tree added.

    Revision 1.2  1997/04/24 14:31:31  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:25:01  roland
    Subsystem IndexTrees adapted to handle indexfiles with less than 4
    index trees. This is needed to create external relation storages for
    graph boundary crossing edges.

    Revision 1.12  1996/11/20 12:22:04  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.11  1996/08/06 16:25:31  roland
    Merge of PAGESERVER and main branch.

    Revision 1.10.2.3  1996/07/25 09:17:39  rbnix
        Obsolete function checkIn removed.

    Revision 1.10.2.2  1996/07/24 09:18:44  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.10.2.1  1996/04/29 13:38:07  roland
    Changes for Page-Server. ITCFiles noe inherit from VirtualRemoteFile
    instead of PoolFile. ExceptionHandling improved.

# Revision 1.10  1994/03/30  17:28:16  pk
# Adaptions for new Files subsystem.
#
# Revision 1.9  1994/01/18  19:06:48  pk
# IllegalOperation is now a local and FATAL exception.
#
# Revision 1.8  1993/11/03  21:13:56  pk
# Parameter type for splitKey changed from CARDINAL to [1 .. 2].
#
# Revision 1.7  1993/11/03  20:53:25  pk
# New methods for splitPage and mergePage hide the old ones and provide
# the functionality of the key specific methods.
#
# Revision 1.6  1993/11/03  20:39:35  pk
# New checkIn and close methods hide the old ones, where truncate and
# maxPage values had to be supplied.
#
# Revision 1.5  1993/11/03  18:25:31  pk
# New naming convention Base and Super introduced.
#
# Revision 1.4  1993/10/26  19:48:20  pk
# Naming of methods and procedures updated.
#
# Revision 1.3  1993/09/30  15:16:18  pk
# Layout.
#
# Revision 1.2  1993/08/19  18:38:13  pk
# dataStart parameter eliminated: useless
#
# Revision 1.1  1993/08/17  12:51:43  pk
# Abstract data type for index tree page files.
#
*)
(***************************************************************************)

IMPORT ITCFile AS Super;
IMPORT Access, VirtualPage, PageFile, VirtualResource;
IMPORT AtomList, Pathname;

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      open (resource : VirtualResource.T;
            fileName : Pathname.T;
            mode     : Access.Mode;
            kind     : Access.Kind;
            new      : BOOLEAN;
            local    : BOOLEAN;
            noOfTrees: CARDINAL           ): T
            RAISES {Access.Denied, PageFile.NoAccess, TreeUnknown};
            (* Open a (new) VirtualFile and initialize it as an ITPFile
               with noOfTrees index trees.  Depending on 'local' a
               VirtualLocalFile or VirtualRemoteFile is used.  TreeUnknown
               is raised, if the number of trees in an old file is not
               equal to the number of required trees. *)

      close () RAISES {InternalError};

      noOfPagesInTree (indexTree: CARDINAL): CARDINAL
                       RAISES {TreeUnknown, Access.Locked, InternalError};
                       (* Return the number of pages in the given index
                          tree. *)


      findPage (    indexTree     : CARDINAL;
                    key1, key2    : CARDINAL;
                VAR depth1, depth2: CARDINAL  ): CARDINAL
                RAISES {TreeUnknown, Access.Locked, InternalError};
                (* Return the page number of the data page with relevant
                   keys key1 and key2.  The depth of the page according to
                   the keys is returned in depth1/2. *)


      findBrother (    pageNo  : CARDINAL;
                   VAR exists  : BOOLEAN;
                   VAR splitKey: [1 .. 2]  ): CARDINAL
                   RAISES {Access.Locked, InternalError};
                   (* Return the brother of page pageNo.  If no brother
                      page exists, exists will be set to FALSE.  The key
                      according to which the pages are brothers is returned
                      in splitKey. *)


      getPage (pageNo: CARDINAL; VAR depth1, depth2: CARDINAL):
               VirtualPage.T RAISES {Access.Locked, InternalError};
               (* Return the data page pageNo and its depths.  Note that
                  writing below dataStart will corrupt the index tree
                  organization. *)


      splitPage (    dataPageNo            : CARDINAL;
                     indexTree             : CARDINAL;
                     splitKey              : [1 .. 2];
                 VAR newPage1No, newPage2No: CARDINAL;
                 VAR oldPage               : VirtualPage.T;
                 VAR newPage1, newPage2    : VirtualPage.T  )
                 RAISES {Access.Locked, TreeUnknown, WrongPageReference,
                         DepthExhausted, InternalError};
                 (* Split the page dataPageNo in index tree indexTree using
                    splitKey.  The new page numbers are returned as well as
                    pointers to all pages.  Note that the old page is
                    removed from the file.  The pointer is valid only until
                    a subsequent call to split or merge methods. *)


      mergePage (    oldPage1No, oldPage2No: CARDINAL;
                     indexTree             : CARDINAL;
                     splitKey              : [1 .. 2];
                 VAR newPageNo             : CARDINAL;
                 VAR oldPage1, oldPage2    : VirtualPage.T;
                 VAR newPage               : VirtualPage.T  )
                 RAISES {Access.Locked, TreeUnknown, WrongPageReference,
                         InternalError};
                 (* Merge the pages oldPage1No and oldPage2No in index tree
                    indexTree.  The pages have to be brothers splitted by
                    splitKey.  The new page number is returned as well as
                    pointers to all pages.  Note that the old pages are
                    removed from the file.  The pointers are valid only
                    until a subsequent call to split or merge methods. *)

      preorderRun (tree: CARDINAL; monitor: TraversalMonitor := NIL)
                   RAISES {Access.Locked, TreeUnknown, InternalError};
                   (* Traverse the specified tree in preorder.  For every
                      node, monitor is called.  If monitor is NIL, the
                      nodes are printed on stdout. *)
    END;

  TraversalMonitor = PROCEDURE (tree            : CARDINAL;
                            depth1, depth2  : CARDINAL;
                            relKey1, relKey2: CARDINAL;
                            node            : BOOLEAN;
                            keyOrPage       : CARDINAL  );

EXCEPTION
  InternalError(AtomList.T);
  WrongPageReference;
  DepthExhausted;
  TreeUnknown;

END ITPFile.
