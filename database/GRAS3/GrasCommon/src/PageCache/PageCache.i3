INTERFACE PageCache;

(*
 | ------------------------------------------------------------------------
 | Created by:  Reiner Nix
 |
 |  $Author$
 |  $Revision$
 |  $Date$
 |  $Log$
 |  Revision 1.1  2003/03/27 15:25:27  hosking
 |  Initial revision
 |
 |  Revision 1.4  1996/02/29 17:41:23  rbnix
 |  	New function GetPage added.
 |
 |  Revision 1.3  1996/02/23 15:01:51  rbnix
 |  	New function FlushPages added to force droping changed pages
 |  	related to a media.
 |
 |  Revision 1.2  1996/02/09 16:37:03  rbnix
 |  	Functions WaitAccess and RemovePage added.
 |
 |  Revision 1.1  1996/01/31 10:04:48  rbnix
 |  	Initial version for subsystem PageCache.
 |
 | ------------------------------------------------------------------------
 *)

(*
 | --- PageCache ----------------------------------------------------------
  This abstract data object represents a virtually unlimited collection
  of pages accessable by handles.
 
  Pages are initially put into the cache by InsertPage. They may be
  relocated accessing the page handle without any restriction. To reload
  you have to use manually ReInsertPage before accessing the page again
  otherwise the page will be swapped in transparently using the given
  page media.
 
  To be safe for multiple threads the cache access must be protected
  by BeginAcces and EndAccess as specified below. This pattern encloses
  multiple cache operation into one atomic operation.
 | ------------------------------------------------------------------------
 *)

IMPORT
  Thread,
  PageData,
  PageHandle, PageMedia;


<* PRAGMA SPEC *>

<* 
  SPEC
  PRIVATE VAR cacheUser :MUTEX
  INITIALLY cacheUser = none
*>


PROCEDURE Init		(        pages		:CARDINAL);
  <* 
    SPEC
    REQUIRES cacheUser = CURRENT
  *>
  

PROCEDURE BeginAccess	();
  <* 
    SPEC
    MODIFIES cacheUser
    WHEN cacheUser = none
    ENSURES cacheUser' = CURRENT
  *>  

PROCEDURE WaitAccess	(         condition	:Thread.Condition)
			RAISES {Thread.Alerted};
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
    ENSURES cacheUser' = CURRENT
    COMPOSITION OF EndAccess; BeginAccess END;
  *>

PROCEDURE EndAccess	();
  <* 
    SPEC
    REQUIRES cacheUser = CURRENT
    MODIFIES cacheUser
    ENSURES cacheUser' = none
  *>


PROCEDURE GetPage	(         pageNo	:CARDINAL;
                                  media		:PageMedia.T) :PageHandle.T;
  <* 
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

PROCEDURE InsertPage    (         pageNo        :CARDINAL;
                                  media		:PageMedia.T;
                         READONLY data		:PageData.T) :PageHandle.T;
  <* 
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

PROCEDURE ReInsertPage	(         handle	:PageHandle.T;
                         READONLY data		:PageData.T);
  <* 
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

PROCEDURE CopyPage	(        handle		:PageHandle.T;
                                 newPageNo	:CARDINAL;
                                 newMedia	:PageMedia.T) :PageHandle.T;
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

PROCEDURE RemovePage	(        handle		:PageHandle.T);
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>
  
PROCEDURE FlushPages	(        media		:PageMedia.T);
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>


PROCEDURE Dump		();
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

END PageCache.

