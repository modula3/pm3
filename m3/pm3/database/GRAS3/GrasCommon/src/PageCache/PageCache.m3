MODULE PageCache EXPORTS PageCache, InternalPageCache;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.9  1998/09/14 08:14:25  roland
    Modified code to remove compiler warnings.

    Revision 1.8  1997/01/20 08:56:36  roland
    Performance: AtomicAccess expanded in all assertions.

    Revision 1.7  1996/08/06 16:25:57  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.1  1996/07/23 13:55:18  rbnix
    	Check for initialize added: only one initialization allowed.

    Revision 1.6  1996/03/08 10:37:18  rbnix
    	Local variables for page data are replaced by use of function
    	getAll.

    	Procedure DumpPage is adjusted to use PageHandle.T.fmt.

    	Functions GetFreePage and GetPageCopy are enhanced to work
    	well in case of removing the page to be replaced while calling
    	dropData.

    Revision 1.5  1996/02/29 17:53:45  rbnix
    	Bug fixed: missing RETURN added.

    Revision 1.4  1996/02/29 17:41:24  rbnix
    	New function GetPage added.

    Revision 1.3  1996/02/23 15:01:54  rbnix
    	New function FlushPages added to force droping changed pages
    	related to a media.

    Revision 1.2  1996/02/09 16:37:04  rbnix
    	Functions WaitAccess and RemovePage added.

    Revision 1.1  1996/01/31 10:04:49  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 | --- PageCache ----------------------------------------------------------
  The PageCache is based on the 2Q page replacement strategy. Therefore
  page handles are held in two queues A1 and Am. A1 is partitioned into
  A1in and A1out. In whole it is used as a FIFO queue containing page
  handles for first time in cache. Am is used as LRU queue containing
  longer used page handles. A1in and Am contains handles with loaded pages,
  A1out contains only handles without page but accessed first in recent
  time therefore beeing candidate to hold longer in the cache when accessed
  again and then moved into Am. Handles leaving Am are outdated and are
  treated like regular as unknown handles like new handles when they should
  be reinserted again.

  To minimize overhead by reducing special cases during normal cache use
  there is no extra notion of empty frames. Empty frames are represented by
  page handles set up to the NullMedia. The queues are filled upto their
  fixed size at initialization time.
 
  Access control is done by a mutex. All functions are using assertions
  to ensure atomic operations as specified.

  References:
   Theodore Johnson, Dennis Shasha,
   2Q: A Low Overhead High Performance Buffer Management Replacement Algorithm, 
   Proceedings of the 20th VLDB, '94
 | ------------------------------------------------------------------------
 *)

IMPORT
  Thread,
  ObjectList,
  PageData, Page,
  PageHandle, InternalPageHandle, BasePageHandle, InternalBasePageHandle,
  PageMedia, NullMedia;

(* For dumping the cache contents *)
FROM Stdio IMPORT stdout;
IMPORT
  Wr, Fmt;
  
<* PRAGMA SPEC *>

(*
 * --- Access control -----------------------------------------------------
 *)
TYPE
  CacheMutex		= MUTEX OBJECT
      user		:Thread.T
			:= NIL
    END;

VAR
  cacheMutex		:=NEW (CacheMutex);
  

PROCEDURE BeginAccess	() =
  BEGIN
    Thread.Acquire (cacheMutex);

    <* ASSERT (cacheMutex.user = NIL) *>
    cacheMutex.user := Thread.Self ();
  END BeginAccess;

  
PROCEDURE WaitAccess	(         condition	:Thread.Condition) 
			RAISES {Thread.Alerted} =
  BEGIN
    TRY
      <* ASSERT (cacheMutex.user = Thread.Self ()) *>
      cacheMutex.user := NIL;

      Thread.AlertWait (cacheMutex, condition);
    FINALLY
      <* ASSERT (cacheMutex.user = NIL) *>
      cacheMutex.user := Thread.Self ()
    END
  END WaitAccess;


PROCEDURE EndAccess	() =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>
    cacheMutex.user := NIL;

    Thread.Release (cacheMutex);
  END EndAccess;


<* UNUSED *>
PROCEDURE AtomicAccess	() :BOOLEAN =
<*
  SPEC
  ENSURES cacheUser = CURRENT
*>
  BEGIN
    (* For efficency, the body of this procedure is expanded in all
       assertions of this module. *)
    RETURN (cacheMutex.user = Thread.Self ());
  END AtomicAccess; 

  
(*
 * --- Page replacement strategy ------------------------------------------
 *)
VAR
  A1in, A1out, Am	:= NEW (ObjectList.T).init ();
  nullMedia		:= NEW (NullMedia.T);
  isInitialized		:BOOLEAN := FALSE;


(*
 | --- Init ---------------------------------------------------------------
 For dumping purpose the initials handles are numbered.
 | ------------------------------------------------------------------------
 *)
PROCEDURE Init		(        cacheSize	:CARDINAL) =
  VAR
    handle		:PageHandle.T;
    A1inSize,
    A1outSize,
    AmSize		:CARDINAL;

  PROCEDURE AddFrame	(        queue		:ObjectList.T;
                                 pageNo		:CARDINAL;
                                 withPage	:BOOLEAN) =
    BEGIN
      handle := NEW (PageHandle.T).init ();
      IF withPage THEN
        handle.setPage (NEW (Page.T));
      ELSE
        handle.setPage (NIL);
      END;
      handle.unmarkChanges ();
      handle.setMedia (nullMedia);
      handle.setPageNo (pageNo);
      queue.addHead (handle);
    END AddFrame;

  (* Init *)
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    <* ASSERT (NOT isInitialized) *>
    isInitialized := TRUE;

    (* check and calculate size *)
    cacheSize := MAX (8, cacheSize);
    A1inSize  := cacheSize DIV 4;		(* 25% of cacheSize	*)
    A1outSize := cacheSize DIV 2;		(* 50% of cacheSize	*)
    AmSize    := cacheSize DIV 4 * 3;		(* 75% of cacheSize	*)

    (* fill A1in *)
    FOR i := 1 TO A1inSize DO
      AddFrame (A1in, 1000+i, withPage := TRUE);
    END;

    (* fill A1out *)
    FOR i := 1 TO A1outSize DO
      AddFrame (A1out, 2000+i, withPage := FALSE);
    END;
    
    (* fill Am *)
    FOR i := 1 TO AmSize DO
      AddFrame (Am, 3000+i, withPage := TRUE);
    END;
  END Init;


(*
 | --- GetFreePage --------------------------------------------------------
 GetFreePage determines in respect to the given handle a page that will
 be assigned to this handle. This forces dropping the old data stored
 on the page.
 | ------------------------------------------------------------------------
 *)
PROCEDURE GetFreePage	(        newHandle	:PageHandle.T) =
  VAR
    oldHandle		:PageHandle.T;
  BEGIN
    <* ASSERT NOT newHandle.isLoad () *>

    IF newHandle.getList () = NIL THEN
      (* drop data from old handle *)
      oldHandle := A1in.getTail ();
      oldHandle.dropData ();

      (* oldHandle may be get obsolete by RemovePage while dropData *)
      IF oldHandle.isLoad () THEN
        (* move old handle to A1out as FIFO list *)
        A1in.remove (oldHandle);
        A1out.addHead (oldHandle);
        EVAL A1out.removeTail ();
      ELSE
        (* oldHandle is replaced by a nullHandle, this it is now obsolete *)
        oldHandle := A1in.removeTail ();
        <* ASSERT (oldHandle.getMedia () = nullMedia) *>
      END;

      (* set up new handle *)
      newHandle.setPage (oldHandle.getPage ());
      A1in.addHead (newHandle);

      (* reset old handle *)
      oldHandle.setPage (NIL);

    ELSE
      <* ASSERT (newHandle.getList () = A1out) *>
      (* drop data from old handle *)
      oldHandle := Am.getTail ();
      oldHandle.dropData ();

      (* oldHandle may be get obsolete by RemovePage while dropData *)
      IF oldHandle.isLoad () THEN
        Am.remove (oldHandle);
        (* create new dummy handle to refill A1out *)
        WITH nullHandle = NEW (PageHandle.T).init () DO
          nullHandle.setPage (NIL);
          nullHandle.unmarkChanges ();
          nullHandle.setMedia (nullMedia);
          nullHandle.setPageNo (2000);
          A1out.addTail (nullHandle);
        END;
      ELSE
        (* oldHandle is replaced by a nullHandle, this can be reused now *)
        oldHandle := Am.removeTail ();
        <* ASSERT (oldHandle.getMedia () = nullMedia) *>
        A1out.addTail (oldHandle);
      END;

      (* set up new handle *)
      newHandle.setPage (oldHandle.getPage ());
      A1out.remove (newHandle);
      Am.addHead (newHandle);

      (* reset old handle *)
      oldHandle.setPage (NIL);
    END
  END GetFreePage;


(*
 | --- RemovePage ---------------------------------------------------------
 
 | ------------------------------------------------------------------------
 *)
PROCEDURE RemovePage	(        handle		:PageHandle.T) =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>
    WITH handleList = NARROW (handle.getList (), ObjectList.T) DO
      IF handleList # NIL THEN
        WITH nullHandle = NEW (PageHandle.T).init () DO
          (* create dummy to fill list *)
          nullHandle.setPage (handle.getPage ());
          nullHandle.unmarkChanges ();
          nullHandle.setMedia (nullMedia);
          nullHandle.setPageNo (2000);
          handleList.addTail (nullHandle);

          (* free old handle *)
          handle.setPage (NIL);
          handleList.remove (handle);
        END;
      ELSE
        <* ASSERT (handle.getPage () = NIL) *>
      END
    END
  END RemovePage;


(*
 | --- GetPageCopy --------------------------------------------------------
  Because data may dropped transparently, e.g. at any time, this procedure
  tries to minimize media access.
 
  If the page to be copied is changed a copy in memory at an other page
  will be created. Otherwise on the unchanged page the old handle will
  loose the page which will be used at the new handle.
 |------------------------------------------------------------------------
 *)
PROCEDURE GetPageCopy	(        oldHandle,
                                 newHandle	:PageHandle.T) =
  VAR
    page		:Page.T;
  BEGIN
    IF NOT (oldHandle.isLoad ()) THEN
      GetFreePage (newHandle);
      newHandle.setMedia (oldHandle.getMedia ());
      newHandle.setPageNo (oldHandle.getPageNo ());
      newHandle.loadData ();
      newHandle.markChanged ();
      
    ELSE
      IF oldHandle.isChanged () THEN
        page := oldHandle.getPage ();
        GetFreePage (newHandle);
        BasePageHandle.T.putData (newHandle, page.data);

      ELSE
        WITH oldList = NARROW (oldHandle.getList (), ObjectList.T) DO
          (* assign page to new handle *)
          newHandle.setPage (oldHandle.getPage ());

          (* release page of old handle *)
          oldHandle.dropData ();

          (* oldHandle may be get obsolete by RemovePage while dropData *)
          IF oldHandle.isLoad () THEN
            oldHandle.setPage (NIL);
            
            (* move old handle to A1out as FIFO list *)
            oldList.remove (oldHandle);
            A1out.addHead (oldHandle);
            EVAL A1out.removeTail ();
          ELSE
            (* oldHandle is replaced by a nullHandle, this is obsolete now *)
            oldHandle := oldList.removeTail ();
            <* ASSERT (oldHandle.getMedia () = nullMedia) *>
            <* ASSERT (oldHandle.getPage () = newHandle.getPage ()) *>
          END;

          (* arrange new handle into old queue *)
          oldList.addHead (newHandle);
        END
      END
    END
  END GetPageCopy; 


(*
 | --- RecognizeAccess ----------------------------------------------------
  Updates the replacement information, that is the position in the queue
  is moved up if the handle resides in the queue Am.
 | ------------------------------------------------------------------------
 *)
PROCEDURE RecognizeAccess (      handle		:PageHandle.T) =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    WITH list = NARROW (handle.getList (), ObjectList.T) DO
      IF list = Am THEN
        list.movetoHead (handle)
      END
    END
  END RecognizeAccess;



(*
 * --- Access Functions ---------------------------------------------------
 *)
PROCEDURE GetPage	(         pageNo	:CARDINAL;
                                  media		:PageMedia.T) :PageHandle.T =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>
    WITH handle = NEW (PageHandle.T).init () DO
      handle.unmarkChanges ();
      handle.setMedia (media);
      handle.setPageNo (pageNo);

      RETURN handle;
    END
  END GetPage;


PROCEDURE InsertPage    (         pageNo        :CARDINAL;
                                  media		:PageMedia.T;
                         READONLY data		:PageData.Part) :PageHandle.T =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    WITH handle = NEW (PageHandle.T).init () DO
      GetFreePage (handle);
      BasePageHandle.T.putData (handle, data);
      handle.unmarkChanges ();
      handle.setMedia (media);
      handle.setPageNo (pageNo);

      RETURN handle
    END
  END InsertPage;

  
PROCEDURE ReInsertPage	(         handle	:PageHandle.T;
                         READONLY data		:PageData.Part) =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    GetFreePage (handle);
    BasePageHandle.T.putData (handle, data);
    handle.unmarkChanges ();
  END ReInsertPage;

  
PROCEDURE CopyPage	(        handle		:PageHandle.T;
                                 newPageNo	:CARDINAL;
                                 newMedia	:PageMedia.T) :PageHandle.T =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    WITH newHandle = NEW (PageHandle.T).init () DO
      GetPageCopy (handle, newHandle);
      newHandle.unmarkChanges ();
      newHandle.setMedia (newMedia);
      newHandle.setPageNo (newPageNo);

      RETURN newHandle
    END
  END CopyPage;

  
PROCEDURE LoadPage	(        handle		:PageHandle.T) =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    GetFreePage (handle);
    handle.loadData();
  END LoadPage;
  

PROCEDURE AssignPage	(	 handle		:PageHandle.T) =
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    GetFreePage (handle);
  END AssignPage; 


PROCEDURE FlushPages	(        media		:PageMedia.T) =
  VAR
    i                   := NEW (ObjectList.Iterator);
    handle		:PageHandle.T;
  BEGIN
    EVAL i.init (A1in);
    handle := i.next ();
    WHILE handle # NIL DO
      IF (handle.getMedia () = media) THEN
        handle.dropData ();
      END;
      handle := i.next ();
    END;
    
    EVAL i.init (Am);
    handle := i.next ();
    WHILE handle # NIL DO
      IF (handle.getMedia () = media) THEN
        handle.dropData ();
      END;
      handle := i.next ();
    END;
  END FlushPages;

(*
 * --- Test code-----------------------------------------------------------
 *)
PROCEDURE DumpList	(        list		:ObjectList.T;
                                 name		:TEXT) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    i			:= NEW (ObjectList.Iterator);
    handle		:PageHandle.T;

  PROCEDURE DumpPage	(        handle		:PageHandle.T) =
    VAR
      data              :ARRAY [1..8] OF PageData.Item;
      text		:TEXT;
    BEGIN
      text := " handle = (" & handle.fmt () & ")";

      IF handle.isLoad () THEN
        BasePageHandle.T.getData (handle, data);
        text := text & ", data =[" & Fmt.Pad (Fmt.Int (data[1]), 3);
        FOR i := 2 TO 8 DO
          text := text & ", " & Fmt.Pad (Fmt.Int (data[i]), 3);
        END;
        text := text & "]";
      END;

      text := text & "\n";
      Wr.PutText (stdout, text);
      Wr.Flush (stdout);
    END DumpPage;

  (* DumpList *)
  BEGIN
    Wr.PutText (stdout, name);
    EVAL i.init (list);
    handle := i.next ();
    WHILE handle # NIL DO
      DumpPage (handle);
      handle := i.next ();
    END;
  END DumpList;


PROCEDURE Dump		() =
  <* FATAL Thread.Alerted, Wr.Failure *>
  (* Dump *)
  BEGIN
    <* ASSERT (cacheMutex.user = Thread.Self ()) *>

    Wr.PutText (stdout, "Cache contents:\n");
    DumpList (A1in, "Queue A1in:\n");
    DumpList (A1out, "Queue A1out:\n");
    DumpList (Am, "Queue Am:\n");
    Wr.PutText (stdout, "\n");    
  END Dump;


BEGIN
END PageCache.
