MODULE BufferedPageFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:28  hosking
    Import of GRAS3 1.1

    Revision 1.3  1998/07/29 15:14:10  roland
    Increasing stack sizes.

    Revision 1.2  1996/11/20 12:16:43  roland
    Method shutdown added to close a buffered file when server is forced
    to terminate.

    Revision 1.1  1996/10/07 13:32:59  rbnix
    	New module for a asynchronous buffered file.

*)
(***************************************************************************)
(*
 | --- BufferedPageFile ---------------------------------------------------
 This implementation uses the data structure of a sequence for the
 buffer used as queue. (This makes flush a no-operation.) Asynchronous
 behavior is implemented using a background thread storing the data into a
 file while the user (foreground) thread fills the queue. Koordination is
 done using a MUTEX variable realising a monitor. To avoid busy waiting
 CONDITION variables are used: the user thread waits for notFull and the
 background thread waits for notEmpty.
 | ------------------------------------------------------------------------
 *)
IMPORT PageFile AS Super;
IMPORT
  Thread, Pathname,
  PageData, 
  PageFile, NumberedPage, NumberedPageSeq;


CONST
  maxQueueSize		=20;

REVEAL
  T			= Public BRANDED OBJECT
      mutex		:MUTEX;
      notFull		:Thread.Condition;
      notEmpty		:Thread.Condition;
      queue		:NumberedPageSeq.T;
      backgroundThread	:Thread.T;
      continueBackground:BOOLEAN;

    OVERRIDES
      init		:= Init;
      open		:= Open;
      close		:= Close;
      shutdown          := Shutdown;
      truncate		:= Truncate;
      flush		:= Flush;
      getData		:= GetData;
      putData		:= PutData;
    END;
  

TYPE
  BackgroundClosure	= Thread.SizedClosure OBJECT
      file		:T;

    OVERRIDES
      apply		:= WriteBack;
    END;


PROCEDURE Init		(         self		:T;
                                  fileName      :Pathname.T;
				  new		:BOOLEAN)
			:Super.T =
  BEGIN
    self.mutex := NEW (MUTEX);
    self.notFull := NEW (Thread.Condition);
    self.notEmpty := NEW (Thread.Condition);
    self.queue := NEW (NumberedPageSeq.T).init (maxQueueSize);

    (* only used when file is open *)
    self.continueBackground := FALSE;
    self.backgroundThread := NIL;

    RETURN Super.T.init (self, fileName, new);
  END Init;


PROCEDURE Open		(         self		:T)
			RAISES {PageFile.NoAccess} =
  BEGIN
    Super.T.open (self);

    self.continueBackground := TRUE;
    self.backgroundThread := Thread.Fork (NEW (BackgroundClosure,
                                               stackSize := 3*PageData.Size,
                                               file := self));
  END Open;

  
PROCEDURE Close		(         self		:T) =
  BEGIN
    (* request background thread to finish and stop *)
    LOCK self.mutex DO
      self.continueBackground := FALSE;
    END;

    (* wait until background thread stops *)
    Thread.Signal (self.notEmpty);
    EVAL Thread.Join (self.backgroundThread);
    self.backgroundThread := NIL;

    (* close file *)
    Super.T.close (self);
  END Close;

PROCEDURE Shutdown	(         self		:T) =
  BEGIN
    (* request background thread to finish and stop *)
    LOCK self.mutex DO
      self.continueBackground := FALSE;
    END;

    (* don't wait for background thread, program terminates and
       could be in critical section. *)
    Thread.Signal (self.notEmpty);
    self.backgroundThread := NIL;

    (* close file *)
    Super.T.close (self);
  END Shutdown;


PROCEDURE Truncate	(         <* UNUSED *>
                                  self		:T;
                                  <* UNUSED *>
                                  newSize       :CARDINAL) =
  BEGIN
    <* ASSERT (FALSE) *>
    (* don't know how to handle this operation *)
  END Truncate;



PROCEDURE Flush		(         <* UNUSED *>
                                  self		:T) =
  BEGIN
    (* nothing to do, it is nonsense to wait and synchronozize *)
  END Flush;



PROCEDURE GetData	(         self		:T;
                                  pageNo        :CARDINAL;
                              VAR data          :PageData.T) =
  BEGIN
    <* ASSERT (self.isOpen ()) *>

    LOCK self.mutex DO
      (* first search in buffer starting with last page *)
      FOR i := self.queue.size () -1 TO 0 BY -1 DO
        WITH currentPage = self.queue.get (i) DO
          IF pageNo = currentPage.getNumber () THEN
            currentPage.getData (data);
            RETURN;
          END
        END
      END;

      (* page not in queue, read it from file *)
      Super.T.getData (self, pageNo, data);
    END;
  END GetData;


PROCEDURE PutData	(         self		:T;
                                  pageNo        :CARDINAL;
	                 READONLY data		:PageData.T) =
  BEGIN
    <* ASSERT (self.isOpen ()) *>

    LOCK self.mutex DO
      WHILE self.queue.size () >= maxQueueSize DO
        Thread.Wait (self.mutex, self.notFull);
      END;

      self.queue.addhi (NEW (NumberedPage.T).init (pageNo, data));
      Thread.Signal (self.notEmpty);
    END;
  END PutData;


(*
 | --- WriteBack ----------------------------------------------------------
  This procedure removes pages from the front of the queue and stores them
  into the file. This is done page for page to allow highly interleaved
  flow of control. Therefore more than one page may be in buffer at one time
  and it is neccessary to signal notEmpty to the own thread to be able to
  proceed. 
 | ------------------------------------------------------------------------
 *)
PROCEDURE WriteBack	(         self		:BackgroundClosure)
			:REFANY =
  VAR
    stop		:BOOLEAN;
    page		:NumberedPage.T;
  BEGIN
    REPEAT
      LOCK self.file.mutex DO
        WHILE (self.file.queue.size () < 1) AND self.file.continueBackground DO
          Thread.Wait (self.file.mutex, self.file.notEmpty);
        END;

        IF 1 <= self.file.queue.size () THEN
          page := self.file.queue.remlo ();
          Super.T.putData (self.file, page.getNumber (), page.data);
          Thread.Signal (self.file.notFull);

          IF 0 < self.file.queue.size () THEN
            Thread.Signal (self.file.notEmpty);
          END;
        END;

        stop := ((self.file.queue.size () = 0) AND
                 (NOT self.file.continueBackground));
      END;
    UNTIL stop;

    RETURN NIL;
  END WriteBack; 


BEGIN
END BufferedPageFile.
