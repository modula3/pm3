MODULE PageFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.3  2005/02/18 20:42:59  hosking
    Merge in latest development branch of persistence support for PM3.
    Includes support for BerkeleyDB as storage engine.
    Also includes PPC_DARWIN port from cm3.

    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:28  hosking
    Import of GRAS3 1.1

    Revision 1.10  1998/09/14 08:14:35  roland
    Modified code to remove compiler warnings.

    Revision 1.9  1997/06/30 14:00:43  roland
    Open files are kept in a list. If half of the system defined maximum
    number of open files are open, lru will be closed temporarily on next
    open. Not physically open files will be opend on demand.

    Revision 1.8  1996/11/20 12:16:46  roland
    Method shutdown added to close a buffered file when server is forced
    to terminate.

    Revision 1.7  1996/08/06 16:26:08  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.1  1996/06/13 12:42:55  rbnix
    	Error handling improved using new module ErrorSupport.

    Revision 1.6  1996/03/06 16:13:53  rbnix
    	New methods isOpen and getFileName added.

    Revision 1.5  1996/03/04 15:58:49  rbnix
    	Bug fixed: open an existing file has truncated the file.

    	Bug fixed: recursive call to flush replaced by file's flush
    	method.

    Revision 1.4  1996/03/02 16:55:09  rbnix
    	Bug fixed: variable file adjusted (used twice before).

    	Bug fixed: recognized that reading unknown pages returns
    	length zero.

    Revision 1.3  1996/02/23 14:57:43  rbnix
    	Method open replaced by init+open. This change allows
    	re-open the file after closing it.

    Revision 1.2  1996/02/21 13:45:50  rbnix
    	Desciptive parameter TEXT added to exception PageFile.NoAccess.

    Revision 1.1  1996/02/09 16:40:45  rbnix
    	First version of a page oriented file type added.

*)
(***************************************************************************)


(*
 | --- PageFile -----------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

IMPORT
  Pathname, FS, RegularFile, OSError,
  PageData,
  ErrorSupport,
  Uresource,
  Utypes;

REVEAL
  T                     = Public BRANDED OBJECT
      opened		:BOOLEAN;
      reallyOpen        :BOOLEAN;
      <*TRANSIENT*>
      fileName		:Pathname.T;
      new		:BOOLEAN;
      file		:RegularFile.T;
      next, prev        :T; (* list of open files *)
    OVERRIDES
      init		:= Init;
      open		:= Open;
      close		:= Close;
      shutdown          := Close;
      truncate		:= Truncate;
      flush		:= Flush;
      getData		:= GetData;
      putData		:= PutData;
      isOpen		:= IsOpen;
      getFileName	:= GetFileName;
  END;

PROCEDURE Init		(         self		:T;
                                  fileName      :Pathname.T;
				  new		:BOOLEAN) :T =
  BEGIN
    self.fileName := fileName;
    self.new := new;
    self.prev := NIL;
    self.next := NIL;
    self.reallyOpen := FALSE;
    self.opened := FALSE;
    
    RETURN self;
  END Init;


PROCEDURE Open		(         self		:T)
			RAISES {NoAccess} =
  BEGIN
    <* ASSERT (NOT self.isOpen ()) *>

    TRY
      IF OpenListFull() THEN
        CloseLRU();
      END;
      IF self.new THEN
        self.file := FS.OpenFile (self.fileName,
                                  truncate := TRUE,
                                  create := FS.CreateOption.Ok);
        self.new := FALSE;
      ELSE
        self.file := FS.OpenFile (self.fileName,
                                  truncate := FALSE,
                                  create := FS.CreateOption.Never);
      END;
    EXCEPT
    | OSError.E (code) =>
      RAISE NoAccess (
                "PageFile.Open: (" & self.fileName & ")" &
                ErrorSupport.Fmt (code));
    END;
    self.opened := TRUE;
    InsertInOpenList(self);
  END Open;

  
PROCEDURE Close			(         self		:T) =
  <* FATAL OSError.E *>
  BEGIN
    <* ASSERT (self.isOpen ()) *>

    IF FileReallyOpen(self) THEN
      self.file.close ();
      RemoveFromOpenList(self);
    END;
    self.opened := FALSE;
  END Close;
  

PROCEDURE Truncate		(         <* UNUSED *>
                                          self		:T;
                                          <* UNUSED *>
                                          newSize	:CARDINAL) =
  BEGIN
  END Truncate;
  

PROCEDURE Flush			(         self		:T) =
  <* FATAL OSError.E *>
  BEGIN
    <* ASSERT (self.isOpen ()) *>
    IF FileReallyOpen(self) THEN
      self.file.flush ();
    END;
  END Flush;
  

PROCEDURE GetData		(         self		:T;
                                          pageNo	:CARDINAL;
                                      VAR data          :PageData.T) =
  <* FATAL OSError.E *>
  VAR
    bytes			:INTEGER;
  BEGIN
    <* ASSERT (self.isOpen ()) *>

    EnsureOpen(self);
    EVAL self.file.seek (RegularFile.Origin.Beginning,
                         BYTESIZE (PageData.T)*pageNo);
    bytes := self.file.read (data);
    <* ASSERT ((bytes = BYTESIZE (data)) OR (bytes = 0)) *>
    RETURN;
  END GetData;


PROCEDURE PutData		(         self		:T;
                                          pageNo        :CARDINAL;
	                         READONLY data		:PageData.T) =
  <* FATAL OSError.E *>
  BEGIN
    <* ASSERT (self.isOpen ()) *>

    EnsureOpen(self);
    EVAL self.file.seek (RegularFile.Origin.Beginning,
                         BYTESIZE (PageData.T)*pageNo);
    self.file.write (data);
  END PutData;
  

PROCEDURE IsOpen		(         self		:T) :BOOLEAN =
  BEGIN
    RETURN self.opened;
  END IsOpen;


PROCEDURE GetFileName		(         self		:T) :Pathname.T =
  BEGIN
    RETURN self.fileName;
  END GetFileName;
  
(* The number of open files is limited for a process. We keep
   a buffer of unix-filedescriptors which size is a half of
   this limit. *)

VAR
  MaxOpenFiles: CARDINAL; (* maximal number of open files *)
  OpenFiles   : CARDINAL; (* current number of open files *)
  ListHead    : T;        (* head of the list of open files *)
  ListTail    : T;        (* tail of the list (points to LRU) *) 

PROCEDURE OpenListFull(): BOOLEAN =
  BEGIN
    RETURN OpenFiles >= MaxOpenFiles;
  END OpenListFull;

PROCEDURE FileReallyOpen(file: T): BOOLEAN =
  BEGIN
    RETURN file.reallyOpen;
  END FileReallyOpen;
  
PROCEDURE InsertInOpenList(file: T) =
  BEGIN
    file.next := ListHead;
    IF ListHead # NIL THEN
      ListHead.prev := file;
    ELSE
      (* first in list *)
      ListTail := file;
    END;
    file.prev := NIL;
    ListHead := file;
    file.reallyOpen := TRUE;
    INC(OpenFiles);
  END InsertInOpenList;

PROCEDURE RemoveFromOpenList(file: T) =
  BEGIN
    IF file.next # NIL THEN
      file.next.prev := file.prev;
    ELSE
      (* end of list *)
      ListTail := file.prev;
    END;
    IF file.prev # NIL THEN
      file.prev.next := file.next;
    ELSE
      (* head of list *)
      ListHead := file.next;
    END;
    DEC(OpenFiles);
    file.reallyOpen := FALSE;
  END RemoveFromOpenList;

PROCEDURE CloseLRU() =
  VAR file: T := ListTail;
  BEGIN
    RemoveFromOpenList(file);
    TRY
      file.file.close();
    EXCEPT
      OSError.E => (* this will show up again later *)
    END;
  END CloseLRU;

PROCEDURE EnsureOpen(file: T) RAISES {OSError.E} =
  BEGIN
    IF NOT file.reallyOpen THEN
      IF MaxOpenFiles <= OpenFiles THEN
        CloseLRU();
      END;
        file.file := FS.OpenFile (file.fileName,
                                truncate := FALSE,
                                create := FS.CreateOption.Never);
      InsertInOpenList(file);
    ELSE
      (* file becomes head of list, so that tail stays LRU *)
      IF file # ListHead THEN
        RemoveFromOpenList(file);
        InsertInOpenList(file);
      END;
    END;
  END EnsureOpen;
  
BEGIN
  VAR
    rlp: Uresource.struct_rlimit;
  BEGIN
    (* determine number of open file-descriptors per process *)
    EVAL Uresource.getrlimit(Uresource.RLIMIT_NOFILE, rlp);
    WITH cur = Utypes.asLong(rlp.rlim_cur),
         max = Utypes.asLong(rlp.rlim_max)
     DO
      IF max # Uresource.RLIM_INFINITY AND cur < max THEN
        (* Set soft limit up to hard limit *)
        rlp.rlim_cur := rlp.rlim_max;
        EVAL Uresource.setrlimit(Uresource.RLIMIT_NOFILE, rlp);
      END;
    END;
    (* we use at most half of all possibly open files *)
    MaxOpenFiles := Utypes.asLong(rlp.rlim_cur) DIV 2;
  END;
  OpenFiles := 0;
  ListHead := NIL;
  ListTail := NIL;
END PageFile.
