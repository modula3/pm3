MODULE BaseScheduledClientFile
EXPORTS BaseScheduledClientFile, InternalBaseScheduledClientFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:47  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:36  hosking
    Import of GRAS3 1.1

    Revision 1.16  1998/01/21 14:11:05  roland
    Method baseName now in public interface.
    Files can now be opened as read-only in read-write-exclusive resources.

    Revision 1.15  1997/06/17 17:03:39  roland
    Temporary files must ensure that their path exists.

    Revision 1.14  1997/06/13 11:56:24  rbnix
    	Adapted to simplified handling with method makeFileName.

    Revision 1.13  1997/06/10 12:53:33  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.12  1997/04/24 12:12:26  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.11  1997/03/26 11:22:18  roland
    Open file routines can now handle subdirectories.

    Revision 1.10  1996/11/18 17:51:33  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.9  1996/10/29 14:59:40  rbnix
    	New method getTransactionNumber added.

    Revision 1.8  1996/10/07 13:36:05  rbnix
    	Use of PageFile replaced by BufferedPageFile.

    Revision 1.7  1996/08/06 16:24:46  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.2  1996/07/30 07:51:39  rbnix
    	Module FS replaced by PageFileSystem.

    Revision 1.6.2.1  1996/06/13 12:47:24  rbnix
    	Name of shadow file normalized.

    Revision 1.6  1996/03/15 14:26:20  rbnix
    	Local shadow file is named in relation to the resourceID.
    	Now clients can share their local directories.

    Revision 1.5  1996/03/11 17:16:06  rbnix
    	Method close moved from public to internal interface.

    	Method getBaseName added.

    Revision 1.4  1996/02/29 09:34:52  rbnix
    	Release of resources added.

    Revision 1.3  1996/02/28 10:59:10  rbnix
    	File and resource pathes are now related to a root path
    	via Config.

    Revision 1.2  1996/02/23 14:58:46  rbnix
    	Adapted to changed init/open methods in PageFile.

    Revision 1.1  1996/02/09 16:46:31  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

IMPORT
  Pathname,
  Page,
  PageFile, PageFileSystem,
  Access, PageLock, Txn,
  RemoteFile,
  OriginalMedia, ShadowMedia,
  BaseScheduledClientRessource, InternalBaseScheduledClientRessource,
  ErrorSupport;

REVEAL
  T				= Internal BRANDED OBJECT
    ressource			:BaseScheduledClientRessource.T;
    originalMedia		:OriginalMedia.T;
    shadowMedia			:ShadowMedia.T;
    <*TRANSIENT*> shadowName	:Pathname.T;
    <*TRANSIENT*> baseName	:Pathname.T;

  OVERRIDES
    open			:= Open;
    close			:= Close;
    waitAccess			:= WaitAccess;
    getData			:= GetData;
    putData			:= PutData;
    getBaseName			:= GetBaseName;
    getOriginalMedia		:= GetOriginalMedia;
    getShadowMedia		:= GetShadowMedia;
    getAccessMode		:= GetAccessMode;
    getTransactionLevel		:= GetTransactionLevel;
    getTransactionNumber	:= GetTransactionNumber;
  END;


PROCEDURE Open			(         self		:T;
                                          ressource	:BaseScheduledClientRessource.T;
                                          baseName	:Pathname.T;
                                          mode          :Access.Mode;
                                          kind		:Access.Kind;
                                          new		:BOOLEAN) :T
				RAISES {Access.Denied, PageFile.NoAccess,
                                        FatalError} =
  VAR
    shadowFile			:PageFile.T;
    originalFile		:RemoteFile.T;
  BEGIN
    CASE ressource.getAccessMode () OF
      Access.Mode.ReadWriteExclusive =>
        (* files can be read and written exclusively or only read *)
        IF mode = Access.Mode.ReadWriteShared THEN
          RAISE Access.Denied("Can't open file '" & baseName & "'. " &
                  "Access mode " & Access.FmtMode (mode) &
                  " conflicts with ressource access mode " &
                  Access.FmtMode (ressource.getAccessMode ()) & ".");
        END;
    | Access.Mode.ReadOnlyShared =>
        (* files can only be read *)
        IF mode # Access.Mode.ReadOnlyShared THEN
          RAISE Access.Denied("Can't open file '" & baseName & "'. " &
                  "Access mode " & Access.FmtMode (mode) &
                  " conflicts with ressource access mode " &
                  Access.FmtMode (ressource.getAccessMode ()) & ".");
        END;
    | Access.Mode.ReadWriteShared =>
        (* ok *)
    END;
    
    (* create shadow if neccessary *)
    CASE mode OF
    | Access.Mode.ReadOnlyShared =>
      self.shadowMedia := NIL;

    | Access.Mode.ReadWriteShared, Access.Mode.ReadWriteExclusive =>
      self.shadowName := ressource.makeFileName (
                             baseName & ".shadow." & ressource.getID (),
                             temporary := TRUE);
      (* make sure shadow path exists *)
      PageFileSystem.MakePath(Pathname.Prefix(self.shadowName));
      shadowFile := NEW (PageFile.T).init (self.shadowName, new := TRUE);
      shadowFile.open ();
      self.shadowMedia := NEW (ShadowMedia.T).init (shadowFile);
    END;

    (* open original *)
    TRY
      originalFile := ressource.openRemoteFile (baseName, mode, kind, new);
    EXCEPT
       BaseScheduledClientRessource.FatalError(info) =>
         RAISE FatalError(ErrorSupport.Propagate(
                              "BaseScheduledClientFile.Open",
                              "BaseScheduledClientRessource.FatalError", info));
    END;
    self.originalMedia := NEW (OriginalMedia.T).init (self, originalFile);

    (* setup other stuff *)
    self.ressource := ressource;
    self.baseName := baseName;
    RETURN self;
  END Open;


PROCEDURE Close			(         self		:T) RAISES {FatalError} =
  BEGIN
    (* close remote file *)
    TRY
      self.ressource.closeRemoteFile (self.originalMedia.getFile ());
    EXCEPT
       BaseScheduledClientRessource.FatalError(info) =>
         RAISE FatalError(ErrorSupport.Propagate(
                              "BaseScheduledClientFile.Close",
                              "BaseScheduledClientRessource.FatalError", info));
    END;

    (* close shadow *)
    IF self.shadowMedia # NIL THEN
      self.shadowMedia.getFile ().close ();
      TRY
        PageFileSystem.DeleteFile (self.shadowName);
      EXCEPT
        PageFile.NoAccess => (* ignore *)
      END;
    END;

    (* some clean up *)
    self.ressource := NIL;
    self.originalMedia := NIL;
    self.shadowMedia := NIL;
  END Close;


PROCEDURE WaitAccess		(         self		:T;
                                          pageNo	:CARDINAL;
		                          lock		:PageLock.ServerMode)
				RAISES {Access.Locked, FatalError} =
  BEGIN
    TRY
      self.ressource.waitAccess (self.originalMedia.getFile (), pageNo, lock);
    EXCEPT
       BaseScheduledClientRessource.FatalError(info) =>
         RAISE FatalError(ErrorSupport.Propagate(
                              "BaseScheduledClientFile.WaitAccess",
                              "BaseScheduledClientRessource.FatalError", info));
    END;
  END WaitAccess;
  

PROCEDURE GetData		(         self		:T;
                                          pageNo	:CARDINAL;
                                 VAR      pageAge	:CARDINAL;
                                          lock		:PageLock.ServerMode;
                                          transferData	:BOOLEAN) :Page.T
				RAISES {Access.Locked, FatalError} =
  BEGIN
    TRY
      RETURN self.ressource.getData (self.originalMedia.getFile (),
                                     pageNo, pageAge, lock, transferData);
    EXCEPT
      BaseScheduledClientRessource.FatalError(info) =>
        RAISE FatalError(ErrorSupport.Propagate(
                             "BaseScheduledClientFile.GetData",
                             "BaseScheduledClientRessource.FatalError", info));
    END;
  END GetData;


PROCEDURE PutData		(         self		:T;
                                          pageNo	:CARDINAL;
                                          pageAge	:CARDINAL;
                                          lock		:PageLock.ServerMode;
                                          page		:Page.T) =
  BEGIN
    self.ressource.putData (self.originalMedia.getFile (),
                            pageNo, pageAge, lock, page);
  END PutData;


PROCEDURE GetBaseName		(         self		:T) :Pathname.T =
  BEGIN
    RETURN self.baseName;
  END GetBaseName;


PROCEDURE GetOriginalMedia	(         self		:T) :OriginalMedia.T =
  BEGIN
    RETURN self.originalMedia;
  END GetOriginalMedia;


PROCEDURE GetShadowMedia	(         self		:T) :ShadowMedia.T =
  BEGIN
    RETURN self.shadowMedia;
  END GetShadowMedia;


PROCEDURE GetAccessMode		(         self		:T) :Access.Mode =
  BEGIN
    RETURN self.ressource.getAccessMode ();
  END GetAccessMode;

  
PROCEDURE GetTransactionLevel	(         self		:T) :Txn.Level =
  BEGIN
    RETURN self.ressource.getTransactionLevel ();
  END GetTransactionLevel;
  
PROCEDURE GetTransactionNumber	(         self		:T) :CARDINAL =
  BEGIN
    RETURN self.ressource.getTransactionNumber ();
  END GetTransactionNumber;


BEGIN
END BaseScheduledClientFile.
