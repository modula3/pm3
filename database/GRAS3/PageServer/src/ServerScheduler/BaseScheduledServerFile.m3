MODULE BaseScheduledServerFile
EXPORTS BaseScheduledServerFile, InternalBaseScheduledServerFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:38  hosking
    Import of GRAS3 1.1

    Revision 1.10  1997/06/27 07:06:15  roland
    Files remove their shadow when they are closed. Therefore, shadow
    files have to initialized on every true open of a file with
    initShadow.

    Revision 1.9  1997/06/16 12:21:36  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.8  1997/06/13 11:46:23  rbnix
    	Adapted to unified resource paths of module
    	BaseServerScheduler.

    Revision 1.7  1997/05/15 17:06:19  roland
    Bugfix: ScheduledClientFile has to set mode and kind in open method when
    it is not used by another client. Therfore: methods setAccessMode and setKind
    added to internal interface of BaseScheduledServerFile.

    Revision 1.6  1997/04/24 12:13:45  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.5  1997/03/26 11:22:37  roland
    Open file routines can now handle subdirectories.

    Revision 1.4  1996/10/07 13:34:55  rbnix
    	Use of PageFile replaced by BufferedPageFile.

    Revision 1.3  1996/08/06 16:32:37  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/30 09:41:51  rbnix
    	New method InternalBaseScheduledServerResource.makeFilePath used.

    Revision 1.2.2.1  1996/07/11 11:06:04  rbnix
    	Parameters of procedure WaitAccess enhanced by lock table.

    Revision 1.2  1996/02/28 11:03:08  rbnix
    	File and resource pathes are now related to a root path
    	via Config. Errors related to creation of are submittet to
    	clients via exception PageFile.NoAccess.

    Revision 1.1  1996/02/26 17:59:29  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- BaseScheduledServerFile --------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  PageFile, BufferedPageFile, PageFileSystem,
  SimpleMedia, ShadowMedia,
  Access, PageLock,
  ServedClient,
  BaseScheduledServerResource, InternalBaseScheduledServerResource, ServerLockTable;

REVEAL
  T			= Internal BRANDED OBJECT
      resource		:BaseScheduledServerResource.T;
      mode              :Access.Mode;
      kind		:Access.Kind;
      <*TRANSIENT*>
      baseName		:Pathname.T;
      persistentMedia	:SimpleMedia.T;
      temporaryMedia	:ShadowMedia.T;

    OVERRIDES
      init		:= Init;
      initShadow        := InitShadow;
      
      setAccessMode     := SetAccessMode;
      setKind           := SetKind;

      getKind           := GetKind;
      getBaseName	:= GetBaseName;
      getPersistentMedia:= GetPersistentMedia;
      getTemporaryMedia	:= GetTemporaryMedia;
      getAccessMode	:= GetAccessMode;

      waitAccess	:= WaitAccess;
    END;


PROCEDURE Init		(         self		:T;
                                  resource	:BaseScheduledServerResource.T;
                                  baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:T
			RAISES {PageFile.NoAccess} =
  VAR
    file		:PageFile.T;
    fileName		:Pathname.T;
  BEGIN
    self.resource := resource;
    self.kind := kind;
    self.mode := mode;
    self.baseName := baseName;

    (* set up persistent file & media *)
    fileName := resource.makeFileName (baseName, temporary := FALSE);
    PageFileSystem.MakePath (Pathname.Prefix (fileName));
    file := NEW (BufferedPageFile.T).init (fileName, new);
    self.persistentMedia := NEW (SimpleMedia.T).init (file);

    RETURN self;
  END Init;

PROCEDURE InitShadow    (         self              :T)
			RAISES {PageFile.NoAccess} =
  VAR
    file		:PageFile.T;
    fileName		:Pathname.T;
  BEGIN
    (* set up temporary file & media *)
    fileName := self.resource.makeFileName (self.baseName, temporary := TRUE);
    PageFileSystem.MakePath (Pathname.Prefix (fileName));
    file := NEW (BufferedPageFile.T).init (fileName, new := TRUE);
    self.temporaryMedia := NEW (ShadowMedia.T).init (file);
  END InitShadow; 

PROCEDURE SetAccessMode (         self          :T;
                                  mode          :Access.Mode) =
  BEGIN
    self.mode := mode;
  END SetAccessMode;

  
PROCEDURE SetKind       (         self          :T;
                                  kind          :Access.Kind) =
  BEGIN
    self.kind := kind;
  END SetKind; 

  
PROCEDURE GetKind	(         self		:T) :Access.Kind =
  BEGIN
    RETURN self.kind;
  END GetKind;


PROCEDURE GetBaseName	(         self		:T) :Pathname.T =
  BEGIN
    RETURN self.baseName;
  END GetBaseName;


PROCEDURE GetPersistentMedia
			(         self          :T) :SimpleMedia.T =
  BEGIN
    RETURN self.persistentMedia;
  END GetPersistentMedia;


PROCEDURE GetTemporaryMedia
			(         self		:T) :ShadowMedia.T =
  BEGIN
    RETURN self.temporaryMedia;
  END GetTemporaryMedia;


PROCEDURE GetAccessMode	(         self		:T) :Access.Mode =
  BEGIN
    RETURN self.mode;
  END GetAccessMode;


PROCEDURE WaitAccess	(         self		:T;
                                  client	:ServedClient.T;
                                  locks		:ServerLockTable.T;
                                  pageNo        :CARDINAL;
                                  lock          :PageLock.ServerMode)
			RAISES {Access.Locked} =
  BEGIN
    self.resource.waitAccess (client, locks, self.baseName, pageNo, lock);
  END WaitAccess;
  

BEGIN
END BaseScheduledServerFile.
