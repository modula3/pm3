INTERFACE VirtualResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.15  1998/03/18 12:13:35  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.14  1998/03/17 14:14:40  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.13  1997/10/31 14:14:50  roland
    Adapted to new RuleEngine.

    Revision 1.12  1997/06/13 12:00:35  rbnix
        Adapted to simplified file handling with methods
        getPath and makeFileName.

    Revision 1.11  1997/06/10 12:54:20  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.10  1997/05/16 08:48:51  roland
    Stack exceptions will never be raised, hence open methods need not raise
    FatalError.

    Revision 1.9  1997/05/09 16:27:02  renehuel
    The files have been changed to enable transaction semantic on closing
    of remote files. You may now close a graph within a transaction without
    an exception to be raised, and the final closing of the file
    Depends On the following action : a commit (of the top level
    transaction) closes the file, an abort aborts and leaves the resource
    still open.

    Revision 1.8  1997/04/24 12:13:03  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.7  1997/04/04 14:15:57  renehuel
    The type has been enhanced for handling of local files.
    The methods copyFile, deleteFile, renameFile, fileSize, existsFile and
    fileInUse now have an additional parameter remoteFile which determines
    whether the operation is to be used on a remote or local file.

    Revision 1.6  1996/11/21 07:54:46  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.5  1996/11/18 17:52:25  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.4  1996/11/14 14:13:07  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.3  1996/09/09 11:46:29  rbnix
        Handling of resource events from ClientScheduler inherited.

    Revision 1.2  1996/08/06 16:34:21  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/08/01 18:09:34  rbnix
        New file administration methods for remote files added:
        deleteFile, copyFile, renameFile, existsFile, fileInUse,
        fileSize and getFiles.

    Revision 1.1.2.1  1996/06/13 12:49:30  rbnix
        Method getID added to relate files to current client.

    Revision 1.1  1996/02/29 17:44:32  rbnix
        First version of subsystem VirtualPages giving transparent
        access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualResource ----------------------------------------------------
 *)
IMPORT Pathname, TextTransientSeq AS TextSeq,
       PageFile, Access, Txn, ClientInfoSeq;
IMPORT AtomList;

TYPE
  T <: Public;

  Public =
    <*TRANSIENT*> ROOT OBJECT
    METHODS
      (* resource administration *)
      open (baseName: Pathname.T; access: Access.Mode; new: BOOLEAN): T
            RAISES {Access.Denied, PageFile.NoAccess};

      close () RAISES {FatalError};


      (* other support *)
      beginTransaction    () RAISES {FatalError};
      commitTransaction   () RAISES {FatalError, NotInTransaction};
      chainTransaction    () RAISES {FatalError, NotInTransaction};
      abortTransaction    () RAISES {FatalError, NotInTransaction};
      getTransactionLevel (): Txn.Level;

      getAccessMode (): Access.Mode;
      getBaseName   (): Pathname.T;
      getPath       (temporary: BOOLEAN): Pathname.Arcs;
      makeFileName (baseName: Pathname.T; temporary: BOOLEAN): Pathname.T
                    RAISES {PageFile.NoAccess};

      getID (): TEXT;
      getRuleEngineID (): CARDINAL;
                       (* Use the rule engine id as transaction unit
                          identifier when signaling events. *)

      (* The following methods allow manipulation of remote and local
         files.  The type of the file is specified by the parameter
         remoteFile.  The default value is TRUE, so that the default type
         of files is remote file.  The parameter baseName determines the
         absolute path of the file to be manipulated.  Note that the
         opening of a local file causes it to be logged into a list keeping
         track of all opened files.  Before an operation like delete, copy
         or rename can be used it has to be checked whether the file is not
         opened, otherwise an exception will be raised. *)

      (* Deletes the file. *)
      deleteFile (baseName: Pathname.T; local: BOOLEAN)
                  RAISES {PageFile.NoAccess, FatalError};

      (* Copies the file with the path sourceName to the file with the path
         destName. *)
      copyFile (sourceName: Pathname.T;
                destName  : Pathname.T;
                local     : BOOLEAN     )
                RAISES {PageFile.NoAccess, FatalError};

      (* Renames the file with the path oldName to the file with the path
         newName. *)
      renameFile (oldName: Pathname.T; newName: Pathname.T; local: BOOLEAN)
                  RAISES {PageFile.NoAccess, FatalError};

      (* Checks if the file exists. *)
      existsFile (baseName: Pathname.T; local: BOOLEAN): BOOLEAN
                  RAISES {FatalError};

      (* This method has a slightly different semantic when used on local
         or remote files.  When used on a remote file it returns TRUE when
         somebody else is using the file, otherwise FALSE.  When used on a
         local file it returns TRUE when the file is opened, otherwise
         FALSE. *)
      fileInUse (baseName: Pathname.T; local: BOOLEAN): BOOLEAN
                 RAISES {FatalError};

      (* This method only makes sense for remote files.  The user of the
         local files is always the current user. *)
      getFileUser (baseName: Pathname.T): ClientInfoSeq.T
                   RAISES {FatalError};

      (* Returns the size of the file. *)
      fileSize (baseName: Pathname.T; local: BOOLEAN := FALSE): CARDINAL
                RAISES {PageFile.NoAccess, FatalError};

      (* Returns a list of all remote files known by the server. *)
      getFiles (): TextSeq.T RAISES {FatalError};

    END;

EXCEPTION
  FatalError(AtomList.T);
  NotInTransaction;

END VirtualResource.
