INTERFACE BaseScheduledClientRessource;

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

    Revision 1.11  1997/06/13 11:55:11  rbnix
    	Adapted to unified path handling of
    	Config.GetRootPrefix. Method getTmpPath is removed using an
    	additional parameter for method getPath. Further file handling
    	simplified with new method makeFileName.

    Revision 1.10  1997/06/10 12:53:36  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.9  1997/03/20 16:54:47  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.8  1996/11/21 07:54:23  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.7  1996/11/18 17:51:35  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.6  1996/11/14 14:12:43  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.5  1996/10/29 15:00:25  rbnix
        New method getTransactionNumber added.

        New parameter for page age added.

    Revision 1.4  1996/08/06 16:24:47  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.2  1996/08/01 18:09:16  rbnix
        New file administration methods for remote files added:
        deleteFile, copyFile, renameFile, existsFile, fileInUse,
        fileSize and getFiles.

    Revision 1.3.2.1  1996/06/13 12:48:20  rbnix
        Method getID moved to exported interface file

    Revision 1.3  1996/02/29 09:34:53  rbnix
        Release of resources added.

    Revision 1.2  1996/02/28 10:59:13  rbnix
        File and resource pathes are now related to a root path
        via Config.

    Revision 1.1  1996/02/09 16:46:33  rbnix
        First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- BaseScheduledClientRessource ---------------------------------------
  This abstract data type represents base access of aggregated ressource
  elements to be scheduled and at least transparently maintained on a remote
  server.

  All opened ressource files are used in transactional mode. Although the
  transaction commands are used to build (closed) nested transactions at
  this point only flat transactions are provided.
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, TextTransientSeq AS TextSeq, PageFile, Access, Txn,
       ClientInfoSeq;

IMPORT AtomList;

TYPE
  T <: Public;

  Public =
    <*TRANSIENT*> ROOT OBJECT
    METHODS
      (* resource administration *)
      init (baseName    : Pathname.T;
            access      : Access.Mode;
            new         : BOOLEAN ): T
            RAISES {Access.Denied, PageFile.NoAccess, FatalError};

      close () RAISES {FatalError};


      (* other file support *)
      registerLocalFile (baseName: Pathname.T)
                         RAISES {PageFile.NoAccess, FatalError};

      unregisterLocalFile (baseName: Pathname.T)
                           RAISES {PageFile.NoAccess, FatalError};

      deleteFile (baseName: Pathname.T)
                  RAISES {PageFile.NoAccess, FatalError};

      copyFile (sourceName: Pathname.T; destName: Pathname.T)
                RAISES {PageFile.NoAccess, FatalError};

      renameFile (oldName: Pathname.T; newName: Pathname.T)
                  RAISES {PageFile.NoAccess, FatalError};

      existsFile (baseName: Pathname.T): BOOLEAN RAISES {FatalError};

      fileInUse (baseName: Pathname.T): BOOLEAN RAISES {FatalError};

      getFileUser (baseName: Pathname.T): ClientInfoSeq.T
                   RAISES {FatalError};

      fileSize (baseName: Pathname.T): CARDINAL
                RAISES {PageFile.NoAccess, FatalError};

      getFiles (): TextSeq.T RAISES {FatalError};


      (* transaction support *)
      startTransaction  () RAISES {FatalError};
      commitTransaction () RAISES {NotInTransaction, FatalError};
      chainTransaction () RAISES {NotInTransaction, FatalError};
      abortTransaction  () RAISES {NotInTransaction, FatalError};

      getTransactionLevel  (): Txn.Level;
      getTransactionNumber (): CARDINAL;

      (* other stuff *)
      getAccessMode (): Access.Mode;
      
      getBaseName   (): Pathname.T;
      (*
        Returns the resource name without further path components.
      *)

      getPath       (temporary :BOOLEAN): Pathname.Arcs;
      (*
        Return the path of persistent/temporary root and the base resource
        name.
      *)

      makeFileName  (baseName :Pathname.T;
                     temporary :BOOLEAN): Pathname.T
		    RAISES {PageFile.NoAccess};
      (*
        Return a file name build with path and the specified baseName. The
        baseName is checked to be relative and containing no directories.
      *)

      getID (): TEXT;
      (*
        Get the current ID. This specifies uniquely the current connection
        of the client to the server.
      *)
    END;

EXCEPTION
  FatalError(AtomList.T);
  NotInTransaction;

END BaseScheduledClientRessource.
