MODULE PageFileSystem;

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

    Revision 1.3  1997/03/26 11:19:26  roland
    New procedure MakePath creates a directory and additionally all
    necessary ancestors.

    Revision 1.2  1996/08/06 16:26:11  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/25 11:49:26  rbnix
    	First version of module PageFileSystem added.

*)
(***************************************************************************)
IMPORT
  Pathname, TextTransientSeq AS TextSeq, FS, File, RegularFile, OSError,
  PageData, PageFile, ErrorSupport;


(* file support *)
PROCEDURE DeleteFile		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    TRY
      FS.DeleteFile (name);
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess ("Can't delete file '" & name & "'" &
                               ErrorSupport.Fmt (code));
    END;
  END DeleteFile;

  
PROCEDURE CopyFile		(         sourceName,
                                          destName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  VAR
    source, dest		:PageFile.T;
    size			:CARDINAL;
    data			:PageData.T;
  BEGIN
    size := FileSize (sourceName);

    source := NEW (PageFile.T).init (sourceName, new := FALSE);
    source.open ();

    dest := NEW (PageFile.T).init (destName, new := TRUE);
    dest.open ();

    FOR i := 0 TO size-1 DO
      source.getData (i, data);
      dest.putData (i, data);
    END;

    source.close ();
    dest.close ();
  END CopyFile;
  

PROCEDURE RenameFile		(         oldName,
                                          newName	:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    TRY
      FS.Rename (oldName, newName);
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't rename file '" & oldName & "' to '" & newName & "'" &
                ErrorSupport.Fmt (code));
    END;
  END RenameFile;


PROCEDURE ExistsFile		(         name		:Pathname.T)
				:BOOLEAN =
  VAR
    status			:File.Status;
  BEGIN
    TRY
      status := FS.Status (name);
    EXCEPT
    | OSError.E =>
      RETURN FALSE;
    END;

    RETURN (status.type = RegularFile.FileType)
  END ExistsFile;


PROCEDURE FileSize		(         name		:Pathname.T)
				:CARDINAL
				RAISES {PageFile.NoAccess} =
  VAR
    status			:File.Status;
  BEGIN
    TRY
      status := FS.Status (name);
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't inspect file '" & name & "'" &
                ErrorSupport.Fmt (code));
    END;

    IF (status.type # RegularFile.FileType) THEN
      RAISE PageFile.NoAccess (
                "Can't determine file size. Name '" & name &
                "' doesn't point to a regular file.");
    END;

    RETURN (status.size + PageData.Size-1) DIV PageData.Size;
  END FileSize;


(* directory support *)
PROCEDURE MakeDir		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    TRY
      FS.CreateDirectory (name);
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't create directory '" & name & "'" &
                ErrorSupport.Fmt (code));
    END;
  END MakeDir;

PROCEDURE MakePath		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess} =
  VAR
    path, act: Pathname.Arcs;
  BEGIN
    TRY
      path := Pathname.Decompose(name);
      act := NEW(TextSeq.T).init();
      (* remove leading '/' for absolute and 'NIL' for relative path names. *)
      act.addhi(path.remlo());
      FOR i := 1 TO path.size() DO
        act.addhi(path.remlo());
        IF NOT ExistsDir(Pathname.Compose(act)) THEN
          FS.CreateDirectory (Pathname.Compose(act));
        END;
      END;
    EXCEPT
    | Pathname.Invalid => RAISE PageFile.NoAccess(
                                    "Can't create path '" & name &
                                    "' pahtname invalid.")
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't create path '" & name & "'" &
                ErrorSupport.Fmt (code));
    END;
  END MakePath;


PROCEDURE RemoveDir		(         name		:Pathname.T)
				RAISES {PageFile.NoAccess} =
  BEGIN
    TRY
      FS.DeleteDirectory (name);
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't remove directory '" & name & "'" &
                ErrorSupport.Fmt (code));
    END;
  END RemoveDir;


PROCEDURE ExistsDir		(         name		:Pathname.T)
				:BOOLEAN =
 VAR
    status			:File.Status;
  BEGIN
    TRY
      status := FS.Status (name);
    EXCEPT
    | OSError.E =>
      RETURN FALSE;
    END;

    RETURN (status.type = FS.DirectoryFileType);
  END ExistsDir;



PROCEDURE GetFileNames		(         dirName	:Pathname.T)
				:TextSeq.T 
				RAISES {PageFile.NoAccess} =
  VAR
    dirNames			:TextSeq.T;
    i				:FS.Iterator;
    name			:TEXT;
    status			:File.Status;
  BEGIN
    TRY
      dirNames := NEW (TextSeq.T).init ();
      i := FS.Iterate (dirName);
      
      TRY
        WHILE i.nextWithStatus (name, status) DO
          IF status.type = RegularFile.FileType THEN
            dirNames.addhi (name);
          END;
        END;
      FINALLY
        i.close ();
      END;
      
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't inspect directory '" & dirName & "'" &
                ErrorSupport.Fmt (code));
    END;

    RETURN dirNames;
  END GetFileNames;


PROCEDURE GetDirNames		(         dirName	:Pathname.T)
				:TextSeq.T
				RAISES {PageFile.NoAccess} =
  VAR
    dirNames			:TextSeq.T;
    i				:FS.Iterator;
    name			:TEXT;
    status			:File.Status;
  BEGIN
    TRY
      dirNames := NEW (TextSeq.T).init ();
      i := FS.Iterate (dirName);
      
      TRY
        WHILE i.nextWithStatus (name, status) DO
          IF status.type = FS.DirectoryFileType THEN
            dirNames.addhi (name);
          END;
        END;
      FINALLY
        i.close ();
      END;
      
    EXCEPT
    | OSError.E (code) =>
      RAISE PageFile.NoAccess (
                "Can't inspect directory '" & dirName & "'" &
                ErrorSupport.Fmt (code));
    END;

    RETURN dirNames;
  END GetDirNames;


BEGIN
END PageFileSystem.
