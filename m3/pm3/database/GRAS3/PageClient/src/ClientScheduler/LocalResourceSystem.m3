MODULE LocalResourceSystem;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:36  hosking
    Import of GRAS3 1.1

    Revision 1.1  1998/02/10 16:36:29  roland
    LocalResourceSystem offers procedures to manage the local part of
    resources.

*)
(***************************************************************************)

IMPORT Pathname, TextTransientSeq AS TextSeq;
IMPORT PageFile, PageFileSystem, Config;

PROCEDURE DeleteEmptyDirs (dir: Pathname.T) RAISES {PageFile.NoAccess} =
  VAR
    name                  : Pathname.T;
    rootPath, resourcePath: Pathname.Arcs;
    n                     : CARDINAL;
  <* FATAL Pathname.Invalid, Config.Unspecified *>
  BEGIN
    rootPath := Pathname.Decompose(Config.GetRootPrefix(FALSE));
    resourcePath := Pathname.Decompose(dir);

    n := resourcePath.size() - 1;

    FOR i := 1 TO n DO rootPath.addhi(resourcePath.get(i)); END;

    (* removing dirs starting from inner most to root *)
    FOR i := n TO 1 BY -1 DO
      name := Pathname.Compose(rootPath);
      IF PageFileSystem.ExistsDir(name)
           AND (PageFileSystem.GetDirNames(name).size() = 0)
           AND (PageFileSystem.GetFileNames(name).size() = 0) THEN
        PageFileSystem.RemoveDir(name);
      ELSE
        RETURN
      END;
      EVAL rootPath.remhi();
    END;
  END DeleteEmptyDirs;

PROCEDURE DeleteLocalResource (baseName: Pathname.T)
  RAISES {PageFile.NoAccess} =

  PROCEDURE DeleteDir (baseName: Pathname.T) RAISES {PageFile.NoAccess} =
    <* FATAL Pathname.Invalid *>
    VAR
      entries : TextSeq.T;
      basePath: Pathname.Arcs;
    BEGIN
      (* delete nested directories *)
      entries := PageFileSystem.GetDirNames(baseName);
      FOR i := 0 TO entries.size() - 1 DO
        basePath := Pathname.Decompose(baseName);
        basePath.addhi(entries.get(i));

        DeleteDir(Pathname.Compose(basePath));
      END;

      (* delete files *)
      entries := PageFileSystem.GetFileNames(baseName);
      FOR i := 0 TO entries.size() - 1 DO
        basePath := Pathname.Decompose(baseName);
        basePath.addhi(entries.get(i));

        PageFileSystem.DeleteFile(Pathname.Compose(basePath));
      END;

      (* delete directory itself *)
      PageFileSystem.RemoveDir(baseName);
    END DeleteDir;

  VAR persistentPath, basePath: Pathname.Arcs;

  (* DeleteResource *)
  BEGIN
    TRY
      (* delete disk resource *)
      basePath := Pathname.Decompose(baseName);
      EVAL basePath.remlo();

      persistentPath :=
        TextSeq.Cat(Pathname.Decompose(
                      Config.GetRootPrefix(temporary := FALSE)), basePath);
      DeleteDir(Pathname.Compose(persistentPath));

      WITH path = Pathname.Decompose(baseName) DO
        EVAL path.remhi();
        DeleteEmptyDirs(Pathname.Compose(path));
      END;
    EXCEPT
    | Config.Unspecified =>
        RAISE PageFile.NoAccess("No root path specified!");
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess("Invalid resource name!");
    END;
  END DeleteLocalResource;

PROCEDURE CopyLocalResource (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess} =
  PROCEDURE CopyDir (sourceBaseName: Pathname.T; destBaseName: Pathname.T)
    RAISES {PageFile.NoAccess} =
    <* FATAL Pathname.Invalid *>
    VAR
      entries             : TextSeq.T;
      sourcePath, destPath: Pathname.Arcs;
    BEGIN
      (* 'copy' directory itself *)
      PageFileSystem.MakeDir(destBaseName);

      (* copy files *)
      entries := PageFileSystem.GetFileNames(sourceBaseName);
      FOR i := 0 TO entries.size() - 1 DO
        sourcePath := Pathname.Decompose(sourceBaseName);
        sourcePath.addhi(entries.get(i));

        destPath := Pathname.Decompose(destBaseName);
        destPath.addhi(entries.get(i));

        PageFileSystem.CopyFile(
          Pathname.Compose(sourcePath), Pathname.Compose(destPath));
      END;

      (* copy nested directories *)
      entries := PageFileSystem.GetDirNames(sourceBaseName);
      FOR i := 0 TO entries.size() - 1 DO
        sourcePath := Pathname.Decompose(sourceBaseName);
        sourcePath.addhi(entries.get(i));

        destPath := Pathname.Decompose(destBaseName);
        destPath.addhi(entries.get(i));

        CopyDir(Pathname.Compose(sourcePath), Pathname.Compose(destPath));
      END;
    END CopyDir;

  VAR destPath, sourcePath: Pathname.Arcs;
  (* CopyResource *)
  BEGIN
    TRY
      IF ExistsLocalResource(destName) THEN
        RAISE PageFile.NoAccess(
                "Can't copy resource '" & sourceName & "' to '" & destName
                  & "'. The destination resource already exists.");
      END;

      sourcePath := Pathname.Decompose(sourceName);
      EVAL sourcePath.remlo();
      sourcePath :=
        TextSeq.Cat(
          Pathname.Decompose(Config.GetRootPrefix(temporary := FALSE)),
          sourcePath);

      destPath := Pathname.Decompose(destName);
      EVAL destPath.remlo();
      destPath :=
        TextSeq.Cat(Pathname.Decompose(
                      Config.GetRootPrefix(temporary := FALSE)), destPath);
      PageFileSystem.MakePath(Pathname.Compose(destPath));

      CopyDir(Pathname.Compose(sourcePath), Pathname.Compose(destPath));
    EXCEPT
    | Config.Unspecified =>
        RAISE PageFile.NoAccess("No root path specified!");
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess("Invalid resource name!");
    END;
  END CopyLocalResource;

PROCEDURE RenameLocalResource (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess} =
  VAR newPath, oldPath: Pathname.Arcs;
  (* RenameResource *)
  BEGIN
    TRY
      IF ExistsLocalResource(newName) THEN
        RAISE PageFile.NoAccess(
                "Can't rename resource '" & oldName & "' to '" & newName
                  & "'. The destination resource already exists.");
      END;

      oldPath := Pathname.Decompose(oldName);
      EVAL oldPath.remlo();
      oldPath :=
        TextSeq.Cat(Pathname.Decompose(
                      Config.GetRootPrefix(temporary := FALSE)), oldPath);

      newPath := Pathname.Decompose(newName);
      EVAL newPath.remlo();
      newPath :=
        TextSeq.Cat(Pathname.Decompose(
                      Config.GetRootPrefix(temporary := FALSE)), newPath);
      WITH last = newPath.remhi() DO
        PageFileSystem.MakePath(Pathname.Compose(newPath));
        newPath.addhi(last);
      END;
      PageFileSystem.RenameFile(
        Pathname.Compose(oldPath), Pathname.Compose(newPath));
      WITH path = Pathname.Decompose(oldName) DO
        EVAL path.remhi();
        DeleteEmptyDirs(Pathname.Compose(path));
      END;
    EXCEPT
    | Config.Unspecified =>
        RAISE PageFile.NoAccess("No root path specified!");
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess("Invalid resource name!");
    END;
  END RenameLocalResource;

PROCEDURE ExistsLocalResource (baseName: Pathname.T): BOOLEAN
  RAISES {PageFile.NoAccess} =
  VAR basePath: Pathname.Arcs;
  (* CopyResource *)
  BEGIN
    TRY
      basePath := Pathname.Decompose(baseName);
      EVAL basePath.remlo();
      basePath :=
        TextSeq.Cat(Pathname.Decompose(
                      Config.GetRootPrefix(temporary := FALSE)), basePath);
      RETURN PageFileSystem.ExistsDir(Pathname.Compose(basePath));
    EXCEPT
    | Config.Unspecified =>
        RAISE PageFile.NoAccess("No root path specified!");
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess("Invalid resource name!");
    END;
  END ExistsLocalResource;

BEGIN
END LocalResourceSystem.
