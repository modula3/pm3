(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* FileSysPosix.m3 *)
(* Last modified on Tue Apr 18 16:46:40 PDT 1995 by wobber *)
(*      modified on Wed Feb 23 09:12:57 PST 1994 by kalsow *)

UNSAFE MODULE FileSysPosix EXPORTS FileSys;

IMPORT Atom, AtomList, M3toC, Text, Uerror, Unix, Ustat, Word;
IMPORT Cerrno, (* Cstring, *) Pathname, OSError, OSErrorPosix, Udir;

FROM Ctypes IMPORT char_star, int;

CONST
  rMode = Ustat.S_IREAD + Ustat.S_GREAD + Ustat.S_OREAD;
  rwMode = Ustat.S_IREAD + Ustat.S_IWRITE + Ustat.S_GREAD + Ustat.S_OREAD;
  rwxMode = rwMode + Ustat.S_IEXEC + Ustat.S_GEXEC + Ustat.S_OEXEC;
  rxMode = rMode + Ustat.S_IEXEC + Ustat.S_GEXEC + Ustat.S_OEXEC;

VAR
  errENOENT, errENOTDIR, errEACCES, errEEXIST, errENOSPC, errEIO: Atom.T;

PROCEDURE ClassifyError(e: OSError.Code) : ErrorClass =
  VAR a: Atom.T := NIL;
  BEGIN
    IF e # NIL THEN a := e.head; END;
    IF a = errENOENT OR a = errENOTDIR THEN
      RETURN ErrorClass.Lookup;
    ELSIF a = errEACCES OR a = errEEXIST THEN
      RETURN ErrorClass.Access;
    ELSIF a = errEIO THEN
      RETURN ErrorClass.IO;
    ELSIF a = errENOSPC THEN
      RETURN ErrorClass.NoRoomInFS;
    ELSE
      RETURN ErrorClass.Other;
    END;
  END ClassifyError;

PROCEDURE GetInfo(path: FN; followLink: BOOLEAN := FALSE) : FileInfo
   RAISES {OSError.E} =
  VAR
    p := ConvertPath(path);
    statBuf: Ustat.struct_stat;
    status: int;
    info: FileInfo;
  BEGIN
    IF followLink THEN
      status := Ustat.stat(p, ADR(statBuf));
    ELSE
      status := Ustat.lstat(p, ADR(statBuf));
    END;
    FreePath (path, p);
    IF status = -1 THEN RaiseError(); END;
    info.date := FLOAT(statBuf.st_mtime, LONGREAL);
    IF Word.And(statBuf.st_mode, Ustat.S_IEXEC) # 0 THEN
      IF Word.And(statBuf.st_mode, Ustat.S_IWRITE) # 0 THEN
        info.perm := FilePermRWX;
      ELSE
        info.perm := FilePermRX;
      END;
    ELSIF Word.And(statBuf.st_mode, Ustat.S_IWRITE) # 0 THEN
      info.perm := FilePermNorm;
    ELSE
      info.perm := FilePermReadOnly;
    END;
    info.length := statBuf.st_size;
    WITH mode = Word.And(statBuf.st_mode, Ustat.S_IFMT) DO
      IF mode = Ustat.S_IFDIR THEN
        info.type := FileType.Dir;
      ELSIF mode = Ustat.S_IFREG THEN
        info.type := FileType.Normal;
      ELSIF mode = Ustat.S_IFLNK THEN
        info.type := FileType.SLink;
      ELSE
        info.type := FileType.Other;
      END;
    END;
    RETURN info;
  END GetInfo;

PROCEDURE SetMode (path: FN; perm: FilePerm) RAISES {OSError.E} =
  VAR
    status, mode: int;
    p := ConvertPath(path);
  BEGIN
    CASE perm OF
    | FilePermRWX => mode := rwxMode;
    | FilePermReadOnly => mode := rMode;
    | FilePermRX => mode := rxMode;
    ELSE
      mode := rwMode;
    END;
    status := Unix.chmod (p, mode);
    FreePath (path, p);
    IF status = -1 THEN RaiseError(); END;
  END SetMode;

PROCEDURE ReadLink(path: FN) : FN RAISES {OSError.E} =
  VAR
    cc: int;
    p := ConvertPath(path);
    linkbuf: ARRAY [0..1023] OF CHAR;
  BEGIN
    cc := Unix.readlink(p, ADR(linkbuf), BYTESIZE(linkbuf)-1);
    FreePath (path, p);
    IF cc < 0 THEN RaiseError(); END;
    linkbuf[cc] := '\000';
    RETURN M3toC.CopyStoT(ADR(linkbuf));
  END ReadLink;

PROCEDURE HardLink(path, referent: FN) RAISES {OSError.E} =
  VAR
    status: int;
    p := ConvertPath(path);
    r := ConvertPath(referent);
  BEGIN
    status := Unix.link (r, p);
    FreePath (path, p);
    FreePath (referent, r);
    IF status = -1 THEN RaiseError(); END;
  END HardLink;

PROCEDURE SymLink(path, referent: FN) RAISES {OSError.E} =
  VAR
    status: int;
    p := ConvertPath(path);
    r := ConvertPath(referent);
  BEGIN
    status := Unix.symlink (r, p);
    FreePath (path, p);
    FreePath (referent, r);
    IF status = -1 THEN RaiseError(); END;
  END SymLink;

PROCEDURE CheckAccess (path: Text.T; write: BOOLEAN; fail: BOOLEAN): BOOLEAN
    RAISES {OSError.E} =
  VAR
    status, mode: int;
    p := ConvertPath(path);
  BEGIN
    IF write THEN
      mode := Word.Or(Unix.W_OK, Unix.R_OK)
    ELSE
      mode := Unix.R_OK
    END;
    status := Unix.access(p, mode);
    FreePath (path, p);
    IF status = -1 THEN
      IF NOT fail AND Cerrno.GetErrno() = Uerror.EACCES THEN
        RETURN FALSE;
      ELSE
        RaiseError();
      END;
    END;
    RETURN TRUE;
  END CheckAccess;

PROCEDURE ConvertPath(t: FN) : char_star =
  BEGIN
    IF t = NIL OR Text.Empty(t) THEN
      RETURN M3toC.CopyTtoS(Pathname.Current);
    ELSE
      RETURN M3toC.TtoS(t);
    END;
  END ConvertPath;

PROCEDURE FreePath(t: FN;  s: char_star) =
  BEGIN
    IF t = NIL OR Text.Empty(t) THEN
      M3toC.FreeCopiedS(s);
    END;
  END FreePath;

(* should be implemented by Directory.GetAbsolute *)

PROCEDURE GetPath(path: TEXT): Text.T RAISES {OSError.E} =
  VAR
    d, parentd : Udir.DIR_star;
    info, pinfo: Ustat.struct_stat;
    status: int;
    done: BOOLEAN;
    newpath, sibling: Text.T;
    de: Udir.struct_dirent_star;  (** Udir.direct_star; **)
    p: char_star;
  BEGIN
    newpath := "";
    IF (path = NIL) OR Text.Empty(path) THEN path := "."; END;
    p := ConvertPath(path) ;
    d := Udir.opendir(p);
    FreePath (path, p);
    IF d = NIL THEN RaiseError(); END;
    TRY
      LOOP
        done := FALSE;
        IF Ustat.fstat(d.dd_fd, ADR(info)) < 0 THEN RaiseError(); END;
        path := path & "/..";
        p := M3toC.TtoS (path);
        parentd := Udir.opendir(p);
        IF parentd = NIL THEN RaiseError(); END;
        IF Ustat.fstat(parentd.dd_fd, ADR(pinfo)) < 0 THEN RaiseError(); END;
        IF (pinfo.st_dev = info.st_dev) THEN
          IF (info.st_ino = pinfo.st_ino) THEN
            EXIT;
            (* parent and child the same ==> at root *)
          END;
          (* Look for info.fileSeq inside parent *)
          WHILE NOT done DO
            de := Udir.readdir(parentd);
            IF de = NIL THEN RaiseError(); END;
            IF de^.d_ino = info.st_ino THEN
              done := TRUE;
              EXIT;
            END;
          END;  (* WHILE *)
        ELSE
          (* Must do stats on all entries in the parent. *)
          done := FALSE;
          WHILE NOT done DO
            de := Udir.readdir(parentd);
            IF de = NIL THEN RaiseError(); END;
            IF de^.d_ino # 0 THEN
              sibling := path & "/" & M3toC.StoT(ADR(de^.d_name));
              p := M3toC.TtoS (sibling);
              status := Ustat.lstat(p, ADR(pinfo));
              IF (status >= 0) AND
                      (pinfo.st_ino = info.st_ino) AND
                      (pinfo.st_dev = info.st_dev) THEN
                done := TRUE;
                EXIT;
              END;
            END;
          END; (* WHILE *)
        END;  (* IF *)
        EVAL Udir.closedir(d);
        d := parentd;
        parentd := NIL;
        IF NOT done THEN RAISE OSError.E(AtomList.Cons(errEIO, NIL)); END;
        newpath := "/" & M3toC.StoT(ADR(de^.d_name)) & newpath;
      END;  (* LOOP *)
    FINALLY
      IF parentd # NIL THEN EVAL Udir.closedir(parentd); END;
      IF d # NIL THEN EVAL Udir.closedir(d); END;
    END;
    IF Text.Empty(newpath) THEN RETURN "/" ELSE RETURN newpath END;
  END GetPath;

PROCEDURE RaiseError() RAISES {OSError.E} =
  BEGIN
    OSErrorPosix.Raise();
    (* RAISE OSError.E(AtomList.Cons(ErrnoAtom(Cerrno.GetErrno()), NIL)); *)
  END RaiseError;

PROCEDURE ErrnoAtom(i: int) : Atom.T =
  BEGIN
    RETURN OSErrorPosix.ErrnoAtom(i);
    (* RETURN Atom.FromText(M3toC.StoT(Cstring.strerror(i))); *)
  END ErrnoAtom;

BEGIN
  errENOENT := ErrnoAtom(Uerror.ENOENT);
  errENOTDIR := ErrnoAtom(Uerror.ENOTDIR);
  errEACCES := ErrnoAtom(Uerror.EACCES);
  errEEXIST := ErrnoAtom(Uerror.EEXIST);
  errENOSPC := ErrnoAtom(Uerror.ENOSPC);
  errEIO := ErrnoAtom(Uerror.EIO);
END FileSysPosix.
