(* Copyright (C) 1992, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Jan 26 13:51:09 PST 1995 by kalsow  *)
(*      modified on Thu Jul 14 11:37:50 PDT 1994 by mcjones *)
(*      modified on Wed May 12 17:21:07 PDT 1993 by meehan  *)
(*      modified on Fri May  7 14:53:27 PDT 1993 by mjordan *)

(* Version for Unix *)

UNSAFE MODULE FSPosix EXPORTS FS;

IMPORT Atom, AtomList, Ctypes, File, FilePosix, M3toC, OSError,
       OSErrorPosix, Pathname, Process, Time, Text, TextSeq, Unix,
       Udir, Uerror, Ustat, Utime, Word;

FROM Unix IMPORT O_RDWR, O_CREAT, O_TRUNC, O_EXCL;

PROCEDURE GetAbsolutePathname(pn: Pathname.T): Pathname.T RAISES {OSError.E} =
  VAR arcs, prefix: Pathname.Arcs;
  BEGIN
    TRY
      arcs := Pathname.Decompose(pn);
      WITH rootDir = arcs.remlo() DO
        IF rootDir = NIL THEN (* "pn" is relative *)
          prefix := Pathname.Decompose(Process.GetWorkingDirectory())
        ELSE (* "pn" is absolute *)
          prefix := Seq1(rootDir)
       END
      END;
      arcs := Expand(prefix, arcs); (* expand symbolic links *)
      Contract(arcs); (* eliminate relative arcs *)
      RETURN Pathname.Compose(arcs)
    EXCEPT Pathname.Invalid => RAISE OSError.E(AtomList.List1(Invalid))
    END;
  END GetAbsolutePathname;

VAR Invalid := Atom.FromText("Invalid Pathname"); (* ??? *)

PROCEDURE Expand(prefix: Pathname.Arcs; tail: TextSeq.T): Pathname.Arcs
  RAISES {OSError.E, Pathname.Invalid} =
  (* "tail" is a sequence of arcs relative to "prefix". "Expand"
     returns an absolute pathname with no "", ".", or ".." arcs that
     names the same object as "TextSeq.Cat(prefix, tail)". *)
  VAR arc, rootDir: TEXT;
  BEGIN
    WHILE tail.size() > 0 DO
      arc := tail.remlo();
      prefix.addhi(arc);
      WITH link = CheckLink(prefix) DO
        IF link # NIL THEN
          tail := TextSeq.Cat(link, tail);
          rootDir := tail.remlo();
          IF rootDir = NIL THEN (* "link" is relative *)
            EVAL prefix.remhi() (* use previous "prefix" *)
          ELSE (* "link" is absolute *)
            prefix := Seq1(rootDir)
          END
        END
      END
    END;
    RETURN prefix
  END Expand;

PROCEDURE Contract(arcs: Pathname.Arcs) =
(* Eliminate relative arcs ("..", ".", or ""). *)
  VAR i := 1;
  BEGIN
    (* Invariant: Sub(arcs, 0, i) contains no relative arc. *)
    WHILE i < arcs.size() DO
      WITH arc = arcs.get(i) DO
        IF Text.Equal(arc, Pathname.Current) OR Text.Equal(arc, "") THEN
          (* a/./b => a/b; a//b => a/./b => a/b *)
          Rem(arcs, i)
        ELSIF Text.Equal(arc, Pathname.Parent) THEN
          IF i = 1 AND Text.Equal(arcs.get(0), "/") THEN
            (* Special case: /../a => /a *)
            Rem(arcs, 1)
          ELSE
            (* a/b/../c => a/c *)
            Rem(arcs, i); DEC(i); Rem(arcs, i)
          END
        ELSE
          INC(i)
        END
      END
    END
  END Contract;

(* TextSeq utility procedures: *)
PROCEDURE Seq1(t: TEXT): TextSeq.T =
  (* Return a new sequence whose only element is "t". *)
  BEGIN RETURN NEW(TextSeq.T).fromArray(ARRAY OF TEXT{t}) END Seq1;

PROCEDURE Rem(s: TextSeq.T; i: CARDINAL) =
  (* Remove the "i"th element of "s". *)
  BEGIN
    FOR j := i TO s.size()-2 DO s.put(j, s.get(j+1)) END;
    EVAL s.remhi()
  END Rem;

PROCEDURE CheckLink(arcs: Pathname.Arcs): Pathname.Arcs
  RAISES {OSError.E, Pathname.Invalid} =
  VAR
    buf: ARRAY [0 .. 1023] OF CHAR;
    path := Pathname.Compose(arcs);
    cc := Unix.readlink(M3toC.TtoS(path), ADR(buf [0]), NUMBER(buf));
    p_buf: ADDRESS := ADR (buf[0]);
  BEGIN
    IF cc <= 0 THEN
      IF Uerror.errno = Uerror.EINVAL THEN (* not a symbolic link *)
        RETURN NIL
      END;
      (* Some component is not a directory, or the file doesn't exist, or too
         many links (shouldn't happen, since we're expanding them one by one),
         or timeout, or ... *)
      OSErrorPosix.Raise()
    END;
    buf[cc] := '\000'; (* terminate the string *)
    RETURN Pathname.Decompose(M3toC.CopyStoT(p_buf))
  END CheckLink;

TYPE ABW = ARRAY BOOLEAN OF Word.T;

CONST OpenFlags = ARRAY CreateOption OF ABW{
  (* truncate =    FALSE                  TRUE                 *)
  (* Never  *) ABW{O_RDWR,                O_RDWR+O_TRUNC        },
  (* Ok     *) ABW{O_RDWR+O_CREAT,        O_RDWR+O_CREAT+O_TRUNC},
  (* Always *) ABW{O_RDWR+O_CREAT+O_EXCL, O_RDWR+O_CREAT+O_EXCL }
  };

CONST AllAccessModes =
  Unix.MSETUID + Unix.MSETGID + Unix.MSTICKY +
  Unix.MROWNER + Unix.MWOWNER + Unix.MXOWNER +
  Unix.MRGROUP + Unix.MWGROUP + Unix.MXGROUP +
  Unix.MROTHER + Unix.MWOTHER + Unix.MXOTHER;

CONST OpenMode = ARRAY AccessOption OF Ctypes.int{
  (*OnlyOwnerCanRead*) Unix.MROWNER+Unix.MWOWNER,
  (*ReadOnly*)         Unix.MROWNER+Unix.MRGROUP+Unix.MROTHER,
  (*Default*)          Unix.Mrwrwrw (* should this be AllAccessModes? *)
  };

PROCEDURE OpenFile(pn: Pathname.T; truncate: BOOLEAN := TRUE; 
    create: CreateOption := CreateOption.Ok; template: File.T := NIL; 
    access: AccessOption := AccessOption.Default
    ): File.T RAISES {OSError.E}=
  VAR
    fd: INTEGER;
    statBuf: Ustat.struct_stat;
    mode: Ctypes.int;
  BEGIN
    IF template # NIL THEN
      IF Ustat.fstat(template.fd, ADR(statBuf)) < 0 THEN
         OSErrorPosix.Raise()
      END;
      mode := Word.And(statBuf.st_mode, AllAccessModes)
    ELSE
      mode := OpenMode[access]
    END;
    fd := Unix.open(M3toC.TtoS(pn), OpenFlags[create, truncate], mode);
    IF fd < 0 THEN OSErrorPosix.Raise() END;
    RETURN FilePosix.New(fd, FilePosix.ReadWrite)
  END OpenFile;

PROCEDURE OpenFileReadonly(pn: Pathname.T): File.T RAISES {OSError.E}=
  BEGIN
    WITH fd = Unix.open(M3toC.TtoS(pn), Unix.O_RDONLY, Unix.Mrwrwrw) DO
      IF fd < 0 THEN OSErrorPosix.Raise() END;
      RETURN FilePosix.New(fd, FilePosix.Read)
    END
  END OpenFileReadonly;

PROCEDURE CreateDirectory(pn: Pathname.T) RAISES {OSError.E}=
  CONST
    (* Default access is rwxrwxrwx. The umask is applied by Unix *)
    RWXRWXRWX = Ustat.S_IREAD + Ustat.S_IWRITE + Ustat.S_IEXEC +
        Ustat.S_GREAD + Ustat.S_GWRITE + Ustat.S_GEXEC +
        Ustat.S_OREAD + Ustat.S_OWRITE + Ustat.S_OEXEC;
  BEGIN
    IF Unix.mkdir(M3toC.TtoS(pn), RWXRWXRWX) < 0 THEN
      OSErrorPosix.Raise()
    END
  END CreateDirectory;

PROCEDURE DeleteDirectory(pn: Pathname.T) RAISES {OSError.E}=
  BEGIN
    IF Unix.rmdir(M3toC.TtoS(pn)) < 0 THEN OSErrorPosix.Raise() END
  END DeleteDirectory;

PROCEDURE DeleteFile(pn: Pathname.T) RAISES {OSError.E}=
  BEGIN
    IF Unix.unlink(M3toC.TtoS(pn)) < 0 THEN OSErrorPosix.Raise() END
  END DeleteFile;

PROCEDURE Rename(pn0, pn1: Pathname.T) RAISES {OSError.E}=
  BEGIN
    IF Unix.rename(M3toC.TtoS(pn0), M3toC.TtoS(pn1)) < 0 THEN
      OSErrorPosix.Raise()
    END
  END Rename;

REVEAL Iterator = PublicIterator BRANDED OBJECT
    pn: Pathname.T; (* pathname of directory being iterated over *)
    d: Udir.DIR_star;
    closed := FALSE; (* has close() been called? *)
  OVERRIDES
    next := IterNext;
    nextWithStatus := IterNextWithStatus;
    close := IterClose
  END;

EXCEPTION IterClosed; <* FATAL IterClosed *>

PROCEDURE Iterate(pn: Pathname.T): Iterator RAISES {OSError.E} =
  BEGIN
    IF NOT Pathname.Absolute(pn) THEN
      pn := Pathname.Join(Process.GetWorkingDirectory(), pn, NIL)
    END;
    WITH d = Udir.opendir(M3toC.TtoS(pn)) DO
      IF d = NIL THEN OSErrorPosix.Raise() END;
      RETURN NEW(Iterator, d := d, pn := pn)
    END
  END Iterate;

PROCEDURE IterNext(iter: Iterator; VAR (*OUT*) name: TEXT): BOOLEAN =
  BEGIN
    IF IterRaw(iter, name) THEN RETURN TRUE END;
    RETURN FALSE
  END IterNext;

PROCEDURE IterNextWithStatus(
    iter: Iterator; VAR (*OUT*) name: TEXT; VAR (*OUT*) status: File.Status)
  : BOOLEAN RAISES {OSError.E} =
  BEGIN
    IF IterRaw(iter, name) THEN
      IF CStatus(M3toC.TtoS(Pathname.Join(iter.pn, name, NIL)), status) < 0
        THEN OSErrorPosix.Raise()
      END;
      RETURN TRUE
    END;
    RETURN FALSE
  END IterNextWithStatus;

TYPE NamePrefix = UNTRACED REF ARRAY [0..2] OF Ctypes.char;

PROCEDURE IterRaw(iter: Iterator; VAR (*OUT*) name: TEXT): BOOLEAN =
  BEGIN
    IF iter.closed THEN RAISE IterClosed END;
    LOOP (* to ignore "." and ".." *)
      IF iter.d = NIL THEN RETURN FALSE
      ELSE
        WITH e = Udir.readdir(iter.d) DO
          IF e = NIL THEN
            EVAL Udir.closedir(iter.d);
            iter.d := NIL;
            RETURN FALSE
          ELSE
            WITH sp = ADR(e.d_name) DO
              IF NOT DotOrDotDot(LOOPHOLE(sp, NamePrefix)) THEN
                name := M3toC.CopyStoT(LOOPHOLE (sp, ADDRESS));
                RETURN TRUE
              END
            END
          END
        END
      END
    END
  END IterRaw;

PROCEDURE DotOrDotDot(n: NamePrefix): BOOLEAN =
  CONST Dot = ORD('.'); Nul = ORD('\000');
  BEGIN
    RETURN n[0] = Dot AND (n[1] = Nul OR n[1] = Dot AND n[2] = Nul)
  END DotOrDotDot;
    
PROCEDURE IterClose(iter: Iterator) =
  BEGIN
    IF iter.d # NIL THEN EVAL Udir.closedir(iter.d); iter.d := NIL END;
    iter.closed := TRUE
  END IterClose;

PROCEDURE Status(pn: Pathname.T): File.Status RAISES {OSError.E} =
  VAR status: File.Status;
  BEGIN
    IF CStatus(M3toC.TtoS(pn), status) < 0 THEN OSErrorPosix.Raise() END;
    RETURN status
  END Status;

PROCEDURE CStatus(s: Ctypes.char_star; VAR status: File.Status): INTEGER =
  VAR statBuf: Ustat.struct_stat;
  BEGIN
    IF Ustat.stat(s, ADR(statBuf)) < 0 THEN RETURN -1 END;
    StatBufToStatus(statBuf, status);
    RETURN 0
  END CStatus;

<*INLINE*>
PROCEDURE StatBufToStatus(
    READONLY statBuf: Ustat.struct_stat; VAR (*OUT*) status: File.Status) =
  BEGIN
    status.type := FilePosix.FileTypeFromStatbuf(statBuf);
    (* Could make following assignments conditional on type: *)
    status.modificationTime := FLOAT(statBuf.st_mtime, LONGREAL);
    status.size := statBuf.st_size
  END StatBufToStatus;

PROCEDURE SetModificationTime(pn: Pathname.T; READONLY t: Time.T)
  RAISES {OSError.E}=
  CONST Accessed = 0; Updated = 1;
  VAR u: ARRAY [Accessed .. Updated] OF Utime.struct_timeval;
  BEGIN
    u[Updated].tv_sec := ROUND(t); u[Updated].tv_usec := 0;
    u[Accessed].tv_sec := ROUND(Time.Now()); u[Accessed].tv_usec := 0;
    IF Unix.utimes(M3toC.TtoS(pn), ADR(u)) < 0 THEN OSErrorPosix.Raise() END
  END SetModificationTime;

BEGIN
END FSPosix.
