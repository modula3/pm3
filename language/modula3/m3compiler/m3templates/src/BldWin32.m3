MODULE BldWin32;

IMPORT BldQuake, BldQRep, QMachRep, FS, OSError, Arg, Utils, Wr, Quake;
IMPORT M3ID, QValue;

PROCEDURE DelFile(<* UNUSED *> t: BldQuake.T; x: TEXT)=
  BEGIN
    TRY
      FS.DeleteFile(x);
    EXCEPT OSError.E =>      
    END;
  END DelFile;

PROCEDURE LinkFile(t: BldQuake.T; from, to: TEXT)=
  BEGIN
    BldQuake.CopyIfNew(t, from, to);
  END LinkFile;

PROCEDURE MakeExec(<* UNUSED *> t: BldQuake.T; <* UNUSED *> script: TEXT)=
  BEGIN
  END MakeExec;

BEGIN
END BldWin32.
