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

PROCEDURE MakeDir(t: BldQuake.T; dir: TEXT) RAISES {Quake.Error} =
  VAR args := Arg.NewList(); val: QValue.T;
  BEGIN
    IF t.get(M3ID.Add("_quiet"), val) THEN
      Wr.PutText(t.cur_wr(), "m3mkdir" & dir & t.CR) END; <* NOWARN *>
    Arg.Append(args, dir);
    EVAL Utils.Execute(t.LIB_USE & "\\m3mkdir", args, NIL, TRUE); <* NOWARN *>
  END MakeDir;

BEGIN
END BldWin32.
