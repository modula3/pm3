MODULE BldPosix;

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
  VAR args := Arg.NewList();
  BEGIN
    t.delete_file(t, to);
    Arg.Append(args, "-s");
    Arg.Append(args, from);
    Arg.Append(args, to);
    EVAL Utils.Execute("ln", args, NIL, FALSE); <* NOWARN *>
  END LinkFile;

PROCEDURE MakeExec(<* UNUSED *> t: BldQuake.T; script: TEXT)=
  VAR args := Arg.NewList();
  BEGIN
    Arg.Append(args, "+x");
    Arg.Append(args, script);
    EVAL Utils.Execute("chmod", args, NIL, TRUE); <* NOWARN *>
  END MakeExec;

PROCEDURE MakeDir(t: BldQuake.T; dir: TEXT) RAISES {Quake.Error} =
  VAR args := Arg.NewList(); val: QValue.T;
  BEGIN
    IF t.get(M3ID.Add("_quiet"), val) THEN
      Wr.PutText(t.cur_wr(), "m3mkdir" & dir & t.CR) END; <* NOWARN *>
    Arg.Append(args, dir);
    EVAL Utils.Execute(t.LIB_USE & "/m3mkdir", args, NIL, TRUE); <* NOWARN *>
  END MakeDir;

BEGIN

END BldPosix.
