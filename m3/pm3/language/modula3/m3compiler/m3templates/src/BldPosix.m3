MODULE BldPosix;

IMPORT BldQuake, BldQRep, FS, OSError, Arg, Utils;

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

BEGIN

END BldPosix.
