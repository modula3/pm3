MODULE Assembly;

IMPORT Wr, Stdio, Globals, Thread;
FROM IO IMPORT Put, PutInt;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE PutString (READONLY a: ARRAY OF CHAR) =
  BEGIN
    Wr.PutString(Stdio.stdout, a);
  END PutString;

PROCEDURE DoNothing (self: T) =
  BEGIN
    IF self.id < 0 THEN
      Put("DoNothing: negative id.\n");
    END;
    IF Globals.debugMode THEN
      Put("==> DoNothing(id = "); PutInt(self.id);
      Put(", type = "); PutString(self.type);
      Put(", buildDate = "); PutInt(self.buildDate); Put(")\n");
    END
  END DoNothing;

BEGIN
END Assembly.
