MODULE BenchmarkLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.2  1997/02/20 16:08:16  roland
    OO1 rewritten with graphical user interface.

    Revision 1.1  1996/10/08 10:37:32  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT Text, Time, Wr, FmtTime, Thread;

CONST MinLen = 5;

TYPE WriterArray = REF ARRAY OF Wr.T;

REVEAL
  T = Public BRANDED OBJECT
        num: CARDINAL    := 0;
        wrs: WriterArray := NIL;
      OVERRIDES
        open         := Open;
        openForN     := OpenForN;
        addWriter    := AddWriter;
        removeWriter := RemoveWriter;
        close        := Close;

        write      := Write;
        writeEvent := WriteEvent;
        writeTime  := WriteTime;
      END;

PROCEDURE Open (l: T; wr: Wr.T): T =
  BEGIN
    l.wrs := NEW(WriterArray, MinLen);
    l.wrs[0] := wr;
    l.num := 1;
    RETURN l;
  END Open;

PROCEDURE OpenForN (l: T; READONLY wrs: ARRAY OF Wr.T): T =
  BEGIN
    l.wrs := NEW(WriterArray, MAX(MinLen, NUMBER(wrs)));
    SUBARRAY(l.wrs^, 0, NUMBER(wrs)) := wrs;
    l.num := NUMBER(wrs);
    RETURN l;
  END OpenForN;

PROCEDURE AddWriter (l: T; wr: Wr.T) =
  BEGIN
    IF l.wrs = NIL THEN
      l.wrs := NEW(WriterArray, MinLen);
      l.wrs^[0] := wr;
      l.num := 1;
    ELSE
      IF l.num = NUMBER(l.wrs^) THEN
        WITH newWrs = NEW(WriterArray, NUMBER(l.wrs^) + MinLen) DO
          SUBARRAY(newWrs^, 0, l.num) := l.wrs^;
          l.wrs := newWrs;
        END;
      END;
      l.wrs^[l.num] := wr;
      INC(l.num);
    END;
  END AddWriter;

PROCEDURE RemoveWriter (l: T; wr: Wr.T) =
  VAR i: CARDINAL;
  BEGIN
    IF l.wrs # NIL THEN
      i := 0;
      WHILE i < l.num AND l.wrs^[i] # wr DO INC(i) END;
      IF i < l.num THEN
        FOR j := i TO l.num - 2 DO l.wrs^[j] := l.wrs^[j + 1] END;
        DEC(l.num);
      END;
    END;
  END RemoveWriter;

PROCEDURE Close (l: T) =
  BEGIN
    TRY
      IF l.wrs # NIL THEN
        FOR i := 0 TO l.num - 1 DO Wr.Flush(l.wrs^[i]); END;
        l.wrs := NIL;
      END;
    EXCEPT
    ELSE
    END;
  END Close;

PROCEDURE Write (l: T; t: TEXT) RAISES {Thread.Alerted} =
  BEGIN
    IF l.wrs # NIL THEN
      FOR i := 0 TO l.num - 1 DO
        TRY
          Wr.PutText(l.wrs[i], t);
          Wr.Flush(l.wrs[i])
        EXCEPT
          Wr.Failure =>          (* ignore *)
        END;
      END;
    END;
  END Write;

PROCEDURE WriteTime (l: T) RAISES {Thread.Alerted} =
  BEGIN
    Write(l, "Time: " & FmtTime.Long(Time.Now()) & "\n");
  END WriteTime;

PROCEDURE WriteEvent (l: T; descr: TEXT) RAISES {Thread.Alerted} =
  BEGIN
    Write(l, "event " & Text.Sub(FmtTime.Long(Time.Now()), 11, 8) & " "
               & descr & "\n");
  END WriteEvent;

BEGIN
END BenchmarkLog.
