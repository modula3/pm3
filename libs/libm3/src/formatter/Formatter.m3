(* Copyright(C) 1989, 1992, Digital Equipment Corporation                    *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Written in Modula-2+ by John Ellis before the dawn of history             *)
(* Converted to Modula-3 by Bill Kalsow on Oct 1989                          *)
(* Cleaned up and documented by J. Stolfi on Nov 1990                        *)

(* Last modified on Thu Jan 26 14:16:56 PST 1995 by kalsow                   *)
(*      modified on Thu Jan 28 10:45:08 PST 1993 by mjordan                  *)
(*      modified on Mon Jun 29 22:09:16 PDT 1992 by muller                   *)
(*      modified on Sun Feb 23 14:45:03 PST 1992 by meehan                   *)
(*      modified on Thu Jul 25 19:30:49 PDT 1991 by stolfi                   *)
(*      modified on Mon Apr 22 17:34:37 1991 by nichols@xerox.com            *)
(*      modified on Tue May  2 10:32:50 1989 by ellis                        *)

MODULE Formatter;

IMPORT Text, Wr, Thread, Scheduler;

<*FATAL Thread.Alerted *>

TYPE 
  (* RefStream = ARRAY [0..LAST(CARDINAL)] OF REFANY; *)

REVEAL
  T = BRANDED REF RECORD
      wr:          Wr.T;    
        
      width:       CARDINAL;   (* Current nominal line width *)
      indent:      CARDINAL;   (* Current offset for breaks *)

      chars:       CharBuf;    (* ExprBuf for compacting PutChar's *)
      numChars:    CARDINAL;   (* Number of chars in /chars/ *)

      cThread:     Thread.T;         (* Consumer thread *)
      lock:        Thread.Mutex;     (* Protects shared Producer/Consumer data *)
      changed:     Thread.Condition; (* Something changed *)

      (* stream:   RefStream; *)     (* Dummy: the input stream *)

      buffer:      ExprBuf;          (* Expression buffer *)
      bufSize:     CARDINAL;         (* NUMBER(buffer^) *)

      start:       ARRAY Who OF CARDINAL; (* Start of each thread's buffer area *)
      size:        ARRAY Who OF CARDINAL; (* Size of each thread's buffer area *)

      next:       CARDINAL;      (* Next undefined position of input stream *)

      pPutLim:    CARDINAL;      (* Time for Producer to put to Consumer *)
      cGetLim:    CARDINAL;      (* Time for Consumer to get from Producer  *)

      waiters:     CARDINAL;     (* Number of threads waiting for Allocate *)
      failure:     BOOLEAN;
      failureCode: REFANY;
    END;
  (*
    Note on the storage scheme:
    
    The two threads (Producer and Consumer) operate on the /input
    stream/, an ideal sequence /t.stream/ of REFANYs that encodes the
    test strings and formatting operations sent to the Formatter.
    
    Ideally the sequence /t.stream/ is infinite, but at any moment only
    its first /t.next/ elements have been specified by the client.
    By definition, /t.stream/ is modified only by the Producer (the
    client), which merely sets t.stream[t.next] to the appropriate
    value, and increments t.pnext.  The values of t.stream[i] for 
    i < t.next are never modified by anyone.
    
    At any moment, each thread "owns" a segment of the input stream
    t.stream.  The segment owned by thread /who/ starts at
    /t.start[who]/ and contain /t.size[who]/ characters.  The two
    segments are contiguous (i.e, t.start[Consumer] + t.size[Consumer]
    = t.start[Producer]), and their total size is equal to the
    t.bufSize.  The next undefined element t.next is either in the
    Producer's segment, or just beyond it.
    
    The two segments are stored circularly in the array /t.buffer/,
    which has length /t.bufSize/.  Specifically, element /i/ of the
    input stream is stored in /t.buffer[i MOD t.bufSize]/, for all /i/
    in the range
    
      [ t.start[Consumer] .. t.start[Producer] + t.size[Producer] ]

    The field /t.start[who]/ is modified only by te thread /who/ itself.
    The field /t.size[who]/ can be modified by either thread, and
    therefore should be accessed only with /t.lock/ held. (However, the
    other thread can only cause /t.size[who]/ to grow, never to shrink).
    For instance, every now and then the Producer will donate an initial
    piece of its segment to the Consumer.  Similarly, every now and
    then the Consumer will throw away an initial piece of its segment,
    and increase the Producer's /t.size/ so as to maintain the total
    size of th etwo segments equal to /t.bufSize/.
    
    Occasionally, the array /t.buffer/ may fill up; at that point it
    it will be expanded automatically, so as to preserve the invariants
    above.  This expansion happens only when both threads are 
    blocked in the same /Allocate/ procedure (below); at other 
    times, it is safe to reference /t.buffer/ and /t.bufSize/ 
    without acquiring /t.lock/. 
    
    In principle the Producer could just pass each input item directly
    to the Consumer, and keep its input stream segment always
    empty.  The reason for not doing so is efficiency: the lock must
    be acquired for each batch.
    
    The Consumer needs random access to its stream segment because it
    has to do unbounded lookahead.   *)

TYPE
  ExprBuf = REF ARRAY OF REFANY;
  CharBuf = ARRAY [0..255] OF CHAR;
  Who    = {Producer, Consumer};

  ConsumerThreadClosure = Thread.SizedClosure OBJECT t: T; METHODS END;

CONST ChunkSize = 128; (* Must be a power of 2 *)

EXCEPTION (* Client errors: *)
  UnmatchedEnd;     (* /End/ without matching /Group/, /Align/, etc. *)
  InvalidAlignRow;  (* A row of an /Align/ is neither a /Group/.../End/ nor /NoAlign/ *)
  
TYPE
  Char  = REF CHAR;
  Int   = REF INTEGER;
  Bool  = REF BOOLEAN;

VAR (*CONST*)
  firstInt: Int;
  lastInt: Int;
  ints: ARRAY [-256..256] OF Int;
  chars: ARRAY CHAR OF Char;
  bools: ARRAY BOOLEAN OF Bool;
  breakTypes: ARRAY BreakType OF REF BreakType;

TYPE
  OpProc = PROCEDURE (t: T; m: Mode; VAR p: Position; i, x: CARDINAL): BOOLEAN
              RAISES {Wr.Failure, Thread.Alerted};

  Op = REF RECORD
      proc: OpProc;
      args: CARDINAL;
      precedence: INTEGER;
    END;

VAR
  GroupOp:        Op;
  BeginOp:        Op;
  AlignOp:        Op;
  ColOp:          Op;
  CharOp:         Op;
  TextOp:         Op;
  NoAlignOp:      Op;
  BreakOp:        Op;
  PartialBreakOp: Op;
  NewLineOp:      Op;
  UnitedBreakOp:  Op;
  EndOp:          Op;
  FlushOp:        Op;

(**********************************************************)
(* PRODUCER-SIDE OPERATIONS                               *)
(**********************************************************)

PROCEDURE New(wr: Wr.T;  width: CARDINAL:= 75): T RAISES {} =
  BEGIN
    WITH t = NEW(T) DO
      t.wr := wr;
      t.width := width;
      t.indent := 0;
  
      t.numChars := 0;
  
      (* t.stream := RefStream{NIL, ..}; *)
  
      t.buffer := NEW (ExprBuf, 4 * ChunkSize);
      t.bufSize := NUMBER (t.buffer^);
      
      t.start[Who.Producer]:= 0;
      t.size[Who.Producer]:= NUMBER (t.buffer^);
  
      t.start[Who.Consumer]:= 0;
      t.size[Who.Consumer]:= 0;
  
      t.next := 0;
  
      t.pPutLim := 0;
  
      t.cGetLim := 0;
  
      t.lock := NEW (Thread.Mutex);
      t.changed := NEW (Thread.Condition);
  
      t.failure := FALSE;
  
      t.cThread := Thread.Fork(
        NEW (ConsumerThreadClosure, 
          apply := PrintTop, 
          t := t, 
          stackSize := 100000
        )
      );
  
      RETURN t
    END
  END New;

PROCEDURE UnderlyingWr(t: T): Wr.T RAISES {} =
  BEGIN
    RETURN t.wr;
  END UnderlyingWr;

PROCEDURE Flush(t: T) RAISES {Wr.Failure} =
  <*FATAL Thread.Alerted*>
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, FlushOp);
    WaitUntilEmpty(t, t.next);
    Wr.Flush(t.wr);
  END Flush;

PROCEDURE Close(t: T) RAISES {Wr.Failure} =
  BEGIN
    Flush(t);
    Thread.Alert(t.cThread);
    IF NIL = Thread.Join(t.cThread) THEN END;
  END Close;

PROCEDURE Group(t: T) RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, GroupOp);
  END Group;

PROCEDURE Begin(t: T;  offset := 0;  width: CARDINAL:= LAST (CARDINAL))
  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, BeginOp);
    AddRef(t, NewInt(offset));
    AddRef(t, NewInt(width));
  END Begin;

PROCEDURE End(t: T) RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, EndOp);
  END End;

PROCEDURE PutChar(t: T;  c: CHAR) RAISES {Wr.Failure} =
  BEGIN
    IF c = '\n' THEN
      NewLine(t, FIRST (INTEGER), FALSE);
    ELSIF c = ' ' THEN
      IF t.numChars > 0 THEN AddChars(t) END;
      AddRef(t, chars[c]);
    ELSE
      IF t.numChars >= NUMBER (t.chars) THEN AddChars(t) END;
      t.chars[t.numChars]:= c;
      INC (t.numChars);
    END;
  END PutChar;

PROCEDURE PutText(t: T;  text: Text.T;  raw := FALSE) RAISES {Wr.Failure} =
  BEGIN
    IF raw THEN
      IF t.numChars > 0 THEN AddChars(t) END;
      AddRef(t, text);
    ELSE
      IF t.numChars > 0 THEN AddChars(t) END;
      FOR i := 0 TO Text.Length(text) - 1 DO
        PutChar(t, Text.GetChar(text, i));
      END;
      IF t.numChars > 0 THEN AddChars(t) END;
    END;
  END PutText;

PROCEDURE Break (t: T;  offset := 0;  type := BreakType.OptimalBreak;  freshLine := TRUE)
  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars (t) END;
    AddRef(t, BreakOp);
    AddRef(t, NewInt (offset));
    AddRef(t, breakTypes[type]);
    AddRef(t, bools[freshLine]);
  END Break;

PROCEDURE PartialBreak(t: T;  offset := 0;  freshLine := TRUE)
  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, PartialBreakOp);
    AddRef(t, NewInt(offset));
    AddRef(t, bools[freshLine]);
  END PartialBreak;

PROCEDURE UnitedBreak(t: T;  offset := 0;  freshLine := TRUE)
  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, UnitedBreakOp);
    AddRef(t, NewInt(offset));
    AddRef(t, bools[freshLine]);
  END UnitedBreak;

PROCEDURE NewLine(t: T;  offset := 0;  freshLine := TRUE)
  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, NewLineOp);
    AddRef(t, NewInt(offset));
    AddRef(t, bools[freshLine]);
  END NewLine;

PROCEDURE Align(
    t: T;
    columns: CARDINAL;
    tryOneLine: BOOLEAN:= TRUE;
    offset: INTEGER:= 0;
    alignPred: AlignPred := NIL;
  )  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, AlignOp);
    AddRef(t, NewInt(columns));
    AddRef(t, bools[tryOneLine]);
    AddRef(t, NewInt(offset));
    AddRef(t, alignPred);
  END Align;

PROCEDURE NoAlign(t: T) RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, NoAlignOp);
  END NoAlign;

PROCEDURE Col (t: T; column: INTEGER; relative := FALSE; space: CARDINAL := 0)
  RAISES {Wr.Failure} =
  BEGIN
    IF t.numChars > 0 THEN AddChars(t) END;
    AddRef(t, ColOp);
    AddRef(t, NewInt(column));
    AddRef(t, bools[relative]);
    AddRef(t, NewInt(space));
  END Col;

PROCEDURE AddRef(t: T; ref: REFANY) RAISES {Wr.Failure, Thread.Alerted} =
  (*
    Adds an element to the Producer buffer.  If the buffer gets big
    enough, or the added element is the "Flush" operator, then reelases
    the buffer's contents to the Consumer thread.  *)
  VAR pFree: CARDINAL;
  BEGIN
    IF t.next = t.pPutLim THEN
      (* Time to send data to Consumer, and get some more space: *)
      pFree := Release(t, Who.Producer, t.pPutLim - t.start[Who.Producer]);
      IF pFree = 0 THEN
        pFree := Allocate(t, Who.Producer, ChunkSize);
      END;
      <* ASSERT t.start[Who.Producer] = t.next *>
      t.pPutLim := t.start[Who.Producer] + MIN(pFree, ChunkSize);
    END;

    (* t.stream[t.next] := ref; *)

    t.buffer[t.next MOD t.bufSize] := ref;
    t.next := t.next + 1;

    IF (t.next = t.pPutLim) OR (ref = FlushOp) THEN
      pFree := Release(t, Who.Producer, t.next - t.start[Who.Producer]);
      <* ASSERT t.start[Who.Producer] = t.next *>
      t.pPutLim := t.start[Who.Producer] + MIN(pFree, ChunkSize);
    END;
  END AddRef;

PROCEDURE AddChars(t: T) RAISES {Wr.Failure} =
  BEGIN
    AddRef(t, Text.FromChars(SUBARRAY (t.chars, 0, t.numChars)));
    t.numChars := 0;
  END AddChars;

(**********************************************************)
(* BUFFERING                                              *)
(**********************************************************)

PROCEDURE Release(
    t: T;  
    this: Who;  
    size: CARDINAL
  ): CARDINAL RAISES {Wr.Failure} =
  (*
    Releases the oldest /size/ elements of the input stream segment
    belonging to thread /this/.  If /this=Producer/, those elements
    are appended to the Consumer's segment. If /this=Consumer/, the
    elements are discarded, and the Producer's segment is extended
    forward so as to use up the space thus vacated in /t.buffer/. 
    
    Returns a lower bound /n/ for the final value t.size[this].
    It is only an estimate, because the other thread may 
    cause t.size[this] grow at any time. *)
  VAR that: Who;  
      newSize: CARDINAL;
  BEGIN
    IF this = Who.Producer
      THEN that := Who.Consumer;
      ELSE that := Who.Producer;
    END;
    LOCK t.lock DO
      t.start[this]:= t.start[this] + size;
      DEC (t.size[this], size);
      INC (t.size[that], size);
      newSize := t.size[this];
      CheckForFailure(t, this);
    END;
    Changed (t);
    RETURN newSize;
  END Release;

PROCEDURE Allocate(
    t: T;
    this: Who;
    minSize: CARDINAL;
  ): CARDINAL RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Ensures that /this/'s segment of the input stream contains at least 
    /minSize/ elements. 
    
    Returns a lower bound /n/ to the final value of t.size[this].
    The value is only a lower bound, because the other thread may 
    cause /t.size[this]/ to grow at any time. *)
  BEGIN
    LOCK t.lock DO
      IF t.size[this] < minSize AND (NOT t.failure) THEN
        INC (t.waiters);
        WHILE (t.size[this] < minSize) AND (NOT t.failure) DO
          IF t.waiters = 2 THEN Expand(t) END;
          Thread.AlertWait(t.lock, t.changed);
        END;
        DEC (t.waiters);
      END;
      CheckForFailure(t, this);
      RETURN t.size[this]
    END;
  END Allocate;

PROCEDURE Expand(t: T) RAISES {} =
  (*
    Expands t.buffer, preserving its contents.
    Safe only if both producer and consumer are blocked in /Allocate/.
    LL >= t.lock *)
  VAR from, to: CARDINAL;
  BEGIN
    <* ASSERT t.waiters = 2 *>
    WITH 
      oldSize = t.bufSize + 0,
      newSize = 2 * oldSize,
      newBuffer = NEW (ExprBuf, newSize)
    DO
      from := t.start[Who.Consumer] MOD oldSize;
      to := t.start[Who.Consumer] MOD newSize;
      FOR i := 0 TO oldSize - 1 DO
        newBuffer[to]:= t.buffer[from];
        from := from + 1;  
        IF from = oldSize THEN from := 0 END;
        to := to + 1;  
        IF to = newSize THEN to := 0 END;
      END;
      t.buffer := newBuffer;
      t.bufSize := newSize;
      INC (t.size[Who.Producer], newSize - oldSize);
      Changed (t);
    END
  END Expand;

PROCEDURE Changed (t: T) RAISES {} =
  BEGIN
    Thread.Broadcast (t.changed);
    Scheduler.Yield ();
  END Changed;

PROCEDURE WaitUntilEmpty(t: T;  index: CARDINAL) RAISES {Wr.Failure} =
  (*
    Blocks until the Consumer thread eats the input stream 
    up to but not including t.stream[index]. *)
  BEGIN
    LOCK t.lock DO
      WHILE (t.start[Who.Consumer] < index) AND (NOT t.failure) DO
        Thread.Wait(t.lock, t.changed);
      END;
      CheckForFailure(t, Who.Producer);
    END;
  END WaitUntilEmpty;

(**********************************************************)
(* FAILURE CODES                                          *)
(**********************************************************)

(*
  These procedures are used to pass Wr.Failure codes from 
  Consumer to Producer. *)

PROCEDURE SetFailure(t: T;  failureCode: REFANY) RAISES {} =
  BEGIN
    LOCK t.lock DO
      t.failure := TRUE;
      t.failureCode := failureCode;
    END;
    Changed (t);
  END SetFailure;

PROCEDURE CheckForFailure(t: T;  who: Who) RAISES {Wr.Failure} =
  BEGIN
    IF (who = Who.Producer) AND (t.failure) THEN
      RAISE Wr.Failure(t.failureCode);
    END;
  END CheckForFailure;

(**********************************************************)
(* CONSUMER-SIDE OPERATIONS                               *)
(**********************************************************)

TYPE
  Mode = {
    Thinking, (* Computing the displacement if we were to print *)
    Writing   (* Actually printing *)
  };

(*
  Invariant:  mode = Writing => maxL = LAST(INTEGER) 
  *)

<*INLINE*> 
PROCEDURE Probe(t: T; i: CARDINAL) RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Blocks until item t.stream[i] is in Consumer's segment *)
  BEGIN
    IF i >= t.cGetLim THEN
      WITH 
        cSize = Allocate(t, Who.Consumer, i - t.start[Who.Consumer] + 1)
      DO
        t.cGetLim := t.start[Who.Consumer] + cSize;
        <* ASSERT t.cGetLim >= i *>
      END
    END;
  END Probe;

<*INLINE*> 
PROCEDURE Get(t: T; i: CARDINAL): REFANY RAISES {} =
  BEGIN
    RETURN t.buffer[i MOD t.bufSize];
  END Get;

<*INLINE*> 
PROCEDURE GetB (t: T; i: CARDINAL): BOOLEAN RAISES {} =
  BEGIN
    RETURN NARROW (t.buffer[i MOD t.bufSize], Bool)^;
  END GetB;

<*INLINE*> 
PROCEDURE GetI (t: T; i: CARDINAL): INTEGER RAISES {} =
  BEGIN
    RETURN NARROW (t.buffer[i MOD t.bufSize], Int)^;
  END GetI;

<*INLINE*> 
PROCEDURE GetBreakType (t: T; i: CARDINAL): BreakType RAISES {} =
  BEGIN
    RETURN NARROW (t.buffer[i MOD t.bufSize], REF BreakType)^;
  END GetBreakType;

PROCEDURE PeekOp(t: T; i: CARDINAL): Op
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    WITH r = Peek(t, i) DO
      TYPECASE r OF
      | NULL   => RETURN TextOp;
      | Text.T => RETURN TextOp;
      | Op(op) => RETURN op;
      | Char   => RETURN CharOp;
      ELSE
        <* ASSERT FALSE *>
      END
    END
  END PeekOp;

PROCEDURE Peek(t: T; i: CARDINAL): REFANY
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Probe(t, i);
    RETURN Get(t, i);
  END Peek;

TYPE
  Position = RECORD
      l: CARDINAL; (* current line of current object, 0-based *)
      c: CARDINAL; (* current column, 0-based *)
      b: CARDINAL; (* trailing blanks after .c *)
      i: CARDINAL; (* next item in buffer *)
    END;

PROCEDURE PrintTop(self: ConsumerThreadClosure): REFANY RAISES {} =
  VAR pos: Position;
  (*
    The printing thread *)
  BEGIN
    TRY
      pos.i := 0;
      pos.l := 0;
      pos.c := 0;
      pos.b := 0;
      LOOP
        EVAL PrintUntil(self.t, Mode.Writing, pos, LAST (INTEGER), FlushOp);
      END;
    EXCEPT
    | Thread.Alerted          => (* exit loop *)
    | Wr.Failure(failureCode) => SetFailure(self.t, failureCode);
    END;
    RETURN NIL;
  END PrintTop;

(*
  The various Print* procedures below return TRUE if mode=Mode.Thinking
  and the stuff to be printed does not fit in the current line. *)

PROCEDURE Print(
    t: T;  
    mode: Mode; 
    VAR pos: Position; 
    maxL: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints the next expression from the input stream *)
  VAR result: BOOLEAN;
      flushed: BOOLEAN := FALSE;
      i: CARDINAL;
  BEGIN
    <* ASSERT mode = Mode.Thinking OR maxL = LAST (INTEGER) *>
    i := pos.i;
    Probe(t, i);
    TYPECASE Get(t, i) OF
    | Text.T(text) =>
          pos.i := i + 1;
          result := DoPrintText(t, mode, pos, text);
    | Op(op) =>
          WITH opArgs = op.args DO
            pos.i := i + 1 + opArgs;
            IF (opArgs > 0) THEN Probe(t, i + opArgs) END;
          END;
          result := op.proc(t, mode, pos, maxL, i+1);
          flushed := (op = FlushOp)
    | Char(ch) =>
          pos.i := i + 1;
          result := DoPrintChar(t, mode, pos, maxL, ch);
    ELSE
      <* ASSERT FALSE *>
    END;
    IF (mode = Mode.Writing)
    AND (flushed OR (pos.i >= t.start[Who.Consumer] + ChunkSize)) THEN
      WITH 
        cSize = Release(t, Who.Consumer, pos.i - t.start[Who.Consumer])
      DO
        <* ASSERT t.start[Who.Consumer] = pos.i *>
        t.cGetLim := t.start[Who.Consumer] + cSize
      END
    END;

    RETURN result;
  END Print;

PROCEDURE PrintRest(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    op: Op
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints expressions from the input stream up to (but not including)
    the first operator with precedence less than or equal to op.  *)
  BEGIN
    LOOP
      WITH nextOp = PeekOp(t, pos.i) DO
        IF nextOp.precedence <= op.precedence THEN EXIT END;
        IF Print(t, mode, pos, maxL) THEN RETURN TRUE END;
      END
    END;
    RETURN FALSE;
  END PrintRest;

PROCEDURE PrintUntil(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    op: Op
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints expressions from the input stream up to the first operator
    /next/ with precedence less than or equal to /op/, inclusive.
    Requires that /next/ be /op/ or a FlushOp.  *)
  VAR nextOp: Op;
      lastOp: Op := NIL; (*DEBUG*)
  BEGIN
    LOOP
      nextOp := PeekOp(t, pos.i);
      IF nextOp.precedence <= op.precedence THEN EXIT END;
      IF Print(t, mode, pos, maxL) THEN RETURN TRUE END;
      lastOp := nextOp; (*DEBUG*) 
    END;
    IF nextOp = FlushOp THEN
      IF (op = FlushOp) THEN
        EVAL Print(t, mode, pos, maxL)
      END;
    ELSE
      <* ASSERT nextOp = op *>
      pos.i := pos.i + 1;
    END;
    RETURN FALSE;
  END PrintUntil;

PROCEDURE PrintGroup(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    <*UNUSED*> args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints expressions from the input stream up to the first operator
    /EndOp/ or /FlushOp/.  *)
  BEGIN
    RETURN PrintUntil(t, mode, pos, maxL, EndOp);
  END PrintGroup;

TYPE 
  BeginState = RECORD
      saveIndent: INTEGER;
      saveWidth: INTEGER;
      saveL: INTEGER;
    END;

PROCEDURE PrintBegin(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints expressions from the input stream up to the first operator
    /EndOp/ or /FlushOp/, modifying locally the /offset/ and /width/.  *)
  VAR beginState: BeginState;  
      b: BOOLEAN;  
  BEGIN
    WITH
      offset = GetI (t, args),
      width = GetI (t, args + 1)
    DO
      EnterBegin(t, mode, pos, maxL, offset, width, beginState);
      b := PrintUntil(t, mode, pos, maxL, EndOp);
      ExitBegin(t, mode, pos, maxL, beginState);
      RETURN b
    END
  END PrintBegin;

PROCEDURE EnterBegin(
    t: T;
    mode: Mode;
    VAR pos: Position;
    VAR maxL: CARDINAL;
    offset: INTEGER;
    width: CARDINAL;
    VAR beginState: BeginState
  ) RAISES {} =
  BEGIN
    beginState.saveIndent := t.indent;
    beginState.saveWidth := t.width;
    beginState.saveL:= pos.l;
    IF width < LAST (INTEGER) THEN t.width := width END;
    IF offset < LAST (INTEGER) THEN
      t.indent := MAX (0, pos.c + pos.b + offset);
    END;
    IF mode = Mode.Thinking THEN maxL:= MAX (0, maxL - pos.l) END;
    pos.l := 0;
  END EnterBegin;

PROCEDURE ExitBegin(
    t: T;
    <*UNUSED*> mode: Mode;
    VAR pos: Position;
    <*UNUSED*> VAR maxL: CARDINAL;
    VAR beginState: BeginState
  ) RAISES {} =
  BEGIN
    pos.l := pos.l + beginState.saveL;
    t.indent := beginState.saveIndent;
    t.width := beginState.saveWidth;
  END ExitBegin;

PROCEDURE PrintEnd(
    <*UNUSED*> t: T;
    <*UNUSED*> mode: Mode;
    <*UNUSED*> VAR pos: Position;
    <*UNUSED*> maxL: CARDINAL;
    <*UNUSED*> args: CARDINAL
  ): BOOLEAN
  RAISES {} =
  (*
    /EndOp/s should have been gobbled up by the respective /Begin/s,
    /Group/s, and /Align/s. *)
  <*FATAL UnmatchedEnd*>
  BEGIN
    RAISE UnmatchedEnd;
    (* RETURN FALSE; *)
  END PrintEnd;

PROCEDURE PrintFlush(
    <*UNUSED*> t: T;
    <*UNUSED*> mode: Mode;
    <*UNUSED*> VAR pos: Position;
    <*UNUSED*> maxL: CARDINAL;
    <*UNUSED*> args: CARDINAL
  ): BOOLEAN RAISES {} =
  (*
    Prints a /Flush/ operator.  Other print procedures should
    watch for it. *)
  BEGIN
    RETURN FALSE;
  END PrintFlush;

PROCEDURE PrintText(
    t: T;
    mode: Mode;
    VAR pos: Position;
    <*UNUSED*> maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints a text string. *)
  BEGIN
    RETURN DoPrintText(t, mode, pos, Get(t, args));
  END PrintText;

PROCEDURE DoPrintText(
    t: T;
    mode: Mode;
    VAR pos: Position;
    text: Text.T
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF pos.b > 0 THEN DoTrailingBlanks(t, mode, pos) END;
    IF mode = Mode.Writing THEN Wr.PutText(t.wr, text) END;
    pos.c := pos.c + Text.Length(text);
    RETURN (mode = Mode.Thinking) AND (pos.c > t.width);
  END DoPrintText;

PROCEDURE PrintChar(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    RETURN DoPrintChar(t, mode, pos, maxL, Get(t, args));
  END PrintChar;

PROCEDURE DoPrintChar(
    t: T;
    mode: Mode;
    VAR pos: Position;
    <*UNUSED*> maxL: CARDINAL;
    ch: Char
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF ch^ = '\n' THEN
      <* ASSERT FALSE *>
      (* RETURN DoNewLine(t, mode, pos, maxL, FIRST (INTEGER)); *)
    ELSIF ch^ = ' ' THEN
      pos.b := pos.b + 1;
      RETURN FALSE;
    ELSE
      IF pos.b > 0 THEN DoTrailingBlanks(t, mode, pos) END;
      IF mode = Mode.Writing THEN Wr.PutChar(t.wr, ch^) END;
      pos.c := pos.c + 1;
      RETURN (mode = Mode.Thinking) AND (pos.c > t.width);
    END;
  END DoPrintChar;

PROCEDURE DoTrailingBlanks(
    t: T;
    mode: Mode;
    VAR pos: Position
    ) RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF mode = Mode.Writing THEN
      FOR i := 1 TO pos.b DO Wr.PutChar(t.wr, ' ') END;
    END;
    pos.c := pos.c + pos.b;
    pos.b := 0;
  END DoTrailingBlanks;

PROCEDURE PrintBreak(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints a /Break/ operator and all succeeding expressions
    until next line break (of any kind), /End/, or /Flush/. *)
  BEGIN
    WITH
      offset = GetI (t, args),
      type = GetBreakType (t, args + 1),
      freshLine = GetB (t, args + 2)
    DO
      IF type = BreakType.NonOptimal THEN
        RETURN DoNonOptimalBreak(t, mode, pos, maxL, offset, freshLine);
      ELSE
        RETURN DoOptimalBreak(t, mode, pos, maxL, offset, freshLine, type)
      END
    END
  END PrintBreak;

PROCEDURE DoNonOptimalBreak(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    offset: INTEGER;
    freshLine: BOOLEAN;
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  VAR pos1: Position; b1: BOOLEAN;
  BEGIN
    (* See if it fits in current line: *)
    pos1:= pos;
    b1 := PrintRest(t, Mode.Thinking, pos1, pos1.l, BreakOp);
    IF NOT b1 AND mode = Mode.Thinking THEN
      pos := pos1;
      RETURN FALSE;
    ELSIF b1 AND DoLine(t, mode, pos, maxL, offset, freshLine) THEN
      RETURN TRUE
    ELSE
      RETURN PrintRest(t, mode, pos, maxL, BreakOp);
    END;
  END DoNonOptimalBreak;

PROCEDURE DoOptimalBreak(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    offset: INTEGER;
    freshLine: BOOLEAN;
    type: BreakType;
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  VAR pos1, pos2: Position;
      willOverflowSameLine, willOverflowOnNewLine: BOOLEAN;
      doBreak: BOOLEAN;
  BEGIN
    (* Check what would happen if we did NOT break here: *)
    pos1:= pos;
    willOverflowSameLine := PrintRest(t, Mode.Thinking, pos1, maxL, BreakOp);
    IF (NOT willOverflowSameLine) AND (pos1.l = pos.l) THEN
      IF mode = Mode.Writing THEN
        RETURN PrintRest(t, mode, pos, maxL, BreakOp);
      ELSE
        pos := pos1;
        RETURN FALSE;
      END;
    END;
    
    (* Check what would happen if we DID break here: *)
    pos2:= pos;
    willOverflowOnNewLine:= 
      DoLine(t, Mode.Thinking, pos2, maxL, offset, freshLine) OR 
      PrintRest(t, Mode.Thinking, pos2, maxL, BreakOp);

    (* Compare the two outcomes, and do the best: *)
    doBreak := 
      ( willOverflowSameLine AND willOverflowOnNewLine ) OR 
      ( (NOT willOverflowOnNewLine) AND 
        ( willOverflowSameLine OR 
          (pos2.l < pos1.l) OR 
          (pos2.l = pos1.l AND pos2.c <= t.width AND type = BreakType.OptimalBreak)
        )
      );
    IF mode = Mode.Writing THEN
      IF doBreak THEN
        RETURN DoLine(t, mode, pos, maxL, offset, freshLine)
      ELSE
        RETURN PrintRest(t, mode, pos, maxL, BreakOp)
      END;
    ELSE
      IF doBreak THEN  
        pos := pos2;  
        RETURN willOverflowOnNewLine;
      ELSE  
        pos := pos1;  
        RETURN willOverflowSameLine;
      END;
    END
  END DoOptimalBreak;

PROCEDURE PrintPartialBreak(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints a /PartialBreak/ operator and all succeeding expressions
    or /Break/s until next /PartialBreak/, /NewLine/, /UnitedBreak/,
    /End/, or /Flush/.  *)
  BEGIN
    WITH
      offset = GetI (t, args),
      freshLine = GetB (t, args + 1)
    DO
      RETURN (pos.l > 0) AND DoLine(t, mode, pos, maxL, offset, freshLine)
    END
  END PrintPartialBreak;

PROCEDURE PrintCol (
    t: T;
    <*UNUSED*> mode: Mode;
    VAR pos : Position;
    <*UNUSED*> maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {} =
  VAR column := GetI(t, args);
  BEGIN
    WITH
      relative = GetB(t, args + 1),
      space = GetI(t, args + 2)
    DO
      IF relative THEN INC(column, t.indent); END;
      IF pos.c + pos.b < column THEN
        pos.b := column - pos.c;
      ELSE
        INC(pos.b, space);
      END
    END;
    RETURN FALSE;
  END PrintCol;

PROCEDURE PrintNewLine(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints a /NewLine/ operator and all succeeding expressions, 
    including /Break/s and /PartialBreaks/, up to the next 
    /NewLine/, /UnitedBreak/, /End/, or /Flush/. *)
  VAR offset: INTEGER;  freshLine: BOOLEAN;
  BEGIN
    offset := GetI (t, args);
    freshLine := GetB (t, args + 1);
    RETURN DoLine(t, mode, pos, maxL, offset, freshLine);
  END PrintNewLine;

PROCEDURE DoLine(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    offset: INTEGER;
    freshLine: BOOLEAN
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF freshLine THEN
      RETURN DoFreshLine(t, mode, pos, maxL, offset);
    ELSE
      RETURN DoNewLine(t, mode, pos, maxL, offset);
    END;
  END DoLine;

PROCEDURE DoNewLine(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    offset: INTEGER
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    IF mode = Mode.Writing THEN Wr.PutChar(t.wr, '\n') END;
    pos.c := 0;
    pos.b := MAX (0, t.indent + offset);
    pos.l := pos.l + 1;
    RETURN (mode = Mode.Thinking) AND (pos.l > maxL);
  END DoNewLine;

PROCEDURE DoFreshLine(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    offset: INTEGER
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  VAR b: CARDINAL;
  BEGIN
    b := MAX (0, t.indent + offset);
    IF b < pos.c + pos.b THEN
      IF mode = Mode.Writing THEN Wr.PutChar(t.wr, '\n') END;
      pos.c := 0;
      pos.b := b;
      pos.l := pos.l + 1;
    END;
    RETURN (mode = Mode.Thinking) AND ((pos.l > maxL) OR (pos.c > t.width));
  END DoFreshLine;

PROCEDURE PrintUnitedBreak(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  (*
    Prints a /UnitedBreak/ operator and all succeeding expressions
    or line breaks until next /End/, or /Flush/.  *)
  VAR pos1: Position; 
      doBreak: BOOLEAN; 
      refany: REFANY;
      offset: INTEGER;  
      freshLine: BOOLEAN;
  BEGIN
    offset := GetI (t, args);
    freshLine := GetB (t, args + 1);
    doBreak := FALSE;
    pos1:= pos;
    LOOP
      IF (pos1.l > 0)
      OR PrintRest(t, Mode.Thinking, pos1, pos.l, UnitedBreakOp) THEN
        doBreak := TRUE;
        EXIT;
      END;
      IF Peek(t, pos1.i) # UnitedBreakOp THEN EXIT END;
      pos1.i := pos1.i + 3;
    END;
    IF (mode = Mode.Thinking) AND (maxL <= pos.l) THEN
      pos := pos1;
      RETURN doBreak;
    END;
    LOOP
      IF doBreak THEN
        IF DoLine(t, mode, pos, maxL, offset, freshLine) THEN
          RETURN TRUE;
        END;
      END;
      IF PrintRest(t, mode, pos, maxL, UnitedBreakOp) THEN RETURN TRUE END;
      IF Peek(t, pos.i) # UnitedBreakOp THEN RETURN FALSE END;
      refany := Peek(t, pos.i);
      offset := NARROW (Peek(t, pos.i + 1), Int)^;
      freshLine := NARROW (Peek(t, pos.i + 2), Bool)^;
      pos.i := pos.i + 3;
    END;
  END PrintUnitedBreak;

TYPE Widths = REF ARRAY OF INTEGER;

PROCEDURE PrintAlign(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    oneLine: BOOLEAN;
    beginState: BeginState;
    pos1: Position;
    start: CARDINAL;
    endRun: CARDINAL;
    maxWidths: Widths;
    widths: Widths;
    op: Op;
    columns: INTEGER;
    tryOneLine: BOOLEAN;
    offset: INTEGER;
    alignPred: AlignPred;
  BEGIN
    columns := GetI (t, args);
    tryOneLine := GetB (t, args + 1);
    offset := GetI (t, args + 2);
    alignPred := Get  (t, args + 3);
    TRY
      oneLine := tryOneLine AND (pos.l = 0);
      EnterBegin(t, mode, pos, maxL, 0, t.width, beginState);

      maxWidths := NEW (Widths, columns);
      widths := NEW (Widths, columns);

      IF oneLine THEN
        pos1:= pos;
        IF NOT PrintUntil(t, Mode.Thinking, pos1, pos.l, EndOp) THEN
          IF mode = Mode.Writing THEN
            RETURN PrintUntil(t, mode, pos, maxL, EndOp);
          ELSE
            pos := pos1;
            RETURN FALSE;
          END;
        END;
      END;

      start := pos.i;
      LOOP (* While another run of rows exists *)
        op := PeekOp(t, pos.i);
        IF (op = EndOp) OR (op = FlushOp) THEN EXIT END;

        pos1:= pos;
        LOOP (* While there are more rows in the run *)
          endRun := pos1.i;
          IF (op = EndOp) OR (op = FlushOp) THEN EXIT END;
          IF op = NoAlignOp THEN
            IF endRun = pos.i THEN endRun := endRun + 1 END;
            EXIT;
          END;
          IF (pos1.i > start) AND DoNewLine(t, Mode.Thinking, pos1, maxL, 0) THEN
            pos := pos1;
            RETURN TRUE;
          END;
          IF PrintRow(t, Mode.Thinking, pos1, pos1.l, maxWidths, widths, offset, alignPred) THEN
            IF endRun = pos.i THEN endRun := pos1.i END;
            EXIT;
          END;
          op := PeekOp(t, pos1.i);
        END;

        LOOP (* While there are more rows in the run *)
          IF pos.i >= endRun THEN EXIT END;
          IF Peek(t, pos.i) = NoAlignOp THEN
            IF Print(t, mode, pos, maxL) THEN RETURN TRUE END;
          ELSIF ((pos.i > start) AND DoNewLine(t, mode, pos, maxL, 0))
          OR PrintRow(t, mode, pos, maxL, maxWidths, widths, offset, NIL) THEN
            RETURN TRUE;
          END;
        END;

        ClearWidths(maxWidths);
      END;

      IF op = EndOp THEN pos.i := pos.i + 1 END;
      RETURN FALSE;

    FINALLY

      ExitBegin(t, mode, pos, maxL, beginState);

    END;
  END PrintAlign;

PROCEDURE PrintRow(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    maxWidths: Widths;
    widths: Widths;
    offset: INTEGER;
    alignPred: AlignPred;
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  <*FATAL InvalidAlignRow*>
  VAR
    beginState: BeginState;
    col: CARDINAL;
    start: CARDINAL;
    op: Op;
  BEGIN
    TRY
      EnterBegin(t, mode, pos, maxL, offset, t.width, beginState);
      ClearWidths(widths);

      op := PeekOp(t, pos.i);
      IF op # GroupOp THEN RAISE InvalidAlignRow END;
      pos.i := pos.i + 1;

      col := 0;
      LOOP (* while there are more items in the row *)
        start := pos.c + pos.b;
        IF (op = EndOp) OR (op = FlushOp) THEN EXIT END;

        IF Print(t, mode, pos, maxL) THEN RETURN TRUE END;

        IF col < NUMBER (widths^) THEN
          widths[col]:= pos.c + pos.b - start;
          IF (alignPred # NIL)
          AND (NOT alignPred.pred(col, maxWidths[col], widths[col])) THEN
            RETURN TRUE;
          END;
          IF DoBlanks(t, mode, pos, maxL, maxWidths[col] - widths[col]) THEN
            RETURN TRUE;
          END;
          col := col + 1;
        END;

        op := PeekOp(t, pos.i);
      END;
      IF op = EndOp THEN pos.i := pos.i + 1 END;

      FOR z := col TO LAST (widths^) DO
        col := z;
        IF DoBlanks(t, mode, pos, maxL, maxWidths[col]) THEN
          RETURN TRUE;
        END;
      END;

      FOR i := 0 TO col - 1 DO
        maxWidths[i]:= MAX (maxWidths[i], widths[i]);
      END;

      RETURN FALSE;
    FINALLY
      ExitBegin(t, mode, pos, maxL, beginState);
    END;
  END PrintRow;

PROCEDURE DoBlanks(
    t: T;
    mode: Mode;
    VAR pos: Position;
    <*UNUSED*> maxL: CARDINAL;
    blanks: INTEGER
  ): BOOLEAN RAISES {} =
  BEGIN
    pos.b := pos.b + MAX (0, blanks);
    RETURN (mode = Mode.Thinking) AND (pos.c + pos.b > t.width);
  END DoBlanks;

PROCEDURE ClearWidths(widths: Widths) RAISES {} =
  BEGIN
    FOR i := 0 TO LAST (widths^) DO widths[i]:= 0 END;
  END ClearWidths;

PROCEDURE PrintNoAlign(
    t: T;
    mode: Mode;
    VAR pos: Position;
    maxL: CARDINAL;
    <*UNUSED*> args: CARDINAL
  ): BOOLEAN RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    RETURN Print(t, mode, pos, maxL);
  END PrintNoAlign;

PROCEDURE NewInt(i: INTEGER): Int =
  BEGIN
    IF (FIRST (ints) <= i) AND (i <= LAST (ints)) THEN
      RETURN ints[i]
    ELSIF (i = FIRST (INTEGER)) THEN
      RETURN firstInt
    ELSIF (i = LAST (INTEGER)) THEN
      RETURN lastInt
    ELSE
      WITH x = NEW(Int) DO x^:= i; RETURN x END
    END;
  END NewInt;

PROCEDURE DefineOp (proc: OpProc; args: CARDINAL; p: INTEGER): Op =
  BEGIN
    RETURN NEW (Op,  proc := proc,  args := args,  precedence := p);
  END DefineOp;
  
BEGIN
  GroupOp        := DefineOp(PrintGroup,        0, 13);
  BeginOp        := DefineOp(PrintBegin,        2, 12);
  AlignOp        := DefineOp(PrintAlign,        4, 11);
  ColOp          := DefineOp(PrintCol,          3, 10);
  TextOp         := DefineOp(PrintText,         1, 9);
  CharOp         := DefineOp(PrintChar,         1, 7);
  NoAlignOp      := DefineOp(PrintNoAlign,      0, 6);
  BreakOp        := DefineOp(PrintBreak,        3, 5);
  PartialBreakOp := DefineOp(PrintPartialBreak, 2, 4);
  NewLineOp      := DefineOp(PrintNewLine,      2, 3);
  UnitedBreakOp  := DefineOp(PrintUnitedBreak,  2, 2);
  EndOp          := DefineOp(PrintEnd,          0, 1);
  FlushOp        := DefineOp(PrintFlush,        0, FIRST (INTEGER));

  FOR i := FIRST (ints) TO LAST (ints) DO
    ints[i]:= NEW (Int);
    ints[i]^:= i;
  END;
  firstInt := NEW (Int);  firstInt^:= FIRST (INTEGER);
  lastInt := NEW (Int);  lastInt^:= LAST (INTEGER);
  FOR c := FIRST (chars) TO LAST (chars) DO
    chars[c]:= NEW (Char);
    chars[c]^:= c;
  END;
  bools [TRUE]:= NEW (Bool);  bools[TRUE]^:= TRUE;
  bools [FALSE]:= NEW (Bool);  bools[FALSE]^:= FALSE;
  FOR k := FIRST(breakTypes) TO LAST(breakTypes) DO
    breakTypes[k] := NEW(REF BreakType);
    breakTypes[k]^ := k;
  END;
END Formatter.
