MODULE AttributeDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.6  1998/05/19 10:18:02  roland
    Support for log-groups implemented.

    Revision 1.5  1998/01/21 12:33:33  roland
    Distinction between forward and backward delta necessary.

    Revision 1.4  1998/01/14 17:46:00  roland
    Bugfix in FreeSclices-List implementation.

    Revision 1.3  1997/09/22 18:40:51  roland
    Buf in FreePut-List-management fixed.

    Revision 1.2  1997/07/21 10:39:33  roland
    A few bug-fixes and implementation of free memory list logging.

    Revision 1.1  1997/05/30 07:51:41  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

IMPORT GraphCommand, Node;
IMPORT Text;
IMPORT Journal, Variant, Fmt;

CONST SliceLength = 100;

TYPE
  SliceIndex = [0 .. SliceLength - 1];

  Slice = ARRAY SliceIndex OF CHAR;
  WriteLog = ARRAY SliceIndex OF BOOLEAN;

  SliceElement = RECORD
                   start  : CARDINAL;
                   content: Slice;
                   log    : WriteLog;
                   next   : REF SliceElement;
                 END;
  SliceList = REF SliceElement;

  Put = RECORD
          slices: SliceList;
          start : CARDINAL;
          length: CARDINAL;
          next  : REF Put;
        END;
  PutList = REF Put;

REVEAL
  T =
    Public BRANDED OBJECT
      node: Node.T;
      attribute: CARDINAL;
      forward        : BOOLEAN;
      slices         : SliceList;
      (** Invariants for slices list *)
      (** $\forall s1, s2 \in slices:
              s1.next = s2 \rightarrow s1.start > s2.start$ *)
      (** $\forall s \in slices: s.start MOD SliceLength = 0$ *)
      (** $truncateTo \geq 0 \rightarrow
              \forall s \in slices: s.start < truncateTo$ *)
      (** $(truncateTo \geq 0
            \wedge \exists s1 \in slices: s1.start + SliceLength > truncateTo)
           \rightarrow
           \forall s2 \in slices:
                s1 \not = s2 \rightarrow s2.start + SliceLength < truncateTo$ *)

      puts: PutList;
      (* invariants for puts list *)
      (** $\forall p1, p2 \in puts:
              p1.next = p2 \rightarrow p1.start > p2.start$ *)
      (** $\forall p1, p2 \in puts:
              p1.start + p1.length < p2.start \vee
              p2.start + p2.length < p1.start$ *)
      (** $\forall p \in puts: p.slices \not = NIL$ \wedge
                               p.start \geq p.slices.start \wedge
                               p.start \leq p.slices.start + SliceLenght$ *)
      (** $truncateTo \geq 0 \rightarrow
              \forall p \in puts: p.start + p.length \leq truncateTo$ *)

      current   : REF Put;
      truncateTo: INTEGER;
      delete    : BOOLEAN;
      dcosts    : CARDINAL;
      loopCount : CARDINAL;
      nextFree  : T;
    OVERRIDES
      init              := Init;
      spare             := Spare;
      putAttribute      := PutAttribute;
      truncateAttribute := TruncateAttribute;
      costs             := Costs;
      loop              := Loop;
      getNext           := GetNext;
      reverseLoop       := ReverseLoop;
      getPrev           := GetPrev;
    END;

CONST
  NullSlice = Slice{'\000', ..};
  NullLog   = WriteLog{FALSE, ..};

(* Unused Slices as well as Puts are kept in a free list. *)
VAR
  FreePuts                : PutList  := NIL;
  MaxFreePuts, FreePutSize: CARDINAL := 0;

  FreeSlices                   : SliceList := NIL;
  MaxFreeSlices, FreeSlicesSize: CARDINAL  := 0;

  FreeDeltas                  : T        := NIL;
  MaxFreeDeltas, FreeDeltaSize: CARDINAL := 0;

PROCEDURE NewSlice (): REF SliceElement =
  VAR res: REF SliceElement;
  BEGIN
    IF FreeSlices # NIL THEN
      res := FreeSlices;
      FreeSlices := FreeSlices.next;
      DEC(FreeSlicesSize);
    ELSE
      res := NEW(REF SliceElement);
    END;
    res^ := SliceElement{start := 0, content := NullSlice, log := NullLog,
                         next := NIL};
    RETURN res;
  END NewSlice;

PROCEDURE NewPut (): REF Put =
  VAR res: REF Put;
  BEGIN
    IF FreePuts # NIL THEN
      res := FreePuts;
      FreePuts := FreePuts.next;
      DEC(FreePutSize);
    ELSE
      res := NEW(REF Put);
    END;
    res^ := Put{start := 0, length := 0, slices := NIL, next := NIL};
    RETURN res;
  END NewPut;

PROCEDURE Init (ad: T; node: Node.T; attr: CARDINAL; forward: BOOLEAN): T =
  BEGIN
    ad.forward := forward;
    ad.puts := NIL;
    ad.slices := NIL;
    ad.node := node;
    ad.attribute := attr;
    ad.current := NIL;
    ad.truncateTo := -1;
    ad.delete := FALSE;
    ad.dcosts := 0;
    ad.nextFree := NIL;
    RETURN ad;
  END Init;

PROCEDURE Spare (ad: T) =
  VAR
    sl : REF SliceElement;
    put: REF Put;
  BEGIN
    (* add all elements of ad's slices list to the free list *)
    sl := ad.slices;
    WHILE sl # NIL DO
      ad.slices := ad.slices.next;
      sl.next := FreeSlices;
      FreeSlices := sl;
      INC(FreeSlicesSize);
      IF Variant.FreeMemoryListLog > 0 THEN
        IF MaxFreeSlices < FreeSlicesSize THEN
          MaxFreeSlices := FreeSlicesSize;
          IF MaxFreeSlices MOD Variant.FreeMemoryListLog = 0 THEN
            Journal.Add(
              "AttributeDelta free slices list " & Fmt.Int(MaxFreeSlices));
          END;
        END;
      END;
      sl := ad.slices;
    END;
    (* add all elements of ad's puts list to the free list *)
    put := ad.puts;
    WHILE put # NIL DO
      ad.puts := ad.puts.next;
      put.next := FreePuts;
      FreePuts := put;
      INC(FreePutSize);
      IF Variant.FreeMemoryListLog > 0 THEN
        IF MaxFreePuts < FreePutSize THEN
          MaxFreePuts := FreePutSize;
          IF MaxFreePuts MOD Variant.FreeMemoryListLog = 0 THEN
            Journal.Add(
              "AttributeDelta free puts list " & Fmt.Int(MaxFreePuts));
          END
        END;
      END;
      put := ad.puts;
    END;
    ad.current := NIL;
  END Spare;

PROCEDURE PutAttribute (ad: T; start: CARDINAL; value: TEXT) =
  VAR
    filledValue       : TEXT;
    fillLength, length: CARDINAL;
    firstSlice        : REF SliceElement;

  PROCEDURE CopyTextToSlice (    slice        : REF SliceElement;
                                 start, length: CARDINAL;
                                 text         : TEXT;
                                 overwrite    : BOOLEAN;
                             VAR bytesCopied  : CARDINAL          ) =
    VAR index, slindex, len: CARDINAL;
    BEGIN
      IF start > slice.start THEN
        (* text start lies within slice *)
        index := 0;
        slindex := start - slice.start;
      ELSE
        (* text begins before slice *)
        index := slice.start - start;
        slindex := 0;
      END;
      len := MIN(length - index, SliceLength - slindex);
      FOR i := 0 TO len - 1 DO
        IF overwrite OR NOT slice.log[slindex + i] THEN
          slice.log[slindex + i] := TRUE;
          slice.content[slindex + i] := Text.GetChar(text, index + i);
        END;
      END;
      bytesCopied := len;
    END CopyTextToSlice;

  PROCEDURE DisjointAndNonNeighbouring (p1, p2: REF Put): BOOLEAN =
    BEGIN
      RETURN (p1.start > p2.start + p2.length)
               OR (p2.start > p1.start + p1.length);
    END DisjointAndNonNeighbouring;

  PROCEDURE MergePuts (act, next: REF Put): BOOLEAN =
    (* Try to merge act with next.  If merging is not possible, result will
       be false.  Else, act will contain both puts after the merge and next
       will be undefined. *)
    BEGIN
      IF next = NIL OR DisjointAndNonNeighbouring(act, next) THEN
        RETURN FALSE;
      ELSE
        WITH maxlen = MAX(act.start + act.length, next.start + next.length) DO
          act.start := MIN(act.start, next.start);
          act.length := maxlen - act.start;
          IF act.slices.start < next.slices.start THEN
            act.slices := next.slices;
          END;
          (* remove next from list *)
          act.next := next.next;
          next.next := FreePuts;
          FreePuts := next;
          INC(FreePutSize);
        END;
        RETURN TRUE;
      END;
    END MergePuts;

  BEGIN
    filledValue := value;
    length := Text.Length(value);

    IF ad.forward THEN
      ad.delete := FALSE;
      IF ad.truncateTo >= 0 AND ad.truncateTo < start + length THEN
        (* Attribute was truncated to a length smaller than end of
           value. *)
        IF ad.truncateTo + 1 < start THEN
          (* We must fill from truncateTo+1 to start-1 with 0 bytes. *)
          fillLength := start - ad.truncateTo - 1;
          start := ad.truncateTo;
          length := length + fillLength;

          WHILE fillLength >= SliceLength DO
            filledValue := Text.FromChars(NullSlice) & filledValue;
            DEC(fillLength, SliceLength);
          END;
          IF fillLength > 0 THEN
            filledValue :=
              Text.FromChars(SUBARRAY(NullSlice, 0, fillLength))
                & filledValue;
          END;
        END;
        (* set new truncation length *)
        ad.truncateTo := start + length;
      END;
    ELSE
      (* backward log *)
      IF ad.truncateTo >= 0 THEN
        IF ad.truncateTo <= start THEN
          (* ignore put completely *)
          RETURN;
        ELSIF ad.truncateTo < start + length THEN
          (* cut put to truncate length *)
          length := start + length - ad.truncateTo;
          filledValue := Text.Sub(filledValue, 0, length);
        END;
      END;
    END;

    VAR
      highSliceStart, lowSliceStart: INTEGER;
      actSliceStart                : INTEGER;
      remaining, bytesCopied       : CARDINAL;
      newsl, sl, prev              : REF SliceElement;
      first                        : BOOLEAN;
    BEGIN
      (* adjust slices *)
      highSliceStart := ((start + length) DIV SliceLength) * SliceLength;
      lowSliceStart := (start DIV SliceLength) * SliceLength;
      (* make sure all slices exist and copy text to them *)
      sl := ad.slices;
      prev := ad.slices;
      actSliceStart := highSliceStart;
      remaining := length;
      first := TRUE;
      WHILE remaining > 0 DO
        IF sl = NIL THEN
          (* insert at end of list *)
          newsl := NewSlice();
          newsl.start := actSliceStart;
          IF prev = NIL THEN
            (* list was empty *)
            ad.slices := newsl;
            ad.slices := newsl;
          ELSE
            prev.next := newsl;
          END;
          prev := newsl;
          CopyTextToSlice(
            prev, start, length, filledValue, ad.forward, bytesCopied);
          IF first THEN firstSlice := prev; first := FALSE; END;
          DEC(actSliceStart, SliceLength);
          DEC(remaining, bytesCopied);
        ELSIF sl.start > actSliceStart THEN
          (* still out of range *)
          prev := sl;
          sl := sl.next;
        ELSIF sl.start = actSliceStart THEN
          (* a slice for this part of the text exists *)
          prev := sl;
          sl := sl.next;
          CopyTextToSlice(
            prev, start, length, filledValue, ad.forward, bytesCopied);
          IF first THEN firstSlice := prev; first := FALSE; END;
          DEC(actSliceStart, SliceLength);
          DEC(remaining, bytesCopied);
        ELSIF sl.start < actSliceStart THEN
          (* slice does not exist *)
          newsl := NewSlice();
          newsl.start := actSliceStart;
          IF sl = ad.slices THEN
            (* insert at beginning of list *)
            newsl.next := sl;
            ad.slices := newsl;
          ELSE
            newsl.next := prev.next;
            prev.next := newsl;
          END;
          prev := newsl;
          CopyTextToSlice(
            prev, start, length, filledValue, ad.forward, bytesCopied);
          IF first THEN firstSlice := prev; first := FALSE; END;
          DEC(actSliceStart, SliceLength);
          DEC(remaining, bytesCopied);
        END;
      END;
    END;

    (* adjust puts *)
    VAR actput, prev, newput: REF Put;
    BEGIN
      newput := NewPut();
      newput.start := start;
      newput.length := length;
      newput.slices := firstSlice;
      newput.next := NIL;
      (* first, find a put that either is not disjoint to the new put, is
         disjoint and ends before start of new put or end of the put list
         is reached.  Insert new put before this position *)
      actput := ad.puts;
      prev := actput;
      WHILE actput # NIL AND actput.start + actput.length > start
              AND DisjointAndNonNeighbouring(newput, actput) DO
        prev := actput;
        actput := actput.next;
      END;
      IF actput = NIL THEN
        (* end of put list reached *)
        IF prev = NIL THEN
          (* list was empty *)
          ad.puts := newput;
        ELSE
          prev.next := newput;
        END;
        INC(ad.dcosts);
      ELSIF DisjointAndNonNeighbouring(newput, actput) THEN
        (* the new put cannot be merged with old puts *)
        IF actput = ad.puts THEN
          (* insert at beginning of list *)
          newput.next := ad.puts;
          ad.puts := newput;
        ELSE
          newput.next := actput;
          prev.next := newput;
        END;
        INC(ad.dcosts);
      ELSE
        INC(ad.dcosts);
        (* insert before first non disjoint put and then merge all non
           disjoint puts in the remaining list *)
        IF actput = ad.puts THEN
          (* insert at beginning of list *)
          newput.next := ad.puts;
          ad.puts := newput;
        ELSE
          newput.next := actput;
          prev.next := newput;
        END;
        actput := newput;
        WHILE actput # NIL AND MergePuts(actput, actput.next) DO
          DEC(ad.dcosts);
        END;
      END;
    END;

  END PutAttribute;

PROCEDURE TruncateAttribute (ad: T; length: CARDINAL) =
  VAR
    put: REF Put;
    sl : REF SliceElement;
  BEGIN
    IF ad.forward THEN
      IF ad.truncateTo < 0 THEN INC(ad.dcosts) END;
      IF ad.truncateTo < 0 OR ad.truncateTo > length THEN
        (* We must only act, if attribute wasn't truncated yet or previous
           truncation > length. *)
        IF length = 0 THEN
          ad.truncateTo := 0;
          ad.delete := TRUE;
        ELSE
          ad.truncateTo := length;
        END;

        (* Adapt slices list to new length *)
        WHILE ad.slices # NIL AND ad.slices.start >= length DO
          sl := ad.slices;
          ad.slices := ad.slices.next;
          sl.next := FreeSlices;
          FreeSlices := sl;
          INC(FreeSlicesSize);
        END;

        (* Adapt put list to new length *)
        WHILE ad.puts # NIL AND ad.puts.start >= length DO
          put := ad.puts;
          ad.puts := ad.puts.next;
          put.next := FreePuts;
          FreePuts := put;
          INC(FreePutSize);
          DEC(ad.dcosts);
        END;
        IF ad.puts # NIL THEN
          (* now first element of ad.puts is either completely within
             length or must be cut down to length *)
          ad.puts.slices :=
            ad.slices;           (* first slice must belong to first put *)
          IF ad.puts.start + ad.puts.length > length THEN
            ad.puts.length := length - ad.puts.start;
          END;
        END;
      END;
    ELSE
      (* truncation affects backward log only if a) delta is empty or b) it
         truncates behind highest put and below current truncation *)
      IF ad.puts = NIL THEN
        IF ad.truncateTo < 0 THEN
          (* empty delta *)
          ad.truncateTo := length;
        ELSIF ad.truncateTo > length THEN
          (* we truncate below current truncate and have no puts *)
          ad.truncateTo := length;
        END;
      ELSIF (ad.truncateTo > length) OR (ad.truncateTo < 0) THEN
        (* check highest put *)
        IF ad.puts.start + ad.puts.length <= length THEN
          (* we truncate beyond highest put and below current truncate *)
          ad.truncateTo := length;
        END;
      END;
    END
  END TruncateAttribute;

PROCEDURE Costs (ad: T): CARDINAL =
  BEGIN
    RETURN ad.dcosts;
  END Costs;

PROCEDURE Loop (ad: T) =
  BEGIN
    ad.current := ad.puts;
    ad.loopCount := 2;
  END Loop;

PROCEDURE GetNext (ad: T; VAR com: GraphCommand.T): BOOLEAN =
  BEGIN
    (* return truncate or delete command first *)
    IF ad.loopCount = 2 AND ad.delete THEN
      GraphCommand.DeleteAttribute(com, ad.node, ad.attribute);
      DEC(ad.loopCount);
    ELSIF ad.loopCount = 2 AND ad.truncateTo > 0 THEN
      GraphCommand.TruncateAttribute(
        com, ad.node, ad.attribute, ad.truncateTo);
      DEC(ad.loopCount);
    ELSIF ad.current = NIL THEN
      RETURN FALSE;
    ELSE
      (* return current put command *)
      GraphCommand.PutAttribute(
        com, ad.node, ad.attribute, ad.current.start, ad.current.length,
        PutToText(ad.current));
      ad.current := ad.current.next;
      ad.loopCount := 1;
    END;
    RETURN TRUE;
  END GetNext;


PROCEDURE ReverseLoop (ad: T) =
  BEGIN
    ad.current := ad.puts;
    ad.loopCount := 2;
  END ReverseLoop;

PROCEDURE GetPrev (ad: T; VAR com: GraphCommand.T): BOOLEAN =
  BEGIN
    IF ad.loopCount = 2 THEN
      IF ad.current = NIL THEN
        DEC(ad.loopCount);
      ELSE
        (* return current put command *)
        GraphCommand.PutAttribute(
          com, ad.node, ad.attribute, ad.current.start, ad.current.length,
          PutToText(ad.current));
        ad.current := ad.current.next;
        RETURN TRUE;
      END;
    END;
    (* return truncate or delete command last *)
    IF ad.loopCount = 1 AND ad.delete THEN
      GraphCommand.DeleteAttribute(com, ad.node, ad.attribute);
      DEC(ad.loopCount);
      RETURN TRUE;
    ELSIF ad.loopCount = 1 AND ad.truncateTo > 0 THEN
      GraphCommand.TruncateAttribute(
        com, ad.node, ad.attribute, ad.truncateTo);
      DEC(ad.loopCount);
      RETURN TRUE;
    END;

    RETURN FALSE;
  END GetPrev;


PROCEDURE New (): T =
  VAR res: T := FreeDeltas;
  BEGIN
    IF res = NIL THEN
      RETURN NEW(T);
    ELSE
      FreeDeltas := FreeDeltas.nextFree;
      DEC(FreeDeltaSize);
      RETURN res;
    END;
  END New;

PROCEDURE Dispose (ad: T) =
  BEGIN
    Spare(ad);
    ad.nextFree := FreeDeltas;
    FreeDeltas := ad;
    INC(FreeDeltaSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFreeDeltas < FreeDeltaSize THEN
        MaxFreeDeltas := FreeDeltaSize;
        IF MaxFreeDeltas MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add(
            "AttributeDelta free delta list " & Fmt.Int(MaxFreeDeltas));
        END;
      END;
    END;
  END Dispose;

PROCEDURE PutToText (put: REF Put): TEXT =
  VAR
    sl : REF SliceElement;
    res: TEXT             := "";
  BEGIN
    sl := put.slices;
    WHILE sl # NIL AND sl.start + SliceLength > put.start DO
      WITH start = MAX(sl.start, put.start),
           index = start - sl.start,
           length = MIN(sl.start + SliceLength, put.start + put.length)
                      - start DO
        res := Text.FromChars(SUBARRAY(sl.content, index, length)) & res;
        sl := sl.next;
      END;
    END;
    RETURN res;
  END PutToText;

BEGIN
END AttributeDelta.
