MODULE AtomicPart;

IMPORT CRandom, Globals, Time, OO7, PartIdSet, Wr, Stdio, Thread, RefSeq,
       RefBag, RefBagList, Connection, AtomicPart, VarParams, GenParams,
       BenchParams;
FROM IO		 IMPORT Put, PutInt;
FROM Support	 IMPORT PrintOp;
IMPORT Transaction;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE PutString (READONLY a: ARRAY OF CHAR) =
  BEGIN
    Wr.PutString(Stdio.stdout, a);
  END PutString;

REVEAL T = OO7.AtomicPart BRANDED "AtomicPart.T" OBJECT
OVERRIDES
  swapXY := SwapXY;
  toggleDate := ToggleDate;
  doNothing := DoNothing;
  traverse := Traverse;
  init := Init;
  delete := Delete;
END;

PROCEDURE Init(self: T; ptId: INTEGER; cp: OO7.CompositePart): OO7.AtomicPart =
  BEGIN
    IF Globals.debugMode THEN
      Put("AtomicPart::AtomicPart(ptId = "); PutInt(ptId); Put(")\n");
    END;
    
    (* initialize internal state of new part *)
    self.id := ptId;
    VAR typeNo := CRandom.random() MOD GenParams.NumTypes;
    BEGIN self.type := Globals.types[typeNo] END;
    self.buildDate :=
        GenParams.MinAtomicDate +
        (CRandom.random() MOD (GenParams.MaxAtomicDate - GenParams.MinAtomicDate + 1));
    self.x := CRandom.random() MOD GenParams.XYRange;
    self.y := CRandom.random() MOD GenParams.XYRange;
    (* fill in a random document id (for query 8) *)
    self.docId :=  CRandom.random() MOD VarParams.TotalCompParts + 1;
    self.to := NEW(RefSeq.T).init(VarParams.NumConnPerAtomic);
    self.from := NEW(RefSeq.T).init(VarParams.NumConnPerAtomic);

    self.partOf := cp;
    (* AllParts.add(this) *)

    VAR bag: RefBag.T;
    BEGIN
      IF NOT Globals.BuildDateIdx.get(self.buildDate, bag) THEN
        bag := NEW(RefBagList.T).init();
        EVAL Globals.BuildDateIdx.put(self.buildDate, bag);
      END;
      bag.insert(self);
    END;

    RETURN self;
  END Init;

PROCEDURE Delete (self: T) =
  VAR
    cn: Connection.T;
    ap: T;
  BEGIN
    (* remove part from extent of all atomic parts *)
    (* AllParts.remove(self); *)

    (* establish iterator over connected parts in "to" direction *)
    FOR i := 0 TO self.to.size() - 1 DO
      cn := self.to.get(i);		 (* get connection record *)
      ap := cn.to;			 (* get the associated atomic part *)
      LOOP (* remove back pointer to connection object *)
        VAR lo := ap.from.remlo();
        BEGIN
          IF cn = lo THEN EXIT END;
          ap.from.addhi(lo);
        END
      END
    END;
    self.to := NIL; (* delete the "to" association *)

    (* establish iterator over connected parts in "from" direction *)
    FOR i := 0 TO self.from.size() - 1 DO
      cn := self.from.get(i);		 (* get next connection record *)
      ap := cn.from;			 (* get the associated atomic part *)
      LOOP (* remove back pointer to connection object *)
        VAR lo := ap.to.remlo();
        BEGIN
          IF cn = lo THEN EXIT END;
          ap.to.addhi(lo);
        END
      END
    END;
    self.from := NIL; (* delete the "from" association *)

    VAR bag: RefBag.T;
    BEGIN
      EVAL Globals.BuildDateIdx.get(self.buildDate, bag);
      EVAL bag.delete(self);
    END
  END Delete;

PROCEDURE SwapXY(self: T) =
  BEGIN
    IF Globals.debugMode THEN
      Put("                    AtomicPart::swapXY(ptId = "); PutInt(self.id);
      Put(", x = "); PutInt(self.x);
      Put(", y = "); PutInt(self.y); Put(")\n");
    END;

    (* exchange X and Y values *)
    VAR tmp := self.x;
    BEGIN
      self.x := self.y;
      self.y := tmp;
    END;

    IF Globals.debugMode THEN
      Put("                    [did swap, so x = "); PutInt(self.x);
      Put(", y = "); PutInt(self.y); Put("]\n");
    END;
  END SwapXY;
  
PROCEDURE ToggleDate(self: T) =
  BEGIN
    IF Globals.debugMode THEN
      Put("                    AtomicPart::toggleDate(ptId = ");
      PutInt(self.id);
      Put(", buildDate = "); PutInt(self.buildDate); Put(")\n");
    END;

    (* delete from index first *)
    VAR bag: RefBag.T;
    BEGIN
      EVAL Globals.BuildDateIdx.get(self.buildDate, bag);
      EVAL bag.delete(self);
    END;

    (* increment build date if odd, decrement it if even *)

    IF self.buildDate MOD 2 # 0 THEN
      (* odd case *)
      INC(self.buildDate);
    ELSE
      (* even case *)
      DEC(self.buildDate);
    END;

    (* update the index *)
    VAR bag: RefBag.T;
    BEGIN
      IF NOT Globals.BuildDateIdx.get(self.buildDate, bag) THEN
        bag := NEW(RefBagList.T).init();
        EVAL Globals.BuildDateIdx.put(self.buildDate, bag);
      END;
      bag.insert(self);
    END;

    IF Globals.debugMode THEN
      Put("                    [did toggle, so buildDate = ");
      PutInt(self.buildDate); Put("]\n");
    END
  END ToggleDate;

PROCEDURE DoNothing(self: T) =
  BEGIN
    IF self.id < 0 THEN 
      Put("DoNothing: negative id.\n");
    END;
    
    IF Globals.debugMode THEN
      Put("==> DoNothing(x = "); PutInt(self.x);
      Put(", y = "); PutInt(self.y);
      Put(", type = "); PutString(self.type); Put(")\n");
    END;
  
    IF RealWork THEN
      FOR i := 1 TO WorkAmount DO
        (* gettimeofday(&tv, &tz); *)
        EVAL Time.Now();
      END
    END;
  END DoNothing;
  
PROCEDURE Traverse (self: T; op: OO7.BenchmarkOp;
                    visitedIds: PartIdSet.T;
                    writer: Transaction.T): INTEGER =
  VAR
    ap: AtomicPart.T;
    cn: Connection.T;
    count := 0;
  BEGIN
    IF Globals.debugMode THEN
      Put("\t\t\tAtomicPart::traverse(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;
    
    CASE op OF

    | OO7.BenchmarkOp.Trav1 =>

      (* just examine the part *)
      INC(count);
      self.doNothing();

    | OO7.BenchmarkOp.Trav2a =>

      (* swap X and Y if first part *)
      IF visitedIds.empty() THEN
        self.swapXY();
        INC(count);
      END

    | OO7.BenchmarkOp.Trav2b =>

      (* swap X and Y *)
      self.swapXY();
      INC(count);

    | OO7.BenchmarkOp.Trav2c =>

      (* swap X and Y repeatedly *)
      FOR i := 1 TO BenchParams.UpdateRepeatCnt DO
        self.swapXY();
        INC(count);
      END;

    | OO7.BenchmarkOp.Trav3a =>

      (* toggle date if first part *)
      IF visitedIds.empty() THEN
        self.toggleDate();
        INC(count);
      END

    | OO7.BenchmarkOp.Trav3b =>

      (* toggle date *)
      self.toggleDate();
      INC(count);

    | OO7.BenchmarkOp.Trav3c =>

      (* toggle date repeatedly *)
      FOR i := 1 TO BenchParams.UpdateRepeatCnt DO
        self.toggleDate();
        INC(count);
      END;

    | OO7.BenchmarkOp.Trav6 =>

      (* examine only the root part *)
      INC(count);
      self.doNothing();
      RETURN count;

    ELSE

      (* atomic parts don't respond to other traversals *)
      Put("*** AtomicPart::PANIC -- illegal traversal!!! ***\n");

    END;
    (* now, record the fact that we've visited this part *)
    visitedIds.insert(self.id);
    
    (* finally, continue with a DFS of the atomic parts graph *)
    (* establish iterator over connected parts *)
    FOR i:= 0 TO self.to.size() - 1 DO
      cn := self.to.get(i);		 (* get next connection record *)
      ap := cn.to;			 (* get the associated atomic part *)
      IF writer # NIL THEN
        writer.lock(ap, Transaction.LockMode.WRITE);
      END;
      IF NOT visitedIds.contains(ap.id) THEN
        INC(count, ap.traverse(op, visitedIds, writer));
      END
    END;
    RETURN count;
  END Traverse;

BEGIN
END AtomicPart.
