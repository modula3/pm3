MODULE ComplexAssembly;

IMPORT OO7, Assembly, BaseAssembly, ComplexAssembly, PartIdSet, Globals,
       CRandom, GenParams, VarParams, RefList, ThreadF, Ctypes;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT PrintOp;
IMPORT Transaction;

REVEAL T = OO7.ComplexAssembly BRANDED "ComplexAssembly.T" OBJECT
OVERRIDES
  traverse := Traverse;
  traverse7 := Traverse7;
  traverseRandom := TraverseRandom;
  doNothing := Assembly.DoNothing;
  init := Init;
END;

PROCEDURE Init (self: T; asId: INTEGER;
                module: OO7.Module; parentAssembly: OO7.ComplexAssembly;
                levelNo: INTEGER): OO7.ComplexAssembly =
  VAR
    nextId: INTEGER;
    subAssembly: ComplexAssembly.T;
    baseAssembly: BaseAssembly.T;
  BEGIN
    IF Globals.debugMode THEN
      Put("ComplexAssembly::ComplexAssembly(asId = "); PutInt(asId);
      Put(", levelNo = "); PutInt(levelNo); Put(")\n");
    END;

    (* initialize the simple stuff *)
    self.id := asId;
    VAR typeNo := CRandom.random() MOD GenParams.NumTypes;
    BEGIN self.type := Globals.types[typeNo]; END;
    self.buildDate :=
        GenParams.MinAssmDate + (CRandom.random() MOD (GenParams.MaxAssmDate - GenParams.MinAssmDate + 1));
    self.superAssembly := parentAssembly;
    self.subAssemblies := NIL;

    (* recursively create subassemblies for this complex assembly *)
    FOR i := 1 TO VarParams.NumAssmPerAssm DO
      IF levelNo < VarParams.NumAssmLevels - 1 THEN
        (* create a complex assembly as the subassembly *)
        nextId := Globals.nextComplexAssemblyId;
        INC(Globals.nextComplexAssemblyId);
        subAssembly := NEW(T).init(nextId, module, self, levelNo + 1);
        self.subAssemblies := RefList.Cons(subAssembly, self.subAssemblies);
      ELSE 
        (* create a base assembly as the subassembly
           and insert its id into a hash index *)
        nextId := Globals.nextBaseAssemblyId;
        INC(Globals.nextBaseAssemblyId);
        baseAssembly := NEW(BaseAssembly.T).init(nextId, module, self);
        self.subAssemblies := RefList.Cons(baseAssembly, self.subAssemblies);
        (* add to index *)
        EVAL Globals.BaseAssemblyIdx.put(nextId, baseAssembly);
      END
    END;
    RETURN self;
  END Init;

PROCEDURE Traverse (self: T; op: OO7.BenchmarkOp): INTEGER =
  VAR assm: Assembly.T;
  BEGIN
    IF Globals.debugMode THEN
      Put("\tComplexAssembly::traverse(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    (* traverse each of the assembly's subassemblies *)
    VAR
      count := 0;
      (* establish subassembly iterator *)
      assmI := self.subAssemblies;
    BEGIN
      WHILE assmI # NIL DO
        assm := assmI.head;
        assmI := assmI.tail;
        INC(count, assm.traverse(op));
      END;
      RETURN count;
    END;
  END Traverse;

PROCEDURE Traverse7 (self: T; visitedComplexIds: PartIdSet.T): INTEGER =
  BEGIN
    IF Globals.debugMode THEN
      Put("\t\t\tComplexAssembly::traverse7(id = "); PutInt(self.id);
      Put(")\n");
    END;

    (* process this assembly and move on up the design hierarchy *)
    VAR count := 1;
    BEGIN
      visitedComplexIds.insert(self.id);
      self.doNothing();
      IF self.superAssembly # NIL THEN
        IF NOT visitedComplexIds.contains(self.superAssembly.id) THEN
          INC(count, self.superAssembly.traverse7(visitedComplexIds));
        END
      END;
      RETURN count;
    END;
  END Traverse7;

PROCEDURE TraverseRandom (self: T; op: OO7.BenchmarkOp;
                          state: Ctypes.void_star;
                          writer: Transaction.T):
  INTEGER =
  VAR
    assm: Assembly.T;
    rand: INTEGER;
  BEGIN
    IF Globals.debugMode THEN
      Put("\tComplexAssembly::traverseRandom(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    ThreadF.SuspendOthers();
    VAR oldstate := CRandom.setstate(state);
    BEGIN
      rand := CRandom.random() MOD VarParams.NumAssmPerAssm;
      EVAL CRandom.setstate(oldstate);
    END;
    ThreadF.ResumeOthers();

    VAR
      (* establish subassembly iterator *)
      assmI := self.subAssemblies;
      i := 0;
    BEGIN
      WHILE assmI # NIL DO
        assm := assmI.head;
        assmI := assmI.tail;
        IF i = rand THEN
          RETURN assm.traverseRandom(op, state, writer);
        END;
        INC(i);
      END;
    END;
    <* ASSERT FALSE *>
  END TraverseRandom;

BEGIN
END ComplexAssembly.
