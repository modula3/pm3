MODULE BaseAssembly;

IMPORT OO7, PartIdSet, Globals, CRandom, Assembly, RefList, CompositePart,
       ThreadF, Ctypes, VarParams, GenParams;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT PrintOp;
IMPORT Transaction;

REVEAL T = OO7.BaseAssembly BRANDED "BaseAssembly.T" OBJECT
OVERRIDES
  traverse := Traverse;
  traverse7 := Traverse7;
  traverseRandom := TraverseRandom;
  doNothing := Assembly.DoNothing;
  init := Init;
END;

PROCEDURE Init (self: T; asId: INTEGER; module: OO7.Module;
                parentAssembly: OO7.ComplexAssembly): OO7.BaseAssembly =
  VAR
    compId: INTEGER;
    lowCompId: INTEGER;
    compIdLimit: INTEGER;
  BEGIN
    IF Globals.debugMode THEN
      Put("BaseAssembly::BaseAssembly(asId = "); PutInt(asId);
      Put(", levelNo = "); PutInt(VarParams.NumAssmLevels); Put(")\n");
    END;

    (* initialize the simple stuff *)
    self.id := asId;
    VAR typeNo := CRandom.random() MOD GenParams.NumTypes;
    BEGIN self.type := Globals.types[typeNo]; END;
    self.buildDate :=
        GenParams.MinAssmDate +
        (CRandom.random() MOD (GenParams.MaxAssmDate - GenParams.MinAssmDate + 1));
    self.superAssembly := parentAssembly;
    self.componentsPriv := NIL;
    self.componentsShar := NIL;

    (* add the base assembly to the extent of all base assemblies in this
       module *)
    module.allBases := RefList.Cons(self, module.allBases);
    
    (* get access to the design library containing composite parts *)
    lowCompId := (module.id - 1) * VarParams.NumCompPerModule + 1;
    compIdLimit := VarParams.NumCompPerModule;

    (* first select the private composite parts for this assembly *)
    FOR i := 1 TO VarParams.NumCompPerAssm DO
      compId := lowCompId + (CRandom.random() MOD compIdLimit);
      (* keep track of which composite parts this base assembly uses as
         private *)
      Globals.private_cp[compId] :=
          RefList.Cons(self, Globals.private_cp[compId]);
    END;

    (* next select the shared composite parts for this assembly *)
    FOR i := 1 TO VarParams.NumSharPerAssm DO
      compId := (CRandom.random() MOD VarParams.TotalCompParts) + 1;
      (* keep track of which composite parts this base assembly uses as
         shared *)
      Globals.shared_cp[compId] :=
          RefList.Cons(self, Globals.shared_cp[compId]);
    END;
    RETURN self; 
  END Init;

PROCEDURE Traverse (self: T; op: OO7.BenchmarkOp): INTEGER =
  VAR cp: CompositePart.T;
  BEGIN
    IF Globals.debugMode THEN
      Put("\t\tBaseAssembly::traverse(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    VAR
      count := 0;
      (* establish iterator of private composite parts *)
      compI := self.componentsPriv;
    BEGIN
      WHILE compI # NIL DO
        cp := compI.head;
        compI := compI.tail;
        INC(count, cp.traverse(op, NIL));
      END;
      RETURN count;
    END
  END Traverse;

PROCEDURE Traverse7 (self: T; visitedComplexIds: PartIdSet.T): INTEGER =
  BEGIN
    IF Globals.debugMode THEN
      Put("\t\tBaseAssembly::traverse7(id = "); PutInt(self.id); Put(")\n");
    END;

    (* visit this assembly and move on up the design hierarchy *)
    VAR count := 1;
    BEGIN
      self.doNothing();
      IF NOT visitedComplexIds.contains(self.superAssembly.id) THEN
        INC(count, self.superAssembly.traverse7(visitedComplexIds));
      END;
      RETURN count;
    END;
  END Traverse7;

PROCEDURE TraverseRandom (self: T; op: OO7.BenchmarkOp;
                          state: Ctypes.void_star;
                          writer: Transaction.T):
  INTEGER =
  VAR
    cp: CompositePart.T;
    rand: INTEGER;
  BEGIN
    IF Globals.debugMode THEN
      Put("\t\tBaseAssembly::traverseRandom(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    ThreadF.SuspendOthers();
    VAR oldstate := CRandom.setstate(state);
    BEGIN
      rand := CRandom.random() MOD VarParams.NumCompPerAssm;
      EVAL CRandom.setstate(oldstate);
    END;
    ThreadF.ResumeOthers();

    VAR
      (* establish iterator of private composite parts *)
      compI := self.componentsPriv;
      i := 0;
    BEGIN
      WHILE compI # NIL DO
        cp := compI.head;
        compI := compI.tail;
        IF i = rand THEN
          RETURN cp.traverse(op, writer);
        END;
        INC(i);
      END;
    END;
    <* ASSERT FALSE *>
  END TraverseRandom;

BEGIN
END BaseAssembly.
