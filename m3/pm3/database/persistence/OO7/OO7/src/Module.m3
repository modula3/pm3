MODULE Module;

IMPORT CRandom, GenParams, ComplexAssembly, Manual, OO7, VarParams, Globals,
       Ctypes;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT PrintOp;
IMPORT Transaction;

REVEAL T = OO7.Module BRANDED "Module.T" OBJECT
OVERRIDES
  traverse := Traverse;
  traverseRandom := TraverseRandom;
  scanManual := ScanManual;
  firstLast := FirstLast;   
  init := Init;
  delete := Delete;
END;

PROCEDURE Init (self: T; modId: INTEGER): OO7.Module =
  BEGIN
    IF Globals.debugMode THEN
      Put("Module::Module(modId = "); PutInt(modId); Put(")\n");
    END;

    (* initialize the simple stuff *)
    self.id := modId;
    VAR typeNo := CRandom.random() MOD GenParams.NumTypes;
    BEGIN self.type := Globals.types[typeNo]; END;
    self.buildDate :=
        GenParams.MinModuleDate + 
        (CRandom.random() MOD (GenParams.MaxModuleDate-GenParams.MinModuleDate+1));
    self.allBases := NIL;
    self.man := NEW(Manual.T).init(modId, self);

    (* now create the assemblies for the module *)
    IF (VarParams.NumAssmLevels > 1) THEN
      VAR assmId := Globals.nextComplexAssemblyId;
      BEGIN
        INC(Globals.nextComplexAssemblyId);
        self.designRoot := NEW(ComplexAssembly.T).init(assmId, self, NIL, 1);
      END
    END;
    RETURN self;
  END Init;    

PROCEDURE Delete (self: T) =
  BEGIN
    IF Globals.debugMode THEN
      Put("Module::~Module(id = "); PutInt(self.id); Put(")\n");
    END;
    (* remove module from extent of all modules *)
    LOOP
      VAR lo := Globals.AllModules.remlo();
      BEGIN
        IF self = lo THEN EXIT END;
        Globals.AllModules.addhi(lo);
      END
    END
  END Delete;

PROCEDURE ScanManual (self: T): INTEGER =
  BEGIN
    RETURN self.man.searchText('I');
  END ScanManual;
  
PROCEDURE FirstLast (self: T): INTEGER  =
  BEGIN
    RETURN self.man.firstLast();
  END FirstLast;

PROCEDURE Traverse(self: T; op: OO7.BenchmarkOp ): INTEGER =
  BEGIN
    IF Globals.debugMode THEN
      Put("Module::traverse(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    (* now traverse the assembly hierarchy *)
    RETURN self.designRoot.traverse(op);
  END Traverse;

PROCEDURE TraverseRandom(self: T; op: OO7.BenchmarkOp;
                         state: Ctypes.void_star;
                         writer: Transaction.T):
  INTEGER =
  BEGIN
    IF Globals.debugMode THEN
      Put("Module::traverseRandom(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    (* now traverse the assembly hierarchy *)
    RETURN self.designRoot.traverseRandom(op, state, writer);
  END TraverseRandom;

BEGIN  
END Module.
