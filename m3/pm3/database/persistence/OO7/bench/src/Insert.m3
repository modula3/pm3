MODULE Insert EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Insert - Create NumNewCompParts new composite parts (which
  // means also creating a set of AtomicParts for each.)  Add
  // a reference to each new composite part from a randomly chosen
  // base assembly (a different random choice of base assembly for
  // each new composite part.)
  //
  //////////////////////////////////////////////////////////////////
*)

IMPORT CompositePart, Globals, Process, CRandom, Stdio, RefList, BaseAssembly;
FROM IO IMPORT Put, PutInt;
FROM BenchParams IMPORT NumNewCompParts;

PROCEDURE insert1 () =
  (* now create the desired number of new composite parts, adding each
     one as a (private) composite part that a randomly selected base
     assembly now wishes to use *)
  VAR
    compId, assmId: INTEGER;
    compH: CompositePart.T;
    assmH: BaseAssembly.T;
  BEGIN
    IF Globals.debugMode THEN
      Put("In Insert, nextCompositeId = ");
      PutInt(Globals.nextCompositeId);
      Put("\nIn Insert, nextAtomicId = ");
      PutInt(Globals.nextAtomicId);
      Put("\nIn Insert, nextBaseAssemblyId = ");
      PutInt(Globals.nextBaseAssemblyId);
      Put("\n");
    END;

    FOR i := 1 TO NumNewCompParts DO
      (* add a new composite part to the design library *)
      IF Globals.debugMode THEN
        Put("In Insert, making composite part ");
        PutInt(Globals.nextCompositeId);
        Put("\n");
      END;
      compId := Globals.nextCompositeId;
      INC(Globals.nextCompositeId);
      compH := NEW(CompositePart.T).init(compId);

      (* add composite part to its index *)
      EVAL Globals.CompPartIdx.put(compId, compH);
      
      (* randomly select a base assembly that should use it and figure
         out which module it resides in *)
      assmId := (CRandom.random() MOD (Globals.nextBaseAssemblyId - 1)) + 1;

      (* now locate the appropriate base assembly object within its module *)
      VAR ref: REFANY;
      BEGIN
        IF NOT Globals.BaseAssemblyIdx.get(assmId, ref) THEN
          Put("ERROR: Can't find base assembly ", Stdio.stderr);
          PutInt(assmId, Stdio.stderr);
          Put("\n", Stdio.stderr);
          Process.Exit(1);
        END;
        assmH := ref;
      END;

      (* finally, add the newly created composite part as a privately used
         member of the randomly selected base assembly *)

      (* first add this assembly to the list of assemblies in which
         this composite part is used as a private member  *)
      compH.usedInPriv := RefList.Cons(assmH, compH.usedInPriv);

      (* then add the composite part compH to the list of private parts used
         in this assembly *)
      assmH.componentsPriv := RefList.Cons(compH, assmH.componentsPriv);

      IF Globals.debugMode THEN
        Put("[just made it be used by base assembly ");
        PutInt(assmId);
        Put("]\n");
      END
    END
  END insert1;

BEGIN
END Insert.
