MODULE CompositePart;

IMPORT CRandom, AtomicPart, Document, Connection, Globals, OO7, Fmt,
       RefList, PartIdSet, BaseAssembly, GenParams, VarParams;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT PrintOp;
IMPORT Transaction;

REVEAL T = OO7.CompositePart BRANDED "CompositePart.T" OBJECT
OVERRIDES
  init := Init;
  delete := Delete;
  traverse := Traverse;
  traverse7 := Traverse7;
END;

PROCEDURE Init (self: T; cpId: INTEGER): OO7.CompositePart =
  VAR
    to: INTEGER;
    atomicid: INTEGER;
    atomicParts := NEW(REF ARRAY OF OO7.AtomicPart, VarParams.NumAtomicPerComp);
    cn: Connection.T;
    ba: BaseAssembly.T;
  BEGIN
    IF Globals.debugMode THEN
      Put("CompositePart::CompositePart(cpId = "); PutInt(cpId); Put(")\n");
    END;

    (* initialize the simple stuff *)
    self.id := cpId;
    VAR typeNo := CRandom.random() MOD GenParams.NumTypes;
    BEGIN self.type := Globals.types[typeNo]; END;

    (* for the build date, decide if this part is young or old, and then *)
    (* randomly choose a date in the required range *)
    
    IF cpId MOD GenParams.YoungCompFrac = 0 THEN
      (* young one *)
      IF Globals.debugMode THEN
        Put("(young composite part, id = "); PutInt(self.id); Put(".)\n");
      END;
      self.buildDate :=
          GenParams.MinYoungCompDate +
          (CRandom.random() MOD (GenParams.MaxYoungCompDate - GenParams.MinYoungCompDate + 1));
    ELSE
      (* old one *)
      IF Globals.debugMode THEN
        Put("(old composite part, id = "); PutInt(self.id); Put(".)\n");
      END;
      self.buildDate :=
          GenParams.MinOldCompDate +
          (CRandom.random() MOD (GenParams.MaxOldCompDate - GenParams.MinOldCompDate + 1));
    END;

    self.parts := atomicParts;
    self.usedInPriv := NIL;
    self.usedInShar := NIL;

    (* initialize the documentation (indexed by its title and id) ... *)
    self.documentation := NEW(Document.T).init(cpId, self);
    
    (* insert title into document index *)
    EVAL Globals.DocumentIdx.put(Fmt.F("Composite Part %08s", Fmt.Int(cpId)),
                         self.documentation);

    (* insert id into document id index *)
    EVAL Globals.DocumentIdIdx.put(cpId, self.documentation);

    (* now create the atomic parts (indexed by their ids) ... *)
    FOR i := 0 TO VarParams.NumAtomicPerComp - 1   DO
      atomicid := Globals.nextAtomicId + i;

      (* create a new atomic part *)
      (* the AtomicPart constructor takes care of setting up *)
      (* the back pointer to the containing CompositePart() *)
      atomicParts[i] := NEW(AtomicPart.T).init(atomicid, self);
      
      (* stick the id of the part into the index *)
      EVAL Globals.AtomicPartIdx.put(atomicid, atomicParts[i]);
    
      (* first atomic part is the root part *)
      IF i = 0 THEN self.rootPart := atomicParts[i]; END;
    END;
    
    (* ... and then wire them semi-randomly together (as a ring plus random
       additional connections to ensure full part reachability for traversals)
    *)
    
    FOR from := 0 TO VarParams.NumAtomicPerComp - 1  DO
      FOR i := 0 TO VarParams.NumConnPerAtomic - 1 DO
        IF i = 0 THEN
          to := (from + 1) MOD VarParams.NumAtomicPerComp;
        ELSE
          to := CRandom.random() MOD VarParams.NumAtomicPerComp;
        END;
        cn := NEW(Connection.T).init(atomicParts[from], atomicParts[to]);
      END
    END;
    INC(Globals.nextAtomicId, VarParams.NumAtomicPerComp);
    
    (* finally insert this composite part as a child of the base
       assemblies that use it *)
    
    (* first the assemblies using the comp part as a shared component *)
    
    (* get the first base assembly *)
    VAR baI := Globals.shared_cp[cpId];
    BEGIN
      WHILE baI # NIL DO
        ba := baI.head;
        baI := baI.tail;		 (* get the next base assembly *)

        (* add this assembly to the list of assemblies in which
           this composite part is used as a shared member *)
        self.usedInShar := RefList.Cons(ba, self.usedInShar);

        (* then add the composite part cp to the list of shared parts used
           in this assembly *)
        ba.componentsShar := RefList.Cons(self, ba.componentsShar);
      END
    END;
    
    (* next the assemblies using the comp part as a private component *)

    (* get the first base assembly *)
    VAR baI := Globals.private_cp[cpId];
    BEGIN
      WHILE baI # NIL DO
        ba := baI.head;
        baI := baI.tail;		 (* get the next base assembly *)

        (* add this assembly to the list of assemblies in which
           this composite part is used as a shared member *)
        self.usedInPriv := RefList.Cons(ba, self.usedInPriv);

        (* then add the composite part cp to the list of shared parts used
           in this assembly *)
        ba.componentsPriv := RefList.Cons(self, ba.componentsPriv);
      END
    END;
    RETURN self;
  END Init; 

PROCEDURE Delete (self: T) =
  VAR
    baH: BaseAssembly.T;
    apH: AtomicPart.T;
    ref: REFANY;
  BEGIN
    IF NOT Globals.DocumentIdx.delete(self.documentation.title, ref)
      OR ref # self.documentation THEN
      Put("error deleting document from title index in CompPart destructor\n");
    END;
    IF NOT Globals.DocumentIdIdx.delete(self.documentation.id, ref)
      OR ref # self.documentation THEN
      Put("error deleting document from title index in CompPart destructor\n");
    END;

    self.documentation.delete();

    (* walk through usedInPriv set deleting all backward references
       to this composite part from base assemblies of which it is a part *)
    VAR baseI := self.usedInPriv; (* establish baseassembly iterator *)
    BEGIN
      WHILE baseI # NIL DO
        baH := baseI.head;
        baseI := baseI.tail;
        VAR pl: RefList.T := NIL; rl := baH.componentsPriv;
        BEGIN
          WHILE rl # NIL DO
            IF self = rl.head THEN
              IF pl = NIL THEN
                baH.componentsPriv := rl.tail;
              ELSE
                pl.tail := rl.tail;
              END;
              EXIT;
            ELSE
              pl := rl;
              rl := rl.tail;
            END
          END
        END
      END
    END;
    self.usedInPriv := NIL;

    (* walk through usedInShar set deleting all backward references
       to this composite part from base assemblies of which it is a part *)
    VAR baseI := self.usedInShar;
    BEGIN
      WHILE baseI # NIL DO
        baH := baseI.head;
        baseI := baseI.tail;
        VAR pl: RefList.T := NIL; rl := baH.componentsShar;
        BEGIN
          WHILE rl # NIL DO
            IF self = rl.head THEN
              IF pl = NIL THEN
                baH.componentsShar := rl.tail;
              ELSE
                pl.tail := rl.tail;
              END;
              EXIT;
            ELSE
              pl := rl;
              rl := rl.tail;
            END
          END
        END
      END
    END;
    self.usedInShar := NIL;

    (* delete all atomic parts that compose this composite part *)
    FOR i := 0 TO LAST(self.parts^) DO
      apH := self.parts[i];
      IF NOT Globals.AtomicPartIdx.delete(apH.id, ref) OR apH # ref THEN
        Put("index error when deleting atomic parts\n");
      END;
      apH.delete();
    END;
    self.parts := NIL;

(*
    dts 3-11-93 AllCompParts is not used, so don't maintain it.
    (used in Reorg code though, but that's not called currently)
    EVAL AllCompParts.delete(this);	 (* finally remove from the extent *)
*)
  END Delete;
    
PROCEDURE Traverse (self: T; op: OO7.BenchmarkOp;
                    writer: Transaction.T): INTEGER =
  VAR result := 0;
  BEGIN
    IF Globals.debugMode THEN
      Put("\t\t\tCompositePart::traverse(id = "); PutInt(self.id);
      Put(", op = "); PrintOp(op); Put(")\n");
    END;

    (* do DFS of the composite part's atomic part graph *)
    IF op >= OO7.BenchmarkOp.Trav1 AND op <= OO7.BenchmarkOp.Trav3c THEN
      (*  do parameterized DFS of atomic part graph *)
      IF writer # NIL THEN
        writer.lock(self.rootPart, Transaction.LockMode.WRITE);
      END;
      VAR visitedIds := NEW(PartIdSet.T);
      PROCEDURE p(): INTEGER =
        BEGIN
          RETURN self.rootPart.traverse(op, visitedIds, writer);
        END p;
      BEGIN
        TRY
          result := p();
        FINALLY
          visitedIds.dispose();
        END;
      END;
    ELSIF op = OO7.BenchmarkOp.Trav4 THEN
      (* search document text for a certain character *)
      result := self.documentation.searchText('I');
    ELSIF op = OO7.BenchmarkOp.Trav5do THEN
      (* conditionally change initial part of document text *)
      result := self.documentation.replaceText("I am", "This is");
    ELSIF op = OO7.BenchmarkOp.Trav5undo THEN
      (* conditionally change back initial part of document text *)
      result := self.documentation.replaceText("This is", "I am");
    ELSIF op = OO7.BenchmarkOp.Trav6 THEN
      (* visit the root part only (it knows how to handle this) *)
      IF writer # NIL THEN
        writer.lock(self.rootPart, Transaction.LockMode.WRITE);
      END;
      VAR visitedIds := NEW(PartIdSet.T);
      PROCEDURE p(): INTEGER =
        BEGIN
          RETURN self.rootPart.traverse(op, visitedIds, writer);
        END p;
      BEGIN
        TRY
          result := p();
        FINALLY
          visitedIds.dispose();
        END;
      END;
    ELSE 
      (* composite parts don't respond to other traversals *)
      Put("*** CompositePart::PANIC -- illegal traversal!!! ***\n");
    END;
    RETURN result;
  END Traverse;

PROCEDURE Traverse7 (self: T): INTEGER =
  VAR result := 0;
  BEGIN
    IF Globals.debugMode THEN
      Put("\tCompositePart::traverse7(id = "); PutInt(self.id); Put(")\n");
    END;

    VAR
      visitedBaseIds := NEW(PartIdSet.T);
      visitedComplexIds := NEW(PartIdSet.T);
    PROCEDURE p(): INTEGER =
      (* search up the design hierarchy (along the private path) *)
      VAR
        count := 0;
        baseI := self.usedInPriv;
        ba: BaseAssembly.T;
      BEGIN
        (* establish iterator of private base assemblies *)
        WHILE baseI # NIL DO
          ba := baseI.head;
          baseI := baseI.tail;
          IF NOT visitedBaseIds.contains(ba.id) THEN
            visitedBaseIds.insert(ba.id);
            INC(count, ba.traverse7(visitedComplexIds));
          END;
        END;
        RETURN count;
      END p;
    BEGIN
      TRY
        result := p();
      FINALLY
        visitedBaseIds.dispose();
        visitedComplexIds.dispose();
      END;
    END;
    RETURN result;
  END Traverse7;
  
BEGIN
END CompositePart.
