MODULE Query6 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #6 - find all assemblies (base or complex) B that "use" 
  // (directly or transitively ) a composite part with a more recent 
  // build date than B's build date.
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT Globals, Assembly, BaseAssembly, ComplexAssembly, CompositePart,
       Module;

FROM IO IMPORT Put, PutInt;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Recursive Workhorse Routine for Query #6
  //
  //////////////////////////////////////////////////////////////////
*)
PROCEDURE checkMaxCompDate(assmH: Assembly.T; VAR count:INTEGER) :INTEGER =
  VAR
    subAssmH: Assembly.T;
    cpH: CompositePart.T;
    rsltDate: INTEGER ;

    (* check to see if this assembly uses a more recently dated composite
       part, and process it if so - handling complex and base assemblies
       differently since the former have subassemblies while the latter
       directly use composite parts *)

    maxSubDate := -1;
  BEGIN
    TYPECASE assmH OF
    | ComplexAssembly.T (compH) =>
      (* complex assembly case - check its subassemblies recursively *)
      IF Globals.debugMode THEN
        Put("In Query6:  complex assembly "); PutInt(assmH.id);
        Put(", buildDate = "); PutInt(assmH.buildDate); Put("\n");
      END;
      VAR assmI := compH.subAssemblies;
      BEGIN
        WHILE assmI # NIL DO
          subAssmH := assmI.head; assmI := assmI.tail;
          rsltDate := checkMaxCompDate(subAssmH, count);
          IF rsltDate > maxSubDate THEN maxSubDate := rsltDate END;
        END
      END;
    | BaseAssembly.T (baseH) =>
      (* base assembly case - check its composite part build dates *)
      IF Globals.debugMode THEN
        Put("In Query6:  base assembly "); PutInt(assmH.id);
        Put(", buildDate = "); PutInt(assmH.buildDate); Put("\n");
        Put("    composite part dates = { ");
      END;
      VAR compI := baseH.componentsPriv;
      BEGIN
        WHILE compI # NIL DO
          cpH := compI.head; compI := compI.tail;
          IF Globals.debugMode THEN PutInt(cpH.buildDate) END;
          IF cpH.buildDate > maxSubDate THEN maxSubDate := cpH.buildDate END;
        END
      END;
      IF Globals.debugMode THEN Put("}\n") END;
    ELSE
      <*ASSERT FALSE*>
    END;

    (* see if the maximum build date of composite parts used by this assembly
       is greater than the assembly's build date, and process the assembly if
       this is found to be the case;  also, return the maximum build date for
       this subassembly *)

    IF maxSubDate > assmH.buildDate THEN
      assmH.doNothing();
      INC(count);
    END;
    RETURN maxSubDate;
  END checkMaxCompDate;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Top-Level Routine to do Query #6
  //
  //////////////////////////////////////////////////////////////////
*)

PROCEDURE query6(): INTEGER =
  VAR
    modH: Module.T;

    (* scan modules, visiting all of the assemblies in each one, checking
       to see if they use (either directly or indirectly) a more recently
       dated composite part;  the visits are done by recursively traversing
       the assemblies, checking them on the way back up (to avoid lots of
       repeated work) *)

    count := 0;
  BEGIN
    FOR i := 0 TO Globals.AllModules.size() - 1 DO
      modH := Globals.AllModules.get(i);
      EVAL checkMaxCompDate((modH.designRoot), count);
    END;
    RETURN count;
  END query6;

BEGIN
END Query6.
