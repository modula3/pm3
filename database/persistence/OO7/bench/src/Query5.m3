MODULE Query5 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #5 - find all base assemblies B that "use" a composite
  // part with a more recent build date than B's build date.
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT Globals, Module, BaseAssembly, CompositePart;
FROM IO IMPORT Put, PutInt;

PROCEDURE query5(): INTEGER =
  VAR
    modH: Module.T;
    baseH: BaseAssembly.T;
    compH: CompositePart.T;
    count := 0;
  BEGIN
    (* scan all modules, scanning all of the base assemblies in each one,
       scanning all of the composite parts that they use, looking for base
       assemblies that use more recently dated composite parts *)

    FOR i := 0 TO Globals.AllModules.size() - 1 DO
      modH := Globals.AllModules.get(i);
      (* for each module iterate over its base assemblies
         this assumes that we maintain a collection of base asemblies
         in each module *)
      VAR baseI := modH.allBases;
      BEGIN
        WHILE baseI # NIL DO
          baseH := baseI.head; baseI := baseI.tail;
          IF Globals.debugMode THEN
            Put("In Query5, processing base assembly "); PutInt(baseH.id);
            Put(", buildDate = "); PutInt(baseH.buildDate); Put("\n");
          END;
        
          VAR compI := baseH.componentsPriv;
          BEGIN
            WHILE compI # NIL DO
              compH := compI.head; compI := compI.tail;
              IF Globals.debugMode THEN
                Put("            [Checking composite part "); PutInt(compH.id);
                Put(", buildDate = "); PutInt(compH.buildDate); Put("]\n");
              END;
              IF compH.buildDate > baseH.buildDate THEN
                baseH.doNothing();
                INC(count);
              END
            END
          END
        END
      END
    END;
    RETURN count;
  END query5;

BEGIN
END Query5.
