MODULE M3CConcTypeSpec;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT Text;
IMPORT AST, M3AST_AS,  M3AST_SM, M3CUnit;


IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;


IMPORT SeqM3AST_AS_DECL_REVL, SeqM3AST_AS_REVELATION, SeqM3AST_AS_TYPE_SPEC,
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_IMPORTED,
    SeqM3AST_AS_Import_item;
IMPORT SeqM3AST_SM_Opaque_type_Revln;

IMPORT M3CTypesMisc, M3CTypeRelation;
IMPORT ASTWalk, M3Context;
IMPORT M3Error;

(* The scope rules for revelations are somewhat complex.
If any of the interfaces directly imported or exported by a unit
contain an identification, then that identification is also in scope
in the unit, plus any specific revelations made in said unit.
*)

TYPE
  RevealType = {Concrete, Opaque};
  RevealLocation = {Direct, Indirect};


VAR
  gUNIT: M3AST_AS.UNIT_NORMAL;


PROCEDURE NotOpaque(id: M3AST_AS.ID) RAISES {}=
  BEGIN
    M3Error.ReportWithId(id,
        "identifier \'%s\' is not declared as an opaque type", id.lx_symrep);
  END NotOpaque;


PROCEDURE IsBranded(
    reveal_ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE reveal_ts OF
    | NULL => RETURN TRUE;
    | M3AST_AS.Ref_type(refType) =>
        RETURN refType.as_brand # NIL;
    | M3AST_AS.Object_type(objectType) =>
        (* any branded concrete object in the supertype chain will do *)
        RETURN objectType.as_brand # NIL;
    ELSE
      RETURN FALSE;
    END;
  END IsBranded;


PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    reveal_ts: M3AST_SM.TYPE_SPEC_UNSET;
    iter: SeqM3AST_AS_DECL_REVL.Iter;
    iter_rev: SeqM3AST_AS_REVELATION.Iter;
    decl_revl: M3AST_AS.DECL_REVL;
    revln: M3AST_AS.REVELATION;
    rhsQualId: M3AST_AS.Used_def_id;
  BEGIN
    gUNIT := cu.as_root;

    PropagateRevelations(gUNIT);

    iter := SeqM3AST_AS_DECL_REVL.NewIter(gUNIT.as_block.as_decl_s);
    WHILE SeqM3AST_AS_DECL_REVL.Next(iter, decl_revl) DO
      TYPECASE decl_revl OF
      | M3AST_AS.Revelation_s(rev_s) =>
          iter_rev := SeqM3AST_AS_REVELATION.NewIter(rev_s.as_reveal_s);
          WHILE SeqM3AST_AS_REVELATION.Next(iter_rev, revln) DO
            rhsQualId := revln.as_qual_id.as_id;
            TYPECASE rhsQualId.sm_def OF
            | NULL =>
            | M3AST_AS.Type_id(opaqueTypeId) =>
                TYPECASE opaqueTypeId.sm_type_spec OF
                | NULL => (* previous error *)
                | M3AST_AS.Opaque_type(opaqueType) =>
                    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
                        revln.as_type, reveal_ts);
                    IF reveal_ts = NIL THEN
                      (* Previous error *)
                    ELSIF NOT M3CTypesMisc.IsRef(reveal_ts) THEN
                      M3Error.Report(revln.as_type,
                          "revealed type is not a reference type");
                    ELSIF IsRecursive(opaqueType, reveal_ts) THEN
                      M3Error.Report(revln.as_type,
                          "illegal recursive revelation");
                    ELSE
                      IF ISTYPE(revln, M3AST_AS.Subtype_reveal) THEN
                        AddRevelation(opaqueTypeId, reveal_ts,
                            RevealType.Opaque, RevealLocation.Direct);
                      ELSE
                        (* Concrete_reveal  *)
                        IF opaqueType.sm_concrete_type_spec = NIL THEN
                          (* must be a branded reference type. *)
                          IF reveal_ts # revln.as_type OR
                              NOT IsBranded(reveal_ts) THEN
                            M3Error.Report(revln.as_type,
                                 "\'REVEAL =\' must be followed by a BRANDED reference type expression");
                          END;
                        ELSE
                          M3Error.ReportWithId(opaqueTypeId,
                              "different \'REVEAL =\' for opaque type %s",
                              opaqueTypeId.lx_symrep);
                        END;
                        (* even in the event of an error we add the revelation
                           to minimise a cascade of errors about missing fields
                           etc. *)
                        AddRevelation(opaqueTypeId, reveal_ts,
                            RevealType.Concrete, RevealLocation.Direct);
                      END; (* concrete-reveal *)
                    END;
                ELSE
                  NotOpaque(rhsQualId);
                END; (* Typecase opaqueTypeId.sm_type_spec *)
            ELSE
              NotOpaque(rhsQualId);
            END; (* typecase *)
          END; (* while SEQREVELATION *)
      ELSE
      END; (* typecase Revelation_s *)
    END; (* while *)
  END Set;


TYPE
  Map = RECORD
    original: M3AST_AS.Opaque_type;
    count: CARDINAL;
    seen: ARRAY [0..15] OF M3AST_AS.TYPE_SPEC;
  END;


EXCEPTION
  RecursionFound;


PROCEDURE Recurse(
    type: M3AST_AS.TYPE_SPEC;
    VAR map: Map)
    RAISES {RecursionFound}=
  BEGIN
    IF type = NIL THEN RETURN END;
    FOR i := 0 TO map.count - 1 DO
      IF map.seen[i] = type THEN RETURN END;
    END;
    IF map.count <= LAST(map.seen) THEN
      map.seen[map.count] := type;
      INC(map.count);
    END;
    TYPECASE type OF
    | M3AST_AS.Object_type(objectType) =>
        IF objectType.as_ancestor # NIL THEN
          VAR
            ts: M3AST_SM.TYPE_SPEC_UNSET;
          BEGIN
            M3CTypesMisc.GetTYPE_SPECFromM3TYPE(objectType.as_ancestor, ts);
            Recurse(ts, map);
          END;
        END;
    | M3AST_AS.Opaque_type(opaqueType) =>
        IF opaqueType = map.original THEN RAISE RecursionFound END;
        Recurse(opaqueType.sm_concrete_type_spec, map);
        VAR
          iter := SeqM3AST_AS_TYPE_SPEC.NewIter(opaqueType.sm_type_spec_s);
          ts: M3AST_AS.TYPE_SPEC;
        BEGIN
          WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, ts) DO
            Recurse(ts, map);
          END;
        END;
    ELSE
    END; (* typecase *)
  END Recurse;


PROCEDURE IsRecursive(
    opaque, revelation: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {}=
  VAR
    map: Map;
  BEGIN
    IF ISTYPE(revelation, M3AST_AS.Ref_type) THEN RETURN FALSE END;
    map.original := opaque;
    map.count := 0;
    TRY
      Recurse(revelation, map);
      RETURN FALSE;
    EXCEPT
    | RecursionFound => RETURN TRUE;
    END;
  END IsRecursive;


PROCEDURE PropagateRevelations(unit: M3AST_AS.UNIT_NORMAL) RAISES {}=
  VAR
    used_intf_id: M3AST_AS.Used_interface_id;
    iter: SeqM3AST_AS_Used_interface_id.Iter;
  BEGIN
    (* The set of revelations for this unit is initialised to all
    those visible in imported/exported units.
    *)
    TYPECASE unit OF
    | M3AST_AS.Module(module) =>
        iter :=
            SeqM3AST_AS_Used_interface_id.NewIter(module.sm_export_s);
        WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_intf_id) DO
          AddDirectRevelations(used_intf_id);
        END; (* while *)
    ELSE
    END;
    VAR 
      iterImports := SeqM3AST_AS_IMPORTED.NewIter(unit.as_import_s);
      import: M3AST_AS.IMPORTED;
    BEGIN
      WHILE SeqM3AST_AS_IMPORTED.Next(iterImports, import) DO
        TYPECASE import OF <*NOWARN*>
        | M3AST_AS.From_import =>
            (* these do not propagate revelations *)
        | M3AST_AS.Simple_import(si) =>
            VAR
              iter_ii := SeqM3AST_AS_Import_item.NewIter(
                  si.as_import_item_s);
              import_item: M3AST_AS.Import_item;
            BEGIN
              WHILE SeqM3AST_AS_Import_item.Next(iter_ii, import_item) DO
                AddDirectRevelations(import_item.as_intf_id);
              END; (* while *)
            END;
        END; (* typecase *)
      END;
    END;
  END PropagateRevelations;


PROCEDURE AddRevelation(
    id: M3AST_AS.Type_id;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    reveal_type: RevealType;
    reveal_loc: RevealLocation)
    RAISES {}=
  VAR
    otr: M3AST_SM.Opaque_type_Revln;
    ot: M3AST_AS.Opaque_type;
  BEGIN
    (* add to UNIT given by gUNIT. *)
    otr := FindRevealedTypeFromId(gUNIT.sm_reveal_s, id);
    IF reveal_type = RevealType.Concrete THEN
      IF reveal_loc = RevealLocation.Direct THEN
        otr.sm_concrete_rev := ts;
      ELSE
        otr.tmp_concrete_rev := ts;
      END;
    ELSE
      IF reveal_loc = RevealLocation.Direct THEN
        SeqM3AST_AS_TYPE_SPEC.AddRear(otr.sm_opaque_rev_s, ts);
      ELSE
        SeqM3AST_AS_TYPE_SPEC.AddRear(otr.tmp_opaque_rev_s, ts);
      END;
    END; (* if *)

    (* add it the the data on the Opaque_type node as well. *)
    ot := Opaque_typeOfRevln(otr);
    IF reveal_type = RevealType.Concrete THEN
      ot.sm_concrete_type_spec := ts;
    ELSE
      VAR iter := SeqM3AST_AS_TYPE_SPEC.NewIter(ot.sm_type_spec_s);
        tts: M3AST_AS.TYPE_SPEC;
      BEGIN
        WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, tts) DO
          IF ts = tts THEN RETURN END;
        END; (* while *)
        SeqM3AST_AS_TYPE_SPEC.AddRear(ot.sm_type_spec_s, ts);
      END;
    END; (* if *)
  END AddRevelation;


PROCEDURE AddIndirectRevelation(
    interfaceId: M3AST_AS.Used_interface_id;
    id: M3AST_AS.Type_id;
    typeSpec: M3AST_AS.TYPE_SPEC;
    revealType: RevealType)
    RAISES {}=
  BEGIN
    IF IsRecursive(id.sm_type_spec, typeSpec) THEN
      M3Error.ReportWithId(interfaceId,
          "revelation for \'%s\' in import or export \'%s\' is recursive",
          id.lx_symrep, interfaceId.lx_symrep);
    ELSE
      AddRevelation(id, typeSpec, revealType, RevealLocation.Indirect);
    END;
  END AddIndirectRevelation;


PROCEDURE AddDirectRevelations(
    usedIntId: M3AST_AS.Used_interface_id)
    RAISES {}=
  VAR
    iter: SeqM3AST_SM_Opaque_type_Revln.Iter;
    iter2: SeqM3AST_AS_TYPE_SPEC.Iter;
    otr: M3AST_SM.Opaque_type_Revln;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    unit_id := NARROW(usedIntId.sm_def, M3AST_AS.UNIT_ID);
  BEGIN
    iter := SeqM3AST_SM_Opaque_type_Revln.NewIter(
        NARROW(unit_id.sm_spec, M3AST_AS.UNIT_WITH_BODY).sm_reveal_s);
    WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter, otr) DO
      IF otr.sm_concrete_rev # NIL THEN
        AddIndirectRevelation(usedIntId,
            otr.sm_type_id, otr.sm_concrete_rev, RevealType.Concrete);
      END; (* if *)
      iter2 := SeqM3AST_AS_TYPE_SPEC.NewIter(otr.sm_opaque_rev_s);
      WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter2, ts) DO
        AddIndirectRevelation(usedIntId,
            otr.sm_type_id, ts, RevealType.Opaque);
      END; (* while *)
    END;
  END AddDirectRevelations;


PROCEDURE FindRevealedTypeFromId(
    VAR (*inout*) reveal_s: SeqM3AST_SM_Opaque_type_Revln.T;
    id: M3AST_AS.Type_id)
    : M3AST_SM.Opaque_type_Revln
    RAISES {}=
  VAR
    iter: SeqM3AST_SM_Opaque_type_Revln.Iter;
    otr: M3AST_SM.Opaque_type_Revln;
  BEGIN
    iter := SeqM3AST_SM_Opaque_type_Revln.NewIter(reveal_s);
    WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter, otr) DO
      IF otr.sm_type_id = id THEN
        RETURN otr;
      END; (* if *)
    END; (* while *)
    otr := NEW(M3AST_SM.Opaque_type_Revln).init();
    otr.sm_concrete_rev := NIL;
    otr.sm_opaque_rev_s := SeqM3AST_AS_TYPE_SPEC.Null;
    otr.tmp_concrete_rev := NIL;
    otr.tmp_opaque_rev_s := SeqM3AST_AS_TYPE_SPEC.Null;
    otr.sm_type_id := id;
    SeqM3AST_SM_Opaque_type_Revln.AddRear(reveal_s, otr);
    RETURN otr;
  END FindRevealedTypeFromId;


PROCEDURE FindRevealedType(
    reveal_s: SeqM3AST_SM_Opaque_type_Revln.T;
    ot: M3AST_AS.Opaque_type)
    : M3AST_SM.Opaque_type_Revln
    RAISES {}=
  VAR
    iter: SeqM3AST_SM_Opaque_type_Revln.Iter;
    otr: M3AST_SM.Opaque_type_Revln;
  BEGIN
    iter := SeqM3AST_SM_Opaque_type_Revln.NewIter(reveal_s);
    WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter, otr) DO
      IF Opaque_typeOfRevln(otr) = ot THEN
        RETURN otr;
      END; (* if *)
    END; (* while *)
    RETURN NIL;
  END FindRevealedType;


PROCEDURE CurrentReveal(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    rts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    TYPECASE ts OF
    | NULL =>
    | M3AST_AS.Opaque_type(ot) =>
        rts := ot.tmp_rev_type_spec;
        IF rts = NIL THEN
          (* view is original declaration, but watch for recursion *)
          NRGetTYPE_SPECFromM3TYPE(ot.as_type, ts);
        ELSE
          ts := rts;   (* view is current revelation *)
        END; (* if *)
    ELSE
    END; (* if *)
    RETURN ts;
  END CurrentReveal;


PROCEDURE SetCurrentReveal(
    cu: M3AST_AS.Compilation_Unit;
    mode: ASTWalk.VisitMode)
    RAISES {}=
  VAR
    iter: SeqM3AST_SM_Opaque_type_Revln.Iter;
    otr: M3AST_SM.Opaque_type_Revln;
    ot: M3AST_AS.Opaque_type;
    reveal_s: SeqM3AST_SM_Opaque_type_Revln.T;
  BEGIN
    gUNIT := cu.as_root;
    reveal_s := gUNIT.sm_reveal_s;
    IF mode = ASTWalk.VisitMode.Exit THEN CheckPartialRevelations(cu); END;

    (* check all revealed types in the unit and set their view to Unset *)
    iter := SeqM3AST_SM_Opaque_type_Revln.NewIter(reveal_s);
    WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter, otr) DO
      ot := Opaque_typeOfRevln(otr);
      ot.tmp_rev_type_spec := NIL;
    END; (* while *)

    IF mode = ASTWalk.VisitMode.Entry THEN
      (* now look at list of revealed types and recompute relevant views. *)
      iter := SeqM3AST_SM_Opaque_type_Revln.NewIter(reveal_s);
      WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter, otr) DO
        ot := Opaque_typeOfRevln(otr);
	ot.tmp_rev_type_spec := MostRevealing(reveal_s, otr);
      END; (* while *)
    END;
  END SetCurrentReveal;

PROCEDURE CheckPartialRevelations(cu: M3AST_AS.Compilation_Unit;
    ) RAISES {}=
  VAR
    iter_revln := SeqM3AST_SM_Opaque_type_Revln.NewIter(
        NARROW(cu.as_root, M3AST_AS.UNIT_WITH_BODY).sm_reveal_s);
    otr: M3AST_SM.Opaque_type_Revln;
  BEGIN
    WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter_revln, otr) DO
      VAR
        partials := NEW(REF ARRAY OF M3AST_AS.TYPE_SPEC,
            SeqM3AST_AS_TYPE_SPEC.Length(otr.sm_opaque_rev_s) +
	    SeqM3AST_AS_TYPE_SPEC.Length(otr.tmp_opaque_rev_s) + 2);
        iter_sm := SeqM3AST_AS_TYPE_SPEC.NewIter(otr.sm_opaque_rev_s);
	iter_tmp := SeqM3AST_AS_TYPE_SPEC.NewIter(otr.tmp_opaque_rev_s);
	i := 0;
	ts, rts_j, rts_k: M3AST_AS.TYPE_SPEC;
      BEGIN
        WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter_sm, ts) DO
	  partials[i] := ts; INC(i);
        END; (* while *)
        WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter_tmp, ts) DO
	  partials[i] := ts; INC(i);
        END; (* while *)
        (* dont forget any concrete revelations *)
        IF otr.sm_concrete_rev # NIL THEN
          partials[i] := otr.sm_concrete_rev; INC(i);
        END; (* if *)
        IF otr.tmp_concrete_rev # NIL THEN
          partials[i] := otr.tmp_concrete_rev; INC(i);
        END; (* if *)
        (* Ok, now for every element of 'partials', check that
        it is a subtype of all the others (or vice versa). *)
        FOR j := 0 TO i-1 DO
          FOR k := 0 TO i-1 DO
	    IF j # k THEN
              (* If the RHS of the revelation is opaque (and revealed)
              we have to see through it here, because M3CTypeRelation.SubType
              only sees through opaque ancestors (otherwise it might find
              two opaque types equal!
              *)
	      rts_j := M3CTypesMisc.Reveal(partials[j]);
	      rts_k := M3CTypesMisc.Reveal(partials[k]);
	      IF NOT (M3CTypeRelation.SubType(rts_j, rts_k) OR
                      M3CTypeRelation.SubType(rts_k, rts_j)) THEN
                VAR
                  report_node: M3Error.ERROR_NODE;
                BEGIN
                  (* This is a hack. The actual Subtype_reveal node
                    should be stored in an M3AST_SM.Opaque_type_Revln,
                    which would allow completely accurate reporting.
                  *)
                  report_node := FindRevelationInUnit(cu, partials[j]);
                  IF report_node = NIL THEN
                    report_node := FindRevelationInUnit(cu, partials[k]);
                    IF report_node = NIL THEN report_node := otr.sm_type_id END;
                  END;
                  M3Error.ReportWithId(report_node,
                    "incompatible partial \'REVEAL\' for opaque type \'%s\'",
                       otr.sm_type_id.lx_symrep);
                END; (* begin *)
	      END; (* if *)
	    END; (* if *)
	  END; (* for *)
        END; (* for *)
      END;
    END; (* while *)    
  END CheckPartialRevelations;

TYPE FClosure = ASTWalk.Closure OBJECT 
    ts: M3AST_AS.TYPE_SPEC;
    n: M3AST_AS.Subtype_reveal := NIL;
  OVERRIDES
    callback := FindRevelation;
  END;

PROCEDURE FindRevelationInUnit(cu: M3AST_AS.Compilation_Unit;
    ts: M3AST_AS.TYPE_SPEC
    ): M3AST_AS.Subtype_reveal RAISES {}=
  <*FATAL ANY*>
  VAR fcl := NEW(FClosure, ts := ts);
  BEGIN
    ASTWalk.VisitNodes(cu, fcl);
    RETURN fcl.n;
  END FindRevelationInUnit;

PROCEDURE FindRevelation(fcl: FClosure; n: AST.NODE; 
    <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {ASTWalk.Aborted}=
  VAR
    reveal_ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Subtype_reveal(s) =>
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
                        s.as_type, reveal_ts);
        IF fcl.ts = reveal_ts THEN
	  fcl.n := s;
	  ASTWalk.Abort();
        END;
    ELSE (* ignore *)
    END;
  END FindRevelation;

PROCEDURE Opaque_typeOfRevln(
    otr: M3AST_SM.Opaque_type_Revln)
    : M3AST_AS.Opaque_type
    RAISES {}=
  BEGIN
    RETURN NARROW(otr.sm_type_id, M3AST_AS.TYPED_ID).sm_type_spec;
  END Opaque_typeOfRevln;


CONST
  BadLength = FIRST(INTEGER);


PROCEDURE MostRevealing(
    reveal_s: SeqM3AST_SM_Opaque_type_Revln.T;
    otr: M3AST_SM.Opaque_type_Revln)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    iter: SeqM3AST_AS_TYPE_SPEC.Iter;
    rts, ts: M3AST_SM.TYPE_SPEC_UNSET;
    l, lt: INTEGER;
  BEGIN
    (* reveal_s is list of all REVEALs that we know about; otr is the
    handle on the opaque type we are trying to set, and otr.sm_opaque_rev_s,
    otr.tmp_opaque_rev_s are the list of partial reveals that we have.
    First check for a concrete revelation...
    *)
    IF otr.sm_concrete_rev # NIL THEN
        rts := otr.sm_concrete_rev;
        l := Length(reveal_s, rts);
    ELSIF otr.tmp_concrete_rev # NIL THEN
        rts := otr.tmp_concrete_rev;
        l := Length(reveal_s, rts);
    ELSE
      rts := NIL;
      l := BadLength;
      iter := SeqM3AST_AS_TYPE_SPEC.NewIter(otr.sm_opaque_rev_s);
      WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, ts) DO
        lt := Length(reveal_s, ts);
        IF (lt >= 0) AND (lt > l) THEN
          l := lt; rts := ts;
        END; (* if *)
      END; (* while *)
      iter := SeqM3AST_AS_TYPE_SPEC.NewIter(otr.tmp_opaque_rev_s);
      WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, ts) DO
        lt := Length(reveal_s, ts);
        IF (lt >= 0) AND (lt > l) THEN
          l := lt; rts := ts;
        END; (* if *)
      END; (* while *)
    END;
    otr.tmp_count := l;
    RETURN rts;
  END MostRevealing;


PROCEDURE Length(
    reveal_s: SeqM3AST_SM_Opaque_type_Revln.T;
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : INTEGER
    RAISES {}=
  VAR
    anc: M3AST_AS.M3TYPE_NULL;
    superType: M3AST_SM.TYPE_SPEC_UNSET;
    otr: M3AST_SM.Opaque_type_Revln;
    l: INTEGER;
  BEGIN
    l := BadLength;
    IF ts = NIL THEN RETURN l END;
    TYPECASE ts OF
    | M3AST_AS.RefAny_type, M3AST_AS.Root_type,
      M3AST_AS.Address_type =>
        l := 0;

    | M3AST_AS.Object_type(objectType)=>
        anc := objectType.as_ancestor;
        IF anc = NIL THEN
          l := 1;
        ELSE
          (* is M3TYPE *)
          NRGetTYPE_SPECFromM3TYPE(anc, superType);
          l := 1 + Length(reveal_s, superType);
        END;

    | M3AST_AS.Opaque_type(opaqueType)=>
        otr := FindRevealedType(reveal_s, ts);
        IF otr = NIL THEN  (* have no REVs for this opaque type  *)
          NRGetTYPE_SPECFromM3TYPE(opaqueType.as_type, ts);
          l := 1 + Length(reveal_s, ts);
        ELSE
          (* we have revelations for this type, which can affect the
             outcome.
          *)
          ts := MostRevealing(reveal_s, otr);
          l := otr.tmp_count;
        END; (* if *)

    ELSE
    END; (* case *)
    RETURN l;
  END Length;


PROCEDURE NRGetTYPE_SPECFromM3TYPE(
    t: M3AST_AS.M3TYPE;
    VAR (*out*) ts: M3AST_SM.TYPE_SPEC_UNSET) RAISES {}=
  VAR
    used_def_id: M3AST_AS.Used_def_id;
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | M3AST_AS.TYPE_SPEC(typeSpec) => (* catches NULL case *)
        ts := typeSpec;
    | M3AST_AS.Named_type(namedType) =>
        used_def_id := namedType.as_qual_id.as_id;
        IF used_def_id.sm_def # NIL AND
            used_def_id.sm_def.tmp_recursive THEN
          ts := NIL;
        ELSE
          ts := namedType.sm_type_spec;
        END;
    END;
  END NRGetTYPE_SPECFromM3TYPE;


VAR
  context_g: M3Context.T;


PROCEDURE Validate(c: M3Context.T) RAISES {}=
  TYPE V_Closure = M3Context.Closure OBJECT OVERRIDES
      callback := ValidateUnit END;
  <*FATAL ANY*>
  BEGIN
    context_g := c;
    M3Context.Apply(c, NEW(V_Closure));
  END Validate;


PROCEDURE ValidateUnit(
    <*UNUSED*> cl: M3Context.Closure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: Text.T;
    cu: M3AST_AS.Compilation_Unit)
    RAISES {}=
  TYPE VU_Closure = ASTWalk.Closure OBJECT OVERRIDES
      callback := ValidateOpaqueType END;
  <*FATAL ANY*>
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    IF cu # NIL AND ut = M3CUnit.Type.Interface THEN
      ASTWalk.VisitNodes(cu, NEW(VU_Closure).init());
    END; (* if *)
  END ValidateUnit;


PROCEDURE ValidateOpaqueType(<*UNUSED*> cl: ASTWalk.Closure;
                             an: AST.NODE;
                             <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {}=
  VAR
    iter: SeqM3AST_AS_TYPE_SPEC.Iter;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    nts_s: SeqM3AST_AS_TYPE_SPEC.T;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Opaque_type(opaqueType) =>
        IF opaqueType.sm_concrete_type_spec # NIL THEN
          ValidateTYPE_SPEC(opaqueType.sm_concrete_type_spec);
          nts_s := SeqM3AST_AS_TYPE_SPEC.Null;
          iter := SeqM3AST_AS_TYPE_SPEC.NewIter(opaqueType.sm_type_spec_s);
          WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, ts) DO
            ValidateTYPE_SPEC(ts);
            IF ts # NIL THEN SeqM3AST_AS_TYPE_SPEC.AddRear(nts_s, ts); END;
          END; (* while *)
          opaqueType.sm_type_spec_s := nts_s;
        END;
    ELSE
    END; (* typecase *)
  END ValidateOpaqueType;


PROCEDURE ValidateTYPE_SPEC(
    VAR (*inout*) ts: M3AST_SM.TYPE_SPEC_UNSET)
    RAISES {}=
  VAR
    unit_id: M3AST_AS.UNIT_ID;
    ut: M3CUnit.Type;
    cu: M3AST_AS.Compilation_Unit;
  BEGIN
    unit_id := ts.tmp_unit_id;
    IF ISTYPE(unit_id, M3AST_AS.Interface_id) THEN
      ut := M3CUnit.Type.Interface;
    ELSE
      ut := M3CUnit.Type.Module;
    END;
    IF NOT M3Context.FindFromId(context_g, unit_id.lx_symrep, ut, cu) THEN
      ts := NIL;
    END; (* if *)
  END ValidateTYPE_SPEC;


BEGIN
END M3CConcTypeSpec.
