MODULE BuildScheme EXPORTS Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.8  1998/09/14 08:15:36  roland
    Modified code to remove compiler warnings.

    Revision 1.7  1998/03/18 13:41:16  roland
    New command line option '-local' enables the tool to work with the
    client part of a pool.

    Revision 1.6  1997/12/15 16:39:16  roland
    New man pages and unified command lines options. New option '-extend'
    to extend an old schema.

    Revision 1.5  1997/05/05 10:56:37  roland
    Adapted to HiGRAS.

    Revision 1.4  1997/03/25 17:07:58  roland
    GrasServerId-Parameter added.

    Revision 1.3  1997/02/28 14:28:00  roland
    Bugfix: use TypeGraphSystem instead of ChgMgmtGraphSystem

    Revision 1.2  1997/02/28 13:29:31  roland
    Both scheme tools now use GrasParams to parse GRAS3 command-line
    parameters.

    Revision 1.1  1997/02/27 15:52:01  roland
    This program parses Textfiles describing a scheme in a simple grammar and
    builds up a corresponding GRAS scheme.

*)
(***************************************************************************)

IMPORT ParseParams, IO, Text, Process, Stdio, Pathname, Rd, FileRd, Thread,
       FloatMode, Lex, OSError, Fmt, Scan, File;
IMPORT Scheme, TypedGraphSystem, Access, ErrorSupport, PageFile,
       TypedGraphPool;
IMPORT GrasParams;

VAR
  rootPath, schemeName: TEXT;
  serverid, nameserver: TEXT;
  poolname            : TEXT;
  cachesize           : CARDINAL;
  input               : TEXT;
  scheme              : Scheme.T;
  pool                : TypedGraphPool.T;
  rd                  : Rd.T;
  quiet               : BOOLEAN          := FALSE;
  keepOld             : BOOLEAN          := FALSE;
  local               : BOOLEAN          := FALSE;

EXCEPTION ParseError(TEXT);

PROCEDURE ErrPut (t: TEXT) =
  BEGIN
    IO.Put(t, Stdio.stderr);
  END ErrPut;

PROCEDURE ReadParams () =
  CONST USAGE = " <pool> <scheme> [-q] [-local] [-extend] " &
  "[-d <decl_file>] [-root <rootPath>]";

  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      ErrPut(err & "\n");
      ErrPut(prog & USAGE & "\n");
      Process.Exit(1);
    END ParseError;

  VAR valid: BOOLEAN;
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        poolname := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Pool name is mandatory first parameter");
      END;
      IF Text.Equal("-root", poolname) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Pool name is mandatory first parameter");
      END;

      TRY
        schemeName := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Scheme name is mandatory second parameter");
      END;
      IF Text.Equal("-root", schemeName) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Scheme name is mandatory second parameter");
      END;

      IF pp.keywordPresent("-q") THEN quiet := TRUE END;
      IF pp.keywordPresent("-extend") THEN keepOld := TRUE END;
      IF pp.keywordPresent("-local") THEN local := TRUE END;

      TRY
        input := NIL;
        IF pp.keywordPresent("-d") THEN input := pp.getNext(); END;
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Parameter '-d' requieres an argument.");
      END;

      GrasParams.ParseComandLine(
        rootPath, valid, cachesize, serverid, nameserver);
      IF NOT valid THEN
        ParseError(Pathname.Last(pp.arg[0]), "Need root path.");
      END;
    END;
  END ReadParams;

PROCEDURE ScanText (rd: Rd.T): TEXT RAISES {Thread.Alerted, Rd.Failure} =
  BEGIN
    Lex.Skip(rd);
    RETURN Lex.Scan(rd);
  END ScanText;

PROCEDURE ScanCard (rd: Rd.T; VAR card: Scheme.Cardinality)
  RAISES {ParseError} =
  VAR b: TEXT;
  BEGIN
    TRY
      Lex.Skip(rd);
      card.min := Lex.Int(rd);
      Lex.Skip(rd);
      b := Lex.Scan(rd);
      IF Text.Equal(b, "n") THEN
        card.max := LAST(CARDINAL);
      ELSE
        card.max := Scan.Int(b);
      END
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in ScanCard");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in ScanCard");
    | FloatMode.Trap => RAISE ParseError("FloatMode.Trap in ScanCard");
    | Lex.Error => RAISE ParseError("Number expected in ScanCard");
    END;
  END ScanCard;

PROCEDURE ScanAttributeKind (rd: Rd.T): Scheme.AttributeKind
  RAISES {ParseError} =
  VAR t: TEXT;
  BEGIN
    TRY
      t := ScanText(rd);
      IF Text.Equal(t, "intrinsic") THEN
        RETURN Scheme.AttributeKind.Intrinsic;
      ELSIF Text.Equal(t, "derived") THEN
        RETURN Scheme.AttributeKind.Derived;
      ELSIF Text.Equal(t, "dummy") THEN
        RETURN Scheme.AttributeKind.Dummy;
      ELSIF Text.Equal(t, "meta") THEN
        RETURN Scheme.AttributeKind.Meta;
      ELSE
        RAISE ParseError("Unknown attribute mode '" & t & "'!");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in ScanAttributeKind");
    | Thread.Alerted =>
        RAISE ParseError("Thread.Alerted in ScanAttributeKind");
    END;
  END ScanAttributeKind;

PROCEDURE ScanDependencyKind (rd: Rd.T): Scheme.DependencyKind
  RAISES {ParseError} =
  VAR t: TEXT;
  BEGIN
    TRY
      t := ScanText(rd);
      IF Text.Equal(t, "self") THEN
        RETURN Scheme.DependencyKind.Self
      ELSIF Text.Equal(t, "incoming") THEN
        RETURN Scheme.DependencyKind.Incoming;
      ELSIF Text.Equal(t, "outgoing") THEN
        RETURN Scheme.DependencyKind.Outgoing;
      ELSE
        RAISE ParseError("Unknown dependency kind '" & t & "'!");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in ScanDependencyKind");
    | Thread.Alerted =>
        RAISE ParseError("Thread.Alerted in ScanDependencyKind");
    END;
  END ScanDependencyKind;

PROCEDURE ScanIndexProperties (rd: Rd.T): Scheme.IndexProperties
  RAISES {ParseError} =
  VAR t: TEXT;
  BEGIN
    TRY
      t := ScanText(rd);
      IF Text.Equal(t, "normal") THEN
        RETURN Scheme.IndexProperties.Normal;
      ELSIF Text.Equal(t, "index") THEN
        RETURN Scheme.IndexProperties.Index;
      ELSIF Text.Equal(t, "key") THEN
        RETURN Scheme.IndexProperties.Key;
      ELSE
        RAISE ParseError("Unknown index property '" & t & "'!");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in ScanIndexProperties");
    | Thread.Alerted =>
        RAISE ParseError("Thread.Alerted in ScanIndexProperties");
    END;
  END ScanIndexProperties;

PROCEDURE ErrorAbort () =
  BEGIN
    TRY pool.abortTransaction(); EXCEPT ELSE END;
  END ErrorAbort;

PROCEDURE FmtID (id: Scheme.ID): TEXT =
  BEGIN
    RETURN "(" & Fmt.Int(id.graph) & ", " & Fmt.Int(id.entity) & ")";
  END FmtID;

PROCEDURE NodeClass (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR
    name : TEXT;
    class: Scheme.ID;
  BEGIN
    TRY
      name := ScanText(rd);
      class := scheme.declareNodeClass(name);
      IF NOT quiet THEN
        IO.Put("class " & name & " " & FmtID(class) & "\n");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in NodeClass");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in NodeClass");
    | Scheme.AlreadyDeclared =>
        RAISE ParseError("Class " & name & " already declared");
    | Scheme.InternalError (info) =>
        RAISE ParseError("NodeClass:\n" & ErrorSupport.ToText(info));
    END
  END NodeClass;

PROCEDURE NodeType (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR
    name: TEXT;
    type: Scheme.ID;
  BEGIN
    TRY
      name := ScanText(rd);
      type := scheme.declareNodeType(name);
      IF NOT quiet THEN
        IO.Put("type " & name & " " & FmtID(type) & "\n");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in NodeType");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in NodeType");
    | Scheme.AlreadyDeclared =>
        RAISE ParseError("Type " & name & " already declared");
    | Scheme.InternalError (info) =>
        RAISE ParseError("NodeType:\n" & ErrorSupport.ToText(info));
    END
  END NodeType;

PROCEDURE EdgeType (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR
    name, source, target  : TEXT;
    sourceCard, targetCard: Scheme.Cardinality;
    sourceCOT, targetCOT  : Scheme.ID;
    etype                 : Scheme.ID;
  <* FATAL Scheme.NotDeclared *>
  BEGIN
    TRY
      name := ScanText(rd);
      source := ScanText(rd);
      IF scheme.existsNodeClassByName(source) THEN
        sourceCOT := scheme.getNodeClassNumber(source);
      ELSIF scheme.existsNodeTypeByName(source) THEN
        sourceCOT := scheme.getNodeTypeNumber(source);
      ELSE
        RAISE
          ParseError("Source class or type '" & source & "' undeclared!");
      END;
      ScanCard(rd, sourceCard);
      target := ScanText(rd);
      IF scheme.existsNodeClassByName(target) THEN
        targetCOT := scheme.getNodeClassNumber(target);
      ELSIF scheme.existsNodeTypeByName(target) THEN
        targetCOT := scheme.getNodeTypeNumber(target);
      ELSE
        RAISE
          ParseError("Target class or type '" & target & "' undeclared!");
      END;
      ScanCard(rd, targetCard);
      etype := scheme.declareEdgeType(
                 name, sourceCOT, targetCOT, sourceCard, targetCard);
      IF NOT quiet THEN
        IO.Put("edge type " & name & " " & FmtID(etype) & "\n");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in EdgeType");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in EdgeType");
    | Scheme.AlreadyDeclared =>
        RAISE ParseError("Edge type " & name & " already declared");
    | Scheme.InternalError (info) =>
        RAISE ParseError("EdgeType:\n" & ErrorSupport.ToText(info));
    END
  END EdgeType;

PROCEDURE Attribute (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR
    cotname, attrname: TEXT;
    kind             : Scheme.AttributeKind;
    indexP           : Scheme.IndexProperties;
    type             : CARDINAL;
    cot              : Scheme.ID;
    len              : CARDINAL;
    constLen         : BOOLEAN;
    card             : Scheme.Cardinality;
    attr             : Scheme.ID;
  <* FATAL Scheme.NotDeclared, Scheme.AttributePlacementClash *>
  BEGIN
    TRY
      cotname := ScanText(rd);
      IF scheme.existsNodeClassByName(cotname) THEN
        cot := scheme.getNodeClassNumber(cotname);
      ELSIF scheme.existsNodeTypeByName(cotname) THEN
        cot := scheme.getNodeTypeNumber(cotname);
      ELSE
        RAISE ParseError("Class or type '" & cotname & "' undeclared!");
      END;
      attrname := ScanText(rd);
      kind := ScanAttributeKind(rd);
      indexP := ScanIndexProperties(rd);
      Lex.Skip(rd);
      type := Lex.Int(rd);
      ScanCard(rd, card);
      Lex.Skip(rd);
      constLen := Lex.Bool(rd);
      Lex.Skip(rd);
      len := Lex.Int(rd);
      attr := scheme.declareAttribute(
                cot, attrname, kind, indexP, type, card, constLen, len);
      IF NOT quiet THEN
        IO.Put("attribute " & cotname & "." & attrname & " " & FmtID(attr)
                 & "\n");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in Attribute");
    | Lex.Error => RAISE ParseError("Lex.Error in Attribute")
    | FloatMode.Trap => RAISE ParseError("FloatMode.Trap in Attribute");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in Attribute");
    | Scheme.AlreadyDeclared =>
        RAISE
          ParseError("Attribute " & attrname
                       & " already declared for class/type " & cotname);
    | Scheme.AttributeNameClash =>
        RAISE ParseError("Attribute name conflict for attribute "
                           & attrname & " at class " & cotname);
    | Scheme.InternalError (info) =>
        RAISE ParseError("Attribute:\n" & ErrorSupport.ToText(info));
    END
  END Attribute;

PROCEDURE AppendClassOrType (rd: Rd.T; scheme: Scheme.T)
  RAISES {ParseError} =
  VAR
    subname, supername: TEXT;
    sub, super        : Scheme.ID;
    type              : BOOLEAN;
  <* FATAL Scheme.NotDeclared, Scheme.AttributePlacementClash *>
  BEGIN
    TRY
      subname := ScanText(rd);
      IF scheme.existsNodeClassByName(subname) THEN
        sub := scheme.getNodeClassNumber(subname);
        type := FALSE;
      ELSIF scheme.existsNodeTypeByName(subname) THEN
        sub := scheme.getNodeTypeNumber(subname);
        type := TRUE;
      ELSE
        RAISE ParseError("Class or type '" & subname & "' undeclared!");
      END;

      supername := ScanText(rd);
      IF scheme.existsNodeClassByName(supername) THEN
        super := scheme.getNodeClassNumber(supername);
      ELSE
        RAISE ParseError("Class '" & supername & "' undeclared!");
      END;

      IF type THEN
        scheme.appendNodeType(sub, super);
        IF NOT quiet THEN
          IO.Put(subname & " instantiates " & supername & "\n");
        END;
      ELSE
        scheme.appendNodeClass(sub, super);
        IF NOT quiet THEN
          IO.Put(subname & " inherits from " & supername & "\n");
        END;
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in Append");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in Append");
    | Scheme.AlreadyDeclared =>
        RAISE
          ParseError(
            "Node type '" & subname & "' already has a defining class");
    | Scheme.Cyclic =>
        RAISE
          ParseError(
            "Introducing '" & subname & "' as sub class of '" & supername
              & "' would form a cycle in the inheritance hierarchy");
    | Scheme.AttributeNameClash =>
        RAISE
          ParseError(
            "Introducing '" & subname & "' as sub class of '" & supername
              & "' results in an attribute name conflict.");
    | Scheme.InternalError (info) =>
        RAISE ParseError("Append:\n" & ErrorSupport.ToText(info));
    END
  END AppendClassOrType;

PROCEDURE EvalFunction (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR
    cotname, attrname, funcname: TEXT;
    cot, attr                  : Scheme.ID;
  <* FATAL Scheme.NotDeclared *>
  BEGIN
    TRY
      cotname := ScanText(rd);
      IF scheme.existsNodeClassByName(cotname) THEN
        cot := scheme.getNodeClassNumber(cotname);
      ELSIF scheme.existsNodeTypeByName(cotname) THEN
        cot := scheme.getNodeTypeNumber(cotname);
      ELSE
        RAISE ParseError("Class or type '" & cotname & "' undeclared!");
      END;
      attrname := ScanText(rd);
      IF scheme.existsAttributeByName(cot, attrname) THEN
        attr := scheme.getAttributeNumber(cot, attrname);
      ELSE
        RAISE ParseError("Attribute " & attrname
                           & " not declared for class/type " & cotname);
      END;
      funcname := ScanText(rd);
      scheme.setEvaluationFunction(cot, attr, funcname);
      IF NOT quiet THEN
        IO.Put("evaluation function for " & cotname & "." & attrname & " "
                 & funcname & "\n");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in EvalFunction");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in EvalFunction");
    | Scheme.InternalError (info) =>
        RAISE ParseError("EvalFunction:\n" & ErrorSupport.ToText(info));
    END
  END EvalFunction;

PROCEDURE Dependency (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR
    cotname, attrname                : TEXT;
    defCotName, defAttrName          : TEXT;
    etypename                        : TEXT;
    kind                             : Scheme.DependencyKind;
    cot, attr, defcot, defattr, etype: Scheme.ID;
  <* FATAL Scheme.NotDeclared *>
  BEGIN
    TRY
      cotname := ScanText(rd);
      IF scheme.existsNodeClassByName(cotname) THEN
        cot := scheme.getNodeClassNumber(cotname);
      ELSIF scheme.existsNodeTypeByName(cotname) THEN
        cot := scheme.getNodeTypeNumber(cotname);
      ELSE
        RAISE ParseError("Class or type '" & cotname & "' undeclared!");
      END;
      attrname := ScanText(rd);
      IF scheme.existsAttributeByName(cot, attrname) THEN
        attr := scheme.getAttributeNumber(cot, attrname);
      ELSE
        RAISE ParseError("Attribute " & attrname
                           & " not declared for class/type " & cotname);
      END;

      defCotName := ScanText(rd);
      IF scheme.existsNodeClassByName(defCotName) THEN
        defcot := scheme.getNodeClassNumber(defCotName);
      ELSIF scheme.existsNodeTypeByName(defCotName) THEN
        defcot := scheme.getNodeTypeNumber(defCotName);
      ELSE
        RAISE ParseError("Class or type '" & defCotName & "' undeclared!");
      END;
      defAttrName := ScanText(rd);
      IF scheme.existsAttributeByName(cot, defAttrName) THEN
        defattr := scheme.getAttributeNumber(cot, defAttrName);
      ELSE
        RAISE ParseError("Attribute " & defAttrName
                           & " not declared for class/type " & defCotName);
      END;

      kind := ScanDependencyKind(rd);
      IF kind # Scheme.DependencyKind.Self THEN
        etypename := ScanText(rd);
        IF scheme.existsEdgeTypeByName(etypename) THEN
          etype := scheme.getEdgeTypeNumber(etypename);
        ELSE
          RAISE ParseError("Edge type " & etypename & "not declared");
        END;
      END;
      scheme.declareDependency(cot, attr, defattr, kind, etype);
      IF NOT quiet THEN
        IO.Put("dependency for " & cotname & "." & attrname & " on "
                 & defCotName & "." & defAttrName);
        IF kind = Scheme.DependencyKind.Incoming THEN
          IO.Put(" via incoming " & etypename);
        ELSIF kind = Scheme.DependencyKind.Outgoing THEN
          IO.Put(" via outgoing " & etypename);
        END;
        IO.Put("\n");
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in Dependency");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in Dependency");
    | Scheme.InternalError (info) =>
        RAISE ParseError("Dependency:\n" & ErrorSupport.ToText(info));
    END
  END Dependency;

PROCEDURE Build (rd: Rd.T; scheme: Scheme.T) RAISES {ParseError} =
  VAR key: TEXT;
  CONST allButNL = SET OF CHAR{'\000'.. '\377'} - SET OF CHAR{'\n'};
  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
        key := ScanText(rd);
        IF Text.Equal(key, "class") THEN
          NodeClass(rd, scheme);
        ELSIF Text.Equal(key, "type") THEN
          NodeType(rd, scheme);
        ELSIF Text.Equal(key, "edge") THEN
          EdgeType(rd, scheme);
        ELSIF Text.Equal(key, "attribute") THEN
          Attribute(rd, scheme);
        ELSIF Text.Equal(key, "append") THEN
          AppendClassOrType(rd, scheme);
        ELSIF Text.Equal(key, "eval") THEN
          EvalFunction(rd, scheme);
        ELSIF Text.Equal(key, "dependency") THEN
          Dependency(rd, scheme);
        ELSIF Text.Length(key) > 0 AND Text.GetChar(key, 0) = '#' THEN
          (* comment *)
          Lex.Skip(rd, allButNL);
        ELSIF NOT Rd.EOF(rd) THEN
          RAISE ParseError("Unknown key '" & key & "'");
        END;
      END;
    EXCEPT
      Rd.Failure => RAISE ParseError("Rd.Failure in Build");
    | Thread.Alerted => RAISE ParseError("Thread.Alerted in Build");
    END;
  END Build;

BEGIN
  VAR hIn, hOut, hErr: File.T;
  BEGIN
    Process.GetStandardFileHandles(
      stdin := hIn, stdout := hOut, stderr := hErr);
  END;
  ReadParams();
  TRY
    TypedGraphSystem.Login(rootPath, cachesize, serverid, nameserver);
    IF input # NIL THEN
      rd := FileRd.Open(input);
    ELSE
      rd := Stdio.stdin;
    END;
    IF TypedGraphSystem.ExistsPool(poolname) THEN
      pool := NEW(TypedGraphPool.T).open(
                poolname, Access.Mode.ReadWriteShared, new := FALSE);
    ELSE
      pool := NEW(TypedGraphPool.T).open(
                poolname, Access.Mode.ReadWriteShared, new := TRUE);
    END;
    scheme := NEW(Scheme.T).open(
                pool, schemeName, local := local,
                access := Scheme.AccessMode.ReadWriteExclusive,
                new := NOT keepOld);
    pool.beginTransaction();
    Build(rd, scheme);
    pool.commitTransaction();
    scheme.close();
    pool.close();
    IF rd # Stdio.stdin THEN
      TRY
        Rd.Close(rd)
      EXCEPT
        Rd.Failure, Thread.Alerted => (* ignore *)
      END;
    END;
  EXCEPT
  | Scheme.Existent => ErrorAbort(); ErrPut("Scheme existent!\n");
  | Scheme.InUse => ErrorAbort(); ErrPut("Scheme in use!\n");
  | Scheme.NotExistent => ErrorAbort(); ErrPut("Scheme not existent!\n");
  | TypedGraphSystem.InternalError (info) =>
      ErrPut(ErrorSupport.ToText(info));
  | Scheme.InternalError (info) =>
      ErrorAbort();
      ErrPut(ErrorSupport.ToText(info));
  | TypedGraphPool.InternalError (info) =>
      ErrorAbort();
      ErrPut(ErrorSupport.ToText(info));
  | TypedGraphPool.CardinalityError =>
      ErrorAbort();
      ErrPut("Cardinality error!\n");
  | TypedGraphPool.NotInTransaction =>
      ErrorAbort();
      ErrPut("Not in transaction!\n");
  | Access.Denied (msg) => ErrPut("Access denied: " & msg & "\n");
  | Access.Locked => ErrPut("Access locked!\n");
  | PageFile.NoAccess (msg) => ErrPut("No access: " & msg & "\n");
  | Scheme.NoValidScheme => ErrPut("Invalid scheme!\n");
  | ParseError (msg) => ErrPut("Parse error: " & msg & "\n");
  | OSError.E (info) =>
      ErrPut("Cannot open '" & input & "': " & ErrorSupport.Fmt(info));
  END
END BuildScheme.
