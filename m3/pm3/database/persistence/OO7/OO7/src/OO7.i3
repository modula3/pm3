INTERFACE OO7;

IMPORT PartIdSet, RefList, RefSeq, Ctypes, Transaction, Text8;

(*--------------------------------------------------------------------------
   Start with some necessary preliminaries
  --------------------------------------------------------------------------*)
CONST
  TypeSize = 10;
  DummySize = 1000;

TYPE
  BenchmarkOp =
    { Trav1, Trav1WW, Trav2a, Trav2b, Trav2c, Trav3a, Trav3b, 
      Trav3c, Trav4, Trav5do, Trav5undo, Trav6, Trav7, Trav8, Trav9,
      Trav10, Query1, Query2, Query3, Query4, Query5, Query6, Query7, Query8, 
      Insert, Delete, Reorg1, Reorg2, WarmUpdate, MultiTrav1, MultiTrav2,
      MultiTrav3, MultiTrav4, MultiTrav5, MultiTrav6 };
  UpdateType = { UpdateOne, UpdateAll, UpdateRepeat };
  UpdateDirectionType = { UpdateDirectionDo, UpdateDirectionUndo };
  Type = ARRAY [0..TypeSize - 1] OF CHAR;

(*--------------------------------------------------------------------------
  AtomicPart objects are the primitives for building up designs
  - modeled after the Sun/OO1 benchmark's parts
  --------------------------------------------------------------------------*)
TYPE
  AtomicPart = OBJECT
    id:         INTEGER;
    type:       Type;
    buildDate:  INTEGER;
    x, y:       INTEGER;
    docId:	INTEGER;
    to:         RefSeq.T;		 (* to connection objects *)
    from:       RefSeq.T;		 (* back pointers *)
    partOf:     CompositePart;		 (* up pointer *)
  METHODS
    swapXY();
    toggleDate();
    doNothing();
    traverse(op: BenchmarkOp; visitedIds: PartIdSet.T;
             writer: Transaction.T): INTEGER;
    init(ptId: INTEGER; cp: CompositePart): AtomicPart;
    delete();
  END;
    
(*--------------------------------------------------------------------------
  Connection objects are used to wire AtomicParts together
  - similarly, modeled after Sun/OO1 connections
  --------------------------------------------------------------------------*)
TYPE
  Connection = OBJECT
    type:       Type;
    length:     INTEGER;
    to, from:   AtomicPart;
  METHODS
    init(fromPart, toPart: AtomicPart): Connection;
  END;

(*--------------------------------------------------------------------------
  CompositeParts are parts constructed from AtomicParts
  - entry in a library of reusesable components
  - implementation is a graph of atomic parts
  - provides unit of significant access locality
  - each has an associated (unique) document object
  --------------------------------------------------------------------------*)
TYPE
  CompositePart = OBJECT
    id: INTEGER;
    type: Type;
    buildDate: INTEGER;
    documentation: Document;
    parts: REF ARRAY OF AtomicPart;
    rootPart: AtomicPart;
    (* list of assemblies in which part is used as a private component *)
    usedInPriv: RefList.T;
    (* list of assemblies in which part is used as a shared component *)
    usedInShar: RefList.T;
  METHODS
    traverse(op: BenchmarkOp; writer: Transaction.T): INTEGER;
    traverse7(): INTEGER;
    reorg1(): INTEGER;
    reorg2(): INTEGER;
    init(cpId :INTEGER): CompositePart;
    delete();
  END;
  
(*--------------------------------------------------------------------------
  Document objects are used to hold a description of some particular
  CompositePart object
  --------------------------------------------------------------------------*)
TYPE
  Document = OBJECT
    title: TEXT;
    id: INTEGER;
    text: REF ARRAY OF CHAR;
    textLen: INTEGER;			 (* actual size of text area *)
    part: CompositePart;
  METHODS
    searchText(c: CHAR): INTEGER;
    replaceText(oldString, newString: Text8.T): INTEGER;
    init(cpId: INTEGER; cp: CompositePart): Document;
    delete();
  END;

(*--------------------------------------------------------------------------
  Manual objects are used to hold a description of some particular
  module --- essentially just large documents.
  --------------------------------------------------------------------------*)
TYPE
  Manual = OBJECT
    title: TEXT;
    id: INTEGER;
    text: REF ARRAY OF CHAR;
    textLen: INTEGER;			 (* actual size of text area *)
    mod: Module;
  METHODS
    searchText(c: CHAR): INTEGER;
    replaceText(oldString, newString: Text8.T): INTEGER;
    firstLast(): INTEGER;
    init(modId: INTEGER; myMod: Module): Manual;
    delete();
  END;

(*--------------------------------------------------------------------------
  Assembly objects are design instances built up recursively from
  from other Assembly objects and (at the leaves only) CompositeParts
  - hierarchical (tree) structure for designs
  - may share composite parts with other assemblies
  - nonleaf and leaf assembly subtypes
  --------------------------------------------------------------------------*)
TYPE
  Assembly = OBJECT
    id: INTEGER;
    type: Type;
    buildDate: INTEGER;
    superAssembly: ComplexAssembly;
  METHODS
    traverse(op: BenchmarkOp): INTEGER;
    traverse7(visitedComplexIds: PartIdSet.T): INTEGER;
    traverseRandom(op: BenchmarkOp; state: Ctypes.void_star;
                   writer: Transaction.T): INTEGER;
    doNothing();
  END;

  ComplexAssembly = Assembly OBJECT
    subAssemblies: RefList.T;		 (* list of composite subassemblies *)
  METHODS
    init(asId: INTEGER; mod: Module; parentAssembly: ComplexAssembly;
         levelNo: INTEGER): ComplexAssembly;
  END;

  BaseAssembly = Assembly OBJECT
    componentsPriv: RefList.T;		 (* list of private composite parts *)
    componentsShar: RefList.T;		 (* list of shared composite parts *)
  METHODS
    init(asId: INTEGER; mod: Module;
         parentAssembly: ComplexAssembly): BaseAssembly;
  END;

(*--------------------------------------------------------------------------
  Modules are the designs resulting from Assembly composition
  - unit of scaleup for the benchmark database
  - may share composite parts with other modules
  --------------------------------------------------------------------------*)
TYPE
  Module = OBJECT
    id: INTEGER;
    type: Type;
    buildDate: INTEGER;
    man: Manual;			 (* manual object *)
    designRoot: ComplexAssembly;
    allBases: RefList.T;
  METHODS
    traverse(op: BenchmarkOp): INTEGER;
    traverseRandom(op: BenchmarkOp; state: Ctypes.void_star;
                   writer: Transaction.T): INTEGER;
    scanManual(): INTEGER;
    firstLast(): INTEGER;
    init(modId: INTEGER): Module;
    delete();
  END;

END OO7. 
