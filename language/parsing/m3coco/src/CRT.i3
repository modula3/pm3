INTERFACE  CRT;
(* Symbol Table and Top-Down Graph *)

IMPORT Rd, Wr ;

CONST
  maxSymbols   =  500; (* max number of symbols
                          (terminals+nonterminals+pragmas) *)
  maxTerminals =  400; (* max number of terminals *)
  maxNt        =  128; (* max number of nonterminals *)
  maxNodes     = 1500; (* max number of top-down graph nodes *)
  maxClasses   =  250; (* max. number of character classes *)
  normTrans    =    0; (* DFA transition during normal scanning *)
  contextTrans =    1; (* DFA transition during scanning of right context *)

  (* node types *)
  unknown =  0;
  t     =  1; (* terminal symbol *)
  pr    =  2; (* pragma *)
  nt    =  3; (* nonterminal symbol *)
  class =  4; (* character class *)
  char  =  5; (* single character *)
  wt    =  6; (* weak terminal symbol *)
  any   =  7; (* symbol ANY *)
  eps   =  8; (* empty alternative *)
  sync  =  9; (* symbol SYNC *)
  sem   = 10; (* semantic action *)
  alt   = 11; (* alternative *)
  iter  = 12; (* iteration *)
  opt   = 13; (* option *)

  noSym = -1;
  eofSy =  0;

  (* token kinds *)
  classToken    = 0;  (* token class *)
  litToken      = 1;  (* literal (e.g. keyword) not recognized by DFA *)
  classLitToken = 2;  (* token class that can also match a literal *)

TYPE
  Position   = RECORD  (* position of stretch of source text *)
    beg:       INTEGER;    (* start relative to beginning of file *)
    len:       INTEGER;  (* length *)
    col:       INTEGER;  (* column number of start position *)
  END;

  SymbolNode = RECORD    (* node of symbol table *)
    typ:       INTEGER;  (* nt, t, pr, unknown *)
    name,                (* symbol name *)
    constant:  TEXT;     (* named constant of symbol *)
    struct:    INTEGER;  (* typ = nt: index of first node of syntax graph *)
                         (* typ = t: token kind: literal, class, ... *)
    deletable: BOOLEAN;  (* typ = nt: TRUE, if nonterminal is deletable *)
    attrPos:   Position; (* position of attributes in source text *)
    semPos:    Position; (* typ = pr: pos of sem action in source text *)
                         (* typ = nt: pos of local decls in source text *)
    line:      INTEGER;  (* source text line number of symbol in this node *)
  END;

  GraphNode = RECORD     (* node of top-down graph *)
    typ : INTEGER;       (* nt,sts,wts,char,class,any,eps,sem,sync,alt,
                            iter,opt*)
    next: INTEGER;       (* to successor node *)
                         (* next < 0: to successor of enclosing structure *)
    p1: INTEGER;         (* typ IN {nt, t, wt}: index to symbol table *)
                         (* typ = any: index to anyset *)
                         (* typ = sync: index to syncset *)
                         (* typ = alt:
                                  index of first node of first alternative *)
                         (* typ IN {iter, opt}: first node in subexpression *)
                         (* typ = char: ordinal character value *)
                         (* typ = class: index of character class *)
    p2: INTEGER;         (* typ = alt:
                                  index of first node of second alternative *)
                         (* typ IN {char, class}: transition code *)
    pos: Position;       (* typ IN {nt, t, wt}:
                                  source pos of actual attributes *)
                         (* typ = sem: source pos of sem action *)
    line: INTEGER;       (* source text line number of item in this node *)
  END;

  Set  = SET OF [0 .. maxTerminals] ;
  MarkList = SET OF [0 .. maxNodes] ;

VAR
  maxT:        INTEGER;  (* terminals stored from 0 .. maxT in symbol table *)
  maxP:        INTEGER;  (* pragmas stored from maxT+1..maxP in symbol table *)
  firstNt:     INTEGER;  (* index of first nt: available after CompSymbolSets *)
  lastNt:      INTEGER;  (* index of last nt: available after CompSymbolSets *)
  maxC:        INTEGER;  (* index of last character class *)
  nNodes:      INTEGER;  (* index of last top-down graph node *)
  root:        INTEGER;  (* index of root node, filled by ATG *)

  semDeclPos:  Position; (* position of global semantic declarations *)
  genScanner:  BOOLEAN;  (* TRUE: a scanner shall be generated *)
  ignoreCase:  BOOLEAN;  (* TRUE: scanner treats lower case as upper case *)
  symNames:    BOOLEAN;  (* TRUE: symbol names have to be assigned *)
  ignored:     Set;      (* characters ignored by the scanner *)
  ddt:         ARRAY ['A' .. 'Z'] OF BOOLEAN;
                         (* parameter, debug and test switches *)
  src:         Rd.T;     (* the input source file *)


PROCEDURE NewName (n: TEXT; s: TEXT);
(* Inserts the pair (n, s) in the token symbol name table *)

PROCEDURE NewSym (t: INTEGER; n: TEXT; line: INTEGER): INTEGER;
(* Generates a new symbol with type t and name n and returns its index *)

PROCEDURE GetSym (sp: INTEGER; VAR sn: SymbolNode);
(* Gets symbol node with index sp in sn. *)

PROCEDURE PutSym (sp: INTEGER; READONLY sn: SymbolNode);
(* Replaces symbol node with index sp by sn. *)

PROCEDURE FindSym (n: TEXT): INTEGER;
(* Gets symbol index for identifier with name n. *)

PROCEDURE NewSet(READONLY s: Set): INTEGER;
(* Stores s as a new set and returns its index. *)

PROCEDURE CompFirstSet (gp: INTEGER; VAR first: Set);
(* Computes start symbols of graph gp. *)

PROCEDURE CompExpected (gp, sp: INTEGER; VAR exp: Set);
(* Computes all symbols expected at location gp in graph of symbol sp. *)

PROCEDURE CompDeletableSymbols(wr : Wr.T);
(* Marks deletable nonterminals and prints them. *)

PROCEDURE CompSymbolSets(wr : Wr.T);
(* Collects first-sets, follow-sets, any-sets, and sync-sets. *)

PROCEDURE PrintSymbolTable(wr : Wr.T);
(* Prints the symbol table (for tracing). *)

PROCEDURE XRef(wr : Wr.T);
(* Produces a cross reference listing of all symbols. *)

PROCEDURE NewClass (name: TEXT; READONLY set: Set): INTEGER;
(* Defines a new character class and returns its index *)

PROCEDURE ClassWithName (name: TEXT): INTEGER;
(* Searches for a class with the given name.  Returns its index or -1 *)

PROCEDURE ClassWithSet (READONLY set: Set): INTEGER;
(* Searches for a class with the given set. Returns its index or -1 *)

PROCEDURE GetClass (n: INTEGER; VAR set: Set);
(* Returns character class n *)

PROCEDURE GetClassName (n: INTEGER; VAR name: TEXT);
(* Returns the name of class n *)

PROCEDURE GetSet (nr: INTEGER; VAR set: Set);
(* Gives access to precomputed symbol sets *)

PROCEDURE NewNode (typ, p1, line: INTEGER): INTEGER;
(* Generates a new graph node with typ, p1, and source line number
   line and returns its index. *)

PROCEDURE GetNode (gp: INTEGER; VAR gn: GraphNode);
(* Gets graph node with index gp in gn. *)

PROCEDURE PutNode (gp: INTEGER; gn: GraphNode);
(* Replaces graph node with index gp by gn. *)

PROCEDURE ConcatAlt (VAR gL1, gR1: INTEGER; gL2, gR2: INTEGER);
(* Makes (gL2, gR2) an alternative of the graph (gL1, gR1).
   The resulting graph is identified by (gL1, gR1). *)

PROCEDURE ConcatSeq (VAR gL1, gR1: INTEGER; gL2, gR2: INTEGER);
(* Concatenates graph (gL1, gR1) with graph (gL2, gR2) via next-chain.
   The resulting graph is identified by (gL1, gR1). *)

PROCEDURE MakeFirstAlt (VAR gL, gR: INTEGER);
(* Generates an alt-node with (gL, gR) as its first and only alternative *)

PROCEDURE MakeIteration (VAR gL, gP: INTEGER);
(* Encloses the graph (gL, gR) into an iteration construct.
   The resulting graph is identified by (gL, gR). *)

PROCEDURE MakeOption (VAR gL, gR: INTEGER);
(* Encloses the graph (gL, gR) into an option construct.
   The resulting graph is identified by (gL, gR). *)

PROCEDURE CompleteGraph (gp: INTEGER);
(* Lets right ends of graph gp be 0 *)

PROCEDURE StrToGraph (s: TEXT; VAR gL, gR: INTEGER);
(* Generates linear graph from characters in s *)

PROCEDURE DelGraph (gp: INTEGER): BOOLEAN;
(* TRUE, if (sub) graph with root gp is deletable. *)

PROCEDURE DelNode (gn: GraphNode): BOOLEAN;
(* TRUE, if graph node gn is deletable, i.e. can be derived into the
   empty string. *)

PROCEDURE PrintGraph(wr : Wr.T);
(* Prints the graph (for tracing). *)

PROCEDURE FindCircularProductions (wr : Wr.T ; VAR ok: BOOLEAN);
(* Finds and prints the circular part of the grammar.
   ok = TRUE means no circular part. *)

PROCEDURE LL1Test (wr : Wr.T ; VAR ll1: BOOLEAN);
(* Checks if the grammar satisfies the LL(1) conditions.
   ll1 = TRUE means no LL(1)-conflicts. *)

PROCEDURE TestCompleteness (wr : Wr.T ; VAR ok: BOOLEAN);
(* ok = TRUE, if all nonterminals have productions. *)

PROCEDURE TestIfAllNtReached (wr : Wr.T ; VAR ok: BOOLEAN);
(* ok = TRUE, if all nonterminals can be reached from the start symbol. *)

PROCEDURE TestIfNtToTerm (wr : Wr.T ; VAR ok: BOOLEAN);
(* ok = TRUE, if all nonterminals can be reduced to terminals. *)

PROCEDURE AssignSymNames (default: BOOLEAN; VAR thereExists: BOOLEAN);

PROCEDURE Restriction (n, limit : INTEGER);
(* Signal compiler restriction and abort program *)

END CRT.

