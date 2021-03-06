MODULE -->Parser EXPORTS -->Grammar ;

(* Parser generated by m3coco *)

IMPORT Rd, Thread ;

-->Declarations

CONST 
  minErrDist = 2 ;  (* minimal distance (good tokens) between two errors *)

TYPE
  SymSetRange = [0 .. -->MaxSets] ;

CONST
  SymSet = ARRAY SymSetRange OF SymbolSet {
    -->SymbolSets
  } ;

  PragmaSet = SymbolSet { -->PragmaSet } ;

REVEAL
  Parser = PublicP BRANDED OBJECT
             s       : Scanner ;
             err     : ErrHandler ;
             cur     : ScanSymbol ; (* current symbol *)
             next    : ScanSymbol ; (* next [lookahead] symbol *)
             errDist : CARDINAL     (* number of tokens since last err *)
           OVERRIDES
             init   := Init ;
             error  := SemError ;
             string := GetString ;
             name   := GetName ;
             offset := GetOffset ;
             line   := GetLine ;
             column := GetColumn ;
             length := GetLength ;
             parse  := Parse
           END ;

PROCEDURE SemError(p : Parser ; msg : TEXT) =
BEGIN
  IF (p.errDist >= minErrDist) THEN
    p.err.error(p.cur.line, p.cur.column, msg)
  END ;
  p.errDist := 0
END SemError ;

PROCEDURE GetString(p : Parser) : TEXT =
BEGIN
  RETURN p.cur.string
END GetString ;

PROCEDURE GetName(p : Parser) : TEXT =
BEGIN
  RETURN p.cur.name
END GetName ;

PROCEDURE GetOffset(p : Parser) : CARDINAL =
BEGIN
  RETURN p.cur.offset
END GetOffset ;

PROCEDURE GetLine(p : Parser) : CARDINAL =
BEGIN
  RETURN p.cur.line
END GetLine ;

PROCEDURE GetColumn(p : Parser) : CARDINAL =
BEGIN
  RETURN p.cur.column
END GetColumn ;

PROCEDURE GetLength(p : Parser) : CARDINAL =
BEGIN
  RETURN p.cur.length
END GetLength ;

PROCEDURE SynError(p : Parser ; msg : TEXT) =
BEGIN
  IF (p.errDist >= minErrDist) THEN
    p.err.error(p.next.line, p.next.column, msg)
  END ;
  p.errDist := 0
END SynError ;

PROCEDURE Get(p : Parser) =
BEGIN
  p.cur := p.next ;
  REPEAT
    p.s.get(p.next) ;
    IF (p.next.sym IN PragmaSet) THEN
      VAR tmp := p.cur ;
      BEGIN
        p.cur := p.next ;
        -->Pragmas
        p.cur := tmp
      END
    END
  UNTIL (NOT (p.next.sym IN PragmaSet)) ;
  INC(p.errDist)
END Get ;

PROCEDURE Expect(p : Parser ; sym : Symbol) =
BEGIN
  IF (p.next.sym = sym) THEN
    Get(p)
  ELSE
    SynError(p, "expected '" & SymbolName[sym] &
                  "', got '" & SymbolName[p.next.sym] & "'")
  END
END Expect ;

<*NOWARN*>PROCEDURE ExpectWeak(p : Parser ; sym : Symbol ; follow : SymSetRange) =
BEGIN
  IF (p.next.sym = sym) THEN
    Get(p)
  ELSE
    SynError(p, "expected '" & SymbolName[sym] &
                  "', got '" & SymbolName[p.next.sym] & "'") ;
    WHILE (NOT (p.next.sym IN SymSet[follow])) DO
      Get(p)
    END
  END
END ExpectWeak ;

<*NOWARN*>PROCEDURE WeakSeparator(p : Parser ; sym : Symbol ;
                        syFol, repFol : SymSetRange) : BOOLEAN =
VAR
  s : SymbolSet ;
BEGIN
  IF (p.next.sym = sym) THEN
    Get(p) ;
    RETURN TRUE
  ELSIF (p.next.sym IN SymSet[repFol]) THEN
    RETURN FALSE
  ELSE
    s := SymSet[0] + SymSet[syFol] + SymSet[repFol] ;
    SynError(p, "expected '" & SymbolName[sym] &
                  "', got '" & SymbolName[p.next.sym] & "'") ;
    WHILE (NOT (p.next.sym IN s)) DO
      Get(p)
    END ;
    RETURN (p.next.sym IN SymSet[syFol])
  END
END WeakSeparator ;

-->Productions

PROCEDURE Parse(p : Parser) RAISES { Rd.Failure, Thread.Alerted } =
BEGIN
  -->ParseRoot
END Parse ;

PROCEDURE Init(p : Parser ; s : Scanner ; e : ErrHandler) : Parser =
BEGIN
  p.s       := s ;
  p.err     := e ;
  p.errDist := minErrDist ;
  RETURN p
END Init ;

BEGIN
END -->Parser.
