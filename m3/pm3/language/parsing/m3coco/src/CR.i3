INTERFACE CR ;

(* auto-generated m3coco interface *)

IMPORT Rd, Thread ;

TYPE
  Symbol = {
    Eof, ident, string, number, sCOMPILER, sPRODUCTIONS, Equal, Dot, sEND, 
    sCHARACTERS, sTOKENS, sNAMES, sPRAGMAS, sCOMMENTS, sFROM, sTO, sNESTED, 
    sIGNORE, sCASE, Plus, Dash, sDotDot, sANY, sCHR, Lbr, Rbr, Bar, sWEAK, 
    Lsqu, Rsqu, Lcurl, Rcurl, sSYNC, sCONTEXT, Less, Gtr, sLbrDot, sDotRbr, 
    Undef, Options
  } ;

  SymbolSet = SET OF Symbol ;

  Scanner <: PublicS ;
  Parser  <: PublicP ;

  ScanSymbol = RECORD
                 sym    : Symbol ;   (* symbol *)
                 offset : CARDINAL ; (* offset in characters into stream *)
                 line   : CARDINAL ; (* line number of starting character *)
                 column : CARDINAL ; (* column number of starting character *)
                 length : CARDINAL ; (* length of symbol string *)
                 string : TEXT ;     (* verbatim text for symbol *)
                 name   : TEXT       (* case-specific text for symbol *)
               END ;

  PublicS = OBJECT
            METHODS
              init(rd : Rd.T ; e : ErrHandler) : Scanner ;
              get(VAR s : ScanSymbol) RAISES { Rd.Failure, Thread.Alerted } ;
            END ;

  PublicP = OBJECT
            METHODS
              init(s : Scanner ; e : ErrHandler) : Parser ;
              error(msg : TEXT) ;
              string() : TEXT ;
              name()   : TEXT ;
              offset() : CARDINAL ;
              line()   : CARDINAL ;
              column() : CARDINAL ;
              length() : CARDINAL ;
              parse() RAISES { Rd.Failure, Thread.Alerted } ;
            END ;

  ErrHandler = OBJECT
               METHODS
                 error(line, col : CARDINAL ; msg : TEXT)
               END ;

CONST
  SymbolName = ARRAY Symbol OF TEXT {
    "EOF", "ident", "string", "number", "COMPILER", "PRODUCTIONS", "=", ".", 
    "END", "CHARACTERS", "TOKENS", "NAMES", "PRAGMAS", "COMMENTS", "FROM", 
    "TO", "NESTED", "IGNORE", "CASE", "+", "-", "..", "ANY", "CHR", "(", ")", 
    "|", "WEAK", "[", "]", "{", "}", "SYNC", "CONTEXT", "<", ">", "(.", ".)", 
    "*UNDEF*", "Options"
  } ;

END CR.
