(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 12:49:11 1994 by luca               *)
(*      modified on Tue Jun 23 20:16:54 1992 by knaff          *)

MODULE Builder;
IMPORT SynParse, SynLocation, SynScan, SynWr, MetaParser, Text;


TYPE
  ClauseExtends = 
    SynParse.Tree BRANDED OBJECT
    extend, iter, iterPosPresent: BOOLEAN;
    iterPos: INTEGER;
  END;
  StringAction =
    SynParse.Action BRANDED OBJECT
    text: TEXT;
  END;
  IntegerAction =
    SynParse.Action BRANDED OBJECT
    int: INTEGER;
  END;
  ProcAction =
    SynParse.Action BRANDED OBJECT
    proc: SynParse.Action;
  END;
    
REVEAL
  GramInfo =
    GramInfoBase BRANDED OBJECT
    clauseList: MetaParser.ClauseList;
    oldKeySet, newKeySet: SynScan.KeywordSet;
  END;


(* symbol table *)


TYPE 
  H = MetaParser.ActionProcEntry ; 

VAR
  (* Sample Action Table *)
   sourceTable := ARRAY [0..27] OF MetaParser.ActionProcEntry 
                    { H { "ClauseList"            , ClauseList             },
                      H { "ClauseExtendsIterNoPos", ClauseExtendsIterNoPos },
                      H { "ClauseExtendsIterPos"  , ClauseExtendsIterPos   },
                      H { "ClauseExtendsIter"     , ClauseExtendsIter      },
                      H { "ClauseExtendsIter"     , ClauseExtendsIter      },
                      H { "ClauseExtendsChoice"   , ClauseExtendsChoice    },
                      H { "ClauseExtendsYes"      , ClauseExtendsYes       },
                      H { "ClauseExtendsNo"       , ClauseExtendsNo        },
                      H { "Storage"               , Storage                },
                      H { "Ide"                   , Ide2                   },
                      H { "Name"                  , Name2                  },
                      H { "GramIde"               , GramIde2               },
                      H { "GramString"            , GramString2            },
                      H { "GramKeyIde"            , GramKeyIde2            },
                      H { "GramKeyName"           , GramKeyName2           },
                      H { "GramKeyInt"            , GramKeyInt2            },
                      H { "GramKeyEof"            , GramKeyEof2            },
                      H { "GramKeyReal"           , GramKeyReal2           },
                      H { "GramKeyChar"           , GramKeyChar2           },
                      H { "GramKeyString"         , GramKeyString2         },
                      H { "GramExpSequence"       , GramExpSequence        },
                      H { "GramExpChoice"         , GramExpChoice          },
                      H { "GramExpIterPos"        , GramExpIterPos         },
                      H { "GramExpIterNoPos"      , GramExpIterNoPos       },
                      H { "GramExpIter"           , GramExpIter            },
                      H { "GramExpBase"           , GramExpBase            },
                      H { "GramExpParens"         , GramExpParens          },
                      H { "GramList"              , GramList               } };

PROCEDURE LinkIn(table : MetaParser.ActionTable ) =
BEGIN
  MetaParser.TableFromArray(sourceTable,table);
END LinkIn;



(* error printing routine *)



(* "getter"routines *)
PROCEDURE GClauseList(p: SynParse.T; loc: INTEGER): MetaParser.ClauseList 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | MetaParser.ClauseList(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError("clause-list", p.stack[loc]);
    <*ASSERT FALSE*>
  END GClauseList;

PROCEDURE GClauseExtends(p: SynParse.T; loc: INTEGER): ClauseExtends 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | NULL=>
    | ClauseExtends(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError("clause-extends", p.stack[loc]);
    <*ASSERT FALSE*>
  END GClauseExtends;

PROCEDURE GIdeNode(p: SynParse.T; loc: INTEGER): MetaParser.TextNode 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | NULL=>
    | MetaParser.TextNode(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError("n ide-node", p.stack[loc]);
    <*ASSERT FALSE*>
  END GIdeNode;

PROCEDURE GGrammar(p: SynParse.T; loc: INTEGER): SynParse.Grammar 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | NULL=>
    | SynParse.Grammar(node) => RETURN node ;
    ELSE
    END;        
    MetaParser.TypeError("grammar",p.stack[loc]);
    <*ASSERT FALSE*>
  END GGrammar;

PROCEDURE GGramList(p: SynParse.T; loc: INTEGER): SynParse.GrammarList 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | SynParse.GrammarList(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError("grammar list",p.stack[loc]);
    <*ASSERT FALSE*>
  END GGramList;

PROCEDURE GArgs(p: SynParse.T; loc: INTEGER):SynParse.Args 
    RAISES {SynParse.Fail}=
  VAR
    n: INTEGER;
    args: SynParse.Tree;
    ret: SynParse.Args;
  BEGIN
    n:= 0;
    args := p.stack[loc];
    LOOP
      TYPECASE args OF
      | NULL=> EXIT
      | Params(node) => INC(n); args:=node.rest;
      ELSE <*ASSERT FALSE*>
      END;
    END;
    ret := NEW(SynParse.Args,n);
    args := p.stack[loc];
    FOR i := 0 TO n-1 DO
      TYPECASE args OF
      | NULL =>
      | Params(node) => 
          ret^[i] := MetaParser.XInt(node.first); 
          args:=node.rest;
      ELSE <*ASSERT FALSE*>
      END;
    END;
    RETURN ret;
  END GArgs;

PROCEDURE Ide(self: SynParse.Identifier; p: SynParse.T; name: TEXT;
              READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(MetaParser.TextNode, text:=name);
  END Ide;
  
PROCEDURE Ide2(<*UNUSED*> self: SynParse.Action; 
               p: SynParse.T; base: INTEGER; 
               READONLY info: SynLocation.Info): SynParse.Tree 
               RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      Ide(NIL, p, MetaParser.GText(p, base+1), info);
  END Ide2;

PROCEDURE Name(self: SynParse.Name; p: SynParse.T; name: TEXT;
              READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(MetaParser.TextNode, text:=name);
  END Name;
  
PROCEDURE Name2(self: SynParse.Action; 
               p: SynParse.T; base: INTEGER; 
               READONLY info: SynLocation.Info): SynParse.Tree
               RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      Name(NIL, p, MetaParser.GText(p, base+1), info);
  END Name2;

  
PROCEDURE Grammar(self:SynParse.Action; p: SynParse.T; base: INTEGER;
                  READONLY info: SynLocation.Info)
                  : SynParse.Tree 
                  RAISES {SynParse.Fail} =
  BEGIN
    (* -- check that names in list are unique, whether extensions or not. *)
    RETURN GClauseList(p, base+1);
  END Grammar;
  


PROCEDURE ClauseList(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  VAR 
    clauseExtends: ClauseExtends;
  BEGIN
    clauseExtends:=GClauseExtends(p, base+2);
    
    RETURN 
      NEW(MetaParser.ClauseList, location:=SynLocation.NewLineLocation(info),
          ide:=GIdeNode(p, base+1),
          args:= GArgs(p, base+5),
          extend:=clauseExtends.extend,
          extendIter:=clauseExtends.iter,
          iterPosPresent:=clauseExtends.iterPosPresent,
          iterPos:=clauseExtends.iterPos,
          gram:=GGrammar(p, base+3),
          rest:=GClauseList(p, base+4));
  END ClauseList;
  
PROCEDURE ClauseExtendsChoice(self: SynParse.Action; p: SynParse.T; 
                              base: INTEGER;
                              READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=TRUE, iter:=FALSE,
               iterPosPresent:=FALSE, iterPos:=0);
  END ClauseExtendsChoice;
  
PROCEDURE ClauseExtendsIterPos(self: SynParse.Action; p: SynParse.T; 
                               base: INTEGER;
                               READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=TRUE, iter:=TRUE,
               iterPosPresent:=TRUE, 
               iterPos:=MetaParser.GInt(p, base+3));
  END ClauseExtendsIterPos;
  
PROCEDURE ClauseExtendsIterNoPos(self: SynParse.Action; p: SynParse.T; 
                                 base: INTEGER;
                                 READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=TRUE, iter:=TRUE,
               iterPosPresent:=FALSE, iterPos:=0);
  END ClauseExtendsIterNoPos;
  
PROCEDURE ClauseExtendsIter(self: SynParse.Action; p: SynParse.T; 
                            base: INTEGER;
                            READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+2];
  END ClauseExtendsIter;
  
PROCEDURE ClauseExtendsNo(self: SynParse.Action; p: SynParse.T; 
                          base: INTEGER;
                          READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=FALSE, iter:=FALSE);
  END ClauseExtendsNo;
  
PROCEDURE ClauseExtendsYes(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+1];
  END ClauseExtendsYes;
  


  (* ****************************** *)
  (*  teminals of client grammar    *)
  (* ****************************** *)

PROCEDURE GramIdeCm(name: TEXT;args: SynParse.Args;
                    READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.NonTerminal, 
               location:=SynLocation.NewLineLocation(info),
               args:=args,
               name:=name);
  END GramIdeCm;

  
PROCEDURE GramIde2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramIdeCm(MetaParser.GText(p, base+1),GArgs(p, base+2),info);
  END GramIde2;



  PROCEDURE GramString(self: SynParse.QuotedString; p: SynParse.T; 
        string: TEXT;
	READONLY info: SynLocation.Info)
      : SynParse.Tree RAISES {SynParse.Fail} =
    VAR name: TEXT;
    BEGIN
      IF Text.Length(string)=0 THEN 
	SynParse.Fault(p, "Invalid token: \"\"") 
      END;
      IF (Text.Length(string)=1) AND 
          SynScan.IsDelimiter(p.Scanner(), Text.GetChar(string,0)) THEN
	RETURN 
	  NEW(SynParse.GivenDelimiter, location:=SynLocation.NewLineLocation(info),
	    delim:=Text.GetChar(string,0)); 
      ELSIF (Text.Length(string)>1) AND (Text.GetChar(string,0)='~') THEN
	name := Text.Sub(string, 1, Text.Length(string)-1);
        IF SynScan.IsIdentifier(p.Scanner(), name) THEN
          RETURN
            NEW(SynParse.GivenName, location:=SynLocation.NewLineLocation(info),
	      text:=name);
        ELSE 
	  SynParse.Fault(p, "Invalid token: "& string);
        <*ASSERT FALSE*>
        END;
      ELSIF SynScan.IsIdentifier(p.Scanner(), string) THEN
        RETURN 
	    NEW(SynParse.GivenKeyword, 
                location:=SynLocation.NewLineLocation(info),
                key:=string);
      ELSE 
	SynParse.Fault(p, "Invalid token: "& string);
        <*ASSERT FALSE*>
      END;
    END GramString;


(*
PROCEDURE GramString(self: SynParse.QuotedString; 
                     p: SynParse.T; string: String.T;
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    IF String.Length(string)=0 THEN 
      SynParse.Fault(p, "Invalid token: \"\"") 
    END;
    IF (String.Length(string)=1) AND SynScan.IsDelimiter(p.sc, string[0]) THEN
      RETURN 
        NEW(SynParse.GivenDelimiter, location:=SynLocation.NewLineLocation(info),
            delim:=string[0]);
      (* don't initialise build, take default action ( i.e. put 
         NIL onto stack )
      *)                       
    ELSIF SynScan.IsIdentifier(p.sc, string) THEN
      RETURN 
        (* Fill the key field later; store it in ide for now. *)
        NEW(GivenNamedKeyword, location:=SynLocation.NewLineLocation(info),
            ide:=String.ToText(string), key:=NIL);
      (* don't initialise build, take default action ( i.e. put 
         NIL onto stack *)
    ELSE SynParse.Fault(p, "Invalid token: "&String.ToText(string));
    END;
  END GramString;
*)  

PROCEDURE GramString2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramString(NIL,p, MetaParser.GText(p, base+1),info);
  END GramString2;


PROCEDURE GramKeyIde(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Identifier, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.IdentifierToTree);
  END GramKeyIde;
  
PROCEDURE GramKeyIde2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramKeyIde(NIL, p, info);
  END GramKeyIde2 ;


PROCEDURE GramKeyName(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Name, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.NameToTree);
  END GramKeyName;
  
PROCEDURE GramKeyName2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramKeyName(NIL, p, info);
  END GramKeyName2 ;


PROCEDURE GramKeyInt(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Integer, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.IntegerToTree);
  END GramKeyInt;

PROCEDURE GramKeyInt2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramKeyInt(NIL, p, info);
  END GramKeyInt2 ;


  
PROCEDURE GramKeyReal(self: SynParse.GivenKeyword; p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Real, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.RealToTree);
  END GramKeyReal;


PROCEDURE GramKeyReal2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramKeyReal(NIL, p, info);
  END GramKeyReal2 ;


  
PROCEDURE GramKeyChar(self: SynParse.GivenKeyword; p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.QuotedChar, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.CharToTree);
  END GramKeyChar;
  

PROCEDURE GramKeyChar2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN      
      GramKeyChar(NIL, p, info);
  END GramKeyChar2 ;



PROCEDURE GramKeyString(self: SynParse.GivenKeyword; p: SynParse.T; 
                        READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.QuotedString, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.StringToTree);
  END GramKeyString;


PROCEDURE GramKeyString2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN      
      GramKeyString(NIL, p, info);
  END GramKeyString2 ;


PROCEDURE GramKeyEof(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Eof, 
               location:=SynLocation.NewLineLocation(info));
  END GramKeyEof;
  

PROCEDURE GramKeyEof2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} =
  BEGIN
    RETURN      
      GramKeyEof(NIL, p, info);
  END GramKeyEof2 ;


  (* ************************************ *)



PROCEDURE GramActionString(self: SynParse.Action;
                           p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info)
                           :SynParse.Tree=
  BEGIN
    RETURN
      MetaParser.TextToTree(NIL, p, NARROW(self,StringAction).text,info);
  END GramActionString;

PROCEDURE GramActionProc(self: SynParse.Action;
                         p: SynParse.T; base: INTEGER;
                         READONLY info: SynLocation.Info)
                         :SynParse.Tree RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      NARROW(self,ProcAction).proc;
  END GramActionProc;    
  
PROCEDURE GramActionInteger(self: SynParse.Action;
                           p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info)
                           :SynParse.Tree=
  BEGIN
    RETURN
      MetaParser.IntegerToTree(NIL, p, NARROW(self,IntegerAction).int,info);
  END GramActionInteger;

  
  (* **************************** *)
  
  
  
PROCEDURE GramList(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN 
      NEW(SynParse.GrammarList, 
          location:=SynLocation.NewLineLocation(info),
          first:=p.stack[base+1],
          rest:=GGramList(p, base+2));
  END GramList;
  
PROCEDURE Storage(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                  READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Storage, 
               location:=SynLocation.NewLineLocation(info),
               position:=MetaParser.GInt(p, base+3),
               item:=GGrammar(p, base+1));
  END Storage;
  
PROCEDURE GramExpSequence(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                          READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Sequence, 
               location:=SynLocation.NewLineLocation(info),
               items:=GGramList(p, base+1));
  END GramExpSequence;
  
PROCEDURE GramExpChoice(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                        READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Choice, 
               location:=SynLocation.NewLineLocation(info),
               choice:=GGramList(p, base+1));
  END GramExpChoice;
  
PROCEDURE GramExpParens(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                        READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN p.stack[base+6];
  END GramExpParens;
  
PROCEDURE GramExpBase(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN p.stack[base+1];
  END GramExpBase;
  
PROCEDURE GramExpIter(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN p.stack[base+5];
  END GramExpIter;
  
PROCEDURE GramExpIterNoPos(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Iter, location:=SynLocation.NewLineLocation(info),
               base:=GGrammar(p, base+1), 
               iter:=GGrammar(p, base+3),
               accum:=FALSE,
               accumPosition:=0);
  END GramExpIterNoPos;
  
PROCEDURE GramExpIterPos(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                         READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Iter, location:=SynLocation.NewLineLocation(info),
               base:= GGrammar(p, base+1), 
               iter:= GGrammar(p, base+3),
               accum:=TRUE,
               accumPosition:=MetaParser.GInt(p, base+4));
  END GramExpIterPos;
  

  (* added following procedure for generating constant strings *)
PROCEDURE ActionString(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): 
                       SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      NEW(StringAction,
          location := SynLocation.NewLineLocation(info),
          grammar := GGrammar(p, base+1),
          text := MetaParser.GText(p, base+3),
          Build := GramActionString);
  END ActionString;


  (* added following procedure for generating constant integers *)
PROCEDURE ActionInteger(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): 
                       SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      NEW(IntegerAction,
          location := SynLocation.NewLineLocation(info),
          grammar := GGrammar(p, base+1),
          int := MetaParser.GInt(p, base+3),
          Build := GramActionInteger);
  END ActionInteger;


PROCEDURE LookupAction(p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): ActionProc
                       RAISES {SynParse.Fail}=
  VAR 
    name : TEXT ;
    ref: REFANY;
  BEGIN                
    (* AK look up action NEW *)
    name := MetaParser.GText(p, base+3);     
    IF actionTable.get(name,ref) THEN                
      TYPECASE ref OF
      | REF ActionProc(node) => RETURN node^;
      ELSE
        SynWr.Text(SynWr.out, "Not an action: "&name&" ", loud:=TRUE);
        SynLocation.PrintLocation(SynWr.out, SynLocation.NewLineLocation(info));
        SynWr.Text(SynWr.out, "\n", loud:=TRUE);
        SynWr.Flush(SynWr.out, loud:=TRUE);
        RAISE SynParse.Fail;
      END;
        
    ELSE
      SynWr.Text(SynWr.out, "Unknown action: "&name&" ", loud:=TRUE);
      SynLocation.PrintLocation(SynWr.out, SynLocation.NewLineLocation(info));
      SynWr.Text(SynWr.out, "\n", loud:=TRUE);
      SynWr.Flush(SynWr.out, loud:=TRUE);
      RAISE SynParse.Fail;
    END;
  END LookupAction;


PROCEDURE AntiquotedAction(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): 
                           SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      NEW(ProcAction,
          location := SynLocation.NewLineLocation(info),
          grammar:= GGrammar(p, base+1),
          proc := NEW(SynParse.Action,
                      location := NIL,
                      grammar := NIL,                
                      Build := LookupAction(p, base,info)),
          Build := GramActionProc);
  END AntiquotedAction;

  (* following procedure for generating actions *)
PROCEDURE Action(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 READONLY info: SynLocation.Info): 
                 SynParse.Tree RAISES {SynParse.Fail} =

  BEGIN
    RETURN       
      NEW(SynParse.Action, 
          location := SynLocation.NewLineLocation(info),
          grammar:= GGrammar(p, base+1),
          Build := LookupAction(p, base, info));
  END Action;

  PROCEDURE Single(self: SynParse.Action; p: SynParse.T; base: INTEGER;
	READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+1];
    END Single;

  PROCEDURE GramExp(self: SynParse.Action; p: SynParse.T; base: INTEGER;
	READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+2];
    END GramExp;

PROCEDURE ConsParam(self: SynParse.Action; p: SynParse.T; base: INTEGER;
               READONLY info: SynLocation.Info): SynParse.Tree=
  BEGIN
    RETURN NEW(Params,  
          location := SynLocation.NewLineLocation(info),
          first:=p.stack[base+1], rest:=p.stack[base+2]);
  END ConsParam;

BEGIN

END Builder.



