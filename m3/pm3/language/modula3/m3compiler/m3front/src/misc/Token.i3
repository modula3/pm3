(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Token.i3                                              *)
(* Last modified on Thu Apr  6 09:39:07 PDT 1995 by kalsow     *)
(*      modified on Sat Mar 16 00:31:16 1991 by muller         *)

INTERFACE Token;

IMPORT M3ID;

TYPE
  T = {tEOF,

       (* lexical classes *)
       tIDENT,
       tCARDCONST,
       tREALCONST, tLONGREALCONST, tEXTENDEDCONST,
       tCHARCONST, tWCHARCONST,
       tTEXTCONST, tWTEXTCONST,

       (* operators *)
       tPLUS, tMINUS, tASTERISK, tSLASH, tASSIGN, tAMPERSAND, tDOT, tCOMMA,
       tSEMI, tLPAREN, tLBRACKET, tLBRACE, tARROW, tEQUAL, tSHARP, tLESS,
       tGREATER, tLSEQUAL, tGREQUAL, tDOTDOT, tCOLON, tRPAREN, tRBRACKET,
       tRBRACE, tBAR, tSUBTYPE, tIMPLIES, tENDPRAGMA,

       (* pragmas (that escape from the scanner) *)
       tINLINE, tEXTERNAL, tASSERT, tUNUSED, tOBSOLETE, tTRACE, tTRANSIENT,
       tCALLCONV, tIMPLICIT, tDEBUG, tFATAL,

       (* reserved words *)
       tAND, tANY, tARRAY, tAS,
       tBEGIN, tBITS, tBRANDED, tBY,
       tCASE, tCONST,
       tDIV, tDO,
       tELSE, tELSIF, tEND, tEVAL, tEXCEPT, tEXCEPTION, tEXIT, tEXPORTS, 
       tFINALLY, tFOR, tFROM,
       tGENERIC,
       tIF, tIMPORT, tIN, tINTERFACE,
       tLOCK, tLOOP,
       tMETHODS, tMOD, tMODULE,
       tNOT,
       tOBJECT, tOF, tOR, tOVERRIDES,
       tPROCEDURE,
       tRAISE, tRAISES, tREADONLY, tRECORD, tREF, tREPEAT, tRETURN, tREVEAL,
       tSET,
       tTHEN, tTO, tTRY, tTYPE, tTYPECASE,
       tUNSAFE, tUNTIL, tUNTRACED,
       tVALUE, tVAR,
       tWHILE, tWITH
       };

CONST
  First_Literal  = T.tIDENT;
  Last_Literal   = T.tWTEXTCONST;
  First_Operator = T.tPLUS;
  Last_Operator  = T.tENDPRAGMA;
  First_Pragma   = T.tINLINE;
  Last_Pragma    = T.tFATAL;
  First_Keyword  = T.tAND;
  Last_Keyword   = T.tWITH;

TYPE
  Set = SET OF T;

CONST
  EmptySet = Set {};

  DeclStart = Set {T.tCONST, T.tTYPE, T.tREVEAL, T.tVAR, T.tIMPLICIT,
                   T.tEXTERNAL, T.tINLINE, T.tUNUSED, T.tOBSOLETE,
                   T.tEXCEPTION, T.tPROCEDURE, T.tFATAL, T.tCALLCONV};

  TypeStart = Set {T.tIDENT, T.tARRAY, T.tBITS, T.tBRANDED, T.tLBRACE,
                   T.tUNTRACED, T.tOBJECT, T.tPROCEDURE, T.tRECORD,
		   T.tREF, T.tSET, T.tTRANSIENT, T.tLBRACKET, T.tLPAREN};

  ExprStart = Set {T.tNOT, T.tPLUS, T.tMINUS, T.tIDENT, T.tCARDCONST,
                   T.tLONGREALCONST, T.tREALCONST, T.tEXTENDEDCONST,
                   T.tCHARCONST, T.tWCHARCONST, T.tTEXTCONST, T.tWTEXTCONST,
                   T.tLPAREN, T.tARRAY, T.tBITS, T.tRECORD, T.tSET};

  StmtStart = Set {T.tCASE, T.tEXIT, T.tEVAL, T.tFOR, T.tIF, T.tLOCK,
                   T.tLOOP, T.tRAISE, T.tREPEAT, T.tRETURN, T.tTRY,
		   T.tTYPECASE, T.tWHILE, T.tWITH, T.tBEGIN, T.tASSERT,
                   T.tIDENT, T.tLPAREN, T.tARRAY, T.tRECORD, T.tDEBUG}
		   + DeclStart;

VAR (*CONST*)
  name: ARRAY T OF M3ID.T;

PROCEDURE Initialize ();

END Token.
