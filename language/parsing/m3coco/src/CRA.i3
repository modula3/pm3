INTERFACE  CRA;
(* Automaton and Scanner Generation *)

IMPORT CR, Wr ;

CONST
  MaxSourceLineLength = 78;

PROCEDURE ConvertToStates (p : CR.Parser ; gp, sp: INTEGER);
(* Converts top-down graph with root gp into a subautomaton that
   recognizes token sp *)

PROCEDURE MatchDFA (p : CR.Parser ; s: TEXT; sp : INTEGER; VAR matchedSp: INTEGER);
(* Returns TRUE, if string s can be recognized by the current DFA.
   matchedSp is the token as that s can be recognized. *)

PROCEDURE NewComment (p : CR.Parser ; from, to: INTEGER; nested: BOOLEAN);
(* Defines a new comment for the scanner. The comment brackets are
   represented by the mini top-down graphs with the roots from and to. *)

PROCEDURE MakeDeterministic (VAR ok: BOOLEAN);
(* Converts the NFA into a DFA. ok indicates if an error occurred. *)

PROCEDURE Generate();
(* Generates the source code fragments for the scanner *)

PROCEDURE PrintStates(wr : Wr.T) ;
(* List the automaton for tracing *)

END CRA.

