MODULE ContextSet;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:15  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT Word, TextSeq, TextIntTbl;

VAR
  KnownContexts: TextSeq.T := NEW(TextSeq.T).init();
  NameMap: TextIntTbl.T := NEW(TextIntTbl.Default).init();

PROCEDURE DeclareContext (name: TEXT): Index =
  VAR ind: INTEGER;
  BEGIN
    IF NameMap.get(name, ind) THEN
      RETURN ind;
    ELSE
      <* ASSERT KnownContexts.size()-1 < LAST(Index) *>
      KnownContexts.addhi(name);
      ind := KnownContexts.size() - 1;
      EVAL NameMap.put(name, ind);
      RETURN ind;
    END;
  END DeclareContext;
  
PROCEDURE ContextIndex (name: TEXT): Index RAISES {Unknown} =
  VAR ind: INTEGER;
  BEGIN
    IF NameMap.get(name, ind) THEN
      RETURN ind;
    ELSE
      RAISE Unknown;
    END;
  END ContextIndex; 
  
PROCEDURE ContextName (context: Index): TEXT RAISES {Unknown} =
  BEGIN
    IF context < KnownContexts.size() THEN
      RETURN KnownContexts.get(context);
    ELSE
      RAISE Unknown;
    END;
  END ContextName;
  
PROCEDURE ExistsContextByName (name: TEXT): BOOLEAN =
  VAR ind: INTEGER;
  BEGIN
    RETURN NameMap.get(name, ind);
  END ExistsContextByName;
  
PROCEDURE ExistsContext (context: Index): BOOLEAN =
  BEGIN
    RETURN context < KnownContexts.size();
  END ExistsContext;
  

PROCEDURE Empty (): T =
  BEGIN
    RETURN 0;
  END Empty;
  
PROCEDURE Insert (cs: T; context: Index): T =
  BEGIN
    <* ASSERT context < KnownContexts.size() *>
    RETURN Word.Or(Word.LeftShift(1, context), cs);
  END Insert;
  
PROCEDURE Remove (cs: T; context: Index): T =
  BEGIN
    <* ASSERT context < KnownContexts.size() *>
    RETURN Word.And(Word.Not(Word.LeftShift(1, context)), cs);
  END Remove;
  
PROCEDURE Contains (cs: T; context: Index): BOOLEAN =
  BEGIN
    <* ASSERT context < KnownContexts.size() *>
    RETURN Word.And(Word.LeftShift(1, context), cs)>0;
  END Contains;

PROCEDURE FromSeq (s: TextSeq.T): T RAISES {Unknown} =
  VAR cs := Empty();
      i: INTEGER;
  BEGIN
    FOR j := 0 TO s.size()-1 DO
      IF NameMap.get(s.get(j), i) THEN
          cs := Insert(cs, i);
      ELSE
        RAISE Unknown;
      END;
    END;
    RETURN cs;
  END FromSeq;
  
PROCEDURE ToSeq (cs: T): TextSeq.T =
  VAR s := NEW(TextSeq.T).init();
  BEGIN
    FOR i := 0 TO KnownContexts.size()-1 DO
      IF Contains(cs, i) THEN
        s.addhi(KnownContexts.get(i));
      END;
    END;
    RETURN s;
  END ToSeq;
  

PROCEDURE Inhibits (inh, act: T): BOOLEAN =
  BEGIN
    RETURN Word.And(Mask(inh), Mask(act)) >0;
  END Inhibits;
  

PROCEDURE Permits (perm, act: T): BOOLEAN =
  BEGIN
    RETURN Word.Or(act, Word.Xor(Mask(perm), Mask(act))) = act;
  END Permits;

PROCEDURE Mask(cs: T): T =
  BEGIN
    RETURN Word.And(cs, Word.Insert(0, -1, 0, KnownContexts.size()));
  END Mask;
  
BEGIN
END ContextSet.
