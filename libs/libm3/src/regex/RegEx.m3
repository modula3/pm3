(*

  RegEx.m3 - regex(3) style regular expressions

  This module implements regex(3) style regular expressions safe for
  multiple threads.  See "man regex" for a description of a regex
  expression.

  Edit History:
   Jan 30 1992		Schilit		Created.
*)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

UNSAFE MODULE RegEx;

IMPORT Text, Word, Wr, Fmt, ASCII, Stdio, Thread, TextWr;
IMPORT Cstring;
FROM Wr IMPORT PutChar, PutText;

<*FATAL Thread.Alerted, Wr.Failure*>

VAR debug: BOOLEAN := FALSE;

REVEAL
  Pattern = BRANDED "Pattern version 1" OBJECT
              buff: REF ARRAY OF CHAR;
            METHODS
            END; (* object *)

TYPE
  failure_point = RECORD
                    s       : INTEGER;
                    prev_s  : INTEGER;
                    b       : INTEGER;
                    amount  : INTEGER;
                    previous: REF failure_point;
                  END; (* record *)

CONST
  MODIFIERS = SET OF CHAR{'*', '+', '?'};
  NBBY      = BITSIZE(CHAR);              (* bits in a char *)

CONST
  NUL            = '\000';
  OP_ENDOFPAT    = '\000'; (* sentinel for end of pattern *)
  OP_CHAR        = '\001'; (* a sequence of characters *)
  OP_ANY         = '\002'; (* any character *)
  OP_CHARSET     = '\003'; (* character class [foo] *)
  OP_NOTCHARSET  = '\004'; (* not character class [^foo] *)
  OP_STARTLINE   = '\005'; (* start line ^ *)
  OP_ENDLINE     = '\006'; (* end of line $ *)
  OP_STARTMEMORY = '\007'; (* start memory \( *)
  OP_STOPMEMORY  = '\010'; (* stop memory \) *)
  OP_MEMORY      = '\011'; (* memory reference \DIGIT *)
  MOD_MANY       = 8_100;  (* modifier for * and + *)
  MOD_ZERO       = 8_200;  (* modifier for * and ? *)
  MOD_STAR       = 8_300;  (* combination of above two *)

PROCEDURE Compile (pat: TEXT): Pattern RAISES {Error} =
  VAR
    pat_len      : INTEGER := Text.Length(pat);
    p            : INTEGER := 0;
    buff_len     : INTEGER := 10;
    buff         : REF ARRAY OF CHAR := NEW(REF ARRAY OF CHAR, buff_len);
    b            : INTEGER := 0;
    last_op      : INTEGER := -1;
    last_ch_op   : INTEGER := -1;
    c, cc, next_c: CHAR;
    mem_stop_cnt : INTEGER := 0;
    mem_start_cnt: INTEGER := 0;
    mem_stack: ARRAY [1 .. MEM_SIZE] OF RECORD mem_start_cnt: INTEGER; END;

    mem_stackp: INTEGER := 1;

  PROCEDURE NextChar (): CHAR =
    (* Return a character from the text pattern or NUL if at the end.
       Store into next_c the following character for lookahead. *)
    VAR this_c: CHAR;
    BEGIN
      this_c := next_c;
      IF p < pat_len - 1 THEN
        INC(p);
        next_c := Text.GetChar(pat, p);
      ELSE
        next_c := NUL;
      END; (* if *)
      RETURN this_c;
    END NextChar;

  PROCEDURE Put (c: CHAR) =
    (* Store c into buff[p] expanding buff if necessary.  The buffer
       pointer p is incremented. *)
    BEGIN
      IF b = buff_len - 1 THEN
        buff_len := 2 * buff_len;
        WITH new_buff = NEW(REF ARRAY OF CHAR, buff_len) DO
          FOR i := 0 TO b DO new_buff[i] := buff[i]; END; (* for *)
          buff := new_buff;
        END; (* with *)
      END; (* if *)
      buff[b] := c;
      INC(b);
    END Put;

  PROCEDURE PutChar (c: CHAR) =
    (* Store an OP_CHAR character.  The operand is a char count followed by
       the characters.  If possible coalesce with the previous OP_CHAR. *)
    BEGIN
      IF next_c IN MODIFIERS OR (* this OP_CHAR will have a modifier *)
           last_ch_op = -1 OR (* or no last OP_CHAR exists *)
           ORD(buff[last_ch_op]) + 1 + last_ch_op # b
           OR (* or not adjacent *)
           buff[last_ch_op] = LAST(CHAR) (* or over capacity *)
        THEN
        Put(OP_CHAR); (* New OP_CHAR *)
        IF NOT next_c IN MODIFIERS THEN
          last_ch_op := b; (* remember new index if coalescing ok *)
        END;
        Put(VAL(1, CHAR));
        Put(c);
      ELSE
        INC(buff[last_ch_op]);
        Put(c);
      END; (* if *)
    END PutChar;

  PROCEDURE PutCharSet (c: CHAR) =
    (* Store char c in charset at last_op in buff.  The buff at last_op
       contains OP_CHARSET|OP_NOTCHARSET, count, <bits>.  Set the bit for c
       and increase count if necessary. *)
    BEGIN
      WITH idx = Word.Divide(ORD(c), NBBY),
           ch  = buff[last_op + 2 + idx] DO
        ch :=
          VAL(Word.Or(ORD(ch), Word.LeftShift(1, Word.Mod(ORD(c), NBBY))),
              CHAR);
        (* keep track of size needed for CharSet *)
        IF idx + 1 > ORD(buff[last_op + 1]) THEN
          buff[last_op + 1] := VAL(idx + 1, CHAR);
        END;
      END; (* with *)
    END PutCharSet;

  BEGIN
    (* Initialize next_c for NextChar() use *)
    IF pat_len # 0 THEN next_c := Text.GetChar(pat, 0); END; (* if *)

    (* Loop processing chars from the pattern text and storing the compiled
       pattern in buff[p]. *)
    LOOP
      c := NextChar();

      (* If not a postfix modifier then remember buff[p] index as the "last
         operator" seen *)
      IF NOT c IN MODIFIERS THEN last_op := b; END; (* if *)

      CASE c OF

      | '^' =>
          (* ^ has special meaning as first char in pattern *)
          IF b = 0 THEN Put(OP_STARTLINE); ELSE PutChar('^'); END;

      | '.' => Put(OP_ANY);

      | '*', '+', '?' =>
          IF last_op = -1 THEN
            PutChar(c);
          ELSE
            WITH lastop = buff[last_op] DO
              IF lastop
                   IN SET OF
                        CHAR{OP_STARTMEMORY, OP_STOPMEMORY, OP_STARTLINE} THEN
                last_op := b;
                PutChar(c);
              ELSE
                IF c = '*' THEN
                  buff[last_op] :=
                    VAL(Word.Or(ORD(lastop), MOD_MANY + MOD_ZERO), CHAR);
                ELSIF c = '+' THEN
                  buff[last_op] :=
                    VAL(Word.Or(ORD(lastop), MOD_MANY), CHAR);
                ELSIF c = '?' THEN
                  buff[last_op] :=
                    VAL(Word.Or(ORD(lastop), MOD_ZERO), CHAR);
                END; (* if *)
              END; (* if *)
            END; (* with *)
          END; (* if *)


      | '$' =>
          (* $ has special meaning only at end of pattern *)
          IF next_c # NUL THEN PutChar('$'); ELSE Put(OP_ENDLINE); END;

      | '[' =>
          IF next_c = '^' THEN
            Put(OP_NOTCHARSET);
            EVAL NextChar();
          ELSE
            Put(OP_CHARSET);
          END; (* if *)
          Put(NUL); (* count byte *)

          (* initialize to zero and ensure enough storage in buff *)
          FOR i := 0 TO Word.Divide(ORD(LAST(CHAR)), NBBY) DO
            Put(NUL);
          END; (* for *)

          IF next_c = '-' OR next_c = ']' THEN
            PutCharSet(NextChar());
          END; (* if *)

          REPEAT
            c := NextChar();
            IF c = NUL THEN RAISE Error("Missing ]"); END;
            IF next_c = '-' THEN
              cc := c; (* start of sequence *)
              EVAL NextChar(); (* '-' *)
              IF next_c = ']' THEN
                PutCharSet(cc);
                PutCharSet('-')
              ELSE
                c := NextChar(); (* end of sequence *)
                IF cc > c THEN
                  RAISE Error("Range error in []");
                END; (* if *)
                FOR ch := cc TO c DO PutCharSet(ch); END;
              END; (* if *)
            ELSE
              PutCharSet(c);
            END;
          UNTIL next_c = ']';
          EVAL NextChar();
          (* set b to the min size necessary to store charset *)
          b := last_op + ORD(buff[last_op + 1]) + 2;

      | '\\' =>
          c := NextChar();
          CASE c OF
          | '(' =>
              IF mem_start_cnt >= MEM_SIZE THEN
                RAISE Error("Too many \\(\\) pairs");
              END;
              INC(mem_start_cnt);
              Put(OP_STARTMEMORY);
              Put(VAL(mem_start_cnt, CHAR));
              mem_stack[mem_stackp].mem_start_cnt := mem_start_cnt;
              INC(mem_stackp);

          | ')' =>
              IF mem_stackp <= FIRST(mem_stack) THEN
                RAISE Error("Unmatched \\)");
              END; (* if *)
              DEC(mem_stackp);
              Put(OP_STOPMEMORY);
              Put(VAL(mem_stack[mem_stackp].mem_start_cnt, CHAR));
              INC(mem_stop_cnt);

          | '1' .. '9' =>
              IF mem_stop_cnt < (ORD(c) - ORD('0')) THEN
                RAISE Error("No matching \\) for \\DIGIT");
              END; (* if *)
              Put(OP_MEMORY);
              Put(VAL(ORD(c) - ORD('0'), CHAR));

          ELSE
            PutChar(c);
          END; (* case *)

      | NUL => EXIT; (* end of input *)

      ELSE
        PutChar(c);
      END; (* case *)

    END; (* loop *)

    IF mem_stop_cnt # mem_start_cnt THEN
      RAISE Error("Missing \\)");
    END; (* if *)
    RETURN NEW(Pattern, buff := buff);
  END Compile;

<*INLINE*> PROCEDURE InCharSet (pat: Pattern; pos: INTEGER; c: CHAR):
  BOOLEAN =
  (* Check for character c in charset with count at position pos. *)
  BEGIN
    WITH idx = Word.Divide(ORD(c), NBBY) DO
      (* see if charset contains that bit *)
      IF idx + 1 > ORD(pat.buff[pos]) THEN RETURN FALSE; END;
      (* if so check the bit *)
      RETURN Word.And(ORD(pat.buff[pos + 1 + idx]),
                      Word.LeftShift(1, Word.Mod(ORD(c), NBBY))) # 0;
    END; (* with *)
  END InCharSet;

PROCEDURE Dump (READONLY pat: Pattern): TEXT =
  VAR wr: Wr.T := TextWr.New();
  BEGIN
    FOR b := FIRST(pat.buff^) TO LAST(pat.buff^) DO
      IF pat.buff[b] IN ASCII.AlphaNumerics THEN
        PutChar(wr, pat.buff[b]);
      ELSE
        PutText(wr, "\\" & Fmt.Pad(Fmt.Int(ORD(pat.buff[b]), 8), 3, '0'));
      END; (* if *)
      IF pat.buff[b] = OP_ENDOFPAT THEN EXIT; END;
    END; (* for *)
    RETURN TextWr.ToText(wr);
  END Dump;

PROCEDURE Decompile (READONLY pat: Pattern): TEXT =

  PROCEDURE Decompile_Part (b, to: INTEGER) =
    (* Decompile the pattern and print the result on wr. *)
    VAR
      op              : CHAR;
      many_ok, zero_ok: BOOLEAN;
    BEGIN
      LOOP
        IF b >= to THEN RETURN; END;

        WITH pch = ORD(pat.buff[b]) DO
          op := VAL(Word.And(pch, Word.Not(MOD_STAR)), CHAR);
          many_ok := Word.And(pch, MOD_MANY) # 0;
          zero_ok := Word.And(pch, MOD_ZERO) # 0;
        END; (* with *)

        INC(b);
        CASE op OF

        | OP_ENDOFPAT => EXIT;

        | OP_CHAR =>
            (* Operand is count followed by the characters *)
            WITH last = b + ORD(pat.buff[b]) DO
              INC(b); (* move past count *)
              WHILE b <= last DO
                PutChar(wr, pat.buff[b]);
                INC(b);
              END; (* while *)
            END;
        | OP_ANY => PutChar(wr, '.');

        | OP_CHARSET, OP_NOTCHARSET =>
            (* Operand is count followed by a bit-vector *)
            PutChar(wr, '[');
            IF op = OP_NOTCHARSET THEN PutChar(wr, '^'); END; (* if *)
            FOR c := FIRST(CHAR) TO LAST(CHAR) DO
              IF InCharSet(pat, b, c) THEN PutChar(wr, c); END;
            END;
            INC(b, ORD(pat.buff[b]) + 1);
            PutChar(wr, ']');

        | OP_STARTLINE => PutChar(wr, '^');
        | OP_ENDLINE => PutChar(wr, '$');
        | OP_STARTMEMORY => PutText(wr, "\\("); INC(b);
        | OP_STOPMEMORY => PutText(wr, "\\)"); INC(b);

        | OP_MEMORY =>
            (* Operand is the memory register number (a digit) *)
            PutChar(wr, '\\');
            PutChar(wr, VAL(ORD(pat.buff[b]) + ORD('0'), CHAR));
            INC(b);

        ELSE
          <*ASSERT FALSE*>(* unknown opcode! *)
        END; (* case *)
        IF many_ok AND zero_ok THEN
          PutChar(wr, '*');
        ELSIF many_ok THEN
          PutChar(wr, '+');
        ELSIF zero_ok THEN
          PutChar(wr, '?');
        END;
      END;
    END Decompile_Part;

  VAR wr: Wr.T := TextWr.New();
  BEGIN
    Decompile_Part(0, LAST(pat.buff^));
    RETURN TextWr.ToText(wr);
  END Decompile;

PROCEDURE Execute (READONLY pat    : Pattern;
                            txt    : TEXT;
                            start  : CARDINAL := 0;
                            len    : CARDINAL := LAST(CARDINAL);
                            usr_mem: REF Memory := NIL           ):
  INTEGER =
  VAR
    mem    : Memory;
    str    : REF ARRAY OF CHAR;
    str_max: CARDINAL;
    str_idx: INTEGER;

  PROCEDURE Advance (s, b: INTEGER): BOOLEAN =
    VAR
      op                      : CHAR;
      many_not_ok, zero_not_ok: BOOLEAN;
      prev_s                  : INTEGER;
      retreat_amt             : INTEGER := 1;
      failure                 : REF failure_point := NIL;

    BEGIN
      IF debug THEN
        PutText(Stdio.stderr,
                "Advance: s = " & Fmt.Int(s) & " b = " & Fmt.Int(b) & "\n");
      END;

      LOOP
        LOOP
          (* fetch the opcode and determine if repeats are allowed *)
          WITH pch = ORD(pat.buff[b]) DO
            op := VAL(Word.And(pch, Word.Not(MOD_STAR)), CHAR);
            many_not_ok := Word.And(pch, MOD_MANY) = 0;
            zero_not_ok := Word.And(pch, MOD_ZERO) = 0;
          END; (* with *)

          (* step past the opcode and remember position *)
          INC(b);
          prev_s := s;

          CASE op OF
          | OP_ENDOFPAT => RETURN TRUE; (* only success if progress *)

          | OP_CHAR =>
              (* compare all characters at once *)
              WITH cnt = ORD(pat.buff[b]) DO
                INC(b); (* move past count *)
                REPEAT
                  (* check if comparison valid *)
                  IF s + cnt > str_max THEN EXIT; END;
                  (* tradeoff fast for unsafe comparison *)
                  IF Cstring.memcmp(
                       ADR(str[0]) + s, ADR(pat.buff[0]) + b, cnt) # 0 THEN
                    EXIT;
                  END;
                  INC(s, cnt);
                UNTIL many_not_ok;
                INC(b, cnt);
              END; (* with *)

          | OP_ANY =>
              IF NOT many_not_ok THEN
                s := str_max;
              ELSIF s < str_max THEN
                INC(s);
              END;

          | OP_CHARSET, OP_NOTCHARSET =>
              REPEAT
                (* check if comparison with data is valid *)
                IF s >= str_max THEN EXIT; END;
                (* See if char present in charset. *)
                IF InCharSet(pat, b, str[s]) THEN
                  IF op # OP_CHARSET THEN EXIT; END;
                ELSE
                  IF op # OP_NOTCHARSET THEN EXIT; END;
                END;
                (* step to next char in data and loop if * or + allowed *)
                INC(s);
              UNTIL many_not_ok;
              (* Move past charset in pattern *)
              INC(b, ORD(pat.buff[b]) + 1);

          | OP_STARTLINE => IF s # 0 THEN EXIT END;

          | OP_ENDLINE => IF s # str_max THEN EXIT END;

            (* Process \( and \) which just store the current data index *)

          | OP_STARTMEMORY => mem[ORD(pat.buff[b])].start := s; INC(b);

          | OP_STOPMEMORY => mem[ORD(pat.buff[b])].stop := s; INC(b);

            (* Process \DIGIT or \DIGIT* which is a reference to a
               substring of the data already matched by a \(x\) sequence *)

          | OP_MEMORY =>
              WITH start = mem[ORD(pat.buff[b])].start DO
                (* In case many_ok needs it, set retreat_amt and cur_s for
                   backtracking at end of loop *)
                retreat_amt := mem[ORD(pat.buff[b])].stop - start;
                REPEAT
                  (* check if comparison with data is valid *)
                  IF s + retreat_amt > str_max THEN EXIT; END;
                  (* process one chunk at a time before incrementing s *)
                  IF Cstring.memcmp(
                       ADR(str[0]) + s, ADR(str[0]) + start, retreat_amt)
                       # 0 THEN
                    EXIT;
                  END;
                  (* step to chunk char in data and loop if * allowed *)
                  INC(s, retreat_amt);
                UNTIL many_not_ok;
                INC(b);
              END; (* with *)

          ELSE
            <*ASSERT FALSE*>(* unknown opcode! *)
          END; (* case *)

          (* Here after matching one or more component.  If many components
             were eaten then add a failure point from the current position
             back until prev_s. *)

          IF op IN SET OF
                     CHAR{
                     OP_CHAR, OP_ANY, OP_CHARSET, OP_NOTCHARSET, OP_MEMORY} THEN
            IF zero_not_ok THEN
              IF prev_s = s AND retreat_amt # 0 THEN EXIT END;
              INC(prev_s, retreat_amt);
            END;
            IF NOT many_not_ok AND prev_s # s THEN
              failure :=
                NEW(REF failure_point, previous := failure, b := b, s := s,
                    prev_s := prev_s, amount := retreat_amt);
            END; (* if *)
            retreat_amt := 1;
          END;

        END; (* loop *)

        (* Here when EXIT from inner loop pop failure point and try
           again *)
        IF failure = NIL THEN RETURN FALSE; END;
        DEC(failure.s, failure.amount);
        IF debug THEN
          PutText(Stdio.stderr,
                  "Failure: b = " & Fmt.Int(b) & " s = " & Fmt.Int(s)
                    & " new b = " & Fmt.Int(failure.b) & " new s = "
                    & Fmt.Int(failure.s) & " prev_s = "
                    & Fmt.Int(failure.prev_s) & "\n");
        END; (* if *)

        s := failure.s;
        b := failure.b;
        IF failure.s <= failure.prev_s THEN
          failure := failure.previous;
        END; (* if *)
      END; (* loop *)
    END Advance;

  BEGIN
    WITH textlen = Text.Length(txt) DO
      IF start > textlen THEN
        RETURN -1; (* not found *)
      ELSE
        str := NEW(REF ARRAY OF CHAR, textlen);
        Text.SetChars(str^, txt);
        str_idx := start;
        str_max := MIN(start + len, textlen)
      END;
    END;

    FOR i := FIRST(mem) TO LAST(mem) DO
      mem[i].start := -1;
      mem[i].stop := -1;
    END; (* for *)

    REPEAT
      IF Advance(str_idx, 0) THEN
        IF usr_mem # NIL THEN usr_mem^ := mem; END; (* if *)
        RETURN str_idx; (* return start index *)
      END;
      INC(str_idx);
    UNTIL str_idx >= str_max;
    RETURN -1; (* no match *)
  END Execute;

BEGIN

END RegEx.
