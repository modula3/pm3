(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Jim Meehan and Mark Manasse                                            *)
(* Last modified on Sun May 30 10:41:21 PDT 1993 by meehan                   *)
(*      modified on Thu Mar  4 21:05:05 PST 1993 by msm                      *)
<* PRAGMA LL *>

MODULE KeyFilter;

IMPORT IntIntTbl, ISOChar, KeyboardKey, Latin1Key, VBT;

(* IMPORT Fmt; FROM SmallIO IMPORT stderr, PutText; (* Debugging *) *)

TYPE
  State = {initial, compose1, compose2};
  PublicComposer =
    T OBJECT METHODS feedback (v: VBT.T; composing: BOOLEAN) END;

REVEAL
  Composer = PublicComposer BRANDED OBJECT
               buf  : VBT.KeySym;
               state               := State.initial
             OVERRIDES
               feedback := Feedback
             END;
  ComposeChar =
    Composer BRANDED OBJECT OVERRIDES apply := ApplyComposeChar END;
  Diacritical =
    Composer BRANDED OBJECT OVERRIDES apply := ApplyDiacritical END;

PROCEDURE Feedback (<* UNUSED *> pc       : Composer;
                    <* UNUSED *> v        : VBT.T;
                    <* UNUSED *> composing: BOOLEAN          ) =
  BEGIN
  END Feedback;

PROCEDURE ApplyComposeChar (comp: ComposeChar; v: VBT.T; cd: VBT.KeyRec) =
  BEGIN
    IF NOT cd.wentDown OR cd.whatChanged = VBT.NoKey THEN (* skip *)
    ELSIF cd.whatChanged = KeyboardKey.Multi_key THEN
      comp.state := State.compose1;
      comp.feedback (v, TRUE)
    ELSIF comp.state = State.initial OR IsModifier (cd.whatChanged) THEN
      (* Allow user to toggle the shift key during compose processing,
         for example *)
      comp.next.apply (v, cd)
    ELSIF VBT.Modifier.Option IN cd.modifiers
            OR NOT IsPrintable (cd.whatChanged) THEN
      comp.state := State.initial;
      comp.feedback (v, FALSE);
      comp.next.apply (v, cd)
    ELSIF comp.state = State.compose1 THEN
      comp.state := State.compose2;
      comp.buf := cd.whatChanged
    ELSE
      comp.state := State.initial;
      cd.whatChanged := Compose (comp.buf, cd.whatChanged);
      comp.feedback (v, FALSE);
      comp.next.apply (v, cd)
    END
  END ApplyComposeChar;

PROCEDURE IsPrintable (c: VBT.KeySym): BOOLEAN =
  BEGIN
    RETURN c >= Latin1Key.space AND c <= Latin1Key.asciitilde
  END IsPrintable;

PROCEDURE IsModifier (c: VBT.KeySym): BOOLEAN =
  BEGIN
    RETURN c >= KeyboardKey.Shift_L AND c <= KeyboardKey.Hyper_R
  END IsModifier;

PROCEDURE Compose (k1, k2: VBT.KeySym): VBT.KeySym =
  VAR res := VBT.NoKey;
  BEGIN
    EVAL Latin1Table.get (k1 * 128 + k2, res);
    RETURN res
  END Compose;

VAR Latin1Table := NEW (IntIntTbl.Default).init (255);

(*
PROCEDURE Dump () =
  PROCEDURE enum (data: REFANY; key: INTEGER; VAR value: INTEGER): BOOLEAN =
    VAR
      k1 := key DIV 128;
      k2 := key MOD 128;
      c1 := VAL (k1, CHAR);
      c2 := VAL (k2, CHAR);
      c  := VAL (value, CHAR);
    BEGIN
      PutText (
        stderr, Fmt.FN ("%s = %s (%s) + %s (%s) -> %s (%s)\n",
                        ARRAY OF
                          TEXT {Fmt.Int (key), Fmt.Int (k1), Fmt.Char (c1),
                                Fmt.Int (k2), Fmt.Char (c2), Fmt.Int (value),
                                Fmt.Char (c)}));
      RETURN FALSE
    END enum;
  VAR key, value: INTEGER;
  BEGIN
    EVAL Latin1Table.enumerate (enum, NIL, key, value)
  END Dump;
*)

TYPE
  f = RECORD
        c1, c2         : CHAR;
        keysym         : VBT.KeySym;
        caseInsensitive: BOOLEAN
      END;

CONST
  KeyTable = ARRAY OF
               f {f {'+', '+', Latin1Key.numbersign, FALSE},
		  f {'\'', ' ', Latin1Key.apostrophe, FALSE},
		  f {'a', 'a', Latin1Key.at, TRUE},
		  f {'(', '(', Latin1Key.bracketleft, FALSE},
		  f {'/', '/', Latin1Key.backslash, FALSE},
		  f {'/', '<', Latin1Key.backslash, FALSE},
		  f {'^', ' ', Latin1Key.asciicircum, FALSE},
		  f {'>', ' ', Latin1Key.asciicircum, FALSE},
		  f {')', ')', Latin1Key.bracketright, FALSE},
		  f {'`', ' ', Latin1Key.grave, FALSE},
		  f {'(', '-', Latin1Key.braceleft, FALSE},
		  f {'/', '^', Latin1Key.bar, FALSE},
		  f {'v', 'l', Latin1Key.bar, FALSE},
		  f {')', '-', Latin1Key.braceright, FALSE},
		  f {'~', ' ', Latin1Key.asciitilde, FALSE},
		  f {'-', ' ', Latin1Key.asciitilde, FALSE},
		  f {' ', ' ', Latin1Key.nobreakspace, FALSE},
                  f {'!', '!', Latin1Key.exclamdown, FALSE},
                  f {'c', '/', Latin1Key.cent, TRUE},
                  f {'c', '$', Latin1Key.cent, TRUE},
		  f {'c', '|', Latin1Key.cent, TRUE},
                  f {'l', '-', Latin1Key.sterling, TRUE},
                  f {'l', '$', Latin1Key.sterling, TRUE},
                  f {'l', '=', Latin1Key.sterling, TRUE},
                  f {'x', 'o', Latin1Key.currency, TRUE},
                  f {'g', '$', Latin1Key.currency, TRUE},
                  f {'y', '-', Latin1Key.yen, TRUE},
                  f {'y', '$', Latin1Key.yen, TRUE},
                  f {'y', '=', Latin1Key.yen, TRUE},
                  f {'|', '|', Latin1Key.brokenbar, FALSE},
                  f {'|', '^', Latin1Key.brokenbar, FALSE},
                  f {'v', 'b', Latin1Key.brokenbar, TRUE},
                  f {'s', 'o', Latin1Key.section, TRUE},
		  f {'S', 'S', Latin1Key.section, FALSE},
                  f {'s', '!', Latin1Key.section, TRUE},
                  f {'"', '"', Latin1Key.diaeresis, FALSE},
                  f {'c', 'o', Latin1Key.copyright, TRUE},
                  f {'a', '_', Latin1Key.ordfeminine, TRUE},
                  f {'s', 'a', Latin1Key.ordfeminine, TRUE},
                  f {'<', '<', Latin1Key.guillemotleft, FALSE},
                  (* left angle quotation mark *)
                  f {'-', ',', Latin1Key.notsign, FALSE},
                  f {'n', 'o', Latin1Key.notsign, TRUE},
                  f {'-', '-', Latin1Key.hyphen, FALSE},
                  f {'r', 'o', Latin1Key.registered, TRUE},
                  f {'-', '^', Latin1Key.macron, FALSE},
                  f {'_', '_', Latin1Key.macron, FALSE},
                  f {'_', '^', Latin1Key.macron, FALSE},
                  f {'0', '^', Latin1Key.degree, FALSE},
                  f {'d', 'e', Latin1Key.degree, TRUE},
                  f {'0', '*', Latin1Key.degree, FALSE},
                  f {'+', '-', Latin1Key.plusminus, FALSE},
                  f {'2', '^', Latin1Key.twosuperior, FALSE},
                  f {'s', '2', Latin1Key.twosuperior, FALSE},
                  f {'3', '^', Latin1Key.threesuperior, FALSE},
                  f {'s', '3', Latin1Key.threesuperior, FALSE},
                  f {'\'', '\'', Latin1Key.acute, FALSE},
                  f {'/', 'u', Latin1Key.mu, TRUE},
                  f {'*', 'm', Latin1Key.mu, TRUE},
                  f {'p', '!', Latin1Key.paragraph, TRUE},
                  f {'p', 'g', Latin1Key.paragraph, TRUE},
                  f {'.', '^', Latin1Key.periodcentered, FALSE},
                  f {'.', '.', Latin1Key.periodcentered, FALSE},
                  f {',', ',', Latin1Key.cedilla, FALSE},
                  f {'1', '^', Latin1Key.onesuperior, FALSE},
                  f {'s', '1', Latin1Key.onesuperior, TRUE},
                  f {'o', '_', Latin1Key.masculine, TRUE},
                  f {'s', '0', Latin1Key.masculine, TRUE},
                  f {'>', '>', Latin1Key.guillemotright, FALSE},
                  (* right angle quotation mark *)
                  f {'1', '4', Latin1Key.onequarter, FALSE},
                  f {'1', '2', Latin1Key.onehalf, FALSE},
                  f {'3', '4', Latin1Key.threequarters, FALSE},
                  f {'?', '?', Latin1Key.questiondown, FALSE},
                  f {'A', '`', Latin1Key.Agrave, FALSE},
                  f {'A', '\'', Latin1Key.Aacute, FALSE},
                  f {'A', '^', Latin1Key.Acircumflex, FALSE},
                  f {'A', '>', Latin1Key.Acircumflex, FALSE},
                  f {'A', '~', Latin1Key.Atilde, FALSE},
                  f {'A', '-', Latin1Key.Atilde, FALSE},
                  f {'A', '"', Latin1Key.Adiaeresis, FALSE},
                  f {'A', '*', Latin1Key.Aring, FALSE},
                  f {'o', 'A', Latin1Key.Aring, FALSE},
                  f {'O', 'A', Latin1Key.Aring, FALSE}, (* But not oa or Oa *)
                  f {'A', 'E', Latin1Key.AE, FALSE},
                  f {'C', ',', Latin1Key.Ccedilla, FALSE},
                  f {'E', '`', Latin1Key.Egrave, FALSE},
                  f {'E', '\'', Latin1Key.Eacute, FALSE},
                  f {'E', '^', Latin1Key.Ecircumflex, FALSE},
                  f {'E', '>', Latin1Key.Ecircumflex, FALSE},
                  f {'E', '"', Latin1Key.Ediaeresis, FALSE},
                  f {'I', '`', Latin1Key.Igrave, FALSE},
                  f {'I', '\'', Latin1Key.Iacute, FALSE},
                  f {'I', '^', Latin1Key.Icircumflex, FALSE},
                  f {'I', '>', Latin1Key.Icircumflex, FALSE},
                  f {'I', '"', Latin1Key.Idiaeresis, FALSE},
                  f {'D', '-', Latin1Key.ETH, FALSE},
                  f {'N', '~', Latin1Key.Ntilde, FALSE},
                  f {'N', '-', Latin1Key.Ntilde, FALSE},
                  f {'O', '`', Latin1Key.Ograve, FALSE},
                  f {'O', '\'', Latin1Key.Oacute, FALSE},
                  f {'O', '^', Latin1Key.Ocircumflex, FALSE},
                  f {'O', '>', Latin1Key.Ocircumflex, FALSE},
                  f {'O', '~', Latin1Key.Otilde, FALSE},
                  f {'O', '-', Latin1Key.Otilde, FALSE},
                  f {'O', '"', Latin1Key.Odiaeresis, FALSE},
                  f {'x', 'x', Latin1Key.multiply, TRUE},
                  f {'m', 'u', Latin1Key.multiply, TRUE},
                  (* terrible choice (mu) *)
                  f {'O', '/', Latin1Key.Ooblique, FALSE},
                  f {'U', '`', Latin1Key.Ugrave, FALSE},
                  f {'U', '\'', Latin1Key.Uacute, FALSE},
                  f {'U', '^', Latin1Key.Ucircumflex, FALSE},
                  f {'U', '>', Latin1Key.Ucircumflex, FALSE},
                  f {'U', '"', Latin1Key.Udiaeresis, FALSE},
                  f {'Y', '\'', Latin1Key.Yacute, FALSE},
                  f {'T', 'H', Latin1Key.THORN, FALSE},
                  f {'|', 'P', Latin1Key.THORN, FALSE},
                  f {'s', 's', Latin1Key.ssharp, FALSE},
                  f {'a', '`', Latin1Key.agrave, FALSE},
                  f {'a', '\'', Latin1Key.aacute, FALSE},
                  f {'a', '^', Latin1Key.acircumflex, FALSE},
                  f {'a', '>', Latin1Key.acircumflex, FALSE},
                  f {'a', '~', Latin1Key.atilde, FALSE},
                  f {'a', '-', Latin1Key.atilde, FALSE},
                  f {'a', '"', Latin1Key.adiaeresis, FALSE},
                  f {'a', '*', Latin1Key.aring, FALSE},
                  f {'o', 'a', Latin1Key.aring, FALSE},
                  f {'O', 'a', Latin1Key.aring, FALSE},
                  f {'a', 'e', Latin1Key.ae, FALSE},
                  f {'c', ',', Latin1Key.ccedilla, FALSE},
                  f {'e', '`', Latin1Key.egrave, FALSE},
                  f {'e', '\'', Latin1Key.eacute, FALSE},
                  f {'e', '^', Latin1Key.ecircumflex, FALSE},
                  f {'e', '>', Latin1Key.ecircumflex, FALSE},
                  f {'e', '"', Latin1Key.ediaeresis, FALSE},
                  f {'i', '`', Latin1Key.igrave, FALSE},
                  f {'i', '\'', Latin1Key.iacute, FALSE},
                  f {'i', '^', Latin1Key.icircumflex, FALSE},
                  f {'i', '>', Latin1Key.icircumflex, FALSE},
                  f {'i', '"', Latin1Key.idiaeresis, FALSE},
                  f {'d', '-', Latin1Key.eth, FALSE},
                  f {'n', '~', Latin1Key.ntilde, FALSE},
                  f {'n', '-', Latin1Key.ntilde, FALSE},
                  f {'o', '`', Latin1Key.ograve, FALSE},
                  f {'o', '\'', Latin1Key.oacute, FALSE},
                  f {'o', '^', Latin1Key.ocircumflex, FALSE},
                  f {'o', '>', Latin1Key.ocircumflex, FALSE},
                  f {'o', '~', Latin1Key.otilde, FALSE},
                  f {'o', '-', Latin1Key.otilde, FALSE},
                  f {'o', '"', Latin1Key.odiaeresis, FALSE},
                  f {'-', ':', Latin1Key.division, FALSE},
                  f {'o', '/', Latin1Key.oslash, FALSE},
                  f {'u', '`', Latin1Key.ugrave, FALSE},
                  f {'u', '\'', Latin1Key.uacute, FALSE},
                  f {'u', '^', Latin1Key.ucircumflex, FALSE},
                  f {'u', '>', Latin1Key.ucircumflex, FALSE},
                  f {'u', '"', Latin1Key.udiaeresis, FALSE},
                  f {'y', '\'', Latin1Key.yacute, FALSE},
                  f {'t', 'h', Latin1Key.thorn, FALSE},
                  f {'|', 'p', Latin1Key.thorn, FALSE},
                  f {'y', '"', Latin1Key.ydiaeresis, FALSE}};


PROCEDURE Mix (c1, c2: CHAR): INTEGER =
  BEGIN
    RETURN 128 * ORD (c1) + ORD (c2)
  END Mix;

PROCEDURE Set (a, b: CHAR; c: VBT.KeySym; bothCases, reversed: BOOLEAN) =
  BEGIN
    IF Latin1Table.put (Mix (a, b), c) THEN
      <* ASSERT FALSE *>
    END;
    IF bothCases THEN
      IF a IN ISOChar.Lowers
           AND Latin1Table.put (Mix (ISOChar.Upper [a], b), c)
         OR b IN ISOChar.Lowers
           AND Latin1Table.put (Mix (a, ISOChar.Upper [b]), c)
         OR a IN ISOChar.Lowers AND b IN ISOChar.Lowers
           AND Latin1Table.put (Mix (ISOChar.Upper [a], ISOChar.Upper [b]), c) THEN
        <* ASSERT FALSE *>
      END
    END;
    IF reversed OR a IN ISOChar.AlphaNumerics AND b IN ISOChar.AlphaNumerics THEN
      (* skip *)
    ELSE
      Set (b, a, c, bothCases, TRUE)
    END
  END Set;

(*
    2/ a mode based entry method which is language specific. This 
       should be settable through an environment variable (not LANG!, 
       but a new one: KEYBOARD_MODE (values are American (default), 
       French, ...).
    
       For French, this mode defines a set of "non spacing diacritical 
       marks" (mark for short):
           '    acute
           `    grave
           ^    circumflex
           "    diaeresis
           ~    tilde
           ,    cedilla
           
       any of those marks, followed by a vowel, n, or c, produces 
       the corresponding accented character (or c cedilla)(not all 
       combinations are legal, if the character cannot be accented, 
       then the mark, followed by the character, are produced). The 
       mark followed by a space produces the mark itself. 
*)

PROCEDURE ApplyDiacritical (d: Diacritical; v: VBT.T; cd: VBT.KeyRec) =
  VAR
    c              := cd.whatChanged;
    ch1, ch2: CHAR;
  BEGIN
    IF NOT cd.wentDown OR c = VBT.NoKey THEN (* skip *)
    ELSIF d.state = State.initial THEN
      IF Latin1Key.space <= c AND c <= Latin1Key.asciitilde THEN
        ch1 := VAL (c, CHAR);
        FOR i := FIRST (Dtable) TO LAST (Dtable) DO
          IF Dtable [i].mark = ch1 THEN
            d.feedback (v, TRUE);
            d.state := State.compose1;
            d.buf := c;
            RETURN
          END
        END
      END;
      d.next.apply (v, cd)
    ELSIF IsModifier (c) THEN
      d.next.apply (v, cd)
    ELSE
      d.state := State.initial;
      d.feedback (v, FALSE);
      IF Latin1Key.space <= c AND c <= Latin1Key.asciitilde THEN
        ch1 := VAL (d.buf, CHAR);
        ch2 := VAL (c, CHAR);
        FOR i := FIRST (Dtable) TO LAST (Dtable) DO
          IF Dtable [i].mark = ch1 AND Dtable [i].letter = ch2 THEN
            cd.whatChanged := Dtable [i].key;
            d.next.apply (v, cd);
            RETURN
          END
        END
      END;
      cd.whatChanged := d.buf;
      d.next.apply (v, cd);
      cd.whatChanged := c;
      d.next.apply (v, cd)          (* The whole reason for this interface! *)
    END
  END ApplyDiacritical;

TYPE g = RECORD mark, letter: CHAR; key: VBT.KeySym END;

CONST
  Dtable = ARRAY OF
             g {g {'\'', 'a', Latin1Key.aacute},
                g {'\'', 'e', Latin1Key.eacute},
                g {'\'', 'i', Latin1Key.iacute},
                g {'\'', 'o', Latin1Key.oacute},
                g {'\'', 'u', Latin1Key.uacute},
                g {'\'', 'y', Latin1Key.yacute},
                g {'\'', '\'', Latin1Key.acute},
                g {'\'', 'A', Latin1Key.Aacute},
                g {'\'', 'E', Latin1Key.Eacute},
                g {'\'', 'I', Latin1Key.Iacute},
                g {'\'', 'O', Latin1Key.Oacute},
                g {'\'', 'U', Latin1Key.Uacute},
                g {'\'', 'Y', Latin1Key.Yacute},
                g {'`', 'a', Latin1Key.agrave},
                g {'`', 'e', Latin1Key.egrave},
                g {'`', 'i', Latin1Key.igrave},
                g {'`', 'o', Latin1Key.ograve},
                g {'`', 'u', Latin1Key.ugrave},
                g {'`', '`', Latin1Key.grave},
                g {'`', 'A', Latin1Key.Agrave},
                g {'`', 'E', Latin1Key.Egrave},
                g {'`', 'I', Latin1Key.Igrave},
                g {'`', 'O', Latin1Key.Ograve},
                g {'`', 'U', Latin1Key.Ugrave},
                g {'"', 'a', Latin1Key.adiaeresis},
                g {'"', 'e', Latin1Key.ediaeresis},
                g {'"', 'i', Latin1Key.idiaeresis},
                g {'"', 'o', Latin1Key.odiaeresis},
                g {'"', 'u', Latin1Key.udiaeresis},
                g {'"', 'y', Latin1Key.ydiaeresis},
                g {'"', '"', Latin1Key.diaeresis},
                g {'"', 'A', Latin1Key.Adiaeresis},
                g {'"', 'E', Latin1Key.Ediaeresis},
                g {'"', 'I', Latin1Key.Idiaeresis},
                g {'"', 'O', Latin1Key.Odiaeresis},
                g {'"', 'U', Latin1Key.Udiaeresis},
                g {'^', 'a', Latin1Key.acircumflex},
                g {'^', 'e', Latin1Key.ecircumflex},
                g {'^', 'i', Latin1Key.icircumflex},
                g {'^', 'o', Latin1Key.ocircumflex},
                g {'^', 'u', Latin1Key.ucircumflex},
                g {'^', '^', Latin1Key.asciicircum},
                g {'^', 'A', Latin1Key.Acircumflex},
                g {'^', 'E', Latin1Key.Ecircumflex},
                g {'^', 'I', Latin1Key.Icircumflex},
                g {'^', 'O', Latin1Key.Ocircumflex},
                g {'^', 'U', Latin1Key.Ucircumflex},
                g {'~', 'a', Latin1Key.atilde},
                g {'~', 'n', Latin1Key.ntilde},
                g {'~', 'o', Latin1Key.otilde},
                g {'~', '~', Latin1Key.asciitilde},
                g {'~', 'A', Latin1Key.Atilde},
                g {'~', 'N', Latin1Key.Ntilde},
                g {'~', 'O', Latin1Key.Otilde},
                g {',', 'c', Latin1Key.ccedilla},
                g {',', 'C', Latin1Key.Ccedilla},
                g {',', ',', Latin1Key.cedilla}};

BEGIN
  FOR i := FIRST (KeyTable) TO LAST (KeyTable) DO
    WITH r = KeyTable [i] DO
      Set (r.c1, r.c2, r.keysym, r.caseInsensitive, r.c1 = r.c2)
    END
  END
END KeyFilter. 
