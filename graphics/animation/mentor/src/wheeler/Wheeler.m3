(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 15:40:21 PST 1995 by kalsow                   *)
(*      modified on Mon Jan  9 12:18:19 PST 1995 by najork                   *)
(*      modified on Thu Aug 18 13:53:52 PDT 1994 by mhb                      *)
(*      modified on Wed Oct 13 18:22:08 PDT 1993 by mann                     *)
(*      modified on Thu Jul 22 14:26:58 PDT 1993 by perl                     *)

MODULE Wheeler EXPORTS Wheeler;

IMPORT Char, CharArraySort, Text, TextArraySort, Text8, VBT;
IMPORT Thread, FormsVBT;

(* Zeus stuff *)
IMPORT Algorithm, WheelerAlgClass, WheelerIE, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE T = WheelerAlgClass.T BRANDED OBJECT
  OVERRIDES
    run := Run
  END;

PROCEDURE New(): Algorithm.T =
  BEGIN
    RETURN
      NEW(T,
          data := ZeusPanel.NewForm("WheelerInput.fv")
(***
          ,
          codeViews := RefList.List1(
                           RefList.List2("Decompress Pseudo-Code View",
                                         "Decompress.pcode"))
***)
          ).init()
  END New;


PROCEDURE Run(alg: T) RAISES {Thread.Alerted} =
  VAR codes: REF ARRAY OF INTEGER;
      pos: CARDINAL;
      alphabet, string: TEXT;
      pause, finalOnly: BOOLEAN;
  BEGIN
    LOCK VBT.mu DO
      alphabet := FormsVBT.GetText(alg.data, "alphabet");
      string := FormsVBT.GetText(alg.data, "string");
      pause := FormsVBT.GetBoolean(alg.data, "pause");
      finalOnly := FormsVBT.GetBoolean(alg.data, "finalOnly");
    END;
    codes := Ws(alg, pause, finalOnly, alphabet, string, pos);
    IF pause AND NOT finalOnly THEN ZeusPanel.Pause(alg) END;
    EVAL UnWs(alg, pause, finalOnly, alphabet, codes, pos)
  END Run;

PROCEDURE Ws(alg: T; pause, finalOnly: BOOLEAN;
             alpha, string: TEXT; VAR pos: CARDINAL
  ): REF ARRAY OF INTEGER
    RAISES {Thread.Alerted} =
(* Apply the Wheeler transformation to the input "string", with
   alphabet "alpha", returning the sequence of integers that are
   codes for the characters of "string" and "pos" which is the
   position of "string" in the sorted rotations. Using these
   return values and the alphabet as inputs, "UnWs" can reconstruct "string". 
*)

  PROCEDURE Rotate(s: TEXT; i: CARDINAL): Text8.T =
    (* Return a new string that is "s" rotated "i" positions
       to the left (cyclically). *)
    VAR sn := Text.Length(s);
        res := Text8.Create(sn);
    BEGIN
      FOR j := 0 TO sn-1 DO
        res.contents[j] := Text.GetChar(string, (i+j) MOD sn);
      END;
      RETURN res
    END Rotate;

  VAR n := Text.Length(string);
      rotations := NEW(REF ARRAY OF TEXT, n);
      lastchars := Text8.Create(n);
      xchars: Text8.T;
  BEGIN
    IF NOT finalOnly THEN WheelerIE.StartPermute(alg, string, alpha) END;

    (* generate an "n * n" array containing the "n" rotations of "string". *)
    FOR i := 0 TO n-1 DO
      rotations[i] := Rotate(string, i);
      IF NOT finalOnly THEN WheelerIE.NextRotation(alg, i, rotations[i]) END;
    END;

    (* sort the rotations *)
    TextArraySort.Sort(rotations^, Text.Compare);

    (* find the index of the original string in the list of sorted rotations *)
    pos := 0;
    WHILE (NOT Text.Equal(string, rotations[pos])) DO INC(pos) END;

    WheelerIE.RotationsSorted(alg, rotations, pos);

    (* pick off the last character of each rotation *)
    FOR i := 0 TO n-1 DO
      lastchars.contents[i] := Text.GetChar(rotations[i], n-1);
    END;
    IF NOT finalOnly THEN WheelerIE.PermuteDone(alg, lastchars, pos) END;
    IF pause AND NOT finalOnly THEN ZeusPanel.Pause(alg) END;

    (* append list of last characters to the alphabet *)
    VAR alen := Text.Length(alpha); BEGIN
      xchars := Text8.Create(alen + n);
      Text.SetChars(SUBARRAY(xchars.contents^, 0, alen), alpha);
      SUBARRAY(xchars.contents^, alen, n) := lastchars.contents^
    END;
    IF NOT finalOnly THEN WheelerIE.StartEncode(alg, alpha) END;

    (* for each character in "lastchars", find the number of distinct
       characters between it and the preceding instance of the same character
       in the string.  If the character does not occur previously in the
       string, we continue the search as though the alphabet had been prepended
       to the original string. *)
    VAR output := NEW(REF ARRAY OF INTEGER, n); BEGIN
      FOR i := 0 TO n-1 DO
        VAR c := lastchars.contents[i];
            seen := NEW(REF ARRAY OF BOOLEAN, 256);
            count := 0;
            j := Text.Length(alpha) + i - 1;
        BEGIN
          IF NOT finalOnly THEN WheelerIE.EncodeNextChar(alg, i, c) END;
          WHILE(xchars.contents[j] # c) DO
            IF NOT seen[ORD(xchars.contents[j])] THEN
              seen[ORD(xchars.contents[j])] := TRUE;
              INC(count);
              IF NOT finalOnly THEN
                WheelerIE.EncodeDistinctCount(alg, i, j, count, c)
              END;
            END;
            DEC(j)
          END;
          output[i] := count;
          IF NOT finalOnly THEN
            WheelerIE.EncodeFoundCode(alg, i, j, count, c)
          END;
        END
      END;
      (* Return the position and the output array. *)
      IF NOT finalOnly THEN WheelerIE.EncodeDone(alg, alpha, output, pos) END;
      RETURN output
    END 
  END Ws;

PROCEDURE UnWs(alg: T; pause, finalOnly: BOOLEAN;
               alpha: TEXT; codes: REF ARRAY OF INTEGER; pos: CARDINAL
  ): TEXT
    RAISES {Thread.Alerted} =
(* Undo a Wheeler transformation. "codes" is the sequence of
   integer codes and "pos" is the row position produced by "Ws".
   "alpha" is the alphabet given to "Ws". Returns the original string.
*)
(*******
  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN IF NOT finalOnly THEN ZeusCodeView.At(alg, line) END; END At;
*******)
  VAR n := NUMBER(codes^);
      alen := Text.Length(alpha);
      xchars := Text8.Create(alen + n);
      lastchars := Text8.Create(n);
      firstchars := Text8.Create(n);
      charmap := NEW(REF ARRAY OF INTEGER, n);
  BEGIN
    (*ZeusCodeView.Enter(alg, procedureName := "Decompress");*)

    (* rederive the "lastchars" string using "codes", the alphabet,
       and the row position*)
    (*At(2);*)
    IF NOT finalOnly THEN WheelerIE.InitDecode(alg, alpha, codes, pos) END;
    IF NOT finalOnly THEN WheelerIE.StartDecode(alg) END;
    Text.SetChars(SUBARRAY(xchars.contents^, 0, alen), alpha);
    FOR i := 0 TO n-1 DO
      VAR count := 0;
          seen := NEW(REF ARRAY OF BOOLEAN, 256);
          j := alen + i;
      BEGIN
        IF NOT finalOnly THEN WheelerIE.DecodeNextCode(alg, i) END;
        WHILE (count < codes[i]+1) DO
          DEC(j);
          IF NOT seen[ORD(xchars.contents[j])] THEN
            seen[ORD(xchars.contents[j])] := TRUE;
            INC(count);
            IF count < codes[i] + 1 THEN
              IF NOT finalOnly THEN
                WheelerIE.DecodeDistinctCount(alg, i, j, count)
              END
            END
          END
        END;
        IF NOT finalOnly THEN
          WheelerIE.DecodeFoundChar(alg, i, j, xchars.contents[j])
        END;
        xchars.contents[alen+i] := xchars.contents[j];
        lastchars.contents[i] := xchars.contents[j]
      END;
    END;

    WheelerIE.DecodeDone(alg, lastchars, pos);
    IF pause AND NOT finalOnly THEN ZeusPanel.Pause(alg) END;
    (*At(3);*)
    WheelerIE.StartReconstruct(alg, lastchars, pos);
    WheelerIE.Reveal(alg, 1);

    (* obtain the array of initial characters in the sorted rotations
       by sorting the "lastchars" array *)
    firstchars.contents^ := lastchars.contents^;
    CharArraySort.Sort(SUBARRAY(firstchars.contents^, 0, n), Char.Compare); 

    (*At(4);*)
    WheelerIE.FirstChars(alg, firstchars);
    WheelerIE.Reveal(alg, 2);

    (* set "charmap[i]" to contain the index in "lastchars" of the character
       corresponding to "firstchar[i]" *)
    (*At(5);*)
    VAR j := 0; BEGIN
      FOR i := 0 TO n-1 DO
        IF i # 0 AND firstchars.contents[i] # firstchars.contents[i-1] THEN
          j := 0;
          WheelerIE.FinishCharRun(alg)
        END;
        WheelerIE.ConsiderChar(alg, i);
        WHILE firstchars.contents[i] # lastchars.contents[j] DO
          INC(j);
        END;
        WheelerIE.EqualChars(alg, i, j);
        charmap[i] := j;
        INC(j)
      END;
      WheelerIE.FinishCharRun(alg);
    END;

    WheelerIE.StartResult(alg);

    (* construct the original string by reading through "firstchars" and
       "lastchars" using "charmap" *)
    (*At(6);*)
    VAR ans := Text8.Create(n); BEGIN
      FOR i := 0 TO n-1 DO
        WheelerIE.ResultNextChar(alg, pos, i);
        ans.contents[i] := firstchars.contents[pos];
        pos := charmap[pos]
      END;
      WheelerIE.EndResult(alg);
      RETURN ans
    END;
  END UnWs;


BEGIN
  ZeusPanel.RegisterAlg(New, "Wheeler Block Sort", "Wheeler")
END Wheeler.
