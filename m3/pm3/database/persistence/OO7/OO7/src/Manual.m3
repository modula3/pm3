MODULE Manual;

IMPORT Fmt, Text, VarParams, Text8, OO7, Wr, GenParams, Stdio, Globals, Thread;
FROM IO IMPORT Put, PutInt;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE PutChar (c: CHAR) =
  BEGIN
    Wr.PutChar(Stdio.stdout, c);
  END PutChar;

REVEAL T = OO7.Manual BRANDED "Manual.T" OBJECT
OVERRIDES
  searchText := SearchText;
  replaceText := ReplaceText;
  firstLast := FirstLast;
  init := Init;
  delete := Delete;
END;
  
PROCEDURE Init (self: T; modId: INTEGER; myModule: OO7.Module): OO7.Manual =
  VAR
    p: REF ARRAY OF CHAR;
    myText := Fmt.F(GenParams.ManualText, Fmt.Int(modId));
    stringLen := Text.Length(myText);
  BEGIN 
    (* fill in id *)
    self.id := modId;
    (* fill in back pointer to composite part*)
    self.mod := myModule;

    (* prepare and fill in the document title *)
    self.title := Fmt.F("Manual         %08s", Fmt.Int(modId));
    IF Globals.debugMode THEN
      Put("Manual::Manual(title = "); Put(self.title); Put(")\n");
    END;

    (* prepare and fill in the document text *)

    (* allocate the space to hold the actual document *)
    p := NEW(REF ARRAY OF CHAR, VarParams.ManualSize);

    (* initialize the document *)
    VAR curChar := 0;
    BEGIN
      WHILE curChar < VarParams.ManualSize-1 DO
        Text.SetChars(SUBARRAY(p^, curChar, stringLen), myText);
        INC(curChar, stringLen);
      END;
    END;
    p[VarParams.ManualSize-1] := '\000';

    (* now make the manual object point at the actual document text *)
    self.text := p;
    self.textLen := VarParams.ManualSize-1;
    RETURN self;
  END Init;

PROCEDURE Delete (self: T) =
  BEGIN
    self.title := NIL;
    self.text := NIL;			 (* delete actual manual text *)
(*
    (* remove document from extent of all documents *)
    AllDocuments.remove (this);
*)
  END Delete;

PROCEDURE SearchText (self: T; c: CHAR): INTEGER =
  VAR
    x: CHAR;
  BEGIN
    IF Globals.debugMode THEN
      Put("                    Manual::searchText(title = ");
      Put(self.title); Put(")\n");
    END;
    x := c;

    (* count occurrences of the the indicated letter (for busy work) *)
    VAR
      i := 0;
      count := 0;
    BEGIN
      WITH text = self.text DO
        WHILE text[i] # '\000' DO
          IF text[i] = x THEN INC(count); END;
          INC(i);
        END;
      END;

      IF Globals.debugMode THEN
        Put("                    [found "); PutInt(count);
        Put(" '"); PutChar(c); Put("'s among "); PutInt(i);
        Put(" characters]\n");
      END;
      RETURN count;
    END
  END SearchText;

PROCEDURE ReplaceText (self: T; oldString, newString: Text8.T): INTEGER =
  VAR
    oldText := self.text;
    oldTextLength := self.textLen;
    oldStrLength := Text.Length(oldString);
    (* check to see if the text starts with the old string *)
    foundMatch :=
        SUBARRAY(oldText^, 0, oldStrLength) =
        SUBARRAY(oldString.contents^, 0, oldStrLength);
  BEGIN
    IF Globals.debugMode THEN
      Put("                    Manual::changeText(title = ");
      Put(self.title); Put(")\n");
    END;

    (* if so, change it to start with the new string instead *)
    IF foundMatch THEN
      VAR
        newStrLength := Text.Length(newString);
        lengthDiff := newStrLength - oldStrLength;
        newTextLength := oldTextLength + lengthDiff;
        newText := oldText;
      BEGIN
        IF lengthDiff = 0 THEN
          SUBARRAY(oldText^, 0, newStrLength)
          := SUBARRAY(newString.contents^, 0, newStrLength);
        ELSE
          IF newTextLength+1 > NUMBER(oldText^) THEN
            newText := NEW(REF ARRAY OF CHAR, newTextLength+1);
            self.text := newText;
          END;
          SUBARRAY(newText^, newStrLength, oldTextLength - oldStrLength)
          := SUBARRAY(oldText^, oldStrLength, oldTextLength - oldStrLength);
          Text.SetChars(newText^, newString);
          newText[newTextLength] := '\000';
          self.textLen := newTextLength;
        END
      END
    END;

    IF Globals.debugMode THEN
      IF foundMatch THEN
        Put("                    [changed \"");
        Put(oldString); Put("\" to \""); Put(newString); Put("\"]\n");
      ELSE
        Put("                    [no match, so no change was made]\n");
      END
    END;

    RETURN ORD(foundMatch);
  END ReplaceText;

PROCEDURE FirstLast (self: T): INTEGER =
  BEGIN
    RETURN ORD(self.text[0] = self.text[self.textLen-1]);
  END FirstLast;

BEGIN
END Manual.
