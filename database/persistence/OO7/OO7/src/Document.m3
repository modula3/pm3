MODULE Document;

IMPORT OO7, VarParams, GenParams, Globals, Fmt, Text, Wr, Stdio, Thread;
FROM IO IMPORT Put, PutInt;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE PutChar (c: CHAR) =
  BEGIN
    Wr.PutChar(Stdio.stdout, c);
  END PutChar;

REVEAL T = OO7.Document BRANDED "Document.T" OBJECT
OVERRIDES
  init := Init;
  delete := Delete;
  searchText := SearchText;
  replaceText := ReplaceText;
END;
  
PROCEDURE Init (self: T; cpId: INTEGER; cp: OO7.CompositePart): OO7.Document =
  VAR
    p: REF ARRAY OF CHAR;
    myText := Fmt.F(GenParams.DocumentText, Fmt.Int(cpId));
    stringLen := Text.Length(myText);
  BEGIN
    (* fill in id *)
    self.id := cpId;
    (* fill in back pointer to composite part *)
    self.part := cp;

    (* prepare and fill in the document title *)
    self.title := Fmt.F("Composite Part %08s", Fmt.Int(cpId));
    IF Globals.debugMode THEN
      Put("Document::Document(title = "); Put(self.title); Put(")\n");
    END;

    (* prepare and fill in the document text *)

    (* allocate the space to hold the actual document *)
    p := NEW(REF ARRAY OF CHAR, VarParams.DocumentSize);

    (* initialize the document *)
    VAR curChar := 0;
    BEGIN
      WHILE curChar < VarParams.DocumentSize-1 DO
        Text.SetChars(SUBARRAY(p^, curChar, VarParams.DocumentSize-curChar),
                      myText);
        INC(curChar, stringLen);
      END;
    END;
    p[VarParams.DocumentSize-1] := '\000';

    (* now make the document object point at the actual document text *)
    self.text := p;
    self.textLen := VarParams.DocumentSize-1;
    RETURN self;
  END Init;

PROCEDURE Delete (self: T) =
  BEGIN
    self.text := NIL;			 (* delete actual document text *)
(*
    dts 3-11-93 AllDocuments is never used, so don't maintain it.
    (* remove document from extent of all documents *)
    AllDocuments.remove (this);
*)
  END Delete;

PROCEDURE SearchText (self: T; c: CHAR): INTEGER =
  VAR
    x: CHAR;
  BEGIN
    IF Globals.debugMode THEN
      Put("                    Document::searchText(title = ");
      Put(self.title); Put(")\n");
    END;
    x := c;

    (* count occurrences of the indicated letter (for busy work) *)
    VAR
      i := 0;
      count := 0;
    BEGIN
      WITH text = self.text DO
        WHILE text[i] # '\000' DO
          IF text[i] = x THEN INC(count) END;
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

PROCEDURE ReplaceText (self: T;
                       READONLY oldString, newString: ARRAY OF CHAR): INTEGER =
  VAR
    oldText := self.text;
    oldTextLength := self.textLen;
    oldStrLength := NUMBER(oldString);
    (* check to see if the text starts with the old string *)
    foundMatch :=
        SUBARRAY(oldText^, 0, oldStrLength) =
        SUBARRAY(oldString, 0, oldStrLength);
  BEGIN
    IF Globals.debugMode THEN
      Put("                    Document::changeText(title = ");
      Put(self.title); Put(")\n");
    END;

    (* if so, change it to start with the new string instead *)
    IF foundMatch THEN
      VAR
        newStrLength := NUMBER(newString);
        lengthDiff := newStrLength - oldStrLength;
        newTextLength := oldTextLength + lengthDiff;
        newText := oldText;
      BEGIN
        IF lengthDiff = 0 THEN
          SUBARRAY(oldText^, 0, newStrLength)
          := SUBARRAY(newString, 0, newStrLength);
        ELSE
          IF newTextLength+1 > NUMBER(oldText^) THEN
            newText := NEW(REF ARRAY OF CHAR, newTextLength+1);
            self.text := newText;
          END;
          SUBARRAY(newText^, newStrLength, oldTextLength - oldStrLength)
          := SUBARRAY(oldText^, oldStrLength, oldTextLength - oldStrLength);
          SUBARRAY(newText^, 0, newStrLength)
          := SUBARRAY(newString, 0, newStrLength);
          newText[newTextLength] := '\000';
          self.textLen := newTextLength;
        END
      END
    END;
    
    IF Globals.debugMode THEN
      IF foundMatch THEN
        Put("                    [changed \"");
        Put(Text.FromChars(oldString));
        Put("\" to \"");
        Put(Text.FromChars(newString));
        Put("\"]\n");
      ELSE
        Put("                    [no match, so no change was made]\n");
      END
    END;

    RETURN ORD(foundMatch);
  END ReplaceText;

BEGIN
END Document.
