
MODULE Main;

(* The m3tohtmlf program receives a Modula-3 file name as command line
   argument and formats the named file in HTML to stdout, according to
   the typesetting rules below.

   All lines up to and including the first blank line
   are skipped.  (This makes it easy to omit the copyright notice
   from the typeset version, for example.)

   The portions of the file that are not in comments will be typeset
   verbatim, in typewriter font, obeying line breaks and indentation.
   Any characters that have special significance to HTML will be quoted.

   The portions of the file that are in comments will be treated
   differently depending on the position of the comment.  There are 
   three cases:

   Comments that do not begin in the first column are typeset in a
   fixed-width font obeying line breaks and indentation, just like 
   program text. In the following example, OUT will be processed
   like the rest of the line:

|    PROCEDURE P(VAR (*OUT*) x: INTEGER);

   Comments that begin in the first column and are not preceded
   by a blank line are indented and typeset in slanted roman text,
   except that double-quoted expressions are typeset in slanted
   typewriter font.  The purpose of this is to allow a brief comment
   after a procedure declaration, indicating what the procedure does.
   For example, here is the specification of "Path.LineTo":

|    PROCEDURE LineTo(path: T; READONLY p: Point.T);
|    (* Extend "path" with a linear segment that starts
|       at its current point and ends at "p". *)

   It is recommended that these slanted comments be kept brief;
   additional details can be provided in longer type 3 comments. 

   Comments that begin in the first column and are preceded by a
   blank line are typeset in a roman font and justified.
   Items enclosed in double-quotes are set in unslanted 
   typewriter font; for example, program identifiers 
   should be double-quoted this way.  Line breaks and
   indentation have no effect in these comments.

   Sometimes you want a portion of a comment to be treated verbatim, like
   program text, for example, when including an example program or a
   table.  Therefore, any line that starts with "| " (vertical bar 
   followed by space) is treated like program text, typesetting it 
   verbatim in typewriter font.

   Comments of type 2 and 3 may contain HTML tags, for example 
   to annotate section headings or emphasize text.
   
   Lines in type-three comments that begin with "| " are treated just
   like program text with one exception: you can slip in words in roman
   font by enclosing them in back-quotes.
       
|    |   ELSIF s `was generated by` LineTo THEN

*)

IMPORT Wx, Buf, Markup;

IMPORT Stdio, Rd, Wr, Params, Text, Thread, OSError;

VAR
  buf: Buf.T;
  file, basename, extension: TEXT;
  wx := Wx.New();

BEGIN
  TRY
    IF Params.Count < 2 THEN RAISE UsageError; END;
    file := Params.Get(1);
    basename := Pathname.LastBase(file);
    extension := Pathname.LastExt(file);

    buf := Buf.FromFile (file, pad := 1);
    IF buf = NIL THEN RAISE Rd.Failure; END;

    MarkUp.Annotate(buf,wx,FALSE);

    IF Text.Equal(extension,".i3") OR Text.Equal(extension,".ig") THEN
      Wr.PutText(wr,"<TITLE> The " & basename & " interface </TITLE>\n");
      Wr.PutText(wr,"<H1> The " & basename & " interface </H1>\n");
    ELSIF Text.Equal(extension,".m3") OR Text.Equal(extension,".mg") THEN
      Wr.PutText(wr,"<TITLE> The " & basename & " module </TITLE>\n");
      Wr.PutText(wr,"<H1> The " & basename & " module </H1>\n");
    END;

    Wr.PutString(Stdio.stdout,Wx.ToText(wx));
  EXCEPT
    Rd.Failure, OSError.E => 
      Wr.PutText(Stdio.stderr, "? can't read file " & file & " \n")
  | UsageError =>
      Wr.PutText(Stdio.stderr,"? usage: m3tohtmlf name.extension\n")
  | Wr.Failure =>
      Wr.PutText(Stdio.stderr, "? can't write file\n")
  END
END Main.
