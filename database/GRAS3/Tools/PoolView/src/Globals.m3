MODULE Globals;

(***************************************************************************)
(** Created by:  Markus Kluck						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1998/09/03 11:07:33  kluck
    Further modules used by PoolView to implement selection.

*)
(***************************************************************************)

IMPORT TextIdSet, TextCursorSet, TextId, IO, Fmt, VirtualResource;

(*--------------------------------------*)
(*--- Help procedures for debug info ---*)
(*--------------------------------------*)

PROCEDURE PrintTextCursorSet (msg: TEXT; set: TextCursorSet.T) =
  VAR
    i : CARDINAL;
    el: TEXT;
    ok: BOOLEAN;
  BEGIN
    i := 1;
    IO.Put("\n>>> " & msg);
    set.loop();
    el := set.get(ok);
    WHILE ok DO
      IO.Put(Fmt.Int(i) & ": " & el & "\n");
      INC(i);
      el := set.get(ok);
    END;
  END PrintTextCursorSet;


PROCEDURE PrintTextIdSet (msg: TEXT; set: TextIdSet.T) =
  VAR
    i : CARDINAL;
    el: TextId.T;
    ok: BOOLEAN;
  BEGIN
    i := 1;
    IO.Put("\n>>> " & msg);
    set.loop();
    el := set.get(ok);
    WHILE ok DO
      IO.Put(Fmt.Int(i) & ": text=\"" & el.text & "\"   id="
               & Fmt.Int(el.id) & "\n");
      INC(i);
      el := set.get(ok);
    END;
  END PrintTextIdSet;


(*----------------------------------------*)
(*--- Help procedures for SET-handling ---*)
(*----------------------------------------*)

PROCEDURE IdToTextCursorSet (IdSet: TextIdSet.T): TextCursorSet.T =
  VAR
    el     : TEXT;
    TextSet          := TextCursorSet.New();
    ok     : BOOLEAN;
  BEGIN
    IdSet.loop();
    el := IdSet.get(ok).text;
    WHILE ok DO TextSet.insert(el); el := IdSet.get(ok).text; END;
    RETURN TextSet;
  END IdToTextCursorSet;


PROCEDURE ErrorAbort (res : VirtualResource.T) =
  BEGIN
    TRY res.abortTransaction(); EXCEPT ELSE END;
  END ErrorAbort;

BEGIN
END Globals.
