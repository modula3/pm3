MODULE ErrorSupport;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.3  1996/11/20 12:19:54  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:26:06  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/06/13 12:42:53  rbnix
        Error handling improved using new module ErrorSupport.

*)
(***************************************************************************)

IMPORT OSError, Atom, AtomList;


PROCEDURE Fmt (code: OSError.Code): TEXT =
  VAR t: TEXT;
  BEGIN
    IF AtomList.Length(code) = 0 THEN
      t := "<unkown error>"

    ELSE
      t := Atom.ToText(AtomList.Nth(code, 0));
      FOR i := 1 TO AtomList.Length(code) - 1 DO
        t := t & " " & Atom.ToText(AtomList.Nth(code, i));
      END;
    END;

    RETURN t;
  END Fmt;

PROCEDURE Create (proc, exception: TEXT): AtomList.T =
  BEGIN
    RETURN AtomList.List1(Atom.FromText(proc & ": " & exception));
  END Create;

PROCEDURE Propagate (proc, exception: TEXT; info: AtomList.T): AtomList.T =
  BEGIN
    RETURN AtomList.Cons(Atom.FromText(proc & ": " & exception), info);
  END Propagate;

PROCEDURE ToText (info: AtomList.T): TEXT =
  VAR
    res: TEXT     := "";
    len: CARDINAL;
  BEGIN
    len := AtomList.Length(info);
    FOR i := 0 TO len - 1 DO
      res := res & "\n" & Atom.ToText(AtomList.Nth(info, i));
    END;
    RETURN res;
  END ToText;


BEGIN
END ErrorSupport.
