(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified onFri Jan  7 13:31:07 PST 1994by msm    *)
(*      modified on Fri Nov  5 13:42:30 PST 1993 by wobber *)
(*      modified on Sun Jan 12 16:17:06 PST 1992 by meehan *)

UNSAFE MODULE IP;

IMPORT Atom, AtomList, Herrno, M3toC, TextF,
       Unetdb, Usocket, Unix, Uin, Utypes;

EXCEPTION FatalError; <*FATAL FatalError*>

VAR mu := NEW(MUTEX);

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
    RAISES {Error} =
  BEGIN
    LOCK mu DO
      VAR h := Unetdb.gethostbyname(ADR(nm[0])); BEGIN
        IF h = NIL THEN InterpretError(); RETURN FALSE; END;
        res := GetAddress(h);
      END;
    END;
    RETURN TRUE;
  END GetHostByName;

PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error} =
  BEGIN
    LOCK mu DO
      VAR h := Unetdb.gethostbyname(ADR(nm[0])); BEGIN
        IF h # NIL THEN
          RETURN M3toC.CopyStoT(h.h_name);
        END;
      END;
    END;
    InterpretError();
    RETURN NIL;
  END GetCanonicalByName;

PROCEDURE GetCanonicalByAddr(addr: Address): TEXT RAISES {Error} =
  VAR ua: Uin.struct_in_addr;
  BEGIN
    ua.s_addr := LOOPHOLE(addr, Utypes.u_int);
    LOCK mu DO
      VAR h := Unetdb.gethostbyaddr(
                   ADR(ua), BYTESIZE(ua), Usocket.AF_INET);
      BEGIN
        IF h # NIL THEN
          RETURN M3toC.CopyStoT(h.h_name);
        END;
      END;
    END;
    InterpretError();
    RETURN NIL;
  END GetCanonicalByAddr;

PROCEDURE GetAddress (ent: Unetdb.struct_hostent_star): Address =
  VAR ua: Uin.struct_in_addr;
  BEGIN
    <* ASSERT ent.h_length <= BYTESIZE(Address) *>
    ua := LOOPHOLE(ent.h_addr_list,
                    UNTRACED REF UNTRACED REF Uin.struct_in_addr)^^;
    RETURN LOOPHOLE(ua.s_addr, Address);
  END GetAddress;

PROCEDURE GetHostAddr(): Address RAISES {Error} =
  VAR hname: ARRAY [0..255] OF CHAR;
  BEGIN
    LOCK mu DO
      IF Unix.gethostname(ADR(hname[0]), BYTESIZE(hname)) # 0 THEN
        RAISE FatalError;
      END;
      VAR h := Unetdb.gethostbyname(ADR(hname[0])); BEGIN
        IF h = NIL THEN InterpretError(); END;
        RETURN GetAddress(h);
      END;
    END;
  END GetHostAddr;

PROCEDURE InterpretError() RAISES {Error} =
  BEGIN
    CASE Herrno.h_errno OF
    | Unetdb.TRY_AGAIN, Unetdb.NO_RECOVERY, Unetdb.NO_ADDRESS =>
        RAISE Error(AtomList.List1(LookupFailure));
    ELSE
    END;
  END InterpretError;

BEGIN
  LookupFailure := Atom.FromText("IP.LookupFailure");
  Unreachable := Atom.FromText("IP.Unreachable");
  PortBusy := Atom.FromText("IP.PortBusy");
  NoResources := Atom.FromText("IP.NoResources");
END IP.
