UNSAFE MODULE VirtualPage EXPORTS VirtualPage, InternalVirtualPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.6  1997/11/19 17:59:45  roland
    Removed grouping of page accesses.

    Revision 1.5  1997/09/18 08:23:33  roland
    Grouping of access to the same page.

    Revision 1.4  1996/12/03 09:49:25  roland
    Replaced Type.Triple with CARDINAL. Special handling in put/getTriple
    of VirtualPage.

    Revision 1.3  1996/11/18 17:52:22  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/11/14 14:13:06  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.1  1996/02/29 17:44:25  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualPage --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Text,
  Type,
  PageData,
  PageCache,
  Access;

REVEAL
  T			= Internal BRANDED OBJECT
    OVERRIDES
      putByte		:= PutByte;
      getByte		:= GetByte;

      putShort		:= PutShort;
      getShort		:= GetShort;

      putTriple		:= PutTriple;
      getTriple		:= GetTriple;

      putInt		:= PutInt;
      getInt		:= GetInt;

      putWord		:= PutWord;
      getWord		:= GetWord;

      putArray		:= PutArray;
      getArray		:= GetArray;
      copyArray		:= CopyArray;

      putText		:= PutText;
      getText		:= GetText;

      putAll		:= PutAll;
      getAll		:= GetAll;
      peekAll		:= PeekAll;
    END;



(*
 | --- base access --------------------------------------------------------
 *)
PROCEDURE PutArray	(         self		:T;
                                  pos           :PageData.Index;
                         READONLY value		:PageData.Part)
			RAISES {Access.Locked, FatalError} =
  VAR pageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.writeAccess (pageAge).putData (value, pos);
    FINALLY
      PageCache.EndAccess ();
    END;
  END PutArray;

PROCEDURE GetArray	(         self		:T;
                                  pos           :PageData.Index;
			 VAR      value		:PageData.Part)
			RAISES {Access.Locked, FatalError} =
  VAR pageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.readAccess (pageAge).getData (value, pos);
    FINALLY
      PageCache.EndAccess ();
    END;
  END GetArray;

PROCEDURE CopyArray	(         self		:T;
                                  source        :PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index)
			RAISES {Access.Locked, FatalError} =
  VAR pageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.writeAccess (pageAge).copyData (source,destination, length);
    FINALLY
      PageCache.EndAccess ();
    END;
  END CopyArray;

PROCEDURE PutAll	(         self		:T;
                                  unswizzler    :PageData.Unswizzler)
			RAISES {Access.Locked, FatalError} =
  VAR pageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.writeAccess (pageAge).putAll (unswizzler);
    FINALLY
      PageCache.EndAccess ();
    END;
  END PutAll;

PROCEDURE GetAll	(         self		:T;
                                  swizzler      :PageData.Swizzler)
			RAISES {Access.Locked, FatalError} =
  VAR pageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.readAccess (pageAge).getAll (swizzler);
    FINALLY
      PageCache.EndAccess ();
    END;
  END GetAll;


PROCEDURE PeekAll	(         self		:T;
                                  swizzler      :PageData.Swizzler)
			RAISES {FatalError} =
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.peekAccess ().getAll (swizzler);
    FINALLY
      PageCache.EndAccess ();
    END;
  END PeekAll;


(*
 | --- typed access -------------------------------------------------------
 *)
PROCEDURE PutByte	(         self		:T;
                                  pos           :PageData.Index;
                                  value		:Type.Byte)
			RAISES {Access.Locked, FatalError} =
  BEGIN
    self.putArray (pos, LOOPHOLE (value, PageData.Part));
  END PutByte;

PROCEDURE GetByte	(         self		:T;
                                  pos           :PageData.Index)
			:Type.Byte
			RAISES {Access.Locked, FatalError} =
  VAR
    value		:Type.Byte;
  BEGIN
    self.getArray (pos, LOOPHOLE (value, PageData.Part));

    RETURN value;
  END GetByte;

PROCEDURE PutShort	(         self		:T;
                                  pos           :PageData.Index;
                                  value		:Type.Short)
			RAISES {Access.Locked, FatalError} =
  BEGIN
    self.putArray (pos, LOOPHOLE (value, PageData.Part));
  END PutShort;

PROCEDURE GetShort	(         self		:T;
                                  pos           :PageData.Index)
			:Type.Short
			RAISES {Access.Locked, FatalError} =
  VAR
    value		:Type.Short;
  BEGIN
    self.getArray (pos, LOOPHOLE (value, PageData.Part));

    RETURN value;
  END GetShort;

PROCEDURE PutTriple	(         self		:T;
                                  pos           :PageData.Index;
                                  value		:CARDINAL (* Type.Triple *))
			RAISES {Access.Locked, FatalError} =
  VAR val: ARRAY [0..2] OF Type.Byte;
  BEGIN
    (* self.putArray (pos, LOOPHOLE (value, PageData.Part)); *)
    val[0] := value MOD 256; value := value DIV 256;
    val[1] := value MOD 256; value := value DIV 256;
    val[2] := value MOD 256;
    self.putArray (pos, val);
  END PutTriple;

PROCEDURE GetTriple	(         self		:T;
                                  pos           :PageData.Index)
			:CARDINAL (*Type.Triple*)
			RAISES {Access.Locked, FatalError} =
  VAR
    value		: CARDINAL (* Type.Triple *);
    val: ARRAY [0..2] OF Type.Byte;
  BEGIN
    (* self.getArray (pos, LOOPHOLE (value, PageData.Part)); *)
    self.getArray (pos, val);
    value := val[0] + 256*val[1] + 256*256*val[2];

    RETURN value;
  END GetTriple;

PROCEDURE PutInt	(         self		:T;
                                  pos           :PageData.Index;
                                  value		:INTEGER)
			RAISES {Access.Locked, FatalError} =
  BEGIN
    self.putArray (pos, LOOPHOLE (value, PageData.Part));
  END PutInt;

PROCEDURE GetInt	(         self		:T;
                                  pos           :PageData.Index)
			:INTEGER
			RAISES {Access.Locked, FatalError} =
  VAR
    value		:INTEGER;
  BEGIN
    self.getArray (pos, LOOPHOLE (value, PageData.Part));

    RETURN value;
  END GetInt;

PROCEDURE PutWord	(         self		:T;
                                  pos           :PageData.Index;
                                  value		:Type.Word)
			RAISES {Access.Locked, FatalError} =
  BEGIN
    self.putArray (pos, LOOPHOLE (value, PageData.Part));
  END PutWord;

PROCEDURE GetWord	(         self		:T;
                                  pos           :PageData.Index)
			:Type.Word
			RAISES {Access.Locked, FatalError} =
  VAR
    value		:Type.Word;
  BEGIN
    self.getArray (pos, LOOPHOLE (value, PageData.Part));

    RETURN value;
  END GetWord;


(*
 | --- PutText, GetText ---------------------------------------------------
 Transfering Text is a little complicated because LOOPHOLE doesn't match
 to the necessary open array...
 | ------------------------------------------------------------------------
 *)
PROCEDURE PutText	(         self		:T;
                                  pos           :PageData.Index;
                         READONLY value		:TEXT)
			RAISES {Access.Locked, FatalError} =
  VAR
    part		:REF PageData.Part;
    length		:CARDINAL;
  BEGIN
    length := Text.Length (value);
    part := NEW (REF PageData.Part, length);

    FOR i := 0 TO length-1 DO
      part[i] := ORD (Text.GetChar (value, i));
    END;

    self.putArray (pos, part^);
  END PutText;

PROCEDURE GetText	(         self		:T;
                                  pos           :PageData.Index;
                                  length	:PageData.Index)
			:TEXT
			RAISES {Access.Locked, FatalError} =
  VAR
    part		:REF PageData.Part;
    chars		:REF ARRAY OF CHAR;
  BEGIN
    part := NEW (REF PageData.Part, length);
    self.getArray (pos, part^);

    chars := NEW (REF ARRAY OF CHAR, length);
    FOR i := 0 TO length-1 DO
      chars[i] := VAL (part[i], CHAR);
    END;

    RETURN Text.FromChars (chars^);
  END GetText;
  

BEGIN
END VirtualPage.
