(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(*  Created on Fri March 22 1996 by collin                                   *)
(*      under a new license:                                                 *)
(*                                                                           *)
(*      Copyright (C) 1997 Jerome Collin                                     *)
(*                                                                           *)
(*      This is a free software; you can redistribute it and/or modify       *)
(*      it under the terms of the GNU General Public License as published    *)
(*      by the Free Software Foundation; either version 2, or                *)
(*      (at your option) any later version.                                  *)
(*                                                                           *)
(*      This software is distributed in the hope that it will be useful,     *)
(*      but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*      GNU General Public License for more details.                         *)
(*                                                                           *)
(*  by modifying NTObjfile created and			                     *)
(*  last modified on Mon Sep 26 14:17:32 PDT 1994 by kalsow                  *)
(*							                     *)
(*  An "ELFObjFile.T" is a LINUX object file (ELF format).                   *)


MODULE ELFObjFile;

IMPORT Text, Wr, Word, Fmt, IntIntTbl, TextIntTbl, IntProcTbl, Process;
IMPORT M3ObjFile, M3ID, Target, TInt AS TargetInt, DebugProcTypes;

FROM M3CG      IMPORT Name, BitOffset, BitSize, ByteOffset, ByteSize, TypeUID, Type;
FROM M3ObjFile IMPORT R_386_32;

FROM DebugProcTypes IMPORT Param, Local, Bloc, Line, Case;

TYPE
  Seg = M3ObjFile.Seg;

TYPE 
  Alignment = [0..4];
CONST
  AlignBytes = ARRAY Alignment OF [1..16] { 1, 2, 4, 8, 16 };

TYPE
  SectionInfo = RECORD
    header  : SectionHeader;
    table   : Chunk;
    data    : Bytes := NIL;
    symbol  : INTEGER := M3ID.NoID;
  END;

  SectionSymbol = RECORD
    header  : SectionHeader;
    table   : Chunk;
    symbol  : SymbolList  := NIL;
    map	    : IntIntTbl.T := NIL;
    remap   : Ints        := NIL;
  END;

  SectionString = RECORD
    header  : SectionHeader;
    table   : Chunk;
    string  : TextList     := NIL;
    map	    : TextIntTbl.T := NIL;
  END;

  SectionRelocs = RECORD
    header  : SectionHeader;
    table   : Chunk;
    relocs  : RelocList := NIL;
    remap   : Ints      := NIL;
  END;

  SectionStab = RECORD
    header  : SectionHeader;
    table   : Chunk;
    entry   : StabList;
    symbol  : INTEGER := M3ID.NoID;
  END;

TYPE
  SectionHeader = RECORD
    index   : REF INTEGER := NIL;
    name    : INTEGER     := 0;
    type    : INTEGER     := 0;
    flags   : INTEGER     := 0;
    offset  : INTEGER     := 0;
    link    : REF INTEGER := NIL;
    info    : REF INTEGER := NIL;
    align   : INTEGER     := 0;
    pad     : INTEGER     := 0;
    used    : BOOLEAN     := FALSE;
  END;

TYPE
  Chunk = RECORD
    cnt        : INTEGER := 0;
    n_bytes    : INTEGER := 0;
    entry_size : INTEGER := 0;
  END;

TYPE
  Bytes = REF ARRAY OF CHAR;

TYPE
  SymbolList = REF ARRAY OF Symbol;
  Symbol = RECORD
    id        : M3ID.T;
    name      : INTEGER;     (* Index in the String Table *)
    value     : INTEGER;     (* Segment Offset *)
    size      : INTEGER;     (* Symbol Size *)
    type      : INTEGER;     (* Object, Function, Section... *)
    bind      : INTEGER;     (* Local, Global, Weak *)
    shindex   : REF INTEGER; (* Section Header Index *)
    used      : BOOLEAN;
    index     : INTEGER;     (* Symbol Table Index After Reordering *)
  END;

TYPE 
  Ints     = REF ARRAY OF INTEGER;
  TextList = REF ARRAY OF TEXT;

TYPE
  RelocList = REF ARRAY OF RelocEntry;
  RelocEntry = RECORD
    kind       : INTEGER;
    src_sym    : INTEGER;
    src_offset : INTEGER;
    target_sym : INTEGER;
  END;

TYPE
  StabList = REF ARRAY OF Internal_NList;
  Internal_NList = RECORD
    n_strx  : INTEGER:= 0;
    n_type  : INTEGER:= N_UNDF;
    n_other : INTEGER:= 0;
    n_desc  : INTEGER:= 0;
    n_value : INTEGER:= 0;
  END;

TYPE
  OutputStream = RECORD
    wr   : Wr.T := NIL;
    len  : INTEGER := 0;
    buf  : ARRAY [0..2047] OF CHAR;
  END;

TYPE 
  StabEntry = RECORD
    entry  : Internal_NList;
    string : TEXT;
  END;

REVEAL
  T = M3ObjFile.T BRANDED "ELFObjFile.T" OBJECT
    index0             : SectionInfo;
    text	       : SectionInfo;
    data	       : SectionInfo;
    bss		       : SectionInfo;
    rodata	       : SectionInfo;
    debug_sfnames      : SectionInfo;
    stab	       : SectionStab;
    stabstr	       : SectionString;
    shstrtab	       : SectionString;
    symtab	       : SectionSymbol;
    strtab	       : SectionString;
    rel_text	       : SectionRelocs;
    rel_data	       : SectionRelocs;
    rel_rodata	       : SectionRelocs;
    rel_stab	       : SectionRelocs;
    in_procedure       : BOOLEAN;
    last_source_line   : INTEGER;
    end_source_line    : INTEGER;
    stab_ndesc	       : INTEGER;
    out                : OutputStream;
    filename           : TEXT;
    filesym	       : INTEGER;
    gen_debugging      := FALSE;

  OVERRIDES
    set_debugging     := SetDebugging;

    cursor            := Cursor;
    append            := Append;
    patch             := Patch;
    relocate          := Relocate;
    import_symbol     := ImportSymbol;
    define_symbol     := DefineSymbol;
    define_bss_symbol := DefineBssSymbol;
    define_sect_sym   := DefineSectSym;
    add_symbol_size   := AddSymbolSize;
    move_symbol       := MoveSymbol;
    export_symbol     := ExportSymbol;
    set_source_file   := SetSourceFile;
    set_source_line   := SetSourceLine;

    begin_unit		:= BeginUnit;
    end_unit		:= EndUnit;
    declare_segment     := DeclareSegment;
    export_unit         := ExportUnit;
    declare_typename    := DeclareTypename;
    declare_array       := DeclareArray;
    declare_open_array  := DeclareOpenArray;
    declare_enum        := DeclareEnum;
    declare_enum_elt    := DeclareEnumElt;
    declare_packed      := DeclarePacked ;
    declare_record      := DeclareRecord;
    declare_field       := DeclareField;
    declare_set         := DeclareSet;
    declare_subrange    := DeclareSubrange;
    declare_pointer     := DeclarePointer;
    declare_indirect    := DeclareIndirect;
    declare_proctype    := DeclareProctype;
    declare_formal      := DeclareFormal;
    declare_raises      := DeclareRaises;
    declare_object      := DeclareObject;
    declare_method      := DeclareMethod;
    declare_opaque      := DeclareOpaque;
    reveal_opaque       := RevealOpaque;
    declare_exception   := DeclareException;
    declare_global      := DeclareGlobal;
    declare_constant    := DeclareConstant;

    declare_local       := DeclareLocal;
    declare_param       := DeclareParam;

    declare_procedure   := DeclareProcedure;

    declare_case_jump   := DeclareCaseJump;

    begin_procedure     := BeginProcedure;
    end_procedure       := EndProcedure;

    begin_block         := BeginBlock;
    end_block           := EndBlock;

    note_procedure_origin := NoteProcedureOrigin;
  END;

CONST	(*  Section Types  *)
  SHT_NULL     = 0;
  SHT_PROGBITS = 1;
  SHT_SYMTAB   = 2;
  SHT_STRTAB   = 3;
  SHT_NOBITS   = 8;
  SHT_REL      = 9;

CONST	(*  Section Attribute Flages  *)
  NONE          = 0;
  SHF_WRITE     = 16_1;
  SHF_ALLOC     = 16_2;
  SHF_EXECINSTR = 16_4;

VAR	(*  Special Section Indexes - READONLY *)
  SHN_UNDEF    := NEW(REF INTEGER);
  SHN_ABS      := NEW(REF INTEGER);

CONST	(*  Symbol Binding  *)
  STB_LOCAL    = 0;
  STB_GLOBAL   = 1;
  STB_WEAK     = 2;

CONST	(*  Symbol Type  *)
  STT_NOTYPE   = 0;
  STT_OBJECT   = 1;
  STT_FUNC     = 2;
  STT_SECTION  = 3;
  STT_FILE     = 4;

CONST  (* Byte size of the structures in the file *)
  ELFHeaderSise     = 52;
  SectionHeaderSize = 40;
  SymTableEntrySize = 16;
  RelocEntrySize    = 8;
  StabEntrySize     = 12;

(*------------------------------------------ initialization -------------*)

PROCEDURE New (): T =
  VAR t := NEW (T);
  BEGIN
    SHN_UNDEF^   := 0;
    SHN_ABS^     := 16_fff1;

    InitSection (t.index0.header, SHT_NULL, NONE, TRUE,
		 0, t.index0.table, 0);
    InitSection (t.text.header, SHT_PROGBITS, SHF_ALLOC+SHF_EXECINSTR, FALSE,
		 4, t.text.table, 0);
    InitSection (t.rel_text.header, SHT_REL, NONE, FALSE,
		 4, t.rel_text.table, RelocEntrySize);
    InitSection (t.data.header, SHT_PROGBITS, SHF_ALLOC+SHF_WRITE, FALSE,
		 4, t.data.table, 0);
    InitSection (t.rel_data.header, SHT_REL, NONE, FALSE,
		 4, t.rel_data.table, RelocEntrySize);
    InitSection (t.bss.header, SHT_NOBITS, SHF_ALLOC+SHF_WRITE, FALSE,
		 4, t.bss.table, 0);
    InitSection (t.rodata.header, SHT_PROGBITS, SHF_ALLOC, FALSE,
		 4, t.rodata.table, 0);
    InitSection (t.rel_rodata.header, SHT_REL, NONE, FALSE,
		 4, t.rel_rodata.table, RelocEntrySize);
    InitSection (t.shstrtab.header, SHT_STRTAB, NONE, TRUE,
		 1, t.shstrtab.table, 0);
    InitSection (t.symtab.header, SHT_SYMTAB, NONE, TRUE,
		 4, t.symtab.table, SymTableEntrySize);
    InitSection (t.strtab.header, SHT_STRTAB, NONE, TRUE,
		 1, t.strtab.table, 0);

    LinkInfo(t.symtab.header, t.strtab.header.index, SHN_UNDEF);
    LinkInfo(t.rel_text.header, t.symtab.header.index, t.text.header.index);
    LinkInfo(t.rel_data.header, t.symtab.header.index, t.data.header.index);
    LinkInfo(t.rel_rodata.header, t.symtab.header.index, t.rodata.header.index);

    AddCompSym (t);

    WITH o = t.out DO
      o.wr  := NIL;
      o.len := 0;
    END;

    t.last_source_line := 0;

    RETURN t;
  END New;

PROCEDURE SetDebugging(t: T) =
  BEGIN
    t.gen_debugging := TRUE;

    InitSection (t.stab.header, SHT_PROGBITS, NONE, TRUE,
		 4, t.stab.table, StabEntrySize);
    InitSection (t.rel_stab.header, SHT_REL, NONE, FALSE,
		 4, t.rel_stab.table, RelocEntrySize);
    InitSection (t.stabstr.header, SHT_STRTAB, NONE, TRUE,
		 1, t.stabstr.table, 0);

    LinkInfo(t.rel_stab.header, t.symtab.header.index, t.stab.header.index);
    LinkInfo(t.stab.header, t.stabstr.header.index, t.index0.header.index);

    EVAL AddSectSym (t, t.stabstr.header);

    t.stab.symbol := AddSectSym (t, t.stab.header);

  END SetDebugging;

PROCEDURE InitSection (VAR sh: SectionHeader; type: INTEGER; flags: INTEGER;
                       used: BOOLEAN; align: INTEGER; VAR st: Chunk; sz  : INTEGER ) =
  BEGIN
    sh.index   := NEW(REF INTEGER);
    sh.used    := used;
    sh.name    := 0;
    sh.type    := type;
    sh.flags   := flags;
    sh.offset  := 0;
    sh.link    := SHN_UNDEF;
    sh.info    := SHN_UNDEF;
    sh.align   := align;
    sh.pad     := 0;
    InitChunk (st, sz);  (* add the entry size *)
  END InitSection;

PROCEDURE InitChunk (VAR c: Chunk;  sz: INTEGER) =
  BEGIN
    c.cnt         := 0;
    c.n_bytes     := 0;
    c.entry_size  := sz;
  END InitChunk;

PROCEDURE LinkInfo(VAR sh: SectionHeader; link, info: REF INTEGER) =
  BEGIN
    sh.link := link;
    sh.info := info;
  END LinkInfo;

PROCEDURE DefineSectSym (t: T;  seg: Seg): INTEGER =
  VAR z: INTEGER;
  BEGIN
    CASE seg OF
    | Seg.Text =>
        z := AddSectSym (t, t.text.header);
	t.text.header.used := TRUE;
	t.text.symbol := z;
    | Seg.Data =>
	z := AddSectSym (t, t.data.header);
	t.data.header.used := TRUE;
	t.data.symbol := z;
    | Seg.Bss  => 
	z := AddSectSym (t, t.bss.header);
	t.bss.header.used := TRUE;
	t.bss.symbol := z;
    | Seg.Rodata => 
	z := AddSectSym (t, t.rodata.header);
	t.rodata.header.used := TRUE;
	t.rodata.symbol := z;
    ELSE (* IGNORE *)
    END;
    RETURN(z)
  END DefineSectSym;

PROCEDURE AddSectSym (t: T; VAR sh: SectionHeader): INTEGER =
  VAR z: INTEGER;
  BEGIN
    z := NewSym (t, M3ID.NoID);
    WITH sym = t.symtab.symbol[z] DO
      sym.type    := STT_SECTION;
      sym.used    := TRUE;
      sym.shindex := sh.index;
    END;

    sh.used := TRUE;
    RETURN(z);
  END AddSectSym;

PROCEDURE AddCompSym (t : T) =
  VAR z := NewSym (t, M3ID.Add ("SRC-Modula3_compiled."));
  BEGIN
    WITH sym = t.symtab.symbol[z] DO
      sym.type  := STT_NOTYPE;
      sym.shindex := t.text.header.index;
      sym.used  := TRUE;
    END;
  END AddCompSym;

(*---------------------------------------------------------- construction ---*)

PROCEDURE Cursor (t: T; s: Seg): INTEGER =
  BEGIN
    IF s = Seg.Text THEN
      RETURN t.text.table.n_bytes;
    ELSIF s = Seg.Data THEN
      RETURN t.data.table.n_bytes;
    ELSIF s = Seg.Bss THEN
      RETURN t.bss.table.n_bytes;
    ELSE
      RETURN t.rodata.table.n_bytes;
    END;
  END Cursor;

PROCEDURE Append (t: T;  s: Seg;  value, length: INTEGER) =
  BEGIN
    IF (s = Seg.Text) THEN
      (* There is raw data for .text => new alignment for this section *)
      t.text.header.align := 16; 
      AddRaw (t.text, value, length);
    ELSIF s = Seg.Data THEN
      AddRaw (t.data, value, length);
    ELSE
      AddRaw (t.rodata, value, length);
    END;
  END Append;

PROCEDURE AddRaw (VAR s: SectionInfo;  value, length: INTEGER) =
  VAR
    offs := s.table.n_bytes;
    seg  := EnsureLength (s.data, offs + length);
  BEGIN
    WHILE (length > 0) DO
      seg[offs] := VAL (Word.And (value, 16_ff), CHAR);
      value := Word.RightShift (value, 8);
      INC (offs);
      DEC (length);
    END;
    s.table.n_bytes := offs;
    s.table.cnt     := offs;
  END AddRaw;

PROCEDURE EnsureLength (VAR b: Bytes;  length: INTEGER): Bytes =
  VAR n, m: INTEGER;
  BEGIN
    IF (b = NIL) THEN  b := NEW (Bytes, 1024);  END;
    n := NUMBER (b^);
    IF (n < length) THEN
      m := n;
      WHILE (m < length) DO INC (m, m); END;
      VAR new := NEW (Bytes, m); BEGIN
        SUBARRAY (new^, 0, n) := b^;
        b := new;
      END;
    END;
    RETURN b;
  END EnsureLength;

PROCEDURE Patch (t: T;  s: Seg;  offset, value, length: INTEGER) =
  BEGIN
    IF s = Seg.Text THEN
      PatchRaw (t.text, offset, value, length);
    ELSIF s = Seg.Data THEN
      PatchRaw (t.data, offset, value, length);
    ELSE
      PatchRaw (t.rodata, offset, value, length);
    END;
  END Patch;

PROCEDURE PatchRaw (VAR s: SectionInfo;  offset, value, length: INTEGER) =
  BEGIN
    <* ASSERT s.table.n_bytes >= offset + length *>

    WHILE (length > 0) DO
      s.data[offset] := VAL (Word.And (value, 16_ff), CHAR);
      value := Word.RightShift (value, 8);
      INC (offset);
      DEC (length);
    END;
  END PatchRaw;

PROCEDURE Relocate (t: T;  src_sym, src_offs, tar_sym, kind: INTEGER) =
  BEGIN
    t.symtab.symbol[src_sym].used := TRUE;
    t.symtab.symbol[tar_sym].used := TRUE;

    WITH index = t.symtab.symbol[src_sym].shindex DO

      IF index = t.text.header.index THEN
        t.rel_text.header.used := TRUE;
        AddReloc (t.rel_text, src_sym, src_offs, tar_sym, kind);

      ELSIF index = t.data.header.index THEN
        t.rel_data.header.used := TRUE;
        AddReloc (t.rel_data, src_sym, src_offs, tar_sym, kind);

      ELSIF index = t.rodata.header.index THEN
        t.rel_rodata.header.used := TRUE;
        AddReloc (t.rel_rodata, src_sym, src_offs, tar_sym, kind);

      ELSIF index = t.stab.header.index THEN
        t.rel_stab.header.used := TRUE;
        AddReloc (t.rel_stab, src_sym, src_offs, tar_sym, kind);
      END;

    END;
  END Relocate;

PROCEDURE AddReloc (VAR s: SectionRelocs; src_sym, src_offs, tar_sym, kind: INTEGER) =
  BEGIN
    IF (s.relocs = NIL) OR (s.table.cnt >= NUMBER (s.relocs^)) THEN
      ExpandRelocs (s);
    END;

    WITH r = s.relocs [s.table.cnt] DO
      r.src_sym       := src_sym;
      r.src_offset    := src_offs;
      r.target_sym    := tar_sym;
      r.kind          := kind;
    END;

    INC (s.table.cnt);
    INC (s.table.n_bytes, RelocEntrySize);
  END AddReloc;

PROCEDURE ExpandRelocs (VAR s: SectionRelocs) =
  VAR n: INTEGER;  new: RelocList;
  BEGIN
    IF (s.relocs = NIL) THEN
      s.relocs := NEW (RelocList, 100);
    ELSE
      n := NUMBER (s.relocs^);
      new := NEW (RelocList, n + n);
      SUBARRAY (new^, 0, n) := s.relocs^;
      s.relocs := new;
    END;
  END ExpandRelocs;

PROCEDURE ImportSymbol (t: T;  id: M3ID.T): INTEGER =
  VAR z := NewSym (t, id);
  BEGIN
    WITH sym = t.symtab.symbol[z] DO
      sym.bind     := STB_GLOBAL;
      sym.shindex  := SHN_UNDEF;
    END;
    RETURN z;
  END ImportSymbol;

PROCEDURE DefineSymbol (t: T;  id: M3ID.T;  s: Seg;  offset: INTEGER): INTEGER =
  VAR z := NewSym (t, id);
  BEGIN

    WITH sym = t.symtab.symbol[z] DO
      sym.value := offset;
      IF s = Seg.Text THEN
         sym.shindex := t.text.header.index;
         sym.type  := STT_FUNC;
         sym.used  := TRUE;
      ELSIF s = Seg.Data THEN
         sym.shindex := t.data.header.index;
         sym.type  := STT_OBJECT;
      ELSE
         sym.shindex := t.rodata.header.index;
         sym.type  := STT_OBJECT;
         sym.used  := TRUE
      END;

    END;
    RETURN z;
  END DefineSymbol;

PROCEDURE DefineBssSymbol (t: T;  id: M3ID.T;  size, align: INTEGER): INTEGER =
  VAR z := NewSym (t, id);  a := FindAlign (align);  ab := AlignBytes[a];
  BEGIN
    (* align the symbol in the segment *)
    t.bss.table.n_bytes := (t.bss.table.n_bytes + ab - 1) DIV ab * ab;

    WITH sym = t.symtab.symbol[z] DO
      sym.value    := t.bss.table.n_bytes;
      sym.type     := STT_NOTYPE;
      sym.shindex  := t.bss.header.index;
    END;

    (* add the space to the segment *)
    INC (t.bss.table.n_bytes, size);
    RETURN z;
  END DefineBssSymbol;

PROCEDURE AddSymbolSize (t: T;  id: M3ID.T;  size: INTEGER) =
  BEGIN
    <* ASSERT id # M3ID.NoID *>
    t.symtab.symbol[id].size := size;
  END AddSymbolSize;

PROCEDURE MoveSymbol (t: T;  sym: INTEGER;  new_offset: INTEGER) =
  BEGIN
    t.symtab.symbol[sym].value := new_offset;
  END MoveSymbol;

PROCEDURE ExportSymbol (t: T;  sym: INTEGER) =
  BEGIN
    WITH s = t.symtab.symbol[sym] DO
      s.bind := STB_GLOBAL;
      s.used := TRUE;
    END;
  END ExportSymbol;

PROCEDURE FindAlign (align: INTEGER): Alignment =
  BEGIN
    FOR i := FIRST (AlignBytes) TO LAST (AlignBytes) DO
      IF (AlignBytes[i] = align) THEN RETURN i; END;
    END;
    <*ASSERT FALSE*>
  END FindAlign;

PROCEDURE NewSym (t: T;  id: M3ID.T): INTEGER =
  VAR x: INTEGER;
  BEGIN
    IF (t.symtab.map = NIL) THEN
      t.symtab.map := NEW (IntIntTbl.Default).init();
    END;

    IF id = M3ID.NoID THEN
      x := NextSym (t.symtab);
    ELSE
      IF t.symtab.map.get (id, x) THEN
	 <*ASSERT FALSE*> (* duplicate symbol *)
      END;
      x := NextSym (t.symtab);
      EVAL t.symtab.map.put (id, x);
    END;

    WITH sym = t.symtab.symbol[x] DO
      sym.id       := id;
      sym.name     := 0;
      sym.value    := 0;
      sym.size     := 0;
      sym.type     := STT_NOTYPE;
      sym.bind     := STB_LOCAL;
      sym.shindex  := SHN_UNDEF; 
      sym.used     := FALSE;
      sym.index    := -1;
    END;
    RETURN x;
  END NewSym;

PROCEDURE NextSym (VAR s: SectionSymbol): INTEGER =
  VAR x := s.table.cnt;
  BEGIN
    WITH tb = s.table DO
      INC (tb.cnt);
      INC (tb.n_bytes, SymTableEntrySize);
    END;

    IF (s.symbol = NIL)	THEN	(*  Symbol Table Entry: Index 0  *)
      ExpandSyms(s);

      s.symbol[0].id       := M3ID.NoID;
      s.symbol[0].name     := 0;
      s.symbol[0].value    := 0;
      s.symbol[0].size     := 0;
      s.symbol[0].type     := 0;
      s.symbol[0].bind     := 0;
      s.symbol[0].shindex  := SHN_UNDEF;
      s.symbol[0].used     := TRUE;
      s.symbol[0].index    := -1;
      x := s.table.cnt;
      INC (s.table.cnt);
      INC (s.table.n_bytes, SymTableEntrySize);

    ELSIF x >= NUMBER (s.symbol^) THEN

      ExpandSyms(s);
    END;

    RETURN x;
  END NextSym;

PROCEDURE ExpandSyms (VAR s: SectionSymbol) =
  VAR n: INTEGER;  new: SymbolList;
  BEGIN
    IF (s.symbol = NIL) THEN
      s.symbol := NEW (SymbolList, 100);

    ELSE

      n := NUMBER (s.symbol^);
      new := NEW (SymbolList, n + n);
      SUBARRAY (new^, 0, n) := s.symbol^;
      s.symbol := new;
    END;
  END ExpandSyms;

VAR included_filename_stab : StabEntry;
    is_source_file_set     : BOOLEAN;

PROCEDURE SetSourceFile (t: T;  filename: TEXT) =
  VAR
    z, pos :  INTEGER;
  BEGIN
    IF NOT is_source_file_set THEN

      is_source_file_set:= TRUE;

      included_filename_stab.entry.n_type := N_SOL;
      included_filename_stab.string:= filename;

      ObjectName(filename);

      (* Add the symbol in the table *)
      z := NewSym (t, M3ID.Add (filename));
      t.filename:= filename;
      t.filesym := z;

      WITH sym = t.symtab.symbol[z] DO
        sym.value    := 0;
        sym.type     := STT_FILE;
        sym.bind     := STB_LOCAL;
        sym.shindex  := SHN_ABS;
        sym.used     := TRUE;
      END;

      IF t.gen_debugging THEN
        pos:= AddString(t.stabstr, filename); (* for the first entry *)

        pos:= AddString(t.stabstr, 
		        <* NOWARN *> Process.GetWorkingDirectory() & "/");
        AddStabEntry(t, pos, N_SO, 0, 0, 0);

        pos:= AddString(t.stabstr, filename);
        AddStabEntry(t, pos, N_SO, 0, 0, 0);

        pos:= AddString(t.stabstr, "SRC-Modula3_compiled.");
        AddStabEntry(t, pos, N_OPT, 0, 0, 0);
      END;

    ELSE (* This happen with generics ... *)

      included_filename_stab.string:= filename;
    END;
  END SetSourceFile;

PROCEDURE ObjectName (VAR src: TEXT) =
  VAR
    len := Text.Length (src);
    last_slash := -1;
    ext : TEXT;
    ch: CHAR;
  BEGIN
    (* chop off any path prefix *)
    FOR i := 0 TO len-1 DO
      ch := Text.GetChar (src, i);
      IF (ch = '/') OR (ch = '\134') THEN last_slash := i; END;
    END;

    INC (last_slash);

    src := Text.Sub (src, last_slash);

    DEC (len, last_slash);

    (* fix the extension *)
    IF (len > 3) THEN
      ext := Text.Sub (src, len-3);
      IF Text.Equal (ext, ".m3") OR Text.Equal (ext, ".i3") THEN
        src := Text.Sub (src, 0, len-1) & "c";
      END;
    END;

  END ObjectName;

PROCEDURE SetSourceLine (t: T;  source_line: INTEGER) =
  BEGIN
    IF (source_line <= 0) OR NOT t.gen_debugging THEN RETURN END;

    IF (t.in_procedure) THEN
      AddSourceLine(source_line, t.text.table.n_bytes);
    END;

    t.last_source_line := source_line;

    IF t.last_source_line > t.end_source_line THEN
      t.end_source_line := t.last_source_line + 1;
    END;
  END SetSourceLine;

PROCEDURE AddStabEntry(t: T; n_strx, n_type, n_other, n_desc, n_value: INTEGER;
							    last_entry:= FALSE) =
  BEGIN
    WITH c = t.stab.table, list = t.stab.entry DO
      IF last_entry THEN
	<* ASSERT list # NIL *>
	WITH entry = list [0] DO
          entry.n_strx  := n_strx;
          entry.n_type  := n_type;
          entry.n_other := n_other;
          entry.n_desc  := n_desc;
          entry.n_value := n_value;
        END;

      ELSE

        IF list = NIL THEN
          ExpandStab (t.stab);
          INC (c.cnt);	               (* Leave space for the first stab entry *)
          INC (c.n_bytes, c.entry_size);
	  t.stab_ndesc := 0;
        ELSIF c.cnt >= NUMBER (list^) THEN
          ExpandStab (t.stab);
        END;

        WITH entry = list [c.cnt] DO
          entry.n_strx  := n_strx;
          entry.n_type  := n_type;
          entry.n_other := n_other;
          entry.n_desc  := n_desc;
          entry.n_value := n_value;

          CASE entry.n_type OF
	  | N_SO => 
              Relocate(t, t.stab.symbol, c.n_bytes + 8, t.text.symbol, R_386_32);
	  | N_SOL => 
              Relocate(t, t.stab.symbol, c.n_bytes + 8, t.text.symbol, R_386_32);
	  | N_FUN => 
              Relocate(t, t.stab.symbol, c.n_bytes + 8, t.text.symbol, R_386_32);
	  | N_LCSYM => 
              Relocate(t, t.stab.symbol, c.n_bytes + 8, t.bss.symbol, R_386_32);
	  | N_STSYM => 
              Relocate(t, t.stab.symbol, c.n_bytes + 8, t.data.symbol, R_386_32);
          ELSE
          END;
        END;

        INC (c.cnt);
        INC (c.n_bytes, c.entry_size);
        INC (t.stab_ndesc);
      END;
    END;
  END AddStabEntry;

PROCEDURE ExpandStab (VAR s: SectionStab) =
  VAR n: INTEGER;  new: StabList;
  BEGIN
    IF (s.entry = NIL) THEN
      s.entry := NEW (StabList, 100);
    ELSE
      n := NUMBER (s.entry^);
      new := NEW (StabList, n + n);
      SUBARRAY (new^, 0, n) := s.entry^;
      s.entry := new;
    END;
  END ExpandStab;

PROCEDURE AddString (VAR s: SectionString; name: TEXT): INTEGER =
  VAR offset: INTEGER := 0;
  BEGIN
    IF name = NIL THEN RETURN (offset);  (* no name or null name *)
    ELSE				 (* must have index zero *)
      IF s.string = NIL THEN	       
	ExpandStrtab(s);
        s.string[0] := "\000";	(* index zero is \0 *)
        INC(s.table.cnt);
        INC(s.table.n_bytes);
      END;

      IF s.map = NIL THEN
	s.map := NEW (TextIntTbl.Default).init ();
      END;

      IF NOT s.map.get (name, offset) THEN
	offset := s.table.n_bytes;
        INC(s.table.n_bytes, 1 + Text.Length(name));
	EVAL s.map.put(name, offset);

        IF s.table.cnt >= NUMBER (s.string^) THEN 
	  ExpandStrtab(s);
        END;

	s.string[s.table.cnt] := name;
	INC(s.table.cnt);
      END;
    END;
    RETURN(offset);
  END AddString;

PROCEDURE ExpandStrtab (VAR s: SectionString) =
  VAR n: INTEGER;  new: TextList;
  BEGIN
    IF s.string = NIL THEN
      s.string := NEW (TextList, 100);
    ELSE
      n := NUMBER (s.string^);
      new := NEW (TextList, n + n);
      SUBARRAY (new^, 0, n) := s.string^;
      s.string := new;
    END;
  END ExpandStrtab;

(*----------------------------------------------------- debugging support ---*)

CONST
  (* For some unknown reasons, some stabs entries have their "desc" *)
  (* field  set to the first source line of the main function.  We  *)
  (* know this source line late so we temporarely set it to         *)
  (* Unit_First_Line and we will change for the real source line    *)
  (* later (during the dump)					    *)

  Unit_First_Line = LAST(INTEGER);

CONST
  N_UNDF   = 16_0;
  N_GSYM   = 16_20;
  N_FUN    = 16_24;
  N_STSYM  = 16_26;
  N_LCSYM  = 16_28;
  N_OPT    = 16_3C;
  N_SLINE  = 16_44;
  N_SO 	   = 16_64;
  N_LSYM   = 16_80;
  N_SOL    = 16_84;
  N_PSYM   = 16_A0;
  N_LBRAC  = 16_C0;
  N_RBRAC  = 16_E0;

CONST
  NULL_TYPE = 0;

TYPE
  Global = REF RECORD
    is_bss : BOOLEAN;
    n      : Name;
    s      : ByteSize;
    next   : Global := NIL;
  END;

TYPE
  Basic_Stab = RECORD
    def1, def2   := "";
    was_defined  := FALSE;
    stab_type_no := 0;
  END;

  Stab_Type = RECORD
    no  := NULL_TYPE;
    def := "";
  END;

VAR
  current_unit_name        : TEXT;
  exported_interfaces      : INTEGER;
  exported_interfaces_names: ARRAY [0..100] OF TEXT;

  stab_type_no    : INTEGER;
  stack_var_no    : INTEGER;

  current_dbg_type_count1:= 0;
  current_dbg_type_count2:= 0;
  current_dbg_type_count3:= 0;
  current_stab           : StabEntry;

  proc_table		: IntProcTbl.T;
  current_proc          : DebugProcTypes.T;
  current_proc_line     : INTEGER;
  current_proc_offset   : INTEGER;

  main_first_line       : INTEGER;
  module_first_line     : INTEGER;

  is_a_INITM_segment    : BOOLEAN;
  is_included_printed   : BOOLEAN;
  is_generated_after_GLOBALVAR : BOOLEAN;

  first_global          : Global := NIL;
  current_global        : Global := NIL;

  basic_stab            : ARRAY Type OF Basic_Stab;

PROCEDURE FmtUID (type: TypeUID): TEXT =
  VAR res:= "";
      digit: INTEGER;
      test : CHAR;
  BEGIN
    IF type = -1 THEN RETURN "zzzzzz" END;

    FOR i:= 5 TO 0 BY -1 DO
      digit:= Word.Mod(type, 62);
      type:= Word.Divide(type, 62);

      IF digit < 26 THEN
        res:= Text.FromChar( VAL(65 + digit, CHAR) ) & res;	(* 65 is ASCII 'A' *)
      ELSIF digit < 52 THEN
        res:= Text.FromChar( VAL(97 + (digit - 26), CHAR) ) & res;
      ELSE							(* 97 is ASCII 'a' *)
        res:= Text.FromChar( VAL(48 + (digit - 52), CHAR) ) & res;
      END;							(* 48 is ASCII '0' *)
    END;

    test:= Text.GetChar(res, 0);
    <* ASSERT (type = 0) AND (test >= 'A') AND ('Z' >= test) *>
    RETURN res;
  END FmtUID;

PROCEDURE PrintStab(t: T) =
  VAR pos:= AddString(t.stabstr, current_stab.string);
  BEGIN
    WITH Z = current_stab.entry DO
      AddStabEntry(t, pos, Z.n_type, Z.n_other, Z.n_desc, Z.n_value);
      Z.n_other:= 0;
      Z.n_desc := 0;
      Z.n_value:= 0;
    END;
  END PrintStab;

PROCEDURE StabEntryNo() : INTEGER =
  BEGIN
    INC(stab_type_no);
    RETURN stab_type_no;
  END StabEntryNo;

PROCEDURE StackVarName() : TEXT =
  BEGIN
    INC(stack_var_no);
    RETURN ( "L_" & Fmt.Int(stack_var_no) );
  END StackVarName;

PROCEDURE GetStabType(t: Type;  s: BitSize:= 0) : TEXT =
  BEGIN
    RETURN GetTargetType(t, s).def;
  END GetStabType;

PROCEDURE GetTargetType(type: Type;  s: BitSize): Stab_Type =
  BEGIN
    CASE type OF
    | Type.Addr   =>
      RETURN BuildTargetType(Type.Addr, Type.Void);

    | Type.Word   =>
      RETURN BuildTargetType(Type.Word, Type.Int);

    | Type.Int    =>
      RETURN BuildTargetType(Type.Int, Type.Int);

    | Type.Reel   =>
      RETURN BuildTargetType(Type.Reel, Type.Int);

    | Type.LReel  =>
      RETURN BuildTargetType(Type.LReel, Type.Int);

    | Type.XReel  =>
      RETURN BuildTargetType(Type.XReel, Type.Int);

    | Type.Int_A  =>
      RETURN BuildTargetType(Type.Int_A, Type.Int_A);

    | Type.Int_B  =>
      RETURN BuildTargetType(Type.Int_B, Type.Int);

    | Type.Int_C  =>
      RETURN BuildTargetType(Type.Int_C, Type.Int);

    | Type.Int_D  =>
      RETURN BuildTargetType(Type.Int_D, Type.Int);

    | Type.Word_A =>
      RETURN BuildTargetType(Type.Word_A, Type.Int);

    | Type.Word_B =>
      RETURN BuildTargetType(Type.Word_B, Type.Int);

    | Type.Word_C =>
      RETURN BuildTargetType(Type.Word_C, Type.Int);

    | Type.Word_D =>
      RETURN BuildTargetType(Type.Word_D, Type.Int);

    | Type.Struct =>
      RETURN BuildStructType(s);

    | Type.Void   =>
      RETURN BuildTargetType(Type.Void, Type.Void);
    END;
  END GetTargetType;

PROCEDURE BuildTargetType(a, b: Type): Stab_Type =
  VAR stab_type : Stab_Type;
  BEGIN
    WITH abs = basic_stab[a], bbs = basic_stab[b] DO

      IF NOT abs.was_defined THEN

	abs.was_defined  := TRUE;
        abs.stab_type_no := StabEntryNo();

        stab_type.def:= Fmt.Int(abs.stab_type_no) & abs.def1;

        IF a = b THEN
           stab_type.def:= stab_type.def & Fmt.Int(abs.stab_type_no);
	ELSIF bbs.was_defined THEN
           stab_type.def:= stab_type.def & Fmt.Int(bbs.stab_type_no);
	ELSE
	   stab_type.def:= stab_type.def & BuildTargetType(b, b).def & abs.def2;
	END;

        stab_type.def:= stab_type.def & abs.def2;
        stab_type.no := abs.stab_type_no;

      ELSE

        stab_type.no := abs.stab_type_no;
        stab_type.def:= Fmt.Int(abs.stab_type_no);

      END;
    END;

    RETURN stab_type;
  END BuildTargetType;

PROCEDURE BuildStructType(size: BitSize): Stab_Type =
  VAR
    stab_type: Stab_Type;
  BEGIN
    stab_type.no  := StabEntryNo(); 
    stab_type.def := Fmt.Int( stab_type.no ) & "=s" & Fmt.Int( size ) & ";";
    RETURN stab_type;
  END BuildStructType;

PROCEDURE TypeT (): TEXT =
  BEGIN
    RETURN ":T" & Fmt.Int( StabEntryNo() ) & "=s1";
  END TypeT;

PROCEDURE GetFeildFmt (): TEXT =
  BEGIN
     RETURN ":" & GetStabType(Type.Int) & ",0,1;";
  END GetFeildFmt;

PROCEDURE AddSourceLine(source_line, address: INTEGER) =
  BEGIN
    IF current_proc.line_head = NIL THEN
      current_proc.line_head := NEW(Line);
      current_proc.line_tail := current_proc.line_head;
    ELSE
      current_proc.line_tail.next := NEW(Line);
      current_proc.line_tail := current_proc.line_tail.next;
    END;

    WITH tail = current_proc.line_tail DO
      tail.line := source_line;
      tail.addr := address - current_proc_offset;
    END;
  END AddSourceLine;

PROCEDURE BeginUnit (t: T) =
  BEGIN
    is_source_file_set  := FALSE;
    IF t.gen_debugging THEN

      exported_interfaces := 0;

      stab_type_no        := 0;
      stack_var_no        := 0;

      current_dbg_type_count1:= 0;
      current_dbg_type_count2:= 0;
      current_dbg_type_count3:= 0;

      proc_table          := NEW (IntProcTbl.Default).init();
      current_proc        := NIL;
      current_proc_line   := 0;
      current_proc_offset := 0;

      main_first_line     := Unit_First_Line;
      module_first_line   := 0;

      is_a_INITM_segment  := FALSE;
      is_included_printed := FALSE;
      is_generated_after_GLOBALVAR := FALSE;

      first_global        := NIL;
      current_global      := NIL;

      basic_stab := ARRAY Type OF Basic_Stab {
                    Basic_Stab{ def1:= "=*", def2:= "" },
                    Basic_Stab{ def1:= "=r", def2:= ";0;-1;" },
                    Basic_Stab{ def1:= "=r", def2:= ";-2147483648;2147483647;" },
                    Basic_Stab{ def1:= "=r", def2:= ";4;0;" },
                    Basic_Stab{ def1:= "=r", def2:= ";8;0;" },
                    Basic_Stab{ def1:= "=r", def2:= ";8;0;" },
                    Basic_Stab{ def1:= "=r", def2:= ";0;127;" },
                    Basic_Stab{ def1:= "=r", def2:= ";-32768;32767;" },
                    Basic_Stab{ def1:= "=r", def2:= ";-2147483648;2147483647;" },
                    Basic_Stab{ def1:= "=r", def2:= ";-2147483648;2147483647;" },
                    Basic_Stab{ def1:= "=r", def2:= ";0;255;" },
                    Basic_Stab{ def1:= "=r", def2:= ";0;65535;" },
                    Basic_Stab{ def1:= "=r", def2:= ";0;-1;" },
                    Basic_Stab{ def1:= "=r", def2:= ";0;-1;" },
                    Basic_Stab{ def1:= "",   def2:= "" },  (* Built differently *)
                    Basic_Stab{ def1:= "=",  def2:= "" }
		  };
    END;
  END BeginUnit;

PROCEDURE EndUnit(t: T) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "Mi_zzzzzz_" & current_unit_name & TypeT();

      FOR j:= 0 TO exported_interfaces-1 DO
        current_stab.string:= current_stab.string & exported_interfaces_names[j]
			      & GetFeildFmt();
      END;
      current_stab.string:= current_stab.string & ";";
      PrintStab(t);

      current_global := first_global;

      WHILE current_global # NIL DO

        WITH cg = current_global DO

          IF NOT cg.is_bss THEN
            current_stab.entry.n_type := N_GSYM;
            current_stab.entry.n_desc := module_first_line;
            current_stab.string := M3ID.ToText(cg.n) & ":G"
				   & Fmt.Int( StabEntryNo() ) & "=s"
			           & Fmt.Int(cg.s);
            PrintStab(t);
          END; (* IF *)

        END; (* WITH *)
        current_global := current_global.next;

      END; (* WHILE *)

      current_global := first_global;

      WHILE current_global # NIL DO

        WITH cg = current_global DO

          IF cg.is_bss THEN
            current_stab.entry.n_type := N_LCSYM;
            current_stab.entry.n_desc := Unit_First_Line;
            current_stab.string := M3ID.ToText(cg.n) & ":S"
				   & Fmt.Int( StabEntryNo() ) & "=s"  
			           & Fmt.Int(cg.s);
            PrintStab(t);
          END; (* IF *)

        END; (* WITH *)
        current_global := current_global.next;

      END; (* WHILE *)

      current_stab.entry.n_type  := N_SO;
      current_stab.entry.n_value := t.text.table.n_bytes;
      current_stab.string := NIL;
      PrintStab(t);

      (* These lists are now useless *)
      first_global   := NIL;
      current_global := NIL;
      proc_table     := NIL;
      current_proc   := NIL;
    END;
  END EndUnit;

PROCEDURE DeclareSegment(t: T; n: Name) =
  VAR name := M3ID.ToText(n);
  BEGIN
    IF name = NIL THEN name := "" END;
    IF t.gen_debugging THEN
      current_unit_name:= Text.Sub(name, 2);
      module_first_line:= t.last_source_line;
    END;
  END DeclareSegment;

PROCEDURE ExportUnit(t: T; n: Name) =
  BEGIN
    IF t.gen_debugging THEN
      exported_interfaces_names[exported_interfaces]:= M3ID.ToText(n);
      INC(exported_interfaces);
    END;
  END ExportUnit;

PROCEDURE DeclareTypename (t: T;  type: TypeUID;  n: Name) =
  VAR fullname, my_id : TEXT;
  BEGIN
    IF t.gen_debugging THEN
      fullname:= current_unit_name & "." & M3ID.ToText(n);
      my_id := FmtUID(type);

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MN_" & my_id & TypeT() & fullname 
			    & GetFeildFmt() & ";";
      PrintStab(t);

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "Mn_" & "zzzzzz" & "_" & fullname
			    & TypeT() & my_id & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclareTypename;

PROCEDURE DeclareArray (t: T;  type, index, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MA_" & FmtUID(type) & "_" & Fmt.Int(s)
			    & TypeT() & FmtUID(index) & GetFeildFmt()
			    & FmtUID(elt) & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclareArray;

PROCEDURE DeclareOpenArray (t: T;  type, elt: TypeUID;  s: BitSize) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MB_" & FmtUID(type) & "_" & Fmt.Int(s)
			    & TypeT() & FmtUID(elt) & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclareOpenArray;

PROCEDURE DeclareEnum (t: T;  type: TypeUID; n_elts: INTEGER;  s: BitSize) =
  BEGIN
    IF t.gen_debugging THEN
      current_dbg_type_count1:= n_elts;

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MC_" & FmtUID(type) & "_" & Fmt.Int(s) & TypeT();
    END;
  END DeclareEnum;

PROCEDURE DeclareEnumElt (t: T;  n: Name) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.string:= current_stab.string & M3ID.ToText(n) & GetFeildFmt();
      DEC(current_dbg_type_count1);

      IF current_dbg_type_count1 = 0 THEN
        current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareEnumElt;

PROCEDURE DeclarePacked (t: T;  type: TypeUID;  s: BitSize;  base: TypeUID) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MD_" & FmtUID(type) & "_" & Fmt.Int(s) & TypeT()
			    & FmtUID(base) & GetFeildFmt() & ";";

      PrintStab(t);
    END;
  END DeclarePacked;

PROCEDURE DeclareRecord (t: T;  type: TypeUID;  s: BitSize;  n_fields: INTEGER) =
  BEGIN
    IF t.gen_debugging THEN
      current_dbg_type_count1:= n_fields;
      current_dbg_type_count2:= 0;

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MR_" & FmtUID(type) & "_" & Fmt.Int(s) & TypeT();

      IF current_dbg_type_count1 = 0 THEN
        current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareRecord;

PROCEDURE DeclareField (t: T;  n: Name;  o: BitOffset;  s: BitSize;  type: TypeUID) =
  BEGIN
    IF t.gen_debugging THEN
      DEC(current_dbg_type_count1);
      current_stab.string:= current_stab.string & FmtUID(type) & "_"
			    & Fmt.Int(o) & "_" & Fmt.Int(s) & "_" & M3ID.ToText(n)
			    & GetFeildFmt();
      IF current_dbg_type_count1 = 0 THEN
	current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareField;

PROCEDURE DeclareSet (t: T;  type, domain: TypeUID;  s: BitSize) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MS_" & FmtUID(type) & "_" & Fmt.Int(s) & TypeT()
			    & FmtUID(domain) & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclareSet;

PROCEDURE DeclareSubrange (t: T;  type, domain: TypeUID; READONLY min,max: 
			   Target.Int;  s: BitSize) =
  VAR int_min, int_max: INTEGER;
  BEGIN
    IF t.gen_debugging THEN
      EVAL TargetInt.ToInt(min, int_min);
      EVAL TargetInt.ToInt(max, int_max);

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MZ_" & FmtUID(type) & "_" & Fmt.Int(s) 
			    & "_" & Fmt.Int(int_min) & "_" & Fmt.Int(int_max)
			    & TypeT() & FmtUID(domain)
			    & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclareSubrange;

PROCEDURE DeclarePointer (t: T;  type, target: TypeUID;  brand: TEXT;  traced: BOOLEAN) =
  VAR traced_text:= "0";
      brand_text := "0";
  BEGIN
    IF t.gen_debugging THEN
      IF traced THEN traced_text:= "1" END;
      IF brand = NIL THEN  brand:= "";  ELSE  brand_text:= "1" END;

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MY_" & FmtUID(type) & "_" 
			    & Fmt.Int(Target.Address.size) & "_"
			    & traced_text & "_" & brand_text & "_" & brand 
			    & TypeT() & FmtUID(target) & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclarePointer;

PROCEDURE DeclareIndirect (t: T;  type, target: TypeUID) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MX_" & FmtUID(type) & "_"
			    & Fmt.Int(Target.Address.size) & TypeT() 
			    & FmtUID(target) & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END DeclareIndirect;

PROCEDURE DeclareProctype (t: T;  type: TypeUID;  n_formals: INTEGER;
                           result: TypeUID;  n_raises: INTEGER) =
  VAR A_or_L : TEXT;
  BEGIN
    IF t.gen_debugging THEN
      current_dbg_type_count1:= n_formals;
      current_dbg_type_count2:= MAX(0, n_raises);
      IF n_raises < 0 THEN A_or_L:= "A"; ELSE A_or_L:= "L"; END;
      A_or_L:= A_or_L & Fmt.Int( current_dbg_type_count2 );

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MP_" & FmtUID(type) & "_" 
			    & Fmt.Int(Target.Address.size) & "_"
 			    & A_or_L & TypeT() & FmtUID(result)
			    & GetFeildFmt();

      IF current_dbg_type_count1 = 0 AND current_dbg_type_count2 = 0 THEN
        current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareProctype;

PROCEDURE DeclareFormal (t: T;  n: Name;  type: TypeUID) =
  BEGIN
    IF t.gen_debugging THEN
      DEC(current_dbg_type_count1);
      current_stab.string:= current_stab.string & FmtUID(type) & "_" & M3ID.ToText(n)
			    & GetFeildFmt();

      IF current_dbg_type_count1 = 0 AND current_dbg_type_count2 = 0 THEN
	current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareFormal;

PROCEDURE DeclareRaises (t: T;  n: Name) =
  BEGIN
    IF t.gen_debugging THEN
      DEC(current_dbg_type_count2);
      current_stab.string:= current_stab.string & M3ID.ToText(n) & GetFeildFmt();

      IF current_dbg_type_count1 = 0 AND current_dbg_type_count2 = 0 THEN
        current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareRaises;

PROCEDURE DeclareObject (t: T;  type, super: TypeUID;  brand: TEXT;
                         traced: BOOLEAN;  n_fields, n_methods: INTEGER;
                         <* UNUSED *> field_size: BitSize) =
  VAR traced_text:= "0";
      brand_text := "0";
  BEGIN
    IF t.gen_debugging THEN
      current_dbg_type_count1:= n_fields;
      current_dbg_type_count2:= n_methods;
      current_dbg_type_count3:= 0;

      IF traced THEN traced_text:= "1" END;
      IF brand = NIL THEN  brand:= "";  ELSE  brand_text:= "1" END;

      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MO_" & FmtUID(type) & "_" & Fmt.Int(Target.Address.size) 
			    & "_" & Fmt.Int(n_fields) & "_" & traced_text & "_"
			    & brand_text & "_" & brand & TypeT() & FmtUID(super) 
			    & GetFeildFmt();

      IF current_dbg_type_count1 = 0 AND current_dbg_type_count2 = 0 THEN
        current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareObject;

PROCEDURE DeclareMethod (t: T;  n: Name;  signature: TypeUID) =
  BEGIN
    IF t.gen_debugging THEN
      DEC(current_dbg_type_count2);
      current_stab.string:= current_stab.string & FmtUID(signature) & "_"
			    & Fmt.Int(current_dbg_type_count3 * Target.Address.size) 
			    & "_" & Fmt.Int(Target.Address.size) & "_" 
			    & M3ID.ToText(n) & GetFeildFmt();
      INC(current_dbg_type_count3);
      IF current_dbg_type_count1 = 0 AND current_dbg_type_count2 = 0 THEN
        current_stab.string:= current_stab.string & ";";
	PrintStab(t);
      END;
    END;
  END DeclareMethod;

PROCEDURE DeclareOpaque (t: T;  type, super: TypeUID) =
  BEGIN
    (* we do not care for that, only the revelation is interesting *)
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL type; EVAL super;
  END DeclareOpaque;

PROCEDURE RevealOpaque (t: T;  lhs, rhs: TypeUID) =
  BEGIN
    IF t.gen_debugging THEN
      current_stab.entry.n_type := N_LSYM;
      current_stab.string:= "MQ_" & FmtUID(lhs) & "_" & Fmt.Int(Target.Address.size) 
			    & TypeT() & FmtUID(rhs) & GetFeildFmt() & ";";
      PrintStab(t);
    END;
  END RevealOpaque;

PROCEDURE DeclareException (t: T;  name: Name;  arg_type: TypeUID;
                            raise_proc: BOOLEAN) =
  BEGIN
    (* nothing yet *)
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL name; EVAL arg_type; EVAL raise_proc;
  END DeclareException;

PROCEDURE DeclareGlobal (t: T;  n: Name;  s: ByteSize;  bss: BOOLEAN) =
  BEGIN
    IF t.gen_debugging THEN

      IF first_global = NIL THEN
        first_global        := NEW(Global);
        first_global.is_bss := bss;
        first_global.n      := n;
        first_global.s      := s;
        current_global      := first_global;
      ELSE
        current_global.next   := NEW(Global);
        current_global        := current_global.next;
        current_global.is_bss := bss;
        current_global.n      := n;
        current_global.s      := s;
      END;

      IF NOT bss THEN
        is_generated_after_GLOBALVAR:= TRUE;
        t.last_source_line := t.end_source_line;
      END;
    END;
  END DeclareGlobal;

PROCEDURE DeclareConstant  (t: T;  sym: INTEGER;  s: ByteSize;  m3t: TypeUID) =
  BEGIN
    (* never called *)
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym; EVAL s; EVAL m3t;
  END DeclareConstant;

PROCEDURE DeclareLocal (t: T;  n: Name;  s: ByteSize;  frame: ByteOffset;  
			m3t: TypeUID;    tp: Type) =

  VAR new_local:= NEW(Local, n:= n, s:= s, frame:= frame, m3t:= m3t, t:= tp,
		      line:= t.last_source_line);
  BEGIN
    IF t.gen_debugging THEN

      IF current_proc.bloc_head = NIL THEN  (* The first block is not created *)

        WITH cp = current_proc DO

          IF cp.local_head = NIL THEN
            cp.local_head:= new_local;
	    cp.local_tail:= cp.local_head;
          ELSE
            cp.local_tail.next:= new_local;
	    cp.local_tail.next.prev:= cp.local_tail;
	    cp.local_tail:= cp.local_tail.next;
          END;

        END; (* WITH *)

      ELSE

        WITH bt = current_proc.bloc_tail DO 

          IF bt.local_head = NIL THEN
            bt.local_head:= new_local;
	    bt.local_tail:= bt.local_head;
          ELSE
            bt.local_tail.next:= new_local;
	    bt.local_tail.next.prev:= bt.local_tail;
	    bt.local_tail:= bt.local_tail.next;
          END;

        END; (* WITH *) 
      END; (* IF *)

    END;
  END DeclareLocal;

PROCEDURE DeclareParam (t: T;  n: Name;  s: ByteSize;  frame: ByteOffset;  
			m3t: TypeUID;  tp: Type) =
  BEGIN
    IF t.gen_debugging AND m3t # 0 AND current_proc # NIL THEN

      IF current_proc.param_head = NIL THEN
        current_proc.param_head:= NEW(Param);
	current_proc.param_tail:= current_proc.param_head;
      ELSE
        current_proc.param_tail.next:= NEW(Param);
	current_proc.param_tail:= current_proc.param_tail.next;
      END;

      WITH tail = current_proc.param_tail DO
	tail.n     := n;
	tail.s     := s;
        tail.line  := t.last_source_line;
	tail.frame := frame;
        tail.m3t   := m3t;
        tail.t     := tp;
      END;
    END;
  END DeclareParam;

PROCEDURE DeclareProcedure (t: T;  n, parent_n:  Name;  return_type: Type;  
			    exported: BOOLEAN;  sym: INTEGER) =
  BEGIN
    IF t.gen_debugging THEN

      current_proc:= NEW(DebugProcTypes.T);
      EVAL proc_table.put(sym, current_proc);

      IF Text.Equal( M3ID.ToText(n), "_INITM_" & current_unit_name ) THEN
	is_a_INITM_segment:= TRUE;
      END;

      WITH p = current_proc.proc_param DO
        p.n           := n;
        p.parent_n    := parent_n;
        p.return_type := return_type;
        p.exported    := exported;
      END;
    END;
  END DeclareProcedure;

PROCEDURE DeclareCaseJump (t: T; n_labels, indexreg: INTEGER; pic: BOOLEAN) =
  BEGIN
    IF t.gen_debugging THEN
      IF current_proc.case_head = NIL THEN
        current_proc.case_head := NEW(Case);
        current_proc.case_tail := current_proc.case_head;
      ELSE
        current_proc.case_tail.next := NEW(Case);
        current_proc.case_tail.next.prev := current_proc.case_tail;
        current_proc.case_tail := current_proc.case_tail.next;
      END;

      WITH tail = current_proc.case_tail DO
        tail.line := t.last_source_line;

        IF pic THEN
          tail.label_loc := t.data.table.n_bytes;
        ELSE
          tail.label_loc := t.rodata.table.n_bytes;
        END;

        tail.label_n := n_labels;
        tail.indexreg := indexreg;
      END;
    END;
  END DeclareCaseJump;

PROCEDURE BeginProcedure (t: T;  sym, o: INTEGER) =
  BEGIN
    <*ASSERT NOT t.in_procedure*>

    t.in_procedure := TRUE;

    current_proc_offset := o;
    current_proc_line   := t.last_source_line;

    t.symtab.symbol[sym].size:= t.text.table.n_bytes;

    IF t.gen_debugging THEN

      EVAL proc_table.get(sym, current_proc);

      IF Text.Equal( M3ID.ToText(current_proc.proc_param.n), 
		                 "_INITM_" & current_unit_name ) THEN
	main_first_line := t.last_source_line;
      END;

      AddSourceLine(t.last_source_line, t.text.table.n_bytes);
    END;
  END BeginProcedure;

PROCEDURE EndProcedure (t: T;  sym: INTEGER) =
  BEGIN
    <*ASSERT t.in_procedure*>

    IF t.gen_debugging THEN
      AddSourceLine(t.last_source_line, t.text.table.n_bytes);
    END;

    t.in_procedure := FALSE;

    WITH sz = t.symtab.symbol[sym].size DO
      sz := t.text.table.n_bytes - sz;
    END;

    IF t.gen_debugging THEN
      PrintProcStabs (t);
    END;

  END EndProcedure;

PROCEDURE PrintProcStabs (t: T) =
  VAR
    declared_variables_printed := FALSE;
    exported_text : TEXT;
  BEGIN
    <* ASSERT t.gen_debugging *>

    WITH cp = current_proc DO

      (* The stab for the procedure itself *)

      WITH p = cp.proc_param DO
        IF p.exported THEN
	    exported_text:= ":F";
        ELSE
	    exported_text:= ":f";
        END;

        current_stab.entry.n_type  := N_FUN;
        current_stab.entry.n_value := current_proc_offset;
        current_stab.entry.n_desc  := current_proc_line;
        current_stab.string := M3ID.ToText(p.n) & exported_text
			       & GetStabType(p.return_type);

        IF p.parent_n # M3ID.NoID THEN
          current_stab.string:= current_stab.string &  ","
			        & M3ID.ToText(p.n)
			        & "," & M3ID.ToText(p.parent_n);
        END;

        PrintStab(t);
      END; (* WITH p *)

      (* Put the parameters of the procedure in order of their declaration *)

      cp.param_tail:= cp.param_head;

      WHILE cp.param_tail # NIL DO

        WITH tail = cp.param_tail DO

	  current_stab.entry.n_type := N_PSYM;

	  IF is_generated_after_GLOBALVAR OR tail.n = M3ID.NoID THEN
	    current_stab.entry.n_desc := tail.line;
          ELSIF is_a_INITM_segment THEN
	    current_stab.entry.n_desc := Unit_First_Line;
          ELSE
	    current_stab.entry.n_desc := module_first_line;
          END;

	  current_stab.entry.n_value := tail.frame;

      	  IF tail.n = M3ID.NoID THEN
	    current_stab.string := StackVarName();
	  ELSE
	    current_stab.string := "M3_" & FmtUID(tail.m3t) & "_" 
				   & M3ID.ToText(tail.n);
	  END;
          current_stab.string := current_stab.string & ":p"
			     	 & GetStabType(tail.t, tail.s);

          PrintStab(t);
        END (* WITH tail *);

	cp.param_tail:= cp.param_tail.next;

      END; (* WHILE parameters *)

      (* Put the "included file" (.m3 or .i3) *)

      IF NOT is_included_printed THEN
	current_stab:= included_filename_stab;
        PrintStab(t);
	is_included_printed:= TRUE;
      END;

      (* Put the line numbers *)

      cp.line_tail := cp.line_head;

      WHILE cp.line_tail # NIL DO          
	current_stab.entry.n_type  := N_SLINE;
	current_stab.entry.n_desc  := cp.line_tail.line;
	current_stab.entry.n_value := cp.line_tail.addr;
        current_stab.string        := NIL;

        PrintStab(t);

        cp.line_tail := cp.line_tail.next;
      END (* WHILE lines *);

      (* Put the case_jump stabs *)

      WHILE cp.case_tail # NIL DO
	current_stab.entry.n_type  := N_STSYM;
	current_stab.entry.n_desc  := cp.case_tail.line;
	current_stab.entry.n_value := cp.case_tail.label_loc;
        current_stab.string        := StackVarName() & ":V" 
				      & Fmt.Int( StabEntryNo() ) & "=ar"
				      & Fmt.Int( cp.case_tail.indexreg ) & ";0;"
				      & Fmt.Int( cp.case_tail.label_n ) & ";"
                                      & GetStabType(Type.Addr);
        PrintStab(t);

        cp.case_tail := cp.case_tail.prev;
      END (* WHILE case_jump *);

      (* Put stabs that delimits blocks and the local variables included *)

      cp.bloc_tail:= cp.bloc_head;

      WHILE cp.bloc_tail # NIL DO

        WITH bt = cp.bloc_tail DO

          IF bt.begin_block THEN

            (* Local variables of a bloc *)

            WHILE bt.local_tail # NIL DO

              WITH lt = bt.local_tail DO
	        current_stab.entry.n_type  := N_LSYM;
	        current_stab.entry.n_desc  := lt.line;
	        current_stab.entry.n_value := lt.frame;

      	        IF lt.n = M3ID.NoID THEN
	          current_stab.string := StackVarName();
	        ELSE
	          current_stab.string := "M3_" & FmtUID(lt.m3t) & "_"
				         & M3ID.ToText(lt.n);
	        END;
                current_stab.string := current_stab.string & ":"
			     	       & GetStabType(lt.t, lt.s);

                PrintStab(t);
              END (* WITH lt *);

	      bt.local_tail:= bt.local_tail.prev;

            END; (* WHILE bt.local_tail *)

	    IF NOT declared_variables_printed THEN

      	      (* Some local variables are declared before a begin_block *)

              WHILE cp.local_tail # NIL DO

                WITH tail = cp.local_tail DO
	          current_stab.entry.n_type  := N_LSYM;

                  IF is_a_INITM_segment THEN
	            current_stab.entry.n_desc := Unit_First_Line;
                  ELSE
	            current_stab.entry.n_desc := module_first_line;
                  END;

	          current_stab.entry.n_value := tail.frame;

      	          IF tail.n = M3ID.NoID THEN
	            current_stab.string := StackVarName();
	          ELSE
	            current_stab.string := "M3_" & FmtUID(tail.m3t) & "_"
				     & M3ID.ToText(tail.n);
	          END;
	          current_stab.string := current_stab.string & ":"
			     	         & GetStabType(tail.t, tail.s);

                  PrintStab(t);
                END; (* WITH tail *)

	        cp.local_tail:= cp.local_tail.prev;

              END; (* WHILE cp.local_tail *)

	    END (* NOT declared_variables_printed *);

            current_stab.entry.n_type  := N_LBRAC;
            current_stab.entry.n_value := bt.value;
            current_stab.string := NIL;
            PrintStab(t);
          ELSE
            current_stab.entry.n_type  := N_RBRAC;
            current_stab.entry.n_value := bt.value;
            current_stab.string := NIL;
            PrintStab(t);

          END;  (* IF bt.begin_block *)

        END; (* WITH bt *)

        cp.bloc_tail:= cp.bloc_tail.next;
      END; (* WHILE cp.bloc_tail *)

    END; (* WITH cp *) 
  END PrintProcStabs;

PROCEDURE BeginBlock (t: T) =
  BEGIN
    IF t.gen_debugging THEN
      IF current_proc.bloc_head = NIL THEN
        current_proc.bloc_head:= NEW(Bloc);
	current_proc.bloc_tail:= current_proc.bloc_head;
      ELSE
        current_proc.bloc_tail.next:= NEW(Bloc);
	current_proc.bloc_tail:= current_proc.bloc_tail.next;
      END;

      WITH tail = current_proc.bloc_tail DO
        tail.value       := Cursor(t, Seg.Text) - current_proc_offset;
        tail.begin_block := TRUE;
      END;
    END;
  END BeginBlock;

PROCEDURE EndBlock (t: T; o:= 0) =
  BEGIN
    IF t.gen_debugging THEN

      IF current_proc.bloc_head = NIL THEN
        current_proc.bloc_head:= NEW(Bloc);
	current_proc.bloc_tail:= current_proc.bloc_head;
      ELSE
        current_proc.bloc_tail.next:= NEW(Bloc);
	current_proc.bloc_tail:= current_proc.bloc_tail.next;
      END;

      WITH tail = current_proc.bloc_tail DO
        tail.value := Cursor(t, Seg.Text) - current_proc_offset + o;
      END;
    END;
  END EndBlock;

PROCEDURE NoteProcedureOrigin (t: T;  sym: INTEGER) =
  BEGIN
    (* never called *)
    IF NOT t.gen_debugging THEN RETURN END;
    EVAL sym;
  END NoteProcedureOrigin;

(*---------------------------------------------------------------- output ---*)

PROCEDURE Dump (t: T;  wr: Wr.T) =
  VAR
    sections_used := 0;
    file_offset   := ELFHeaderSise;

  BEGIN
    t.out.wr  := wr;

    IF t.gen_debugging THEN
      AddStabEntry(t, AddString(t.stabstr, t.filename), N_UNDF, 0,
		   t.stab_ndesc, t.stabstr.table.n_bytes, TRUE);
    END;

    ReorderSymbols (t);

    SetSection (t, t.index0.header, NIL, sections_used);
    SetSection (t, t.text.header, ".text", sections_used);
    SetSection (t, t.rel_text.header, ".rel.text", sections_used);
    SetSection (t, t.data.header, ".data", sections_used);
    SetSection (t, t.rel_data.header, ".rel.data", sections_used);
    SetSection (t, t.bss.header, ".bss", sections_used);
    SetSection (t, t.rodata.header, ".rodata", sections_used);
    SetSection (t, t.rel_rodata.header, ".rel.rodata", sections_used);
    IF t.gen_debugging THEN
      SetSection (t, t.stab.header, ".stab", sections_used);
      SetSection (t, t.rel_stab.header, ".rel.stab", sections_used);
      SetSection (t, t.stabstr.header, ".stabstr", sections_used);
    END;
    SetSection (t, t.shstrtab.header, ".shstrtab", sections_used);
    SetSection (t, t.symtab.header, ".symtab", sections_used);
    SetSection (t, t.strtab.header, ".strtab", sections_used);

    (* compute the file and address space layouts *)

    LayoutSection (t.text.header, t.text.table, t.text.data, file_offset);
    LayoutSection (t.data.header, t.data.table, t.data.data, file_offset);
    LayoutSection (t.bss.header, t.bss.table, t.bss.data, file_offset);
    LayoutSection (t.rodata.header, t.rodata.table, t.rodata.data,
		   file_offset);

    IF t.gen_debugging THEN
      LayoutSection (t.stab.header, t.stab.table, 
		     t.stab.entry, file_offset);
      LayoutSection (t.stabstr.header, t.stabstr.table, 
		     t.stabstr.string, file_offset);
    END;

    LayoutSection (t.shstrtab.header, t.shstrtab.table,
		   t.shstrtab.string, file_offset);
    LayoutSection (t.symtab.header, t.symtab.table,
		   t.symtab.symbol, file_offset);
    LayoutSection (t.strtab.header, t.strtab.table,
		   t.strtab.string, file_offset);

    LayoutSection (t.rel_text.header, t.rel_text.table,
		   t.rel_text.relocs, file_offset);
    LayoutSection (t.rel_data.header, t.rel_data.table,
		   t.rel_data.relocs, file_offset);
    LayoutSection (t.rel_rodata.header, t.rel_rodata.table,
		   t.rel_rodata.relocs, file_offset);

    IF t.gen_debugging THEN
      LayoutSection (t.rel_stab.header, t.rel_stab.table,
		     t.rel_stab.relocs, file_offset);
    END;

    WriteFileHeader(t, file_offset, sections_used);

    WriteSectionInfo (t, t.index0);
    WriteSectionInfo (t, t.text);
    WriteSectionInfo (t, t.data);
    WriteSectionInfo (t, t.bss);
    WriteSectionInfo (t, t.rodata);

    IF t.gen_debugging THEN
      WriteSectionStab (t, t.stab);
      WriteSectionString (t, t.stabstr);
    END;

    WriteSectionString (t, t.shstrtab);
    WriteSectionSymbol (t, t.symtab);
    WriteSectionString (t, t.strtab);

    WriteSectionRelocs (t, t.rel_text);
    WriteSectionRelocs (t, t.rel_data);
    WriteSectionRelocs (t, t.rel_rodata);

    IF t.gen_debugging THEN
      WriteSectionRelocs (t, t.rel_stab);
    END;

    WriteSectionHeader (t, t.index0.header, t.index0.table);
    WriteSectionHeader (t, t.text.header, t.text.table);
    WriteSectionHeader (t, t.rel_text.header, t.rel_text.table);
    WriteSectionHeader (t, t.data.header, t.data.table);
    WriteSectionHeader (t, t.rel_data.header, t.rel_data.table);
    WriteSectionHeader (t, t.bss.header, t.bss.table);
    WriteSectionHeader (t, t.rodata.header, t.rodata.table);
    WriteSectionHeader (t, t.rel_rodata.header, t.rel_rodata.table);
    IF t.gen_debugging THEN
      WriteSectionHeader (t, t.stab.header, t.stab.table);
      WriteSectionHeader (t, t.rel_stab.header, t.rel_stab.table);
      WriteSectionHeader (t, t.stabstr.header, t.stabstr.table);
    END;

    WriteSectionHeader (t, t.shstrtab.header, t.shstrtab.table);
    WriteSectionHeader (t, t.symtab.header, t.symtab.table);
    WriteSectionHeader (t, t.strtab.header, t.strtab.table);

    Flush (t);
    t.out.wr := NIL;  (* give the collector a chance... *)
  END Dump;


PROCEDURE ReorderSymbols (t: T) =
  VAR next_index       := 0;
      first_stb_global := NEW(REF INTEGER);
  BEGIN
    WITH st = t.symtab DO
      st.remap := NEW (Ints, t.symtab.table.cnt);
      FOR i:= 0 TO st.table.cnt-1 DO
        st.remap[i] := -1
      END;
    END;

    (* set the "index0" symbol *)
    SetSym (t, 0, next_index);

    (* set the source file name symbol *)
    IF t.filesym # 0 THEN
      SetSym (t, t.filesym, next_index);
    END;

    (* set symbols with STB_LOCAL binding other then STT_SECTION *)
    FOR i := 0 TO t.symtab.table.cnt - 1 DO
      WITH sym = t.symtab.symbol[i] DO
        IF (sym.bind = STB_LOCAL) AND (sym.used) AND (sym.index < 0) AND
           (sym.type # STT_SECTION) THEN
          SetSym (t, i, next_index);
        END;
      END;
    END;

    (* set symbols with STT_SECTION type *)
    FOR i := t.symtab.table.cnt - 1 TO 1 BY -1 DO
     WITH sym = t.symtab.symbol[i] DO
        IF (sym.bind = STB_LOCAL) AND (sym.type = STT_FUNC) AND 
	   (sym.used) AND (sym.index < 0) THEN
          SetSym (t, i, next_index);
        END;
      END;
    END;

    (* set the section number symbols *)
    FOR i := 1 TO t.symtab.table.cnt - 1 DO
      WITH sym = t.symtab.symbol[i] DO
        IF (sym.bind = STB_LOCAL) AND (sym.type = STT_SECTION) 
	   AND (sym.index < 0) THEN
            SetSym (t, i, next_index);
        END;
      END;
    END;

    first_stb_global^    := next_index;
    t.symtab.header.info := first_stb_global;
		      (* index of the first global symbol *)

    (* set the symbol with STB_GLOBAL or STB_WEAK binding *)
    FOR i := 0 TO t.symtab.table.cnt - 1 DO
      WITH sym = t.symtab.symbol[i] DO
        IF (sym.bind = STB_GLOBAL) OR (sym.bind = STB_WEAK) THEN
          IF (sym.used) AND (sym.index < 0) THEN
            SetSym (t, i, next_index);
          END;
        END;
      END;
    END;

    t.symtab.table.n_bytes := next_index * SymTableEntrySize;
  END ReorderSymbols;

PROCEDURE SetSym (t: T;  old: INTEGER;  VAR next_index: INTEGER) =
  VAR name : TEXT;
  BEGIN
    WITH sym = t.symtab.symbol[old] DO
      sym.index := next_index;

      IF sym.id # M3ID.NoID THEN
        name := M3ID.ToText (sym.id);
        sym.name  := AddString(t.strtab, name);
      END;

      t.symtab.remap[next_index] := old;
      INC (next_index);
   END;
  END SetSym;

PROCEDURE SetSection (t: T;  VAR sh: SectionHeader;  name: TEXT;
		      VAR sections_used: INTEGER) =
  BEGIN
    IF sh.used THEN
      sh.name   := AddString (t.shstrtab, name);
      sh.index^ := sections_used;
      INC (sections_used);
    END;
  END SetSection;


PROCEDURE LayoutSection (VAR header: SectionHeader;  VAR table: Chunk;
			 data:       REFANY;         VAR offs : INTEGER) =
  VAR
    pad : INTEGER;
  BEGIN
    IF (table.n_bytes > 0) AND (data # NIL) THEN
      WITH a = header.align DO
        (* section alignment padding *)
        pad := ( (offs + a - 1) DIV a * a ) - offs; 
        header.pad := pad;
        INC (offs, pad);
        header.offset := offs;
        INC (offs, table.n_bytes);
      END;

    ELSE

      header.offset := offs;
    END;
  END LayoutSection;

PROCEDURE WriteFileHeader (t: T;  offset : INTEGER; sections_used: INTEGER) =
  BEGIN
    OutIntIn1Byte (t, 16_7f);
    OutText (t, "ELF");
    OutIntIn1Byte (t, 1);    (* ELFCLASS32 *)
    OutIntIn1Byte (t, 1);    (* ELFDATALSB *)
    OutIntIn1Byte (t, 1);    (* EV_CURRENT = 1 *)

    FOR i := 7 TO 15 DO
      OutIntIn1Byte (t, 0);  (* pad of e_ident *)
    END;

    OutIntIn2Bytes (t, 1);   (* relocatable file *)
    OutIntIn2Bytes (t, 3);   (* Intel 80386      *)
    OutIntIn4Bytes (t, 1);   (* EV_CURRENT = 1   *)
    OutIntIn4Bytes (t, 0);   (* no associated entry point *)
    OutIntIn4Bytes (t, 0);   (* no program header *)

    OutIntIn4Bytes (t, offset);            (* sect. header table's file offset *)
    OutIntIn4Bytes (t, 0);	 	   (* no flag for 80386   *)
    OutIntIn2Bytes (t, ELFHeaderSise);	   (* ELF header size     *)
    OutIntIn2Bytes (t, 0);		   (* no programme header *)
    OutIntIn2Bytes (t, 0);		   (* no programme header *)
    OutIntIn2Bytes (t, SectionHeaderSize); (* section header size *)
    OutIntIn2Bytes (t, sections_used);     (* number of section headers *)
    OutIntIn2Bytes (t, t.shstrtab.header.index^);  
					   (* sect. string table's head. index *)
  END WriteFileHeader;

PROCEDURE WriteSectionHeader (t: T;  VAR sh: SectionHeader;  ch: Chunk) =
  BEGIN
    IF sh.used THEN
      OutIntIn4Bytes(t, sh.name);
      OutIntIn4Bytes(t, sh.type);
      OutIntIn4Bytes(t, sh.flags);
      OutIntIn4Bytes(t, 0);		(* no memory image address *)
      OutIntIn4Bytes(t, sh.offset);
      OutIntIn4Bytes(t, ch.n_bytes);
      OutIntIn4Bytes(t, sh.link^);
      OutIntIn4Bytes(t, sh.info^);
      OutIntIn4Bytes(t, sh.align);
      OutIntIn4Bytes(t, ch.entry_size);
    END;
  END WriteSectionHeader;

PROCEDURE WriteSectionInfo (t: T;  VAR s: SectionInfo) =
  BEGIN
    IF s.header.used THEN
      FOR i := 1 TO s.header.pad DO
        OutC (t, '\000');
      END;

      IF (s.data # NIL) THEN
        FOR i := 0 TO s.table.n_bytes-1 DO
          OutC (t, s.data[i]);
        END;
      END;
    END;
  END WriteSectionInfo;

PROCEDURE WriteSectionSymbol (t: T;  VAR s: SectionSymbol) =
  BEGIN
    <* ASSERT s.header.used = TRUE *>
    FOR i := 1 TO s.header.pad DO
        OutC (t, '\000');
    END;

    FOR i := 0 TO s.table.cnt-1 DO
      IF s.remap[i] >= 0 THEN
        WriteSym (t, s.symbol [ s.remap [i] ]);
      END;
    END;
  END WriteSectionSymbol;

PROCEDURE WriteSym (t: T;  READONLY sym: Symbol) =
  BEGIN
    OutIntIn4Bytes (t, sym.name);
    OutIntIn4Bytes (t, sym.value);
    OutIntIn4Bytes (t, sym.size);
    Out2IntIn1Byte (t, sym.bind, sym.type);
    OutIntIn1Byte  (t, 0);		 (* member with no defined meaning *)
    OutIntIn2Bytes (t, sym.shindex^);
  END WriteSym;

PROCEDURE WriteSectionString (t: T;  VAR s: SectionString) =
  BEGIN
    <* ASSERT s.header.used = TRUE *>
    FOR i := 1 TO s.header.pad DO
        OutC (t, '\000');
    END;

    IF (s.string # NIL) THEN
      OutText(t, s.string[0]);		 (* first entry is \000 *)
      FOR i := 1 TO s.table.cnt-1 DO
 	OutText(t, s.string[i]);
        OutC   (t, '\000');		 (* strings are separate by \000 *)
      END;
    END;
  END WriteSectionString;

PROCEDURE WriteSectionStab (t: T;  VAR s: SectionStab) =
  BEGIN
    FOR i := 1 TO s.header.pad DO
        OutC (t, '\000');
    END;
    FOR i := 0 TO s.table.cnt-1 DO

      WITH entry = s.entry[i] DO
        OutIntIn4Bytes (t, entry.n_strx);
        OutIntIn1Byte  (t, entry.n_type);
        OutIntIn1Byte  (t, entry.n_other);

        IF entry.n_desc = Unit_First_Line THEN
          OutIntIn2Bytes (t, main_first_line);
	ELSE
          OutIntIn2Bytes (t, entry.n_desc);
        END;

        OutIntIn4Bytes (t, entry.n_value);

      END;
    END;
  END WriteSectionStab;

PROCEDURE WriteSectionRelocs (t: T;  VAR s: SectionRelocs) =
  BEGIN
    IF s.header.used THEN
      FOR i := 1 TO s.header.pad DO
        OutC (t, '\000');
      END;

      FOR i := 0 TO s.table.cnt-1 DO
        WITH r = s.relocs[i] DO
          WITH target_index = t.symtab.symbol[r.target_sym].index DO
            OutIntIn4Bytes (t, r.src_offset);
	    Out2IntIn4Bytes (t, target_index, r.kind);
          END
        END;
      END;
    END;
  END WriteSectionRelocs;

(*--------------------------------------------- low-level output routines ---*)

PROCEDURE OutIntIn4Bytes (t: T;  i: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));
  END OutIntIn4Bytes;

PROCEDURE OutIntIn2Bytes (t: T;  i: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));
  END OutIntIn2Bytes;

PROCEDURE OutIntIn1Byte (t: T;  i: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));
  END OutIntIn1Byte;

PROCEDURE Out2IntIn1Byte (t: T;  i, j: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (i*16 + j, 16_ff), CHAR));
  END Out2IntIn1Byte;

PROCEDURE Out2IntIn4Bytes (t: T;  i, j: INTEGER) =
  BEGIN
    OutC (t, VAL (Word.And (j, 16_ff), CHAR));
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));  i := Word.RightShift (i, 8);
    OutC (t, VAL (Word.And (i, 16_ff), CHAR));
  END Out2IntIn4Bytes;

PROCEDURE OutText (t: T;  txt: TEXT) =
  VAR len := Text.Length (txt);
  BEGIN
    FOR i := 0 TO len-1 DO OutC (t, Text.GetChar (txt, i)); END;
  END OutText;

PROCEDURE OutC (t: T;  c: CHAR) =
  BEGIN
    IF (t.out.len >= NUMBER (t.out.buf)) THEN Flush (t); END;
    t.out.buf [t.out.len] := c;
    INC (t.out.len);
  END OutC;

PROCEDURE Flush (t: T) =
  <*FATAL ANY*>
  BEGIN
    Wr.PutString (t.out.wr, SUBARRAY (t.out.buf, 0, t.out.len));
    t.out.len := 0;
  END Flush;

BEGIN
END ELFObjFile.
