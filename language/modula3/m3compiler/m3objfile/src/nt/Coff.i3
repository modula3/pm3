(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Sep  7 12:03:23 PDT 1994 by kalsow   *)

(* This interface defines the structures found in COFF object files. *)

INTERFACE Coff;

TYPE
  char    = BITS  8 FOR [0..255];
  u_short = BITS 16 FOR [0..16_ffff];
  short   = BITS 16 FOR [-16_8000..16_7fff];
  long    = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];

TYPE
  FileHeader = RECORD
    f_magic  : u_short;  (* magic number *)
    f_nscns  : u_short;  (* number of sections *)
    f_timdat : long;     (* time & date stamp *)
    f_symptr : long;     (* file pointer to symtab *)
    f_nsyms  : long;     (* number of symtab entries *)
    f_opthdr : u_short;  (* sizeof(optional hdr) *)
    f_flags  : u_short;  (* flags *)
  END;

CONST (* bit field values for f_flags field *)
  F_RELFLG =  8_0001;  (* => file contains no relocation *)
  F_EXEC   =  8_0002;  (* => file is executable *)
  F_LNNO   =  8_0004;  (* => line numbers have been stripped from file *)
  L_SYMS   =  8_0010;  (* => local symbols have been stripped from file *)

TYPE
  Name = ARRAY [0..7] OF CHAR;  (* an 8-character name *)
TYPE
  SectionHeader = RECORD
    s_name    : Name;    (* section name *)
    s_paddr   : long;    (* physical address *)
    s_vaddr   : long;    (* virtual address *)
    s_size    : long;    (* section size *)
    s_scnptr  : long;    (* file ptr to raw data for section *)
    s_relptr  : long;    (* file ptr to relocation info for section *)
    s_lnnoptr : long;    (* file ptr to line numbers *)
    s_nreloc  : u_short; (* number of relocation entries *)
    s_nlnno   : u_short; (* number of line number entries *)
    s_flags   : long;    (* type and content flags *)
  END;

TYPE
  Relocation = RECORD
    r_vaddr  : long;     (* address of reference *)
    r_symndx : long;     (* index into symbol table *)
    r_type   : u_short;  (* relocation type *)
  END;

TYPE
  Symbol = RECORD
    n_name   : Name;       (* n_name[0..3]=0 => SymbolNamePtr *)
    n_value  : long;       (* value of symbol *)
    n_scnum  : short;      (* section number *)
    n_type   : u_short;    (* type and derived type *)
    n_sclass : char;       (* storage class *)
    n_numaux : char;       (* number of aux. entries *)
  END;
 
  SymbolNamePtr = RECORD   (* the "other" interpretation of n_name *)
    n_zeros  : long;       (* must be zero. *)
    n_offset : long;       (* offset into the string table *)
  END;

END Coff.
