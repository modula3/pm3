(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Dec 20 13:52:29 PST 1994 by kalsow     *)
(*      modified on Tue Jun  1 13:27:55 PDT 1993 by muller     *)

(*  Modula-3 code generator interface

This interface in conjunction with M3CG_Ops provides an intermediate
language for compiler front ends to generate Modula-3 procedures
and object modules.   Its primary client is the Modula-3 compiler.
The bulk of this interface was derived (i.e. stolen) from "M2CG" by
John Ellis and John DeTreville.

The intermediate language (this interface) allows for different actual
code generators, providing two degrees of freedom:

    - multiple target architectures
    - code generators optimized for different goals (code quality
      versus fast turn-around)

The interface to the intermediate language is procedural: clients call
methods to construct intermediate-language programs.  Thus a
fast-turn-around code generator can compile the intermediate language
on the fly without much consing, while a high-quality slower code
generator may construct an entire intermediate-language module before
compiling it.

This interface defines a single object type, M3CG.T.  The operations
available on an M3CG.T are defined in M3CG_Ops.
*)

INTERFACE M3CG;

IMPORT Target, M3ID;

TYPE T <: ROOT; (* a code generator *)

TYPE
  Type = Target.CGType;
  MType = [ Type.Addr .. Type.Word_D ];  (* "memory" types *)
  IType = [ Type.Word .. Type.Int ];     (* "integer" types *)
  RType = [ Type.Reel .. Type.XReel ];   (* "real" types *)
  AType = [ Type.Word .. Type.XReel ];   (* "arithmetic" types *)
  ZType = [ Type.Addr .. Type.XReel ];   (* "operator" types *)
  (* The code generator manipulates scalar values of the types
     listed above.  The notation "sN.B" denotes the stack value
     that is "N" elements from the top of stack and has the type
     whose first letter is "B".  Only loads and stores manipulate
     MTypes, when loaded values are sign or zero extended as necessary
     to produce ZTypes.  *)

TYPE
  Sign = { Positive, Negative, Unknown };
  (* extra compile-time information for DIV and MOD *)

TYPE
  Name = M3ID.T;

TYPE
  Var    = BRANDED "M3CG.Var"  OBJECT END; (* represents a variable *)
  Proc   = BRANDED "M3CG.Proc" OBJECT END; (* represents a procedure *)

TYPE
  BitOffset  = INTEGER;  (* bit offset of a field *)
  BitSize    = CARDINAL; (* bit size of a memory reference or variable *)
  ByteOffset = INTEGER;  (* byte offset of a field *)
  ByteSize   = CARDINAL; (* byte size of a memory reference or variable *)
  Alignment  = CARDINAL; (* minimum byte alignment *)

TYPE
  Frequency = [0..100];
  (* estimated frequency that a branch will be taken or variable referenced. *)

CONST
  Never  : Frequency = FIRST (Frequency);
  Maybe  : Frequency = (Never + Always) DIV 2;
  Likely : Frequency = (Never + 8 * Always) DIV 10;
  Always : Frequency = LAST (Frequency);

TYPE
  TypeUID = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];
  (* a 32-bit unique id (fingerprint) for each type.  *)

TYPE
  Label = INTEGER;
  (* a unique value for each label.  The client is responsible for
     allocating these id's with the 'next_label' field declared below. *)

CONST
  No_label: Label = -1;

TYPE
  CallingConvention = Target.CallingConvention;

END M3CG.



