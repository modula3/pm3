(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TargetMap.m3                                          *)
(* Last Modified On Fri Nov 19 09:33:51 PST 1993 By kalsow     *)

MODULE TargetMap;

IMPORT Target;
FROM Target IMPORT CGType;

PROCEDURE Init () =
 BEGIN
    InitI (CGType.Addr,   Target.Address);
    InitI (CGType.Word,   Target.Integer);
    InitI (CGType.Int,    Target.Integer);
    InitF (CGType.Reel,   Target.Real);
    InitF (CGType.LReel,  Target.Longreal);
    InitF (CGType.XReel,  Target.Extended);
    InitI (CGType.Int_A,  Target.Int_A);
    InitI (CGType.Int_B,  Target.Int_B);
    InitI (CGType.Int_C,  Target.Int_C);
    InitI (CGType.Int_D,  Target.Int_D);
    InitI (CGType.Word_A, Target.Word_A);
    InitI (CGType.Word_B, Target.Word_B);
    InitI (CGType.Word_C, Target.Word_C);
    InitI (CGType.Word_D, Target.Word_D);
    InitI (CGType.Struct, Target.Void);
    InitI (CGType.Void,   Target.Void);

    CG_Base[CGType.Int]   := CGType.Int;
    CG_Base[CGType.Int_A] := CGType.Int;
    CG_Base[CGType.Int_B] := CGType.Int;
    CG_Base[CGType.Int_C] := CGType.Int;
    CG_Base[CGType.Int_D] := CGType.Int;

    CG_Base[CGType.Word]   := CGType.Word;
    CG_Base[CGType.Word_A] := CGType.Word;
    CG_Base[CGType.Word_B] := CGType.Word;
    CG_Base[CGType.Word_C] := CGType.Word;
    CG_Base[CGType.Word_D] := CGType.Word;

    Int_types[0] := Target.Word_A;
    Int_types[1] := Target.Int_A;
    Int_types[2] := Target.Word_B;
    Int_types[3] := Target.Int_B;
    Int_types[4] := Target.Word_C;
    Int_types[5] := Target.Int_C;
    Int_types[6] := Target.Word_D;
    Int_types[7] := Target.Int_D;
  END Init;

PROCEDURE InitI (type: CGType;  READONLY x: Target.Int_type) =
  BEGIN
    CG_Align [type]       := x.align;
    CG_Align_bytes [type] := x.align DIV Target.Byte;
    CG_Size [type]        := x.size;
    CG_Bytes [type]       := x.bytes;
    CG_Base [type]        := type;
  END InitI;

PROCEDURE InitF (type: CGType;  READONLY x: Target.Float_type) =
  BEGIN
    CG_Align [type]       := x.align;
    CG_Align_bytes [type] := x.align DIV Target.Byte;
    CG_Size [type]        := x.size;
    CG_Bytes [type]       := x.bytes;
    CG_Base [type]        := type;
    Float_types[x.pre]    := x;
  END InitF;

BEGIN
END TargetMap.
