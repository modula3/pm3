(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:10 PST 1992 by muller   *)
(*      modified on Sat Nov  2 16:21:44 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 17:12:15 PDT 1991 by msm      *)
<*PRAGMA LL*>

INTERFACE KeyboardKey;

(* Define constants for common keyboard symbols.  *)

CONST 
  VoidSymbol = 16_FFFFFF;	(* void symbol *)
  BackSpace = 16_FF08;	(* back space, back char *)
  Tab = 16_FF09;
  Linefeed = 16_FF0A;	(* Linefeed, LF *)
  Clear = 16_FF0B;
  Return = 16_FF0D;	(* Return, enter *)
  Pause = 16_FF13;	(* Pause, hold *)
  Scroll_Lock = 16_FF14;
  Escape = 16_FF1B;
  Delete = 16_FFFF;	(* Delete, rubout *)
  Multi_key = 16_FF20;  (* Multi-key character compose *)

(* Modifiers *)

  Shift_L = 16_FFE1;	(* Left shift *)
  Shift_R = 16_FFE2;	(* Right shift *)
  Control_L = 16_FFE3;	(* Left control *)
  Control_R = 16_FFE4;	(* Right control *)
  Caps_Lock = 16_FFE5;	(* Caps lock *)
  Shift_Lock = 16_FFE6;	(* Shift lock *)

  Meta_L = 16_FFE7;	(* Left meta *)
  Meta_R = 16_FFE8;	(* Right meta *)
  Alt_L = 16_FFE9;	(* Left alt *)
  Alt_R = 16_FFEA;	(* Right alt *)
  Super_L = 16_FFEB;	(* Left super *)
  Super_R = 16_FFEC;	(* Right super *)
  Hyper_L = 16_FFED;	(* Left hyper *)
  Hyper_R = 16_FFEE;	(* Right hyper *)

(* Cursor control & motion *)

  Home = 16_FF50;
  Left = 16_FF51;	(* Move left, left arrow *)
  Up = 16_FF52;	(* Move up, up arrow *)
  Right = 16_FF53;	(* Move right, right arrow *)
  Down = 16_FF54;	(* Move down, down arrow *)
  Prior = 16_FF55;	(* Prior, previous *)
  Next = 16_FF56;	(* Next *)
  End = 16_FF57;	(* EOL *)
  Begin = 16_FF58;	(* BOL *)


(* Misc Functions *)

  Select = 16_FF60;	(* Select, mark *)
  Print = 16_FF61;
  Execute = 16_FF62;	(* Execute, run, do *)
  Insert = 16_FF63;	(* Insert, insert here *)
  Undo = 16_FF65;	(* Undo, oops *)
  Redo = 16_FF66;	(* redo, again *)
  Menu = 16_FF67;
  Find = 16_FF68;	(* Find, search *)
  Cancel = 16_FF69;	(* Cancel, stop, abort, exit *)
  Help = 16_FF6A;	(* Help, ? *)
  Break = 16_FF6B;
  Mode_switch = 16_FF7E;	(* Character set switch *)
  script_switch = 16_FF7E;  (* Alias for mode_switch *)
  Num_Lock = 16_FF7F;

(* Keypad Functions, keypad numbers cleverly chosen to map to ascii *)

  KP_Space = 16_FF80;	(* space *)
  KP_Tab = 16_FF89;
  KP_Enter = 16_FF8D;	(* enter *)
  KP_F1 = 16_FF91;	(* PF1, KP_A, ... *)
  KP_F2 = 16_FF92;
  KP_F3 = 16_FF93;
  KP_F4 = 16_FF94;
  KP_Equal = 16_FFBD;	(* equals *)
  KP_Multiply = 16_FFAA;
  KP_Add = 16_FFAB;
  KP_Separator = 16_FFAC;	(* separator, often comma *)
  KP_Subtract = 16_FFAD;
  KP_Decimal = 16_FFAE;
  KP_Divide = 16_FFAF;

  KP_0 = 16_FFB0;
  KP_1 = 16_FFB1;
  KP_2 = 16_FFB2;
  KP_3 = 16_FFB3;
  KP_4 = 16_FFB4;
  KP_5 = 16_FFB5;
  KP_6 = 16_FFB6;
  KP_7 = 16_FFB7;
  KP_8 = 16_FFB8;
  KP_9 = 16_FFB9;



(*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 *)

  F1 = 16_FFBE;
  F2 = 16_FFBF;
  F3 = 16_FFC0;
  F4 = 16_FFC1;
  F5 = 16_FFC2;
  F6 = 16_FFC3;
  F7 = 16_FFC4;
  F8 = 16_FFC5;
  F9 = 16_FFC6;
  F10 = 16_FFC7;
  F11 = 16_FFC8;
  L1 = 16_FFC8;
  F12 = 16_FFC9;
  L2 = 16_FFC9;
  F13 = 16_FFCA;
  L3 = 16_FFCA;
  F14 = 16_FFCB;
  L4 = 16_FFCB;
  F15 = 16_FFCC;
  L5 = 16_FFCC;
  F16 = 16_FFCD;
  L6 = 16_FFCD;
  F17 = 16_FFCE;
  L7 = 16_FFCE;
  F18 = 16_FFCF;
  L8 = 16_FFCF;
  F19 = 16_FFD0;
  L9 = 16_FFD0;
  F20 = 16_FFD1;
  L10 = 16_FFD1;
  F21 = 16_FFD2;
  R1 = 16_FFD2;
  F22 = 16_FFD3;
  R2 = 16_FFD3;
  F23 = 16_FFD4;
  R3 = 16_FFD4;
  F24 = 16_FFD5;
  R4 = 16_FFD5;
  F25 = 16_FFD6;
  R5 = 16_FFD6;
  F26 = 16_FFD7;
  R6 = 16_FFD7;
  F27 = 16_FFD8;
  R7 = 16_FFD8;
  F28 = 16_FFD9;
  R8 = 16_FFD9;
  F29 = 16_FFDA;
  R9 = 16_FFDA;
  F30 = 16_FFDB;
  R10 = 16_FFDB;
  F31 = 16_FFDC;
  R11 = 16_FFDC;
  F32 = 16_FFDD;
  R12 = 16_FFDD;
  R13 = 16_FFDE;
  F33 = 16_FFDE;
  F34 = 16_FFDF;
  R14 = 16_FFDF;
  F35 = 16_FFE0;
  R15 = 16_FFE0;

(* Japanese keyboard support *)

  Kanji = 16_FF21;	(* Kanji, Kanji convert *)
  Muhenkan = 16_FF22;  (* Cancel Conversion *)
  Henkan_Mode = 16_FF23;  (* Start/Stop Conversion *)
  Henkan = 16_FF23;  (* Alias for Henkan_Mode *)
  Romaji = 16_FF24;  (* to Romaji *)
  Hiragana = 16_FF25;  (* to Hiragana *)
  Katakana = 16_FF26;  (* to Katakana *)
  Hiragana_Katakana = 16_FF27;  (* Hiragana/Katakana toggle *)
  Zenkaku = 16_FF28;  (* to Zenkaku *)
  Hankaku = 16_FF29;  (* to Hankaku *)
  Zenkaku_Hankaku = 16_FF2A;  (* Zenkaku/Hankaku toggle *)
  Touroku = 16_FF2B;  (* Add to Dictionary *)
  Massyo = 16_FF2C;  (* Delete from Dictionary *)
  Kana_Lock = 16_FF2D;  (* Kana Lock *)
  Kana_Shift = 16_FF2E;  (* Kana Shift *)
  Eisu_Shift = 16_FF2F;  (* Alphanumeric Shift *)
  Eisu_toggle = 16_FF30;  (* Alphanumeric toggle *)

END KeyboardKey.
