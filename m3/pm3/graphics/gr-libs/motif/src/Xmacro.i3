(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:45:38 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)
(* Updated by Harry George (harry.g.george@boeing.com), 11/28/97 *)
(*    revised to handle ArgArray*)

UNSAFE INTERFACE Xmacro;

IMPORT Word,Xt,ASCII,RefSeq;
FROM Ctypes IMPORT 
  char_star,short,unsigned_short,unsigned_long;
IMPORT M3toC;

(*------------------------*)
(* X level                *)
(*------------------------*)
(*
PROCEDURE RootWindowOfScreen(screen:X.ScreenStar):
*)

(*------------------------*)
(* Args and ArgLists      *)
(*------------------------*)
TYPE
  ArgArray = ARRAY [0..Xt.MaxSizeList*2] OF ADDRESS;

PROCEDURE AddrVal(value:ADDRESS):Xt.ArgVal;
PROCEDURE CharVal(value:CHAR):Xt.ArgVal;
PROCEDURE IntVal(value:INTEGER):Xt.ArgVal;
PROCEDURE ShortVal(value:short):Xt.ArgVal;
PROCEDURE TextVal(value:TEXT):Xt.ArgVal;
PROCEDURE UShortVal(value:unsigned_short):Xt.ArgVal;

PROCEDURE Args(args:Xt.ArgList;
               READONLY arr:ArgArray; 
               VAR count:CARDINAL
             ):Xt.ArgList;
(*arr must be provided in pairs*)
(*typical usage:
| FROM Xmacros IMPORT ArgArray,Args,
|      AddrVal,CharVal,IntVal,ShortVal,TextVal,UShortVal;
| PROCEDURE init()=
| VAR 
|    args:=NEW(Xt.ArgList);
|    argCount:CARDINAL;
| BEGIN
| ...
| main_w:=Xmw.CreateMainWindow(
|   parent:=toplevel,
|   name:=String("main_w"),
|   args:=Args(args,ArgArray{
|              XmN.scrollBarDisplayPolicy, IntVal(Xm.AS_NEEDED),
|              XmN.scrollingPolicy,        IntVal(Xm.AUTOMATIC),
|              NIL,..},argCount),
|   argCount:=argCount
|   );
| ...
*)

<*EXTERNAL"XtVaSetValues"*>
PROCEDURE XtVaSetValues(widget:Xt.Widget;
                 n1,v1,  n2,v2,  n3,v3,  n4,v4,  n5,v5,
                 n6,v6,  n7,v7,  n8,v8,  n9,v9,  n10,v10,
                 n11,v11,n12,v12,n13,v13,n14,v14,n15,v15,
                 end:ADDRESS:=NIL);


<*EXTERNAL"XtVaGetValues"*>
PROCEDURE XtVaGetValues(widget:Xt.Widget;
                 n1,v1,  n2,v2,  n3,v3,  n4,v4,  n5,v5,
                 n6,v6,  n7,v7,  n8,v8,  n9,v9,  n10,v10,
                 n11,v11,n12,v12,n13,v13,n14,v14,n15,v15,
                 end:ADDRESS:=NIL);

<*EXTERNAL "XtVaCreateManagedWidget"*>
PROCEDURE XtVaCreateManagedWidget(name:char_star;
	         widget_class: Xt.WidgetClass;
		 parent: Xt.Widget;
                 n1,v1,  n2,v2,  n3,v3,  n4,v4,  n5,v5,
                 n6,v6,  n7,v7,  n8,v8,  n9,v9,  n10,v10,
                 n11,v11,n12,v12,n13,v13,n14,v14,n15,v15,
                 end:ADDRESS:=NIL):Xt.Widget;

<*EXTERNAL "XtVaCreateWidget"*>
PROCEDURE XtVaCreateWidget(name:char_star;
	         widget_class: Xt.WidgetClass;
		 parent: Xt.Widget;
                 n1,v1,  n2,v2,  n3,v3,  n4,v4,  n5,v5,
                 n6,v6,  n7,v7,  n8,v8,  n9,v9,  n10,v10,
                 n11,v11,n12,v12,n13,v13,n14,v14,n15,v15,
                 end:ADDRESS:=NIL):Xt.Widget;


(*------------------*)
(* Strings          *)
(*------------------*)
CONST String = M3toC.TtoS;
PROCEDURE XtNewString(str:char_star):char_star;

(*----------------------*)
(* Masks                *)
(*----------------------*)
TYPE Mask = SET OF [0..Word.Size-1];
PROCEDURE MakeMask(mask:Mask):unsigned_long;


(*----------------------*)
(* Menus                *)
(*----------------------*)
(*---std approach per, MotifProgramming Manual, 1991, ex 16-11---*) 
TYPE
  MenuItem = REF RECORD
    label:      TEXT;
    class:      Xt.WidgetClass;
    mnemonic:   CHAR:=ASCII.NUL;
    accelerator:TEXT:=NIL;
    accel_text: TEXT:=NIL;
    callback:   Xt.CallbackProc:=NIL;
    callback_data: Xt.Pointer:=NIL;
    subitems:   RefSeq.T:=NIL; (*seq of subordinate menu items*) 
  END;

PROCEDURE BuildMenu(
(*This one assumes a nested tree of subitems, which means the internal nodes
are not readily known to the program as a whole*)
                    parent:Xt.Widget;
                    menu_type:INTEGER;
                    menu_title:TEXT;
                    menu_mnemonic:CHAR:=ASCII.NUL;
                    items:RefSeq.T:=NIL (*seq of REF MenuItem*)
                    ):Xt.Widget;

(*---HGG approach---*)
PROCEDURE MakeMenu(
                    parent:Xt.Widget;
                    menu_type:INTEGER;
                    menu_title:TEXT;
                    menu_mnemonic:CHAR:=ASCII.NUL;
                    VAR cascade:Xt.Widget;
                    ):Xt.Widget;
(*pulldowns and popups: return the menu*)
(*options: return the result of XmCreateOptionMenu*)


PROCEDURE BuildMenuItem(
    parent:Xt.Widget;
    class:      Xt.WidgetClass:=NIL;
    label:      TEXT:=NIL;
    mnemonic:   CHAR:=ASCII.NUL;
    accelerator:TEXT:=NIL;
    accel_text: TEXT:=NIL;
    callback:   Xt.CallbackProc:=NIL;
    callback_data: Xt.Pointer:=NIL;
                ):Xt.Widget;


END Xmacro.
