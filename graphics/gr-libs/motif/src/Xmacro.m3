(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 11:20:52 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE Xmacro;

IMPORT Word,Xt,Xm, Xmw,XmN,M3toC, Cstring,ASCII, RefSeq;
FROM Ctypes IMPORT short, unsigned_short, unsigned_long,char_star;

(*-----------------*)
PROCEDURE AddrVal(value:ADDRESS):Xt.ArgVal =
BEGIN
  RETURN LOOPHOLE(value,Xt.ArgVal);
END AddrVal;

(*-----------------*)
PROCEDURE CharVal(value:CHAR):Xt.ArgVal=
  BEGIN
    RETURN LOOPHOLE (ORD (value), Xt.ArgVal);
    (* does this work on big-endian machines?? *)
  END CharVal;

(**** not portable to 64 bit machines *****
PROCEDURE CharVal(value:CHAR):Xt.ArgVal=
  VAR tmp: RECORD pad1:short; pad2:CHAR; val:CHAR; END;
  BEGIN
    tmp.pad1:=0; tmp.pad2:='\000'; tmp.val:=value;
    RETURN LOOPHOLE(tmp, Xt.ArgVal);
  END CharVal;
****************************************)

(*-----------------*)
PROCEDURE IntVal(value:INTEGER):Xt.ArgVal=
BEGIN
  RETURN LOOPHOLE(value,Xt.ArgVal);
END IntVal;

(*-----------------*)
PROCEDURE ShortVal(value:short):Xt.ArgVal=
  BEGIN
    RETURN LOOPHOLE (ORD (value), Xt.ArgVal);
  END ShortVal;

(**** not portable to 64 bit machines *****
PROCEDURE ShortVal(value:short):Xt.ArgVal=
  VAR tmp: RECORD pad:short; val:short; END;
  BEGIN
    tmp.pad:=0; tmp.val:=value;
    RETURN LOOPHOLE(tmp, Xt.ArgVal);
  END ShortVal;
****************************************)

(*-----------------*)
PROCEDURE TextVal(value:TEXT;  VAR(*OUT*) str: char_star):Xt.ArgVal=
BEGIN
  str := M3toC.CopyTtoS(value);
  RETURN LOOPHOLE(str, Xt.ArgVal);
END TextVal;

(*-----------------*)
PROCEDURE UShortVal(value:unsigned_short):Xt.ArgVal=
  BEGIN
    RETURN LOOPHOLE (ORD (value), Xt.ArgVal);
  END UShortVal;

(**** not portable to 64 bit machines *****
PROCEDURE UShortVal(value:unsigned_short):Xt.ArgVal=
  VAR tmp: RECORD pad:short; val:unsigned_short; END;
  BEGIN
    tmp.pad:=0; tmp.val:=value;
    RETURN LOOPHOLE(tmp,Xt.ArgVal);
  END UShortVal;
****************************************)
PROCEDURE Args(args:Xt.ArgList;
               READONLY arr:ArgArray; 
               VAR count:CARDINAL
             ):Xt.ArgList=
(*arr must be provided in pairs*)
VAR
  name,value:ADDRESS;
BEGIN
  count:=0;
  FOR i:=FIRST(arr) TO LAST(arr)-4 BY 2 DO
    IF arr[i] = NIL THEN EXIT; END;
    name:=arr[i]; value:=arr[i+1];
    args[count]:=Xt.Arg{name:=name, value:=value};
    INC(count);    
  END;
  IF count = 0 THEN 
    RETURN NIL;
  ELSE
    args[count+1]:=Xt.Arg{name:=NIL, value:=NIL};
    RETURN args;
  END;
END Args;
(*----------------------*)




(*-----------------*)
PROCEDURE XtNewString(str:char_star):char_star =
VAR
  len:CARDINAL;
  newstr:char_star;
BEGIN
  len:=Cstring.strlen(str);
  newstr:=LOOPHOLE(Xt.Malloc(len+1),char_star);
  EVAL Cstring.strncpy(newstr,str,len);  
  RETURN newstr;  
END XtNewString;
(*----------------------*)
(* Masks                *)
(*----------------------*)
PROCEDURE MakeMask(mask:Mask):unsigned_long=
VAR
  val:unsigned_long:=0;
BEGIN
  FOR i:=0 TO Word.Size-1 DO
    IF i IN mask THEN
       INC(val,Word.Shift(1,i));
    END;
  END;
  RETURN val;
END MakeMask;

(*----------------------*)
(* Menus                *)
(*----------------------*)

PROCEDURE BuildMenu(parent:Xt.Widget;
                    menu_type:INTEGER;
                    menu_title:Xt.String;
                    menu_mnemonic:CHAR:=ASCII.NUL;
                    items:RefSeq.T:=NIL):Xt.Widget=
VAR
  menu,cascade,widget:Xt.Widget;
  str, acc: Xm.String;
  args:=NEW(Xt.ArgList);
  argCount:CARDINAL;
  callback_type:char_star;
BEGIN
  IF (menu_type = Xm.MENU_PULLDOWN) OR (menu_type = Xm.MENU_OPTION) THEN
    menu:=Xmw.CreatePulldownMenu(parent,M3toC.FlatTtoS("_pulldown"),NIL,0);
  ELSIF (menu_type = Xm.MENU_POPUP) THEN
    menu:=Xmw.CreatePopupMenu(parent,M3toC.FlatTtoS("_popup"),NIL,0);
  ELSE
    Xt.Warning(M3toC.FlatTtoS("Invalid menu type passed to BuildMenu"));
    RETURN NIL;
  END;

  (*---pulldowns need a cascade button---*)
  IF  (menu_type = Xm.MENU_PULLDOWN) THEN
    str:=Xm.StringCreateSimple(menu_title);
    cascade:=Xmw.CreateCascadeButton(
        parent:=parent,
        name:=menu_title,
        args:=Args(args,ArgArray{
             XmN.subMenuId,     menu,
             XmN.labelString,   str,
             XmN.mnemonic,      CharVal(menu_mnemonic),
             NIL,..},argCount),
        argCount:=argCount);
     Xm.StringFree(str);
     Xt.ManageChild(cascade);

  ELSIF menu_type = Xm.MENU_OPTION THEN
    str:=Xm.StringCreateSimple(menu_title);
    cascade:=Xmw.CreateOptionMenu(
        parent:=parent,
        name:=menu_title,
        args:=Args(args,ArgArray{
             XmN.subMenuId,     menu,
             XmN.labelString,   str,
             NIL,..},argCount),
        argCount:=argCount);
     Xm.StringFree(str);
     Xt.ManageChild(cascade);
  END;


  (*---add the menu items---*)
  IF items#NIL THEN
    FOR i:=0 TO items.size()-1 DO 
    WITH item = NARROW(items.get(i),MenuItem) DO
      (*call recursively as needed*)
      IF item.subitems#NIL THEN
        IF menu_type = Xm.MENU_OPTION THEN
          Xt.Warning(M3toC.FlatTtoS("You cannot have submenus from option menu"));
        ELSE
          str:=M3toC.SharedTtoS(item.label);
          widget:=BuildMenu(menu,Xm.MENU_PULLDOWN,
                  menu_title   :=str,
                  menu_mnemonic:=item.mnemonic,
                  items        :=item.subitems);
          M3toC.FreeSharedS(item.label, str);
        END;
      ELSE (*no subitems...end of recursion*)
        str:=M3toC.SharedTtoS(item.label);
        widget:=Xt.CreateManagedWidget(
              name:=str,
              widget_class:=item.class,
              parent:=menu,
              args:=NIL,
              num_args:=0);
        M3toC.FreeSharedS(item.label, str);
      END; 
  
      (*---mnemonic---*)
      IF item.mnemonic # ASCII.NUL THEN 
        XtVaSetValues(widget,XmN.mnemonic,CharVal(item.mnemonic),NIL);   
      END;
     
      (*---accelerator---*)
      IF item.accelerator # NIL THEN
        acc:=M3toC.SharedTtoS(item.accel_text);
        str:=Xm.StringCreateSimple(acc);
        M3toC.FreeSharedS(item.accel_text, acc);
	acc:=M3toC.SharedTtoS(item.accelerator);
        XtVaSetValues(widget,
           XmN.accelerator, acc,
           XmN.acceleratorText, str,
           NIL);
	M3toC.FreeSharedS(item.accelerator, acc);
        Xm.StringFree(str);
      END;
   
      (*---callback---*)
      IF item.callback#NIL THEN
        IF item.class = Xmw.xmToggleButtonWidgetClass THEN
          callback_type:=XmN.valueChangedCallback;
        ELSE
          callback_type:=XmN.activateCallback;
        END;
        Xt.AddCallback(widget,callback_type,
             item.callback,item.callback_data);
      END;
    END; (*with*)   
    END; (*for*)
  END; (*if items*)    

  (*popups: return the menu*)
  (*pulldowns: return the cascade button*)
  (*options: return the result of XmCreateOptionMenu*)
  IF menu_type= Xm.MENU_POPUP THEN
    RETURN menu;
  ELSE
    RETURN cascade;
  END;
END BuildMenu;

(*-------------------------*)
(* HGG approach            *)
(*-------------------------*)
PROCEDURE MakeMenu(
                    parent:Xt.Widget;
                    menu_type:INTEGER;
                    menu_title:Xt.String;
                    menu_mnemonic:CHAR:=ASCII.NUL;
                    VAR cascade:Xt.Widget;
                    ):Xt.Widget=
VAR
  menu:Xt.Widget;
  str: Xm.String;
  args:=NEW(Xt.ArgList);
  argCount:CARDINAL;
BEGIN
  IF (menu_type = Xm.MENU_PULLDOWN) OR (menu_type = Xm.MENU_OPTION) THEN
    menu:=Xmw.CreatePulldownMenu(parent,M3toC.FlatTtoS("_pulldown"),NIL,0);
  ELSIF (menu_type = Xm.MENU_POPUP) THEN
    menu:=Xmw.CreatePopupMenu(parent,M3toC.FlatTtoS("_popup"),NIL,0);
  ELSE
    Xt.Warning(M3toC.FlatTtoS("Invalid menu type passed to MakeMenu"));
    RETURN NIL;
  END;

  (*---pulldowns need a cascade button---*)
  IF  (menu_type = Xm.MENU_PULLDOWN) THEN
    str:=Xm.StringCreateSimple(menu_title);
    cascade:=Xmw.CreateCascadeButton(
        parent:=parent,
        name:=menu_title,
        args:=Args(args,ArgArray{
             XmN.subMenuId,     menu,
             XmN.labelString,   str,
             XmN.mnemonic,      CharVal(menu_mnemonic),
             NIL,..},argCount),
        argCount:=argCount);
     Xm.StringFree(str);
     Xt.ManageChild(cascade);

  ELSIF menu_type = Xm.MENU_OPTION THEN
    str:=Xm.StringCreateSimple(menu_title);
    cascade:=Xmw.CreateOptionMenu(
        parent:=parent,
        name:=menu_title,
        args:=Args(args,ArgArray{
             XmN.subMenuId,     menu,
             XmN.labelString,   str,
             NIL,..},argCount),
        argCount:=argCount);
     Xm.StringFree(str);
     Xt.ManageChild(cascade);
  END;

  IF menu_type= Xm.MENU_POPUP OR  menu_type= Xm.MENU_PULLDOWN THEN
    RETURN menu;
  ELSE
    RETURN cascade;
  END;
END MakeMenu;

(*------------------*)
PROCEDURE BuildMenuItem(
    parent:Xt.Widget;
    class:      Xt.WidgetClass:=NIL;
    label:      Xt.String:=NIL;
    mnemonic:   CHAR:=ASCII.NUL;
    accelerator:TEXT:=NIL;
    accel_text: TEXT:=NIL;
    callback:   Xt.CallbackProc:=NIL;
    callback_data: Xt.Pointer:=NIL;
                ):Xt.Widget=
VAR
  widget:Xt.Widget;
  str, acc: Xm.String;
  callback_type:char_star;
BEGIN
  widget:=Xt.CreateManagedWidget(
        name:=label,
        widget_class:=class,
        parent:=parent,
        args:=NIL,
        num_args:=0);
 
  IF class = Xmw.xmSeparatorWidgetClass THEN
    RETURN widget;
  END;
 
  (*---mnemonic---*)
  IF mnemonic # ASCII.NUL THEN 
    XtVaSetValues(widget,
       XmN.mnemonic,CharVal(mnemonic),
       NIL);   
  END;
 
  (*---accelerator---*)
  IF accelerator # NIL THEN
    acc:=M3toC.SharedTtoS(accel_text);
    str:=Xm.StringCreateSimple(acc);
    M3toC.FreeSharedS(accel_text, acc);
    acc:=M3toC.SharedTtoS(accelerator);
    XtVaSetValues(widget,
       XmN.accelerator, acc,
       XmN.acceleratorText, str,
       NIL);
    M3toC.FreeSharedS(accelerator, acc);
    Xm.StringFree(str);
  END;
   
  (*---callback---*)
  IF callback#NIL THEN
    IF class = Xmw.xmToggleButtonWidgetClass THEN
      callback_type:=XmN.valueChangedCallback;
    ELSE
      callback_type:=XmN.activateCallback;
    END;
    Xt.AddCallback(widget,callback_type,
         callback,callback_data);
  END;

  RETURN widget;
END BuildMenuItem;

(*-----------------*)
BEGIN
END Xmacro.


