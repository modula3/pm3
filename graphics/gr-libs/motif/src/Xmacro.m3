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
PROCEDURE TextVal(value:TEXT):Xt.ArgVal=
BEGIN
  RETURN LOOPHOLE(M3toC.TtoS(value), Xt.ArgVal);
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
CONST ftn = "Args";
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
                    menu_title:TEXT;
                    menu_mnemonic:CHAR:=ASCII.NUL;
                    items:RefSeq.T:=NIL):Xt.Widget=
CONST ftn = "BuildMenu";
VAR
  menu,cascade,widget:Xt.Widget;
  str: Xm.String;
  menu_title_str:=String(menu_title);
  args:=NEW(Xt.ArgList);
  argCount:CARDINAL;
  i:INTEGER;
  callback_type:char_star;
BEGIN
  IF (menu_type = Xm.MENU_PULLDOWN) OR (menu_type = Xm.MENU_OPTION) THEN
    menu:=Xmw.CreatePulldownMenu(parent,String("_pulldown"),NIL,0);
  ELSIF (menu_type = Xm.MENU_POPUP) THEN
    menu:=Xmw.CreatePopupMenu(parent,String("_popup"),NIL,0);
  ELSE
    Xt.Warning(String("Invalid menu type passed to " & ftn));
    RETURN NIL;
  END;

  (*---pulldowns need a cascade button---*)
  IF  (menu_type = Xm.MENU_PULLDOWN) THEN
    str:=Xm.StringCreateSimple(menu_title_str);
    cascade:=Xmw.CreateCascadeButton(
        parent:=parent,
        name:=menu_title_str,
        args:=Args(args,ArgArray{
             XmN.subMenuId,     menu,
             XmN.labelString,   str,
             XmN.mnemonic,      CharVal(menu_mnemonic),
             NIL,..},argCount),
        argCount:=argCount);
     Xm.StringFree(str);
     Xt.ManageChild(cascade);

  ELSIF menu_type = Xm.MENU_OPTION THEN
    str:=Xm.StringCreateSimple(menu_title_str);
    cascade:=Xmw.CreateOptionMenu(
        parent:=parent,
        name:=menu_title_str,
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
          Xt.Warning(String("You cannot have submenus from option menu"));
        ELSE
          widget:=BuildMenu(menu,Xm.MENU_PULLDOWN,
                  menu_title   :=item.label,
                  menu_mnemonic:=item.mnemonic,
                  items        :=item.subitems);
        END;
      ELSE (*no subitems...end of recursion*)
        widget:=Xt.CreateManagedWidget(
              name:=String(item.label),
              widget_class:=item.class,
              parent:=menu,
              args:=NIL,
              num_args:=0);
      END; 
  
      (*---mnemonic---*)
      IF item.mnemonic # ASCII.NUL THEN 
        XtVaSetValues(widget,XmN.mnemonic,CharVal(item.mnemonic),NIL);   
      END;
     
      (*---accelerator---*)
      IF item.accelerator # NIL THEN
        str:=Xm.StringCreateSimple(String(item.accel_text));
        XtVaSetValues(widget,
           XmN.accelerator,     TextVal(item.accelerator),
           XmN.acceleratorText, str,
           NIL);
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
                    menu_title:TEXT;
                    menu_mnemonic:CHAR:=ASCII.NUL;
                    VAR cascade:Xt.Widget;
                    ):Xt.Widget=
CONST ftn = "MakeMenu";
VAR
  menu,widget:Xt.Widget;
  str: Xm.String;
  menu_title_str:=String(menu_title);
  args:=NEW(Xt.ArgList);
  argCount:CARDINAL;
BEGIN
  IF (menu_type = Xm.MENU_PULLDOWN) OR (menu_type = Xm.MENU_OPTION) THEN
    menu:=Xmw.CreatePulldownMenu(parent,String("_pulldown"),NIL,0);
  ELSIF (menu_type = Xm.MENU_POPUP) THEN
    menu:=Xmw.CreatePopupMenu(parent,String("_popup"),NIL,0);
  ELSE
    Xt.Warning(String("Invalid menu type passed to " & ftn));
    RETURN NIL;
  END;

  (*---pulldowns need a cascade button---*)
  IF  (menu_type = Xm.MENU_PULLDOWN) THEN
    str:=Xm.StringCreateSimple(menu_title_str);
    cascade:=Xmw.CreateCascadeButton(
        parent:=parent,
        name:=menu_title_str,
        args:=Args(args,ArgArray{
             XmN.subMenuId,     menu,
             XmN.labelString,   str,
             XmN.mnemonic,      CharVal(menu_mnemonic),
             NIL,..},argCount),
        argCount:=argCount);
     Xm.StringFree(str);
     Xt.ManageChild(cascade);

  ELSIF menu_type = Xm.MENU_OPTION THEN
    str:=Xm.StringCreateSimple(menu_title_str);
    cascade:=Xmw.CreateOptionMenu(
        parent:=parent,
        name:=menu_title_str,
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
    label:      TEXT:=NIL;
    mnemonic:   CHAR:=ASCII.NUL;
    accelerator:TEXT:=NIL;
    accel_text: TEXT:=NIL;
    callback:   Xt.CallbackProc:=NIL;
    callback_data: Xt.Pointer:=NIL;
                ):Xt.Widget=
VAR
  widget:Xt.Widget;
  str: Xm.String;
  args:=NEW(Xt.ArgList);
  argCount:CARDINAL;
  callback_type:char_star;
BEGIN
  widget:=Xt.CreateManagedWidget(
        name:=String(label),
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
    str:=Xm.StringCreateSimple(String(accel_text));
    XtVaSetValues(widget,
       XmN.accelerator,     TextVal(accelerator),
       XmN.acceleratorText, str,
       NIL);
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


