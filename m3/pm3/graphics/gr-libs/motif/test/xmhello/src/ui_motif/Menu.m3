(*Public Domain*)


UNSAFE MODULE Menu;

IMPORT Process,IO,Fmt,RefSeq;
FROM Ctypes IMPORT char_star;

IMPORT X, Xt, XtN, Xm, XmN, Xmw, Xmacro;

FROM Xmacro IMPORT
  Args,ArgArray,
  TextVal,CharVal,IntVal,
  XtVaSetValues, XtVaCreateManagedWidget,XtVaCreateWidget;

FROM M3toC IMPORT
  TtoS,StoT;

FROM AppModel IMPORT app;
FROM UI IMPORT ui;

IMPORT MainWindow;

CONST Module = "Menu.";


(*----------------------*)
PROCEDURE debug(ftn,txt:TEXT)=
BEGIN
  IO.Put(Module & ftn & ":" & txt & "\n");
END debug;

(*----------------------*)
PROCEDURE init(parent:Xt.Widget)=
BEGIN
  menubar:=Xmw.CreateMenuBar(
        parent:=parent,
        name:=TtoS("menubar"),
        args:=NIL,
        argCount:=0);

  BuildFileMenu(menubar);
  BuildEditMenu(menubar);
  BuildHelpMenu(menubar);

  Xt.ManageChild(menubar);    
END init;

(*====================*)
(* File Menu          *)
(*====================*)

PROCEDURE BuildFileMenu(parent:Xt.Widget)=
BEGIN
  file_menu:=Xmacro.MakeMenu(
        parent:=menubar,
        menu_type:=Xm.MENU_PULLDOWN,
        menu_title:="File",
        menu_mnemonic:='F',
        cascade:=file_csb);

  new_btn:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "New",
        mnemonic   := 'N',
        callback   := new_callback);

  open_btn:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Open",
        mnemonic   := 'O',
        callback   := open_callback);

  open_dialog:=Xmw.CreateFileSelectionDialog(
      parent:=MainWindow.topLevel,
      name:= TtoS("open_dialog"),
      args:=NIL,
      argCount:=0);
  Xt.AddCallback(open_dialog,XmN.okCallback,open_ok_callback);
  Xt.AddCallback(open_dialog,XmN.cancelCallback,open_cancel_callback);


  save_btn:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Save",
        mnemonic   := 'S',
        callback   := save_callback);

  saveas_btn:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Save As",
        mnemonic   := 'A',
        callback   := saveas_callback);
  saveas_dialog:=Xmw.CreateFileSelectionDialog(
      parent:=MainWindow.topLevel,
      name:= TtoS("saveas_dialog"),
      args:=NIL,
      argCount:=0);
  Xt.AddCallback(saveas_dialog,XmN.okCallback,saveas_ok_callback);
  Xt.AddCallback(saveas_dialog,XmN.cancelCallback,saveas_cancel_callback);

  file_sep1:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmSeparatorWidgetClass);

  print_btn:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Print",
        mnemonic   := 'P',
        callback   := print_callback);

  file_sep1:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmSeparatorWidgetClass);

  exit_btn:=Xmacro.BuildMenuItem(
        parent     := file_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Exit",
        mnemonic   := 'x',
        callback   := exit_callback);

END BuildFileMenu;

(*-------------------*)
PROCEDURE new_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "new_callback";
BEGIN
  debug(ftn,"begin");
  app.file_new();
END new_callback;


(*-------------------*)
PROCEDURE open_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "open_callback";
BEGIN
  debug(ftn,"begin");
  Xt.ManageChild(open_dialog);
END open_callback;
(*-------------------*)
PROCEDURE open_ok_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "open_ok_callback";
VAR
  cbs:=LOOPHOLE(call_data,REF Xm.FileSelectionBoxCallbackStruct);
  str:char_star;
  value:TEXT; 
BEGIN
  debug(ftn,"begin");
  EVAL Xm.StringGetLtoR(cbs.value,TtoS(""),str);
  value:=StoT(str);
  app.file_open(value);
  Xt.UnmanageChild(widget);
END open_ok_callback;
(*-------------------*)
PROCEDURE open_cancel_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "open_cancel_callback";
BEGIN
  debug(ftn,"begin");
  Xt.UnmanageChild(widget);
END open_cancel_callback;
(*-------------------*)
PROCEDURE save_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "save_callback";
BEGIN
  debug(ftn,"begin");
  app.file_save();
END save_callback;
(*-------------------*)
PROCEDURE saveas_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "saveas_callback";
BEGIN
  debug(ftn,"begin");
  Xt.ManageChild(saveas_dialog);
END saveas_callback;
(*-------------------*)
PROCEDURE saveas_ok_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "saveas_ok_callback";
VAR
  cbs:=LOOPHOLE(call_data,REF Xm.FileSelectionBoxCallbackStruct);
  str:char_star;
  value:TEXT; 
BEGIN
  debug(ftn,"begin");
  EVAL Xm.StringGetLtoR(cbs.value,TtoS(""),str);
  value:=StoT(str);
  app.file_saveas(value);
  Xt.UnmanageChild(widget);
END saveas_ok_callback;
(*-------------------*)
PROCEDURE saveas_cancel_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "saveas_cancel_callback";
BEGIN
  debug(ftn,"begin");
  Xt.UnmanageChild(widget);
END saveas_cancel_callback;
(*-------------------*)
PROCEDURE print_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "print_callback";
BEGIN
  debug(ftn,"begin");
  app.file_print();
END print_callback;
(*-------------------*)
PROCEDURE exit_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "exit_callback";
BEGIN
  IF app.exit() THEN
    ui.exit();
  ELSE
    ui.err("Cannot exit");
  END;
END exit_callback;

(*====================*)
(* Edit Menu          *)
(*====================*)

PROCEDURE BuildEditMenu(parent:Xt.Widget)=
BEGIN
  edit_menu:=Xmacro.MakeMenu(
        parent:=menubar,
        menu_type:=Xm.MENU_PULLDOWN,
        menu_title:="Edit",
        menu_mnemonic:='E',
        cascade:=edit_csb);

  copy_btn:=Xmacro.BuildMenuItem(
        parent     := edit_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Copy",
        mnemonic   := 'C',
        accelerator:= "Ctrl<Key>C",
        accel_text := "Ctrl C",
        callback   := copy_callback);

  cut_btn:=Xmacro.BuildMenuItem(
        parent     := edit_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Cut",
        mnemonic   := 'u',
        accelerator:= "Ctrl<Key>X",
        accel_text := "Ctrl X",
        callback   := cut_callback);

  paste_btn:=Xmacro.BuildMenuItem(
        parent     := edit_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "Paste",
        mnemonic   := 'P',
        accelerator:= "Ctrl<Key>P",
        accel_text := "Ctrl P",
        callback   := paste_callback);


END BuildEditMenu;

(*-------------------*)
PROCEDURE cut_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "cut_callback";
BEGIN
  debug(ftn,"begin");

END cut_callback;
(*-------------------*)
PROCEDURE copy_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "copy_callback";
BEGIN
  debug(ftn,"begin");

END copy_callback;
(*-------------------*)
PROCEDURE paste_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "paste_callback";
BEGIN
  debug(ftn,"begin");
END paste_callback;
(*====================*)
(* Help Menu          *)
(*====================*)

PROCEDURE BuildHelpMenu(parent:Xt.Widget)=
VAR
  str:Xm.String;
  args:=NEW(Xt.ArgList);
  argCount:CARDINAL;
BEGIN
  help_menu:=Xmacro.MakeMenu(
        parent:=menubar,
        menu_type:=Xm.MENU_PULLDOWN,
        menu_title:="Help",
        menu_mnemonic:='H',
        cascade:=help_csb);

  XtVaSetValues(menubar,
        XmN.menuHelpWidget, help_csb,
        NIL);

  about_btn:=Xmacro.BuildMenuItem(
        parent     := help_menu,
        class      := Xmw.xmPushButtonWidgetClass,
        label      := "About",
        callback   := about_callback);

  str:=Xm.StringCreateSimple(TtoS("Hello World, in Modula3"));
  about_dialog:=Xmw.CreateInformationDialog(
      parent:=about_btn,
      name:= TtoS("about_dialog"),
      args:=Args(args,ArgArray{
        XmN.messageString,    str,
        NIL,..},argCount),
      argCount:=argCount);
  Xm.StringFree(str);
  Xt.AddCallback(about_dialog,XmN.okCallback,about_ok_callback);

END BuildHelpMenu;

(*-------------------*)
PROCEDURE about_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "about_callback";
BEGIN
  debug(ftn,"begin");
  Xt.ManageChild(about_dialog);
  Xt.Popup(Xt.Parent(about_dialog),Xt.GrabNone);        
END about_callback;

(*-------------------*)
PROCEDURE about_ok_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "about_ok_callback";
BEGIN
  debug(ftn,"begin");       
  Xt.Popdown(Xt.Parent(about_dialog));        
  Xt.UnmanageChild(about_dialog);
END about_ok_callback;
(*===================================*)
BEGIN
END Menu.
