(*Public Domain*)


UNSAFE MODULE Toolbar;

IMPORT Process,IO,Fmt,RefSeq;
FROM Ctypes IMPORT char_star;

IMPORT X, Xt, XtN, Xm, XmN, Xmw, Xmacro;

FROM Xmacro IMPORT
  Args,ArgArray,
  TextVal,CharVal,IntVal,
  XtVaSetValues, XtVaCreateManagedWidget,XtVaCreateWidget,
  MenuItem,BuildMenuItem;

FROM M3toC IMPORT
  TtoS,StoT;

IMPORT AppModel,UI,MainWindow;

CONST Module = "Toolbar.";
(*----------------------*)
PROCEDURE debug(ftn,txt:TEXT)=
BEGIN
  IO.Put(Module & ftn & ":" & txt & "\n");
END debug;

(*----------------------*)
PROCEDURE init(parent:Xt.Widget)=
VAR
  
BEGIN
  toolbar:=XtVaCreateWidget(TtoS("toolbar"),
    Xmw.xmRowColumnWidgetClass,parent,
    XmN.orientation,        IntVal(Xm.HORIZONTAL),
    end:=NIL);

  hello:=BuildMenuItem(
    parent:=toolbar,
    label:="Hello",
    class:=Xmw.xmPushButtonWidgetClass,
    callback:=hello_callback);

  goodbye:=BuildMenuItem(
    parent:=toolbar,
    label:="Goodbye",
    class:=Xmw.xmPushButtonWidgetClass,
    callback:=goodbye_callback);

  Xt.ManageChild(toolbar);
END init;
(*-------------------------------*)
PROCEDURE hello_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "hello_callback";
VAR
  txt:TEXT;
  str:Xm.String;
BEGIN
  debug(ftn,"begin"); 
  Xt.UnmanageChild(MainWindow.main_label);
  str:=Xm.StringCreateSimple(TtoS(AppModel.app.hello()));
  XtVaSetValues(MainWindow.main_label,
     XmN.labelString,  str);
  Xt.ManageChild(MainWindow.main_label);      
END hello_callback;
(*-------------------------------*)
PROCEDURE goodbye_callback(widget:Xt.Widget;
                       closure:Xt.Pointer;
                       call_data:Xt.Pointer)=
CONST ftn = "goodbye_callback";
BEGIN
  debug(ftn,"begin");  
  UI.ui.exit();     
END goodbye_callback;
(*===================================*)
BEGIN
END Toolbar.
