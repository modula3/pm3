(*Public Domain*)

UNSAFE MODULE MainWindow EXPORTS MainWindow,UI;

IMPORT RTLinker,M3toC,IO,Fmt,Process;

IMPORT X, Xt, XtN, Xm, XmN, Xmw, Xmacro;

FROM Xmacro IMPORT
  XtVaSetValues, XtVaCreateManagedWidget,XtVaCreateWidget,
  TextVal,CharVal,IntVal,Args,ArgArray;

FROM M3toC IMPORT FlatTtoS;

IMPORT AppModel,Menu,Toolbar;

(*-----------------*)
(* Utilities       *)
(*-----------------*)
VAR Module:="MainWindow";

(*-----------------*)
PROCEDURE debug(level:INTEGER; ftn, str:TEXT) =
BEGIN
  IO.Put (Module & ":" & ftn & ":" & str);
END debug;




(*=====================*)
(* Public              *)
(*=====================*)
REVEAL 
  T = PublicT BRANDED OBJECT
  OVERRIDES
    init:=Init;
    run:=Run;
    exit:=Exit;
    msg:=Msg;
    err:=Err;
    warn:=Warn;
    info:=Info;
    working:=Working;
  END;


(*---------------------*)
PROCEDURE Init(self:T):T =
  CONST ftn = "init";
  VAR
    argc : Xt.Cardinal := RTLinker.argc;
    argv : X.Argv      := RTLinker.argv;
  BEGIN
    debug(1,ftn,"begin\n");

    topLevel := Xt.AppInitialize(
                  app_context_return := appContext,
                  application_class  := FlatTtoS("Hello"),
                  options            := NIL,
                  num_options        := 0,
                  argc_in_out        := argc,
                  argv_in_out        := argv);

    XtVaSetValues(topLevel,
               XtN.x,IntVal(50),
               XtN.y,IntVal(50),
               XtN.width,IntVal(400),
               XtN.height,IntVal(200));
     
    MakeWidgets();
    Xt.RealizeWidget(topLevel);
    display:=Xt.Display(topLevel);

    RETURN self;
  END Init;
(*---------------------*)
PROCEDURE Run(self:T) =
BEGIN
    Xt.AppMainLoop(appContext);    
END Run;
(*---------------------*)
PROCEDURE Exit(self:T) =
BEGIN
  (*or Xt.DestroyApplicationContext(appContext);*)
  Process.Exit(0);
END Exit;

(*===================*)
(* Private           *)
(*===================*)
PROCEDURE MakeWidgets() =
CONST ftn = "MakeWidgets";
VAR
  str:Xm.String;
  args:=NEW(Xt.ArgList);
  argc:CARDINAL;
BEGIN
  debug(1,ftn,"begin\n");

  (*---main window---*)
  main_w:=XtVaCreateManagedWidget(
        name        :=FlatTtoS("main_window"),
        widget_class:=Xmw.xmMainWindowWidgetClass,
        parent      :=topLevel,
        n1:=XmN.scrollBarDisplayPolicy, v1:=IntVal(Xm.AS_NEEDED),
        n2:=XmN.scrollingPolicy,        v2:=IntVal(Xm.AUTOMATIC),
        n3:=XmN.showSeparator,          v3:=IntVal(X.True),
        end:=NIL);

  Menu.init(main_w);
  
  work_rc:=XtVaCreateWidget(
        name        :=FlatTtoS("work_rc"),
        widget_class:=Xmw.xmRowColumnWidgetClass,
        parent      :=main_w,
        n1:=XmN.orientation,            v1:=IntVal(Xm.VERTICAL),
        end:=NIL);

  XtVaSetValues(main_w,
        XmN.workWindow,       work_rc);

  Toolbar.init(work_rc);      

  (*---make area for new hello labels---*)
  str:=Xm.StringCreateSimple(FlatTtoS("Hello, world"));
  main_label:=XtVaCreateManagedWidget(
    FlatTtoS("main_label"),Xmw.xmLabelWidgetClass,work_rc,
    XmN.labelString,   str,
    end:=NIL);
  Xm.StringFree(str);


  (*---manage work_rc---*)
  Xt.ManageChild(work_rc);

  (*---std messages---*)
  str:=Xm.StringCreateSimple(FlatTtoS("dummy"));
  args:=Args(args,ArgArray{XmN.messageString,str,NIL,..},argc);
  any_msg     :=Xmw.CreateMessageDialog(main_w,FlatTtoS("any_msg"),args,argc);
  err_msg     :=Xmw.CreateErrorDialog(main_w,FlatTtoS("err_msg"),args,argc);
  warn_msg    :=Xmw.CreateWarningDialog(main_w,FlatTtoS("warn_msg"),args,argc);
  info_msg    :=Xmw.CreateInformationDialog(main_w,FlatTtoS("info_msg"),args,argc);
  working_msg :=Xmw.CreateWorkingDialog(main_w,FlatTtoS("working_msg"),args,argc);
  Xm.StringFree(str);
END MakeWidgets;


(*========================*)
(* Message Dialogs        *)
(*========================*)
(*------------------------*)
PROCEDURE Msg(self:T; txt:TEXT)=
VAR
  t:=StringCreateSimple(txt);
BEGIN
  XtVaSetValues(any_msg,
    XmN.messageString,     t,
    end:=NIL);
  Xt.ManageChild(any_msg);
  Xm.StringFree(t);
  Xt.Popup(Xt.Parent(any_msg),Xt.GrabNone);
END Msg;
(*------------------------*)
PROCEDURE Err(self:T;txt:TEXT)=
VAR
  t:=StringCreateSimple(txt);
BEGIN
  XtVaSetValues(err_msg,
    XmN.messageString,     t,
    end:=NIL);
  Xt.ManageChild(err_msg);
  Xm.StringFree(t);
  Xt.Popup(Xt.Parent(err_msg),Xt.GrabNone);
END Err;
(*------------------------*)
PROCEDURE Warn(self:T;txt:TEXT)=
VAR
  t:=StringCreateSimple(txt);
BEGIN
  XtVaSetValues(warn_msg,
    XmN.messageString,     t,
    end:=NIL);
  Xt.ManageChild(warn_msg);
  Xm.StringFree(t);
  Xt.Popup(Xt.Parent(warn_msg),Xt.GrabNone);
END Warn;
(*------------------------*)
PROCEDURE Info(self:T;txt:TEXT)=
VAR
  t:=StringCreateSimple(txt);
BEGIN
  XtVaSetValues(info_msg,
    XmN.messageString,     t,
    end:=NIL);
  Xt.ManageChild(info_msg);
  Xm.StringFree(t);
  Xt.Popup(Xt.Parent(info_msg),Xt.GrabNone);
END Info;
(*------------------------*)
PROCEDURE Working(self:T;txt:TEXT)=
VAR
  t:=StringCreateSimple(txt);
BEGIN
  XtVaSetValues(working_msg,
    XmN.messageString,     t,
    end:=NIL);
  Xt.ManageChild(working_msg);
  Xm.StringFree(t);
  Xt.Popup(Xt.Parent(working_msg),Xt.GrabNone);
END Working;

(*========================*)

PROCEDURE StringCreateSimple(txt: TEXT): Xt.String =
  BEGIN
    WITH str = M3toC.SharedTtoS(txt) DO
      TRY
        RETURN Xm.StringCreateSimple(str);
      FINALLY
        M3toC.FreeSharedS(txt, str);
      END;
    END;
  END StringCreateSimple;
    

BEGIN
END MainWindow.

