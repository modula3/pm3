(*Public Domain*)

UNSAFE INTERFACE MainWindow;

IMPORT X,Xt;

VAR (*const after init*)
  display:X.DisplayStar;
  appContext: Xt.AppContext;

  topLevel,
    main_w,
      work_rc,
        main_label,

    any_msg,err_msg,warn_msg,info_msg,working_msg:Xt.Widget; (*std dialogs*)

END MainWindow.
