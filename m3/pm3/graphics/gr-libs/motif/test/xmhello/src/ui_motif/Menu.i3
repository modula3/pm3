(*Public Domain*)

UNSAFE INTERFACE Menu;
IMPORT Xt;

VAR (*const after init*)
  menubar,
    file_csb,file_menu,
      new_btn,new_dialog,
      open_btn,open_dialog,
      save_btn,save_dialog,
      saveas_btn,saveas_dialog,
      file_sep1,
      print_btn,print_dialog,
      file_sep2,
      exit_btn,exit_dialog,

    edit_csb,edit_menu,
      copy_btn,
      cut_btn,
      paste_btn,

    help_csb,help_menu,
      about_btn,about_dialog:Xt.Widget;
 
        
PROCEDURE init(parent:Xt.Widget);

END Menu.
