(*Public Domain*)

INTERFACE AppModel;

IMPORT Pathname;

TYPE
  T <: PublicT;
  PublicT = OBJECT
  METHODS
    init():T;
    exit():BOOLEAN;  (*return true if ok to exit*)

    (*---file actions---*)
    file_new();
    file_open(path:Pathname.T);
    file_save();
    file_saveas(path:Pathname.T);
    file_print(); (*need to collect lots more data for real work*)

    (*---edit actions---*)
    cut();
    copy();
    paste();

    (*---help actions---*)
    help();

    (*---toolbar actions---*)
    hello():TEXT; (*returns TEXT for label*)
    goodbye();

  END;

VAR (*const after init*)
  app:T;
END AppModel.
