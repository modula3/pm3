(*Public Domain*)

INTERFACE UI;

TYPE
  T <: PublicT;
  PublicT = OBJECT
  METHODS
    init():T;
    run();
    exit();
    (*---std messages--------*)
    msg(txt:TEXT);
    warn(txt:TEXT);
    err(txt:TEXT);
    info(txt:TEXT);
    working(txt:TEXT);
  END;

VAR (*const after init*)
  ui:T;
END UI.

