(*Public Domain*)

UNSAFE INTERFACE Toolbar;
IMPORT Xt;

VAR (*const after init*)
  toolbar,
    hello,goodbye:Xt.Widget;
        
PROCEDURE init(parent:Xt.Widget);

END Toolbar.
