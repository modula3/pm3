(*Public Domain*)

MODULE AppModel;

IMPORT Pathname,File,FS;

IMPORT UI;


REVEAL
  T = PublicT BRANDED OBJECT
    filename:Pathname.T;
    file:File.T;
    label_ndx:=0;
  OVERRIDES
    init:=Init;
    exit:=Exit;

    file_new:=FileNew;
    file_open:=FileOpen;
    file_save:=FileSave;
    file_saveas:=FileSaveas;
    file_print:=FilePrint;
    cut:=Cut;
    copy:=Copy;
    paste:=Paste;
    help:=Help;
    hello:=Hello;
    goodbye:=Goodbye;
  END;

(*--------------------*)
PROCEDURE Init(self:T):T=
BEGIN
  RETURN self;
END Init;  

(*--------------------*)
PROCEDURE Exit(self:T):BOOLEAN=
BEGIN
  RETURN TRUE;
END Exit;  

(*---file actions---*)
(*------------------*)
PROCEDURE FileNew(self:T)=
BEGIN
END FileNew;
(*------------------*)
PROCEDURE FileOpen(self:T ;path:Pathname.T)=
BEGIN
END FileOpen;
(*------------------*)
PROCEDURE FileSave(self:T)=
BEGIN
END FileSave;
(*------------------*)
PROCEDURE FileSaveas(self:T  ;path:Pathname.T)=
BEGIN
END FileSaveas;
(*------------------*)
PROCEDURE FilePrint(self:T)=
BEGIN
END FilePrint;

(*---edit actions---*)
(*-----------------*)
PROCEDURE Cut(self:T)=
BEGIN
END Cut;
(*-----------------*)
PROCEDURE Copy(self:T)=
BEGIN
END Copy;
(*-----------------*)
PROCEDURE Paste(self:T)=
BEGIN
END Paste;

(*---help actions---*)
(*------------------*)
PROCEDURE Help(self:T)=
BEGIN
END Help;

(*---toolbar actions---*)
(*------------------*)
PROCEDURE Hello(self:T):TEXT=
CONST 
  labels = ARRAY OF TEXT {"Hello","Good Day","Bonjour","Buenos Dias"};
VAR
  len:=NUMBER(labels);   
BEGIN
  IF self.label_ndx >= len-1 THEN
    self.label_ndx := 0;
  ELSE
    INC(self.label_ndx);
  END;
  RETURN labels[self.label_ndx];
 
END Hello;
(*------------------*)
PROCEDURE Goodbye(self:T)=
BEGIN
  EVAL self.exit();
END Goodbye;
(*============================*)
BEGIN
END AppModel.
