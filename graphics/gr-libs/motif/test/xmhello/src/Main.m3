(*Public Domain*)

MODULE Main;
IMPORT AppModel,UI;

BEGIN
  AppModel.app:=NEW(AppModel.T).init();
  UI.ui:=NEW(UI.T).init();
  UI.ui.run();
END Main.
