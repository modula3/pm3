MODULE PoolView EXPORTS Main;

(***************************************************************************)
(** Created by:  Markus Kluck						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.3  1998/09/14 16:11:27  roland
    Man page added.

    Revision 1.2  1998/09/03 11:03:04  kluck
    Selection of displayed Collections and Relations now possible via icon.

    Revision 1.1  1998/08/13 11:48:29  kluck
    Poolviewer using DaVinci for graphical display.

*)
(***************************************************************************)

(****    View the contents of a pool with DaVinci    ****)

IMPORT Thread, IO, Process, Stdio, Pathname, Text, GrasParams, Rsrc,
       TempFiles, FileWr, Rd, Wr, FS, ResourceBundle, OSError, Fmt,
       OSErrorPosix;
IMPORT ParseParams, VirtualResource, VirtualResourceSystem, Access,
       PageFile, Names, TextCursorSet, TextIdSet;
IMPORT DaVinci, DaVinciMsg;
IMPORT Globals, DaVinciGraph;


CONST
  SelectionIcon = "Selection";
  SelectionFn   = "Selection.xbm";
  SelectionDescription = "Select relations and collections to be displayed";

VAR
  rootPath            : TEXT;
  poolname            : TEXT;
  nameserver, serverId: TEXT;
  cacheSize           : CARDINAL;
  debug               : BOOLEAN  := FALSE;


(*----- Get command line parameters ----------------------------*)
PROCEDURE ReadParams () =
  CONST
    USAGE = " <pool> {-sc <collections>} {-hc <collection>}\n" &
      "{-sr <relation>} {-hr <relation>} [-local] -root <rootPath>";

  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      IO.Put("\n" & err & "\n");
      IO.Put("USAGE: " & prog & USAGE & "\n");
      Process.Exit(1);
    END ParseError;

  VAR
    valid: BOOLEAN;
    coll : TEXT    := "";
    rel  : TEXT    := "";
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        poolname := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Pool name is mandatory first parameter");
      END;
      IF Text.Equal("-root", poolname) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Pool name is mandatory first parameter");
      END;

      (*--- local entries ? *)
      IF pp.keywordPresent("-local") THEN Globals.local := TRUE END;

      (*--- print debug info ? *)
      IF pp.keywordPresent("-debug") THEN Globals.debug := TRUE END;

      (*--- scan for collections to show *)
      WHILE pp.keywordPresent("-sc") DO
        TRY
          coll := pp.getNext();
          Globals.visibleCollections.insert(coll);
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "-sc must be followed by one collections.");
        END;
      END;

      (*--- scan for collections to hide *)
      WHILE pp.keywordPresent("-hc") DO
        TRY
          coll := pp.getNext();
          Globals.invisibleCollections.insert(coll);
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "-hc must be followed by one collections.");
        END;
      END;

      (*--- scan for relations to show *)
      WHILE pp.keywordPresent("-sr") DO
        TRY
          rel := pp.getNext();
          Globals.visibleRelations.insert(rel);
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "-sr must be followed by one relation.");
        END;
      END;

      (*--- scan for relations to hide *)
      WHILE pp.keywordPresent("-hr") DO
        TRY
          rel := pp.getNext();
          Globals.invisibleRelations.insert(rel);
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "-hr must be followed by one relation.");
        END;
      END;

      (*--- look for root values *)
      GrasParams.ParseComandLine(
        rootPath, valid, cacheSize, serverId, nameserver);
      IF NOT valid THEN
        ParseError(Pathname.Last(pp.arg[0]), "Need root path.");
      END;
    END;
  END ReadParams;


(*------------------------------------*)
(*--- creating a tempfile resource ---*)
(*------------------------------------*)

PROCEDURE ResourceToTempFile (ResName: TEXT): TEXT =
  VAR
    path: Rsrc.Path := Rsrc.BuildPath(".", ResourceBundle.Get());
    res : TEXT;
    wr  : Wr.T;
    fn  : TEXT;
  BEGIN
    TRY
      (*--- get the resource from the bundle as TEXT *)
      res := Rsrc.Get(ResName, path);

      (*--- write THIS resource to a tempfile *)
      fn := TempFiles.Get();
      TempFiles.Note(fn);

      wr := NEW(FileWr.T).init(FS.OpenFile(fn));
      Wr.PutText(wr, res);
      Wr.Flush(wr);
      Wr.Close(wr);
    EXCEPT
    | Rd.Failure =>
        IO.Put(
          "Rd.Failure: Error while using writer for \"" & fn & "\" !\n");
    | Rsrc.NotFound =>
        IO.Put("Resource \"" & ResName & "\" not found !\n");
    | Thread.Alerted => IO.Put("Thread.Alerted !\n");
    | OSError.E (e) =>
        IO.Put("Cannot create tempfile \"" & fn & "\" : errno = "
                 & Fmt.Int(OSErrorPosix.AtomToErrno(e.head)));
    | Wr.Failure =>
        IO.Put(
          "Wr.Failure: Error while using writer for \"" & fn & "\" !\n");
    END;

    RETURN fn;
  END ResourceToTempFile;


(*-------------------------------------------*)
(*--- Procedures to interact with DaVinci ---*)
(*-------------------------------------------*)

PROCEDURE Quit (<* UNUSED *> handler: DaVinci.EventHandler;
                <* UNUSED *> type   : DaVinci.MsgType;
                <* UNUSED *> msg    : TEXT                  ) =
  BEGIN
    Globals.viewer.quit();
    Thread.Signal(Terminate);
  END Quit;


(*--------------------*)
(*--- Main program ---*)
(*--------------------*)
VAR
  msg      : DaVinciMsg.T;
  Terminate               := NEW(Thread.Condition);
  Lock                    := NEW(MUTEX);
  quitter                 := NEW(DaVinci.EventHandler, notify := Quit);
  iconSelection := NEW(DaVinci.EventHandler,
                       notify := DaVinciGraph.IconSelection);
  printer := NEW(DaVinci.EventHandler, notify := DaVinciGraph.MsgPrinter);
  TempFn: TEXT := "";

BEGIN
  (*--- declare global collections *)
  Globals.declaredCollections := TextIdSet.New();
  Globals.visibleCollections := TextCursorSet.New();
  Globals.invisibleCollections := TextCursorSet.New();
  Globals.declaredRelations := TextIdSet.New();
  Globals.visibleRelations := TextCursorSet.New();
  Globals.invisibleRelations := TextCursorSet.New();

  ReadParams();
  IF debug THEN IO.Put("Displaying pool " & poolname & "\n"); END;

  TRY
    VirtualResourceSystem.Login(rootPath, cacheSize, serverId, nameserver);

    (*--- create a new virtual resource *)
    Globals.virtualResource :=
      NEW(VirtualResource.T).open(
        poolname, Access.Mode.ReadWriteShared, new := FALSE);

    (*--- With the new VirtualResource one can log into names *)
    Globals.virtualResource.beginTransaction();
    Globals.names := NEW(Names.T);
    Globals.names.login(Globals.virtualResource, ".GRAS");
    Globals.virtualResource.commitTransaction();

    (*--- get all possible collections and relations *)
    Globals.virtualResource.beginTransaction();
    Globals.declaredCollections := Globals.names.getAllCollections();
    Globals.declaredRelations := Globals.names.getAllRelations();
    Globals.virtualResource.commitTransaction();

    (*--- convert to TextCursorSet *)
    Globals.declaredCollectionsText :=
      Globals.IdToTextCursorSet(Globals.declaredCollections);
    Globals.declaredRelationsText :=
      Globals.IdToTextCursorSet(Globals.declaredRelations);

    (*--- eliminate collections and relations not declared *)
    Globals.visibleCollections.intersection(
      Globals.declaredCollectionsText);
    Globals.invisibleCollections.intersection(
      Globals.declaredCollectionsText);
    Globals.visibleRelations.intersection(Globals.declaredRelationsText);
    Globals.invisibleRelations.intersection(Globals.declaredRelationsText);

    IF Globals.visibleCollections.isEmpty() THEN
      Globals.visibleCollections := Globals.declaredCollectionsText.copy();
    END;
    Globals.visibleCollections.difference(Globals.invisibleCollections);

    IF Globals.visibleRelations.isEmpty() THEN
      Globals.visibleRelations := Globals.declaredRelationsText.copy();
    END;
    Globals.visibleRelations.difference(Globals.invisibleRelations);

    IF debug THEN
      Globals.PrintTextIdSet("Content of Globals.declaredCollections\n",
                             Globals.declaredCollections);
      Globals.PrintTextCursorSet("Content of Globals.visibleCollections\n",
                                 Globals.visibleCollections);
      Globals.PrintTextCursorSet(
        "Content of Globals.invisibleCollections\n",
        Globals.invisibleCollections);
      Globals.PrintTextIdSet("Content of Globals.declaredRelations\n",
                             Globals.declaredRelations);
      Globals.PrintTextCursorSet(
        "Content of Globals.visibleRelations\n", Globals.visibleRelations);
      Globals.PrintTextCursorSet("Content of Globals.invisibleRelations\n",
                                 Globals.invisibleRelations);
    END;

    (*--- initiate a new DaVinci process *)
    Globals.viewer := NEW(DaVinci.T).init();

    (*--- we want root entries on top *)
    Globals.viewer.send("menu(layout(orientation(top_down)))\n");

    (*--- insert further menus & icons *)
    TempFn := ResourceToTempFile(SelectionFn);
    msg := NEW(DaVinciMsg.T).init();
    Globals.viewer.send(
      msg.insertIcon(SelectionIcon, TempFn, SelectionDescription));
    Globals.viewer.send(msg.activateIcon(SelectionIcon));

    (*--- register DaVinci's method handlers *)
    Globals.viewer.registerHandler(DaVinci.MsgType.Quit, quitter);
    Globals.viewer.registerHandler(
      DaVinci.MsgType.IconSelection, iconSelection);
    IF Globals.debug THEN
      FOR t := FIRST(DaVinci.MsgType) TO LAST(DaVinci.MsgType) DO
        Globals.viewer.registerHandler(t, printer);
      END;
    END;

    (*--- display the contents of NAMES *)
    Globals.virtualResource.beginTransaction();
    DaVinciGraph.View(Globals.virtualResource, Globals.names,
                      Globals.viewer, Globals.local, Globals.debug);
    Globals.virtualResource.commitTransaction();

    LOCK Lock DO Thread.Wait(Lock, Terminate); END;

    (*--- logout *)
    Globals.names.logout();
    Globals.virtualResource.close();

    (*--- dispose sets *)
    Globals.declaredCollections.dispose();
    Globals.declaredCollectionsText.dispose();
    Globals.declaredRelations.dispose();
    Globals.declaredRelationsText.dispose();
    Globals.visibleCollections.dispose();
    Globals.invisibleCollections.dispose();
    Globals.visibleRelations.dispose();
    Globals.invisibleRelations.dispose();
  EXCEPT
  | DaVinci.Error (msg) => IO.Put(msg & "\n");
  | Access.Denied (msg) => IO.Put("Main: Access denied: " & msg & "\n");
  | PageFile.NoAccess (msg) => IO.Put("Main: No access: " & msg & "\n");
  | VirtualResource.FatalError =>
      Globals.ErrorAbort(Globals.virtualResource);
      IO.Put("View: VirtualResource.FatalError!\n");
  | VirtualResource.NotInTransaction =>
      Globals.ErrorAbort(Globals.virtualResource);
      IO.Put("View: VirtualResource.NotInTransaction!\n");
  | Access.Locked => IO.Put("View: Access.Locked!\n");
  | Names.InternalError => Globals.ErrorAbort(Globals.virtualResource);
  END;
END PoolView.
