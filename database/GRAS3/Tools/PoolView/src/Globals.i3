INTERFACE Globals;

(***************************************************************************)
(** Created by:  Markus Kluck						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1998/09/03 11:07:32  kluck
    Further modules used by PoolView to implement selection.

*)
(***************************************************************************)

IMPORT VirtualResource, Names;
IMPORT TextIdSet, TextCursorSet;
IMPORT DaVinci;

VAR
  viewer                 : DaVinci.T;
  names                  : Names.T;
  virtualResource        : VirtualResource.T;
  local                  : BOOLEAN           := FALSE;
  debug                  : BOOLEAN           := FALSE;
  declaredCollections    : TextIdSet.T;
  declaredCollectionsText: TextCursorSet.T;
  visibleCollections     : TextCursorSet.T;
  invisibleCollections   : TextCursorSet.T;
  declaredRelations      : TextIdSet.T;
  visibleRelations       : TextCursorSet.T;
  invisibleRelations     : TextCursorSet.T;
  declaredRelationsText  : TextCursorSet.T;


PROCEDURE PrintTextCursorSet (msg: TEXT; set: TextCursorSet.T);
PROCEDURE PrintTextIdSet (msg: TEXT; set: TextIdSet.T);
PROCEDURE IdToTextCursorSet (IdSet: TextIdSet.T): TextCursorSet.T;
PROCEDURE ErrorAbort (res: VirtualResource.T);

END Globals.
