(*
* TrackerPosition.m3 -- a tracker position object
* Copyright (C) Blair MacIntyre 1995
* Author          : Blair MacIntyre
* Created On      : Thu Jan 19 17:38:38 1995
* Last Modified By: Blair MacIntyre
* Last Modified On: Sun Jul 30 23:58:20 1995
* Update Count    : 87
* Status          : Unknown, Use with caution!
*
* $Source$
* $Date$
* $Author$
* $Revision$
*
* $Log$
* Revision 1.1  2003/04/08 22:00:51  hosking
* Merge of PM3 with Persistent M3 and CM3 release 5.1.8
*
* Revision 1.1.3.1  2003/01/26 00:04:12  hosking
* Import of CM3 5.1.8
*
* Revision 1.1.1.1  2001/12/02 13:14:14  wagner
* Blair MacIntyre's sharedobj package
*
* Revision 1.1.1.1  1996/03/03 19:20:26  bm
* Imported Sources
*
*
* HISTORY
*)

MODULE TrackerPosition EXPORTS TrackerPosition, TrackerPositionF;

PROCEDURE Set (self: S; READONLY val: Data) =
  BEGIN
    self.data := val;
  END Set;

PROCEDURE Get (self: S; VAR val: Data) =
  BEGIN
    val := self.data;
  END Get;

BEGIN
END TrackerPosition.
