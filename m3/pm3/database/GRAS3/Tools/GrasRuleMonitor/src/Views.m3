MODULE Views;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:28:33  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

VAR Num: CARDINAL := 0;

PROCEDURE AddView() = BEGIN INC(Num) END AddView;
PROCEDURE RemoveView() = BEGIN DEC(Num) END RemoveView;
PROCEDURE Number(): CARDINAL = BEGIN RETURN Num END Number;

BEGIN
END Views.
