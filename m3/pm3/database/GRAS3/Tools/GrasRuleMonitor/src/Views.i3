INTERFACE Views;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:28:32  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

PROCEDURE AddView();
PROCEDURE RemoveView();
PROCEDURE Number(): CARDINAL;
  
END Views.
