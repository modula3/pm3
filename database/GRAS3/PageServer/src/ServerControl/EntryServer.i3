INTERFACE EntryServer;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.6  1997/03/26 16:12:08  renehuel
    A new pageserver now logs in to the nameserver with his id, handle
    and new : the id of the user who started it.

    Revision 1.5  1997/03/20 16:55:25  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.4  1996/11/20 12:11:11  roland
    Improved exception handling and startup.

    Revision 1.3  1996/08/06 16:32:23  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/04/29 14:22:59  roland
    ExceptionHandling and error messages improved.

    Revision 1.2  1996/03/01 13:38:27  rbnix
        New method EntryServer.T.exit to remove port.

    Revision 1.1  1996/02/26 17:58:20  rbnix
        First version of subsystem ServerControl.

*)
(***************************************************************************)

(*
 | --- EntryServer --------------------------------------------------------
  This specialized data type module offers an entry port to clients.
 | ------------------------------------------------------------------------
 *)
IMPORT EntryPort AS Super;
IMPORT NetObj, Thread, NameServer;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             init (ID: TEXT; UserID : INTEGER := 0): T
                   RAISES {NetObj.Error, NetObj.Invalid, Thread.Alerted,
                           NameServer.InvalidServerIdentification,
                           NameServer.ServerAlreadyInList,
                           NameServer.NoNameServer};

             exit (ID: TEXT)
                   RAISES {NetObj.Error, NetObj.Invalid, Thread.Alerted};

           END;

END EntryServer.
