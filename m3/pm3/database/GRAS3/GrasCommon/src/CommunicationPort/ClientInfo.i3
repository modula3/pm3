INTERFACE ClientInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/11/21 07:53:44  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

*)
(***************************************************************************)

IMPORT Process;

CONST
  Brand = "Client-Info";

TYPE
  T =
    RECORD
      username: TEXT;
      userid  : INTEGER;
      hostname: TEXT;
      pid     : Process.ID;
    END;


END ClientInfo.
