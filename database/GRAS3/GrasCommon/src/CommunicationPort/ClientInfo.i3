INTERFACE ClientInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

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
      <*TRANSIENT*>
      username: TEXT;
      userid  : INTEGER;
      <*TRANSIENT*>
      hostname: TEXT;
      pid     : Process.ID;
    END;


END ClientInfo.
