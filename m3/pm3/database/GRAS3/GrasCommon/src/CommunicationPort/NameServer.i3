INTERFACE NameServer;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.3  1998/01/30 18:28:39  roland
    New method cleanup to remove dead servers from the list.

    Revision 1.2  1997/03/26 16:06:13  renehuel
    Changed some identifiers to match naming conventions.
    Removed some minor bugs.
    Added OwnerID to server identification in list and method to read all
    attributes.

    Revision 1.1  1997/03/20 16:43:30  renehuel
    These files implement the new gras nameserver.
    This server is used to keep a list of running gras servers, and
    lets the clients choose one to which they want to connect.
    This means that by now it is possible to run more than one gras-server.

*)
(***************************************************************************)

IMPORT NetObj;
IMPORT Atom, Thread, EntryPort;

CONST GrasNameServerID = "GRAS_NAMESERVER";

VAR EntryExists: Atom.T;

TYPE
  (* Type of the return parameter of method listallservers *)

  AllServerList = REF ARRAY OF
                        RECORD
                          ID     : TEXT;
                          OwnerID: INTEGER;
                        END;
  ServerInfo = RECORD OwnerID: INTEGER;  END;
  T =
    NetObj.T OBJECT
    METHODS

      (* Returns a pointer to an array of text which contains the IDs of
         all currently known Gras-servers. *)
      listallservers (): AllServerList
                      RAISES {NetObj.Error, Thread.Alerted};

      (* Adds the server with the id ID and the handle Handle to the list.
         Raises the exception NoNameServer, if no nameserver is currently
         running. *)
      addserver (ID: TEXT; Handle: EntryPort.T; OwnerID: INTEGER := 0)
                 RAISES {Thread.Alerted, NetObj.Error,
                         InvalidServerIdentification, ServerAlreadyInList};

      (* Removes the server with the id ID from the List.  Raises the
         exception ServerNotInList if the server with the id ID is not in
         the list.*)
      removeserver (ID: TEXT)
                    RAISES {NetObj.Error, Thread.Alerted, ServerNotInList,
                            InvalidServerIdentification};

      (* Returns an handle to the server with the id ID.  If this server is
         not in the list, an exception is raised. *)
      getserver (ID: TEXT): EntryPort.T
                 RAISES {NetObj.Error, Thread.Alerted, ServerNotInList,
                         InvalidServerIdentification};

      (* Returns TRUE if the server with the id ID is in the list otherwise
         FALSE. *)
      serverinlist (ID: TEXT): BOOLEAN
                    RAISES {NetObj.Error, Thread.Alerted,
                            InvalidServerIdentification};

      (* Ping all registered servers and remove server which are not
         responding from the list. *)
      cleanup () RAISES {NetObj.Error, Thread.Alerted};


      (* Tries to shut down a running name server and returns the boolean
         value indicating the success of this operation.*)
      shutdown (): BOOLEAN RAISES {NetObj.Error, Thread.Alerted};

      (* Returns a record with the information to a specific server from
         the list. *)
      getserverinfo (ID: TEXT): ServerInfo
                     RAISES {NetObj.Error, Thread.Alerted,
                             InvalidServerIdentification};

    END;

EXCEPTION ServerNotInList;
EXCEPTION InvalidServerIdentification;
EXCEPTION ServerAlreadyInList;
EXCEPTION NoNameServer;

END NameServer.
