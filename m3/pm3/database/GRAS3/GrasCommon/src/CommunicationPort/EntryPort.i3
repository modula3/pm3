INTERFACE EntryPort;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.9  1997/03/20 16:43:29  renehuel
    These files implement the new gras nameserver.
    This server is used to keep a list of running gras servers, and
    lets the clients choose one to which they want to connect.
    This means that by now it is possible to run more than one gras-server.

    Revision 1.8  1996/11/21 07:53:46  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.7  1996/11/14 14:12:26  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.6  1996/11/08 14:45:42  roland
    GetResources in ServerScheduler handles PageFile.NoAccess correct now.

    Revision 1.5  1996/08/06 16:25:00  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.2  1996/08/01 18:12:14  rbnix
        New resource administration methods for remote resource parts
        added: deleteResource, copyResource, renameResource,
        existsResource, resourceInUse and getResources.

    Revision 1.4.2.1  1996/07/24 12:48:12  rbnix
        In method open: parameter clientInfo added.

    Revision 1.4  1996/03/15 14:22:53  rbnix
        In method open attribute clientID added.

    Revision 1.3  1996/02/29 09:24:13  rbnix
        Method shutdown to terminate server added.

    Revision 1.2  1996/02/26 17:56:14  rbnix
        Exception Access.Invalid added.

    Revision 1.1  1996/02/09 16:43:00  rbnix
        First version of specification layer for network objects
        added.

*)
(***************************************************************************)

(*
 | --- EntryPort ----------------------------------------------------------
 The abstract data type EntryPort specifies a common known communication
 interface exported by the server possibly retrieved by a name service.

 The method open establishes a bidirectional communication path through the
 given CallbackPort and the returned CommunicationPort. The manipulated
 ressources shares the attributes baseName, access.
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, TextSeq, Thread, NetObj, PageFile, Access, Termination,
       CommunicationPort, CallbackPort, ClientInfo, ClientInfoSeq;

TYPE
  T = NetObj.T BRANDED "EntryPort" OBJECT
      METHODS
        (* resource access *)
        open (              baseName  : Pathname.T;
                            access    : Access.Mode;
                            new       : BOOLEAN;
                            callback  : CallbackPort.T;
                            clientInfo: ClientInfo.T;
              VAR (* out *) clientID  : TEXT            ):
              CommunicationPort.T
              RAISES {Thread.Alerted, NetObj.Error, PageFile.NoAccess,
                      Access.Invalid, Access.Denied};


        (* resource administration *)
        deleteRemoteResource (baseName: Pathname.T)
                              RAISES {Thread.Alerted, NetObj.Error,
                                      PageFile.NoAccess};

        copyRemoteResource (sourceName: Pathname.T; destName: Pathname.T)
                            RAISES {Thread.Alerted, NetObj.Error,
                                    PageFile.NoAccess};

        renameRemoteResource (oldName: Pathname.T; newName: Pathname.T)
                              RAISES {Thread.Alerted, NetObj.Error,
                                      PageFile.NoAccess};

        existsRemoteResource (baseName: Pathname.T): BOOLEAN
                              RAISES {Thread.Alerted, NetObj.Error};

        remoteResourceInUse (baseName: Pathname.T): BOOLEAN
                             RAISES {Thread.Alerted, NetObj.Error};

        getRemoteResourceUser (baseName: Pathname.T): ClientInfoSeq.T
                               RAISES {Thread.Alerted, NetObj.Error};

        getRemoteResources (): TextSeq.T
                            RAISES {Thread.Alerted, NetObj.Error,
                                    PageFile.NoAccess};


        (* server control *)
        shutdown (termination: Termination.Mode)
                  RAISES {Thread.Alerted, NetObj.Error,
                          Termination.StillInUse};

        ping () RAISES {NetObj.Error, Thread.Alerted};
      END;

END EntryPort.
