INTERFACE CommunicationServer;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.5  1996/11/21 07:55:37  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.4  1996/11/14 14:13:25  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.3  1996/08/06 16:32:20  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/07/24 12:52:44  rbnix
    	New parameter clientInfo added.

    Revision 1.2  1996/03/15 14:22:23  rbnix
    	In method init/open attribute clientID added.

    Revision 1.1  1996/02/26 17:58:15  rbnix
    	First version of subsystem ServerControl.

*)
(***************************************************************************)

(*
 | --- CommunicationServer ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT CommunicationPort AS Super;
IMPORT
  Pathname,
  PageFile,
  Access, 
  CallbackPort, ClientInfo;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         baseName	:Pathname.T;
                                  access	:Access.Mode;
                                  new		:BOOLEAN;
			          callback	:CallbackPort.T;
                                  clientInfo	:ClientInfo.T;
                         VAR (* out *) clientID	:TEXT)
			:T
			RAISES {PageFile.NoAccess, Access.Denied,
                                Access.Invalid};
    END;

END CommunicationServer.
