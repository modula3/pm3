INTERFACE VirtualPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.5  1997/11/19 17:59:43  roland
    Removed grouping of page accesses.

    Revision 1.4  1997/09/18 08:23:32  roland
    Grouping of access to the same page.

    Revision 1.3  1996/12/03 09:49:24  roland
    Replaced Type.Triple with CARDINAL. Special handling in put/getTriple
    of VirtualPage.

    Revision 1.2  1996/11/18 17:52:21  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.1  1996/02/29 17:44:24  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualPage --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Type,
  PageData,
  Access;
IMPORT
  AtomList;

CONST
  Brand			= "VirtualPage";

  
TYPE
  T			<: Public;

  Public		= OBJECT
    METHODS
      putByte		(         pos   	:PageData.Index;
                                  value		:Type.Byte)
			RAISES {Access.Locked, FatalError};

      getByte		(         pos   	:PageData.Index)
			:Type.Byte
			RAISES {Access.Locked, FatalError};


      putShort		(         pos   	:PageData.Index;
                                  value		:Type.Short)
			RAISES {Access.Locked, FatalError};

      getShort		(         pos   	:PageData.Index)
			:Type.Short
			RAISES {Access.Locked, FatalError};


      putTriple		(         pos   	:PageData.Index;
                                  value		:CARDINAL (* Type.Triple *))
			RAISES {Access.Locked, FatalError};

      getTriple		(         pos   	:PageData.Index)
			: CARDINAL (* Type.Triple *)
			RAISES {Access.Locked, FatalError};


      putInt		(         pos   	:PageData.Index;
                                  value		:INTEGER)
			RAISES {Access.Locked, FatalError};

      getInt		(         pos   	:PageData.Index)
			:INTEGER
			RAISES {Access.Locked, FatalError};


      putWord		(         pos   	:PageData.Index;
                                  value		:Type.Word)
			RAISES {Access.Locked, FatalError};

      getWord		(         pos   	:PageData.Index)
			:Type.Word
			RAISES {Access.Locked, FatalError};


      putArray		(         pos   	:PageData.Index;
                         READONLY value		:PageData.Part)
			RAISES {Access.Locked, FatalError};

      getArray		(         pos   	:PageData.Index;
			 VAR      value		:PageData.Part)
			RAISES {Access.Locked, FatalError};

      copyArray		(         source 	:PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index)
			RAISES {Access.Locked, FatalError};


      putText		(         pos   	:PageData.Index;
                         READONLY value		:TEXT)
			RAISES {Access.Locked, FatalError};

      getText		(         pos   	:PageData.Index;
                                  length	:PageData.Index)
			:TEXT
			RAISES {Access.Locked, FatalError};


      putAll		(READONLY value		:PageData.T)
			RAISES {Access.Locked, FatalError};

      getAll		()
			:PageData.T
			RAISES {Access.Locked, FatalError};

    END;

EXCEPTION
  FatalError(AtomList.T);
  
END VirtualPage.
