MODULE BasePageHandle EXPORTS BasePageHandle, InternalBasePageHandle;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.4  1996/03/08 10:29:45  rbnix
    	PageHandles are now tagged by an internal id. Therefore the
    	method init is introduced and the method fmt is enhanced.

    Revision 1.3  1996/03/06 14:00:58  rbnix
    	New method fmt added to get a formatted representation of the
    	handle's value.

    Revision 1.2  1996/02/29 17:40:01  rbnix
    	New methods getAll and copyData added.

    Revision 1.1  1996/01/31 10:04:30  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- BasePageHandle -----------------------------------------------------
 * Access to the page's data will be redirected while write access is
 * recognized.
 * ------------------------------------------------------------------------
 *)
IMPORT Fmt AS StdFmt;
IMPORT
  PageData, Page;

REVEAL
  T                     = Internal BRANDED OBJECT
      id		:CARDINAL := 0;
      page		:Page.T;
      changed		:BOOLEAN;

    OVERRIDES
      init		:= Init;

      isLoad		:= IsLoad;

      isChanged		:= IsChanged;
      markChanged	:= MarkChanged;
      unmarkChanges     := UnmarkChanges;

      setPage		:= SetPage;
      getPage		:= GetPage;

      putData		:= PutData;
      getData		:= GetData;
      putAll		:= PutAll;
      getAll		:= GetAll;
      copyData		:= CopyData;

      fmt		:= Fmt;
    END;


VAR
  lastId		:= 0;


PROCEDURE Init		(        self		:T) :T =
  BEGIN
    INC (lastId);
    self.id := lastId;
    self.page := NIL;
    self.changed := FALSE;

    RETURN self;
  END Init;


PROCEDURE IsLoad	(        self		:T) :BOOLEAN =
  BEGIN
    RETURN (self.page # NIL);
  END IsLoad;


PROCEDURE IsChanged	(        self		:T) :BOOLEAN =
  BEGIN
    RETURN (self.changed);
  END IsChanged;


PROCEDURE MarkChanged	(        self		:T) =
  BEGIN
    self.changed := TRUE;
  END MarkChanged;


PROCEDURE UnmarkChanges	(        self		:T) =
  BEGIN
    self.changed := FALSE;
  END UnmarkChanges;


PROCEDURE SetPage	(        self		:T;
                                 page		:Page.T) =
  BEGIN
    self.page := page;
  END SetPage;


PROCEDURE GetPage	(        self		:T) :Page.T =
  BEGIN
    RETURN self.page;
  END GetPage;

  
PROCEDURE PutData	(         self		:T;
			 READONLY data		:PageData.Part;
			          pos		:= FIRST (PageData.Index)) =
  BEGIN
    self.page.putData (data, pos);
    self.changed := TRUE;
  END PutData;
                                 
                                 
PROCEDURE GetData	(         self		:T;
			 VAR      data		:PageData.Part;
			          pos		:= FIRST (PageData.Index)) =
  BEGIN
    self.page.getData (data, pos);
  END GetData;


PROCEDURE PutAll	(         self		:T;
                                  unswizzler    :PageData.Unswizzler) =
  BEGIN
    unswizzler(self.page.data);
    self.changed := TRUE;
  END PutAll;


PROCEDURE GetAll	(         self		:T;
                                  swizzler      :PageData.Swizzler) =
  BEGIN
    swizzler(self.page.data);
  END GetAll;


PROCEDURE CopyData	(         self		:T;
                                  source	:PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index) =
  BEGIN
    self.page.copyData (source, destination, length);
    self.changed := TRUE;
  END CopyData;


PROCEDURE Fmt		(         self		:T) :TEXT =
  BEGIN
    RETURN   "id = " & StdFmt.Int (self.id) &
           ", load = " & StdFmt.Bool (self.isLoad ()) &
           ", changed = " & StdFmt.Bool (self.isChanged ());           
  END Fmt;

BEGIN
END BasePageHandle.
