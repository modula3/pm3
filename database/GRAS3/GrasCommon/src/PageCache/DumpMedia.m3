MODULE DumpMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:04:34  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

FROM Stdio IMPORT stdout;
IMPORT
  Thread, Wr, Fmt,
  PageHandle;

<* FATAL Thread.Alerted, Wr.Failure *>

REVEAL
  T                     = Public BRANDED OBJECT
    OVERRIDES
      loadData		:= LoadData;
      dropData		:= DropData;
    END;


PROCEDURE LoadData      (        <* UNUSED *>
				 self            :T;
                                 handle		:PageHandle.T) =
  BEGIN
    Wr.PutText (stdout,
                "DummyMedia.LoadData (" &
                Fmt.Int (handle.getPageNo ()) &
                ")\n");
  END LoadData;

  
PROCEDURE DropData      (        <* UNUSED *>
				 self		:T;
                                 handle		:PageHandle.T) =
  BEGIN
    Wr.PutText (stdout,
                "DummyMedia.DropPage (" &
                Fmt.Int (handle.getPageNo ()) &
                ", changed = " &
                Fmt.Bool (handle.isChanged ()) &
                ")\n");
  END DropData;


BEGIN
END DumpMedia.
