GENERIC MODULE Stream(Element);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.4  1997/10/17 16:48:58  renehuel
    bugfix in stream write operation.

    Revision 1.3  1997/07/07 15:35:36  roland
    Reduced number of page accesses due to specialized load and store
    procedures for position and small entries.

    Revision 1.2  1997/04/24 14:30:43  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:33  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:21:21  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/20 13:59:12  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:58:09  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* The first page of a stream is used as index page.  The first bytes of
   this page are used to store the first and last block positions of the
   stream.  In a forward stream the first position is always right after
   these two informations, so it can be used to determine if the stream is
   a forward or backward stream (this is important for applying a delta).
   For this purpose the the first position in a forward stream is always
   (0,0) (instead of (1, 2*FilePos.TSize) what would be the real position
   of the first block). *)

IMPORT FilePos;
IMPORT Pathname, Word;
IMPORT VirtualRemoteFile, VirtualResource, Access, VirtualPage, PageFile,
       PageData, Type;
IMPORT ErrorSupport;

TYPE StreamPosition = {Start, End, Current};

CONST
  IndexPageNo = 0;

  EndPos = ARRAY BOOLEAN OF
             StreamPosition{StreamPosition.Start, StreamPosition.End};
  (* For forward streams, the technical end of the stream (i.e.  the
     position that was written last) is stored in its logical end position.
     For backward streams, its the start position. *)

  StreamPosSize = BYTESIZE(CARDINAL) + BYTESIZE(Type.Short);
  (* raw size of a FilePos.T record *)

  PageStart = FIRST(FilePos.FileIndex);
  (* position 'before' first page entry on normal page *)

  PageOneStart = PageStart + NUMBER(PositionOffset) * StreamPosSize;
  (* position before FIRST page entry on page 1 *)

  PositionOffset = ARRAY StreamPosition OF
                     FilePos.FileIndex{
                     PageStart, PageStart + StreamPosSize,
                     PageStart + 2 * StreamPosSize};
(* PositionOffset holds the offsets into the first page of a stream where
   its start, end and current position are written. *)



REVEAL
  T = Public BRANDED OBJECT
        file      : VirtualRemoteFile.T;
        dirForward: BOOLEAN;
        indexP    : VirtualPage.T;
      OVERRIDES
        open             := OpenStream;
        close            := CloseStream;
        save             := SaveStream;
        endOfStream      := EndOfStream;
        getPosition      := GetPosition;
        setPosition      := SetPosition;
        truncate         := Truncate;
        getLastPosition  := GetLastPosition;
        getFirstPosition := GetFirstPosition;
        forward          := Forward;
        backward         := Backward;
        write            := Write;
        read             := Read;
      END;

TYPE PosArray = ARRAY [0 .. StreamPosSize - 1] OF Type.Byte;

PROCEDURE PosToByteArray (READONLY Pos: FilePos.T; VAR A: PosArray) =
  VAR page: CARDINAL := Pos.page;
  BEGIN
    A[0] := page MOD 256;
    page := page DIV 256;
    A[1] := page MOD 256;
    page := page DIV 256;
    A[2] := page MOD 256;
    A[3] := page DIV 256;
    A[4] := Pos.pPos MOD 256;
    A[5] := Pos.pPos DIV 256;
  END PosToByteArray;

PROCEDURE ByteArrayToPos (READONLY A: PosArray; VAR Pos: FilePos.T) =
  BEGIN
    Pos.page := 256 * A[3] + A[2];
    Pos.page := Pos.page * 16_10000 + 256 * A[1] + A[0];
    Pos.pPos := 256 * A[5] + A[4];
  END ByteArrayToPos;

PROCEDURE SavePos (stream: T; which: StreamPosition; Pos: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  (* Write Pos on page 1 of the stream.  which determines if current,
     start, or end position will be written. *)
  VAR array: PosArray;
  BEGIN
    TRY
      PosToByteArray(Pos, array);
      stream.indexP.putArray(PositionOffset[which] + 1, array);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Stream.SavePos", "VirtualPage.FatalError", info));
    END;
  END SavePos;

PROCEDURE LoadPos (stream: T; which: StreamPosition; VAR Pos: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  (* Read a FilePos.T from page 1.  which determines if the start, end, or
     current position will be read. *)
  VAR array: PosArray;
  BEGIN
    TRY
      IF stream.dirForward AND which = StreamPosition.Start THEN
        Pos.page := IndexPageNo;
        Pos.pPos := PageOneStart;
      ELSE
        stream.indexP.getArray(PositionOffset[which] + 1, array);
        ByteArrayToPos(array, Pos);
      END;
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Stream.LoadPos", "VirtualPage.FatalError", info));
    END;
  END LoadPos;

PROCEDURE LoadAllPos (stream: T; VAR Start, End, Curr: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  VAR allarray: ARRAY [0 .. 3 * StreamPosSize - 1] OF Type.Byte;
  BEGIN
    TRY
      stream.indexP.getArray(
        PositionOffset[StreamPosition.Start] + 1, allarray);
      IF stream.dirForward THEN
        Start.page := IndexPageNo;
        Start.pPos := PageOneStart;
      ELSE
        ByteArrayToPos(SUBARRAY(allarray, 0, StreamPosSize), Start);
      END;
      ByteArrayToPos(SUBARRAY(allarray, StreamPosSize, StreamPosSize), End);
      ByteArrayToPos(
        SUBARRAY(allarray, 2 * StreamPosSize, StreamPosSize), Curr);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Stream.LoadAllPos", "VirtualPage.FatalError", info));
    END;
  END LoadAllPos;

PROCEDURE CreateNextPage (stream: T): VirtualPage.T
  RAISES {Access.Locked, InternalError} =
  (* Prepare the next page for writing.  If the page already exists it will
     only be demanded.  If it does not exist it will be created. *)
  VAR
    page            : VirtualPage.T;
    eos, curr, start: FilePos.T;
  BEGIN
    (* Load end and current position *)
    LoadAllPos(stream, start, eos, curr);
    (* next page has next higher page number *)
    INC(curr.page);
    page := stream.file.getPage(curr.page);

    (* determine the position for the next block *)
    IF stream.dirForward THEN
      (* in a forward stream it is the position of the first possible page
         entry *)
      IF curr.page = IndexPageNo THEN
        curr.pPos := PageOneStart
      ELSE
        curr.pPos := PageStart
      END;
    ELSE
      (* in a backward stream it is the last position on a page *)
      curr.pPos := PageData.Size;
    END;
    SavePos(stream, StreamPosition.Current, curr);
    IF curr.page > eos.page THEN
      SavePos(stream, StreamPosition.End, curr);
    END;
    RETURN page;
  END CreateNextPage;

(* ------------------------------ methods ---------------------------- *)

PROCEDURE OpenStream (stream  : T;
                      resource: VirtualResource.T;
                      path    : Pathname.T;
                      access  : Access.Mode;
                      new     : BOOLEAN;
                      forward : BOOLEAN            ): T
  RAISES {Access.Locked, PageFile.NoAccess, DirectionMismatch,
          InternalError, Access.Denied} =
  VAR zeroPos, start, eos, curr: FilePos.T;
  <* FATAL VirtualResource.NotInTransaction *>
  BEGIN
    TRY
      (* open the stream file *)
      stream.file := NEW(VirtualRemoteFile.T).open(
                       resource, path, access, Access.Kind.Log, new);
      stream.indexP := stream.file.getPage(IndexPageNo);
      IF new THEN
        stream.dirForward := forward;
        curr.page := IndexPageNo;
        VirtualResource.T.beginTransaction(resource);
        IF forward THEN
          zeroPos.page := 0;
          zeroPos.pPos := 0;
          SavePos(stream, StreamPosition.Start, zeroPos);
          zeroPos.page := IndexPageNo;
          zeroPos.pPos := PageOneStart;
        ELSE
          zeroPos.pPos := PageData.Size;
          zeroPos.page := IndexPageNo;
          SavePos(stream, StreamPosition.Start, zeroPos);
        END;
        stream.indexP := stream.file.getPage(IndexPageNo);
        SavePos(stream, StreamPosition.End, zeroPos);
        SavePos(stream, StreamPosition.Current, zeroPos);
        VirtualResource.T.commitTransaction(resource);
      ELSE
        VirtualResource.T.beginTransaction(resource);
        (* read start, end and current position from the stream *)
        LoadPos(stream, StreamPosition.Start, start);
        LoadPos(stream, StreamPosition.End, eos);
        LoadPos(stream, StreamPosition.Current, curr);
        VirtualResource.T.commitTransaction(resource);
        IF start.page = 0 AND start.pPos = 0 AND forward THEN
          (* it is a forward stream *)
          stream.dirForward := TRUE;
        ELSIF (start.page = 0 AND start.pPos = 0 AND NOT forward)
                OR ((start.page # 0 OR start.pPos # 0) AND forward) THEN
          RAISE DirectionMismatch;
        ELSE
          (* it has to be a backward stream *)
          stream.dirForward := FALSE;
        END;
      END;
      RETURN stream;
    EXCEPT
    | VirtualResource.FatalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Stream.OpenStream", "VirtualResource.FatalError", info));
    END;
  END OpenStream;

PROCEDURE CostOfPosition (pos: FilePos.T; forward: BOOLEAN): CARDINAL =
  BEGIN
    IF pos.page > 0 THEN
      IF forward THEN
        RETURN (pos.page - 1) * (PageData.Size DIV 4) + (pos.pPos DIV 4);
      ELSE
        RETURN (pos.page - 1) * (PageData.Size DIV 4)
                 + ((PageData.Size - pos.pPos) DIV 4);
      END;
    ELSE
      RETURN 0
    END;
  END CostOfPosition;

PROCEDURE SaveStream (stream: T; VAR Cost: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR curr: FilePos.T;
  BEGIN
    LoadPos(stream, StreamPosition.Current, curr);
    SavePos(stream, EndPos[stream.dirForward], curr);
    Cost := CostOfPosition(curr, stream.dirForward);
    (* Durch 4 dividieren, damit die Zahlen nicht zu gross werden. *)
  END SaveStream;

PROCEDURE CloseStream (stream: T) RAISES {InternalError} =
  BEGIN
    TRY
      stream.file.close();
    EXCEPT
    | VirtualResource.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Stream.CloseStream",
                              "VirtualResource.FatalError", info));
    END;
  END CloseStream;


(**************************************************************************)

PROCEDURE EndOfStream (stream: T): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  VAR eos, curr: FilePos.T;
  BEGIN
    LoadPos(stream, EndPos[stream.dirForward], eos);
    LoadPos(stream, StreamPosition.Current, curr);
    RETURN curr = eos;
  END EndOfStream;

PROCEDURE GetPosition (stream: T; VAR Pos: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    LoadPos(stream, StreamPosition.Current, Pos);
  END GetPosition;

PROCEDURE SetPosition (stream: T; Pos: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    SavePos(stream, StreamPosition.Current, Pos);
  END SetPosition;

PROCEDURE Truncate (stream: T) RAISES {Access.Locked, InternalError} =
  VAR curr: FilePos.T;
  BEGIN
    LoadPos(stream, StreamPosition.Current, curr);
    SavePos(stream, EndPos[stream.dirForward], curr);
  END Truncate;

PROCEDURE GetLastPosition (stream: T; VAR Pos: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    LoadPos(stream, StreamPosition.End, Pos);
  END GetLastPosition;

PROCEDURE GetFirstPosition (stream: T; VAR Pos: FilePos.T)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    IF stream.dirForward THEN
      Pos.page := IndexPageNo;
      Pos.pPos := PageOneStart;
    ELSE
      LoadPos(stream, StreamPosition.Start, Pos);
    END;
  END GetFirstPosition;

PROCEDURE Forward (stream: T) RAISES {Access.Locked, EOS, InternalError} =
  VAR
    start, curr, end, ncurr: FilePos.T;
    len                    : CARDINAL;
  BEGIN
    LoadAllPos(stream, start, end, curr);
    (* read length information *)
    ReadByteArray(stream, cardbuffer, 4);
    len := Word.Insert(len, cardbuffer[0], 0, 8);
    len := Word.Insert(len, cardbuffer[1], 8, 8);
    len := Word.Insert(len, cardbuffer[2], 16, 8);
    len := Word.Insert(len, cardbuffer[3], 24, 8);
    (* skip element and leading and trailing length information*)
    ncurr := curr;
    FilePos.Increment(ncurr, len + 4 + 4);
    IF FilePos.Compare(ncurr, end) > 0 THEN
      (* restore old posiiton *)
      SavePos(stream, StreamPosition.Current, curr);
      RAISE EOS;
    END;
    SavePos(stream, StreamPosition.Current, ncurr);
  END Forward;

PROCEDURE Backward (stream: T) RAISES {Access.Locked, EOS, InternalError} =
  VAR
    end, curr, start: FilePos.T;
    len             : CARDINAL;
    error           : BOOLEAN;
  BEGIN
    (* move position before trailing length information *)
    LoadAllPos(stream, start, end, curr);
    FilePos.Decrement(curr, 4, error);
    IF error OR FilePos.Compare(curr, start) < 0 THEN RAISE EOS END;
    SavePos(stream, StreamPosition.Current, curr);
    (* read length information *)
    ReadByteArray(stream, cardbuffer, 4);
    len := Word.Insert(len, cardbuffer[0], 0, 8);
    len := Word.Insert(len, cardbuffer[1], 8, 8);
    len := Word.Insert(len, cardbuffer[2], 16, 8);
    len := Word.Insert(len, cardbuffer[3], 24, 8);
    (* skip element and leading and trailing length information*)
    FilePos.Decrement(curr, len + 4, error);
    IF error OR FilePos.Compare(curr, start) < 0 THEN RAISE EOS; END;
    SavePos(stream, StreamPosition.Current, curr);
  END Backward;

VAR
  WriteBuffer: ARRAY [0 .. 2 * PageData.Size - 1] OF Type.Byte;
  cardbuffer : ARRAY [0 .. 3] OF Type.Byte;

PROCEDURE ForwardWriteByteArray (         stream   : T;
                                          overwrite: BOOLEAN;
                                          len      : CARDINAL;
                                 READONLY ba       : Type.ByteArray)
  RAISES {InternalError, Access.Locked} =
  VAR
    rem      : CARDINAL;
    curr, end: FilePos.T;
    page     : VirtualPage.T;
  BEGIN
    TRY
      LoadPos(stream, StreamPosition.Current, curr);
      rem := PageData.Size - curr.pPos;
      page := stream.file.getPage(curr.page);
      IF rem >= len THEN
        page.putArray(curr.pPos + 1, SUBARRAY(ba, 0, len));
        INC(curr.pPos, len);
      ELSE
        IF rem > 0 THEN
          page.putArray(curr.pPos + 1, SUBARRAY(ba, 0, rem));
        END;
        page := CreateNextPage(stream);
        FOR j := 1 TO (len - rem) DIV PageData.Size DO
          page.putArray(PageStart + 1, SUBARRAY(ba, rem, PageData.Size));
          INC(rem, PageData.Size);
          page := CreateNextPage(stream);
        END;
        LoadPos(stream, StreamPosition.Current, curr);
        page.putArray(curr.pPos + 1, SUBARRAY(ba, rem, len - rem));
        INC(curr.pPos, len - rem);
      END;
      SavePos(stream, StreamPosition.Current, curr);
      LoadPos(stream, StreamPosition.End, end);
      IF overwrite OR FilePos.Compare(curr, end) > 0 THEN
        SavePos(stream, StreamPosition.End, curr);
      END;
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Stream.ForwardWriteByteArray",
                                       "VirtualPage.FatalError", info));
    END;
  END ForwardWriteByteArray;

PROCEDURE ForwardWriteByteArrayWithLen (stream   : T;
                                        overwrite: BOOLEAN;
                                        READONLY ba : Type.ByteArray;
                                                 len: CARDINAL        )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    (* Convert lenght to array of byte *)
    cardbuffer[0] := Word.Extract(len, 0, 8);
    cardbuffer[1] := Word.Extract(len, 8, 8);
    cardbuffer[2] := Word.Extract(len, 16, 8);
    cardbuffer[3] := Word.Extract(len, 24, 8);
    SUBARRAY(WriteBuffer, 0, 4) := cardbuffer;
    IF len + 8 <= NUMBER(WriteBuffer) THEN
      SUBARRAY(WriteBuffer, 4, len) := SUBARRAY(ba, 0, len);
      SUBARRAY(WriteBuffer, len + 4, 4) := cardbuffer;
      ForwardWriteByteArray(stream, overwrite, len + 8, WriteBuffer);
    ELSE
      ForwardWriteByteArray(stream, overwrite, 4, cardbuffer);
      ForwardWriteByteArray(stream, overwrite, len, ba);
      ForwardWriteByteArray(stream, overwrite, 4, cardbuffer);
    END;
  END ForwardWriteByteArrayWithLen;

PROCEDURE BackwardWriteByteArray (         stream   : T;
                                           overwrite: BOOLEAN;
                                           len      : CARDINAL;
                                  READONLY ba       : Type.ByteArray)
  RAISES {Access.Locked, InternalError} =
  VAR
    rem        : CARDINAL;
    page       : VirtualPage.T;
    curr, start: FilePos.T;
  BEGIN
    TRY
      LoadPos(stream, StreamPosition.Current, curr);
      IF curr.page = IndexPageNo THEN
        rem := curr.pPos - PageOneStart;
      ELSE
        rem := curr.pPos - PageStart;
      END;
      page := stream.file.getPage(curr.page);
      IF rem >= len THEN
        page.putArray(curr.pPos - len + 1, SUBARRAY(ba, 0, len));
        DEC(curr.pPos, len);
      ELSE
        IF rem > 0 THEN
          page.putArray(curr.pPos - rem + 1, SUBARRAY(ba, len - rem, rem));
        END;
        page := CreateNextPage(stream);
        FOR j := 1 TO (len - rem) DIV PageData.Size DO
          INC(rem, PageData.Size);
          page.putArray(
            PageStart + 1, SUBARRAY(ba, len - rem, PageData.Size));
          page := CreateNextPage(stream);
        END;
        LoadPos(stream, StreamPosition.Current, curr);
        page.putArray(
          curr.pPos - (len - rem) + 1, SUBARRAY(ba, 0, len - rem));
        DEC(curr.pPos, len - rem);
      END;
      SavePos(stream, StreamPosition.Current, curr);
      LoadPos(stream, StreamPosition.Start, start);
      IF overwrite OR FilePos.Compare(curr, start) < 0 THEN
        SavePos(stream, StreamPosition.Start, curr);
      END;
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Stream.BackwardWriteByteArray",
                                       "VirtualPage.FatalError", info));
    END;
  END BackwardWriteByteArray;

PROCEDURE BackwardWriteByteArrayWithLen (stream   : T;
                                         overwrite: BOOLEAN;
                                         READONLY ba : Type.ByteArray;
                                                  len: CARDINAL        )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    (* Convert lenght to array of byte *)
    cardbuffer[0] := Word.Extract(len, 0, 8);
    cardbuffer[1] := Word.Extract(len, 8, 8);
    cardbuffer[2] := Word.Extract(len, 16, 8);
    cardbuffer[3] := Word.Extract(len, 24, 8);
    SUBARRAY(WriteBuffer, 0, 4) := cardbuffer;
    IF len + 8 <= NUMBER(WriteBuffer) THEN
      SUBARRAY(WriteBuffer, 4, len) := SUBARRAY(ba, 0, len);
      SUBARRAY(WriteBuffer, len + 4, 4) := cardbuffer;
      BackwardWriteByteArray(stream, overwrite, len + 8, WriteBuffer);
    ELSE
      BackwardWriteByteArray(stream, overwrite, 4, cardbuffer);
      BackwardWriteByteArray(stream, overwrite, len, ba);
      BackwardWriteByteArray(stream, overwrite, 4, cardbuffer);
    END;
  END BackwardWriteByteArrayWithLen;

PROCEDURE Write (stream: T; READONLY el: Element.T; overwrite: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  VAR
    buffer: REF ARRAY OF Type.Byte;
    len   : CARDINAL;
  BEGIN
    (* Convert element to array of byte *)
    Element.ToByteArray(el, len, buffer);
    (* bracket the element to write with length information, so that length
       can be determined in both directions. *)
    IF stream.dirForward THEN
      ForwardWriteByteArrayWithLen(stream, overwrite, buffer^, len);
    ELSE
      BackwardWriteByteArrayWithLen(stream, overwrite, buffer^, len);
    END;
  END Write;

(*************************************************************************)

PROCEDURE ReadByteArray (stream: T; VAR ba: Type.ByteArray; len: CARDINAL)
  RAISES {Access.Locked, EOS, InternalError} =
  VAR
    page     : VirtualPage.T;
    rem      : CARDINAL;
    curr, end: FilePos.T;

  PROCEDURE GetNextPage (stream: T) RAISES {} =
    BEGIN
      IF stream.dirForward THEN INC(curr.page); ELSE DEC(curr.page); END;
      IF curr.page = IndexPageNo THEN
        curr.pPos := PageOneStart;
      ELSE
        curr.pPos := PageStart;
      END;
    END GetNextPage;

  BEGIN
    TRY
      LoadPos(stream, StreamPosition.Current, curr);
      LoadPos(stream, StreamPosition.End, end);
      IF curr = end THEN RAISE EOS END;
      rem := PageData.Size - curr.pPos;
      page := stream.file.getPage(curr.page);
      IF rem >= len THEN
        page.getArray(curr.pPos + 1, SUBARRAY(ba, 0, len));
        INC(curr.pPos, len);
      ELSE
        IF rem > 0 THEN
          page.getArray(curr.pPos + 1, SUBARRAY(ba, 0, rem));
        END;
        FOR i := 1 TO (len - rem) DIV PageData.Size DO
          GetNextPage(stream);
          page := stream.file.getPage(curr.page);
          page.getArray(
            curr.pPos + 1, SUBARRAY(ba, rem, PageData.Size - curr.pPos));
          INC(rem, PageData.Size - curr.pPos);
        END;
        IF (len - rem) > 0 THEN
          GetNextPage(stream);
          page := stream.file.getPage(curr.page);
          page.getArray(curr.pPos + 1, SUBARRAY(ba, rem, len - rem));
          INC(curr.pPos, len - rem);
        END;
      END;
      SavePos(stream, StreamPosition.Current, curr);
    EXCEPT
    | VirtualPage.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Stream.ReadByteArray", "VirtualPage.FatalError", info));
    END;
  END ReadByteArray;

PROCEDURE Read (stream: T; VAR el: Element.T)
  RAISES {Access.Locked, ElementError, EOS, InternalError} =
  VAR
    len   : CARDINAL;
    buffer: REF ARRAY OF Type.Byte := NIL;
    ok    : BOOLEAN;
    curr  : FilePos.T;
  BEGIN
    (* read length information *)
    ReadByteArray(stream, cardbuffer, 4);
    len := Word.Insert(len, cardbuffer[0], 0, 8);
    len := Word.Insert(len, cardbuffer[1], 8, 8);
    len := Word.Insert(len, cardbuffer[2], 16, 8);
    len := Word.Insert(len, cardbuffer[3], 24, 8);
    (* read element *)
    buffer := NEW(REF Type.ByteArray, len);
    ReadByteArray(stream, buffer^, len);
    Element.FromByteArray(buffer^, len, el, ok);
    IF NOT ok THEN RAISE ElementError END;
    (* skip trailing length information *)
    LoadPos(stream, StreamPosition.Current, curr);
    FilePos.Increment(curr, 4);
    SavePos(stream, StreamPosition.Current, curr);
  END Read;

BEGIN
END Stream.
