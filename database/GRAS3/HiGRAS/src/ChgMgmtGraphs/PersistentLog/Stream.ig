GENERIC INTERFACE Stream(Element);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:30:39  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:29  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:21:19  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/20 13:59:11  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:58:07  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)


IMPORT Pathname, FilePos;
IMPORT Access, VirtualResource, PageFile;
IMPORT AtomList;

TYPE

  (* A Stream is a file in which instances of Element.T can be logged.
     Streams can be forward or backward.  The elemets of a forward stream
     are read in the same order as they are written.  The elements of a
     backward stream are read in reverse order.  Each stream has a start,
     end, and current position.  Elements are read and written from and to
     the current position.  The current position is set to the next element
     after reading and writing.  A stream can be truncated by setting its
     end position to the current position with method 'save'.

     Streams are built from VirtualFiles, i.e.  they are page oriented.  It
     is possible to set the current position of a stream to an arbitrary
     FilePos.T, but the result will be unpredictable (and certainly wrong)
     if the position given does not conform with the start/end of an
     element in the stream.  If SetPosition is used, care must be taken,
     that the position information is always consistent to the contents of
     the stream.  A safer way to move the current position of a stream is
     to use the methods forward and backward that set the current position
     to the position before the next and previous Element.T in the steam,
     respectivly.

     The interface Element must contain a type Element.T and two conversion
     routines for this type into an array of byte and from an array of byte
     into an Element.T: *)

  (**  PROCEDURE ToByteArray (el     : Element.T;
                              VAR len: CARDINAL;
                              VAR ba : REF Type.ByteArray); *)

  (* Stores all information about an element in an array of bytes ba.  ba
     can be easily transferred to physical storage.  The actual length of
     the stored information is returned in len.  The procedure must
     allocate memory for ba on its own. *)

  (** PROCEDURE FromByteArray (READONLY ba : Type.ByteArray;
                                        len: CARDINAL;
                               VAR      el : Element.T;
                               VAR      ok : BOOLEAN         ); *)

  (* Converts the information in ba to an Element.T.  len holds additional
     information about the used length of the byte array.  If the contents
     of ba do not represent a command ok will be FALSE. *)


  T <: Public;
  Public =
    OBJECT
    METHODS
      open (resource: VirtualResource.T;
            path    : Pathname.T;
            access  : Access.Mode;
            new     : BOOLEAN;
            forward : BOOLEAN            ): T
            RAISES {Access.Locked, PageFile.NoAccess, DirectionMismatch,
                    InternalError, Access.Denied};
            (* Open a stream.  If new, then create a new stream with name
               'path'.  If 'forward' is true the stream will be a forward
               stream (i.e.  elements will be stored in order) otherwise a
               backward stream (elements are stored in reverse order).  If
               new is FALSE, it is checked, whether the stream conforms to
               'forward'.  If not, DirectionMismatch is raised. *)

      save (VAR cost: CARDINAL) RAISES {Access.Locked, InternalError};
            (* The end of stream marker is set to current position.
               Attention: call this method only within a transaction. *)

      close () RAISES {InternalError};
             (* close the stream.  All position information remains
                unchanged. *)

      endOfStream (): BOOLEAN RAISES {Access.Locked, InternalError};
                   (* Returns true if the end of the stream was reached
                      while reading (i.e.  current position = end
                      position). *)

      getPosition (VAR Pos: FilePos.T)
                   RAISES {Access.Locked, InternalError};
                   (* Delivers the current position of the stream
                      pointer. *)

      setPosition (Pos: FilePos.T) RAISES {Access.Locked, InternalError};
                   (* Sets the stream pointer to Pos.  Users of this method
                      have to make sure that the position Pos points to an
                      element boundary in the stream (this will be the
                      case, if Pos was determined by getPosition and then
                      held conform to the read and write operations on the
                      stream). *)

      truncate () RAISES {Access.Locked, InternalError};
                (* For a forward stream the end position is set to the
                   current position.  For a backward stream the start
                   position is set to the current position. *)

      getLastPosition (VAR Pos: FilePos.T)
                       RAISES {Access.Locked, InternalError};
                       (* Delivers the end position of the stream. *)

      getFirstPosition (VAR Pos: FilePos.T)
                        RAISES {Access.Locked, InternalError};
                        (* Delivers the start position of the stream (only
                           interesting for backward streams, constant for
                           all forward streams). *)

      forward () RAISES {Access.Locked, EOS, InternalError};
               (* The current position of the stream is set to the next
                  element *)

      backward () RAISES {Access.Locked, EOS, InternalError};
                (* The current position of the stream is set to the
                   previous element in the stream.  Note that the exception
                   EOS here means that the beginning of the stream is
                   reached. *)

      write (READONLY co: Element.T; overwrite: BOOLEAN)
             RAISES {Access.Locked, InternalError};
             (* Write element el to the stream at the current position and
                advance current position.  If the write exceeds the lenght
                of the stream, it will be enlarged.  If overwrite is TRUE
                and the current position is not the end (start for backward
                streams) of the stream, the end (start) posiiton is set to
                the position after the write (the stream is shortened). *)

      read (VAR el: Element.T)
            RAISES {Access.Locked, ElementError, EOS, InternalError};
            (* Read next element from stream at current position.  If
               current position is end of stream, raise EOS. *)
    END;


EXCEPTION
  ElementError;                  (* Converting a block to an element
                                    failed *)
  DirectionMismatch;             (* A backward stream was opened as forward
                                    or the other way round *)
  EOS;                           (* The end of a stream was reached while
                                    reading *)
  InternalError(AtomList.T);

END Stream.
