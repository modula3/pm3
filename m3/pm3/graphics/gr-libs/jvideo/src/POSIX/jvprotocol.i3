(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Jan 26 23:27:35 PST 1995 by msm      *)
(*      modified on Thu Aug 19 10:29:32 PDT 1993 by sfreeman *)
INTERFACE jvprotocol;

IMPORT Utime;
FROM Ctypes IMPORT int, unsigned_int, unsigned_short_int;

(* The ports were once upon a time htons'ed.  INET ports are in local
   endian (yes I know its wrong) unlike addreses which are never messed
   with.  So to make sure the new code which doesn't mess with the ports
   interoperates with the old stuff we have to use some bizzare numbers
   here. *)

CONST
  PORT_AUDIO_SOURCE = 53001;
  PORT_AUDIO_HEAR   = 53000;
  PORT_VIDEO        = 53001;

(* J-Video Protocol definitions *)

(*|

 Client -> server messages use a single character  (0 - 255):

     1 -  15: Number of credits - add to stream's current balance
    16 -  31: Max credits - 1 (upper limit of balance)
    48 -  63: '0' - '?' Quality
          71: 'H' Heartbeat (source does nothing)
            : 'M' Client can handle multicast
          81: 'Q' Qualities (reply with a set of strings)
          83: 'S' Status (reply with a set of strings)
            : 'U' UDP request OK.
            : 'u' UDP request failed.
            : 'V' Suspend stream
            : 'v' Resume stream
   128 - 254: Sync, where code is to be returned in sync reply.
         255: Extended (more than one byte) command
*)
TYPE
  ClientRequest = BITS 8 FOR [16_0 .. 16_ff];
  ClientNumCredits = [16_01 .. 16_0f];
  ClientMaxCredits = [16_10 .. 16_1f];
  ClientQualityVal = [16_30 .. 16_3f];
  ClientSyncVal = [16_80 .. 16_ff];

CONST
  ClientHeartbeat        = 16_48; (* 'H' *)
  ClientMulticastAble    = 16_4D; (* 'M' *)
  ClientQualities        = 16_51; (* 'Q' *)
  ClientStatus           = 16_53; (* 'S' *)
  ClientUDPRequestOK     = 16_55; (* 'U' *)
  ClientUDPRequestFailed = 16_75; (* 'u' *)
  ClientSuspendStream    = 16_56; (* 'V' *)
  ClientResumeStream     = 16_76; (* 'v' *)
  ExtendedCommand        = 16_FF;

(* Extended commands *)
  FrameRateHint          = 1;

TYPE FrameRateHintRec = RECORD
  type: int;
  period: int;
END;

(* server -> client messages *)

CONST
  JVP_FirstEvent         = JVP_VIDEO;
  JVP_VIDEO              = 1;    (* used *)
  JVP_RESOLUTION         = 2;    (* not used *)
  JVP_TYPE               = 3;    (* used *)
  JVP_AUDIO              = 4;    (* used *)
  JVP_SYNC               = 5;    (* used *)
  JVP_STATUS             = 6;    (* used *)
  JVP_QUALITIES          = 7;    (* used *)
  JVP_ENDMARK            = 8;    (* not used *)
  JVP_ERROR              = 9;    (* not used *)
  JVP_INFO               = 10;   (* not used *)
  JVP_AUDIO_SILENCE      = 11;   (* not used *)
  JVP_AUDIO_MULTICAST    = 12;
  JVP_VIDEO_UDP_REQUEST  = 13;
  JVP_VIDEO_UDP_RESPONSE = 14;
  JVP_VIDEO_UDP_DATA     = 15;
  JVP_LastEvent          = JVP_VIDEO_UDP_DATA; 

TYPE TypeCode = BITS BITSIZE(int) FOR [JVP_FirstEvent .. JVP_LastEvent];

CONST MaxControlMsgBytes =  128; (* storage for non-video data *)
      MaxControlMsgSize = 128 DIV BYTESIZE(CHAR);
TYPE ControlBuffer = ARRAY [0 .. MaxControlMsgSize - 1] OF CHAR;

TYPE
  VideoFrame = RECORD
                 type     : int                    := JVP_VIDEO;
                 length   : int;
                 timestamp: Utime.struct_timeval;
                 (* data goes here for 'length' bytes *)
               END;
  VideoFramePtr = UNTRACED REF VideoFrame;

TYPE
  Resolution =
    RECORD
      type           : int   := JVP_RESOLUTION;
      swidth, sheight: int;      (* Source width, height *)
      cwidth, cheight: int;      (* Width & height when compressed *)
    END;
  ResolutionPtr = UNTRACED REF Resolution;

CONST JVP_KIND_JPEG = 0;
TYPE
  VideoType = RECORD
                type   : int   := JVP_TYPE;
                kind   : int   := JVP_KIND_JPEG;
                qfactor: int;    (* kludge; should send table *)
                width  : int;
                height : int;
              END;
  VideoTypePtr = UNTRACED REF VideoType;

TYPE
  AudioFrame = RECORD
                 type: int := JVP_AUDIO;
                 seqNum: unsigned_int;  (* not consecutive; cumulative
                                           count of samples *)
                 length   : int;
                 timestamp: Utime.struct_timeval;
                 (* data goes here for 'length' bytes *)
               END;
  AudioFramePtr = UNTRACED REF AudioFrame;

TYPE
  SyncFrame = RECORD
                type: int   := JVP_SYNC;
                code: int;
                time: int;
              END;
  SyncFramePtr = UNTRACED REF SyncFrame;

TYPE
  StatusFrame = RECORD
                  type  : int   := JVP_STATUS;
                  length: int;
                END;
  StatusFramePtr = UNTRACED REF StatusFrame;

TYPE
  QualitiesFrame = RECORD
                     type  : int   := JVP_QUALITIES;
                     length: int;
                     (* Strings of the form "%d %d %d %d" quality, width,
                        height, qfactor *)
                     (* Terminated by a zero-length string *)
                   END;
  QualitiesFramePtr = UNTRACED REF QualitiesFrame;

TYPE
  EndMark = RECORD
              type     : int                    := JVP_ENDMARK;
              timestamp: Utime.struct_timeval;
            END;
  EndMarkPtr = UNTRACED REF EndMark;

TYPE
  ErrorFrame = RECORD
                 type  : int   := JVP_ERROR;
                 length: int;
                 (* Null terminated error string *)
               END;
  ErrorFramePtr = UNTRACED REF ErrorFrame;

TYPE
  InfoFrame = RECORD
                type  : int   := JVP_INFO;
                length: int;     (* of data following *)
                code  : int;     (* whatever *)
                timestamp: Utime.struct_timeval;
                (* Additional data here *)
              END;
  InfoFramePtr = UNTRACED REF InfoFrame;

TYPE
  AudioSilenceFrame =
    RECORD
      type: int := JVP_AUDIO_SILENCE;
      seqNum: unsigned_int;      (* not consecutive; cumulative count of
                                    samples *)
      length: int;               (* number of silent samples *)
      timestamp: Utime.struct_timeval;
    END;
  AudioSilenceFramePtr = UNTRACED REF AudioSilenceFrame;

TYPE
  AudioMulticastInfo = RECORD
                         type: int            := JVP_AUDIO_MULTICAST;
                         addr: unsigned_int;
                       END;
  AudioMulticastInfoPtr = UNTRACED REF AudioMulticastInfo;

TYPE
  VideoUdpControl = RECORD       (* both req/response *)
                      type      : int;
                      jvpu_daddr: unsigned_int;
                      jvpu_saddr: unsigned_int;
                      jvpu_dport: unsigned_short_int;
                      jvpu_sport: unsigned_short_int;
                    END;
  VideoUdpControlPtr = UNTRACED REF VideoUdpControl;

TYPE
  VideoUdpData = RECORD
                   type       : int   := JVP_VIDEO_UDP_DATA;
                   jvpu_length: int;  (* total data *)
                   jvpu_timestamp: ARRAY [0 .. 1] OF Utime.struct_timeval;
                   (* lance wanted two *)
                   jvpu_sequence: int;  (* for reassembly *)
                 END;
  VideoUdpDataPtr = UNTRACED REF VideoUdpData;

CONST
  HdrSizes = ARRAY TypeCode OF
               CARDINAL{
               BYTESIZE(VideoFrame), BYTESIZE(Resolution),
               BYTESIZE(VideoType), BYTESIZE(AudioFrame),
               BYTESIZE(SyncFrame), BYTESIZE(StatusFrame),
               BYTESIZE(QualitiesFrame), BYTESIZE(EndMark),
               BYTESIZE(ErrorFrame), BYTESIZE(InfoFrame),
               BYTESIZE(AudioSilenceFrame), BYTESIZE(AudioMulticastInfo),
               BYTESIZE(VideoUdpControl), BYTESIZE(VideoUdpControl),
               BYTESIZE(VideoUdpData)};

(* no message is shorter than an AnyHeader, so we can always read in the
   number of bytes in an AnyHeader *)
TYPE
  AnyHeader = RECORD
                type : int;
                dummy: int;
              END;
  AnyHeaderPtr = UNTRACED REF AnyHeader;

CONST
  MaxHdrSize = MaxHdrBytes DIV BYTESIZE(CHAR);
  MaxHdrBytes = 
    MAX( BYTESIZE(ARRAY [0 .. 63] OF CHAR),
    MAX( BYTESIZE(AnyHeader),
    MAX( HdrSizes[JVP_VIDEO],
    MAX( HdrSizes[JVP_RESOLUTION],
    MAX( HdrSizes[JVP_TYPE],
    MAX( HdrSizes[JVP_AUDIO],
    MAX( HdrSizes[JVP_SYNC],
    MAX( HdrSizes[JVP_STATUS],
    MAX( HdrSizes[JVP_QUALITIES],
    MAX( HdrSizes[JVP_ENDMARK],
    MAX( HdrSizes[JVP_ERROR],
    MAX( HdrSizes[JVP_INFO],
    MAX( HdrSizes[JVP_AUDIO_SILENCE],
    MAX( HdrSizes[JVP_AUDIO_MULTICAST],
    MAX( HdrSizes[JVP_VIDEO_UDP_REQUEST],
    MAX( HdrSizes[JVP_VIDEO_UDP_RESPONSE],
         HdrSizes[JVP_VIDEO_UDP_DATA]))))))))))))))));

TYPE
  Header = ARRAY [0 .. MaxHdrSize - 1] OF CHAR;
  HeaderPtr = UNTRACED REF Header;

END jvprotocol.
