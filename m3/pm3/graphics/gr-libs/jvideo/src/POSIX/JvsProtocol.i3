(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Mar 22 17:59:18 PST 1995 by msm      *)
(*      modified on Wed Oct 20 18:19:49 PDT 1993 by sfreeman *)

(* protocol definition for talking to the jvs driver, taken from jvs.h *)

INTERFACE JvsProtocol;

FROM Ctypes IMPORT char, int;

CONST
  Socket = 2510;                 (* the port number for the local server *)
  PipeName    = "/tmp/jvideo/jvideo.2510";
  (* client and server communicate via this named pipe *)

  Allocate       = 1;
  Compress       = 2;
  old_Decompress = 3;
  SetCompress    = 4;
  SetDecompress  = 5;
  Colormap       = 6;
  Ping           = 7;
  Deallocate     = 8;            (* not used *)
  Decompress     = 9;
  Command        = 10;
  Allocate2      = 14;

  CompressBuf   = 1;
  DecompressBuf = 2;

  MaxXServerNameLen = 256;

  Input = 1;
  Output = 2;

  JPEG = 1;
  Dithered = 2;
  YUV = 3;

TYPE
  RequestCode = BITS BITSIZE(int) FOR [Allocate .. Allocate2];
  AllocateSize = BITS BITSIZE(int) FOR [CompressBuf .. DecompressBuf];

TYPE
  PingReq = RECORD requestCode: RequestCode := Ping;  END;
  PingReqPtr = UNTRACED REF PingReq;

  PingRep = RECORD requestCode: RequestCode := Ping;  END;
  PingRepPtr = UNTRACED REF PingRep;


  AllocateReq = RECORD
                  requestCode: RequestCode    := Allocate;
                  direction (* Input, Output *),
                  type (* JPEG, Dithered, YUV *),
                  width, height: int;
                END;
  AllocateReqPtr = UNTRACED REF AllocateReq;

  AllocateRep = RECORD
                  requestCode: RequestCode := Allocate;
                  shmid      : int;
                END;
  AllocateRepPtr = UNTRACED REF AllocateRep;


  DeallocateReq = RECORD
                    requestCode: RequestCode := Deallocate;
                    shmid      : int;
                  END;
  DeallocateReqPtr = UNTRACED REF DeallocateReq;

  DeallocateRep = RECORD
                    requestCode: RequestCode := Deallocate;
                    replyCode  : int;
                  END;
  DeallocateRepPtr = UNTRACED REF DeallocateRep;


  CompressReq = RECORD
                  requestCode: RequestCode := Compress;
                  shmid      : int;
                END;
  CompressReqPtr = UNTRACED REF CompressReq;

  CompressRep = RECORD
                  requestCode: RequestCode := Compress;
                  shmid      : int;
                  length     : int;
                END;
  CompressRepPtr = UNTRACED REF CompressRep;


  DecompressReq = RECORD
                    requestCode   : RequestCode := Decompress;
                    cshmid, dshmid: int;
                    length: int;  (* length of compressed image *)
                  END;
  DecompressReqPtr = UNTRACED REF DecompressReq;

  old_DecompressReq = RECORD
                        requestCode   : RequestCode := old_Decompress;
                        cshmid, dshmid: int;
                      END;
  old_DecompressReqPtr = UNTRACED REF old_DecompressReq;

  DecompressRep = RECORD
                    requestCode   : RequestCode := Decompress;
                    cshmid, dshmid: int;
                    ncolors       : int;
                  END;
  DecompressRepPtr = UNTRACED REF DecompressRep;


  SetCompressReq = RECORD
                     requestCode: RequestCode := SetCompress;
                     qfactor    : int;
                     xdec, ydec : int;
                     frameskip  : int;
                   END;
  SetCompressReqPtr = UNTRACED REF SetCompressReq;

  SetCompressRep = RECORD
                     requestCode: RequestCode := SetCompress;
                     width, height: int;  (* result dimensions of
                                             compressed image *)
                   END;
  SetCompressRepPtr = UNTRACED REF SetCompressRep;


  SetDecompressReq = RECORD
                       requestCode         : RequestCode := SetDecompress;
                       qfactor             : int;
                       inX, inY, outX, outY: int;
                       brightness, contrast, saturation: int;
                     END;
  SetDecompressReqPtr = UNTRACED REF SetDecompressReq;

  SetDecompressRep = RECORD
                       requestCode: RequestCode := SetDecompress;
                       actualOutX, actualOutY, linePadding: int;
                     END;
  SetDecompressRepPtr = UNTRACED REF SetDecompressRep;


  ColormapReq = RECORD
                  requestCode: RequestCode := Colormap;
                  monochrome : int;
                  nColors    : int;
                  id         : int;        (* Colormap id *)
                  serverName: ARRAY [0 .. MaxXServerNameLen - 1] OF char;
                  (* X server (DISPLAY variable) to talk to *)
                END;
  ColormapReqPtr = UNTRACED REF ColormapReq;

  ColormapRep = RECORD
                  requestCode: RequestCode := Colormap;
                  nColors    : int;
                END;
  ColormapRepPtr = UNTRACED REF ColormapRep;


  CommandReq = RECORD
                 requestCode: RequestCode := Command;
                 command    : int;
                 dataLength : int;
                 (* put data (if any) here *)
               END;
  CommandReqPtr = UNTRACED REF CommandReq;

  CommandRep = RECORD
                 requestCode: RequestCode := Command;
                 replyCode  : int;
                 dataLength : int;
               END;
  CommandRepPtr = UNTRACED REF CommandRep;

END JvsProtocol.
