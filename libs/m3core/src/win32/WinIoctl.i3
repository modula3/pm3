(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* by Roumen Roupski                                           *)
(*                                                             *)
(* Last modified on Mon Aug 19 10:39:57 PDT 1996 by heydon     *)
(*      modified on Tue Wen 19 04:44:10 EST 1996 by rroupski   *)

INTERFACE WinIoctl;

(* This module defines the 32-Bit Windows Device I/O control codes.

   It defines the various device type values.  Note that values used
   by Microsoft Corporation are in the range 0-32767, and 32768-65535
   are reserved for use by customers. The values in this interface
   correspond to build version 0013 of Microsoft's "winioctl.h". *)

IMPORT WinNT;

FROM WinBaseTypes IMPORT BYTE, WORD, DWORD;

TYPE
  DEVICE_TYPE = DWORD;

CONST
  FILE_DEVICE_BEEP                = 16_00000001;
  FILE_DEVICE_CD_ROM              = 16_00000002;
  FILE_DEVICE_CD_ROM_FILE_SYSTEM  = 16_00000003;
  FILE_DEVICE_CONTROLLER          = 16_00000004;
  FILE_DEVICE_DATALINK            = 16_00000005;
  FILE_DEVICE_DFS                 = 16_00000006;
  FILE_DEVICE_DISK                = 16_00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM    = 16_00000008;
  FILE_DEVICE_FILE_SYSTEM         = 16_00000009;
  FILE_DEVICE_INPORT_PORT         = 16_0000000A;
  FILE_DEVICE_KEYBOARD            = 16_0000000B;
  FILE_DEVICE_MAILSLOT            = 16_0000000C;
  FILE_DEVICE_MIDI_IN             = 16_0000000D;
  FILE_DEVICE_MIDI_OUT            = 16_0000000E;
  FILE_DEVICE_MOUSE               = 16_0000000F;
  FILE_DEVICE_MULTI_UNC_PROVIDER  = 16_00000010;
  FILE_DEVICE_NAMED_PIPE          = 16_00000011;
  FILE_DEVICE_NETWORK             = 16_00000012;
  FILE_DEVICE_NETWORK_BROWSER     = 16_00000013;
  FILE_DEVICE_NETWORK_FILE_SYSTEM = 16_00000014;
  FILE_DEVICE_NULL                = 16_00000015;
  FILE_DEVICE_PARALLEL_PORT       = 16_00000016;
  FILE_DEVICE_PHYSICAL_NETCARD    = 16_00000017;
  FILE_DEVICE_PRINTER             = 16_00000018;
  FILE_DEVICE_SCANNER             = 16_00000019;
  FILE_DEVICE_SERIAL_MOUSE_PORT   = 16_0000001A;
  FILE_DEVICE_SERIAL_PORT         = 16_0000001B;
  FILE_DEVICE_SCREEN              = 16_0000001C;
  FILE_DEVICE_SOUND               = 16_0000001D;
  FILE_DEVICE_STREAMS             = 16_0000001E;
  FILE_DEVICE_TAPE                = 16_0000001F;
  FILE_DEVICE_TAPE_FILE_SYSTEM    = 16_00000020;
  FILE_DEVICE_TRANSPORT           = 16_00000021;
  FILE_DEVICE_UNKNOWN             = 16_00000022;
  FILE_DEVICE_VIDEO               = 16_00000023;
  FILE_DEVICE_VIRTUAL_DISK        = 16_00000024;
  FILE_DEVICE_WAVE_IN             = 16_00000025;
  FILE_DEVICE_WAVE_OUT            = 16_00000026;
  FILE_DEVICE_8042_PORT           = 16_00000027;
  FILE_DEVICE_NETWORK_REDIRECTOR  = 16_00000028;
  FILE_DEVICE_BATTERY             = 16_00000029;
  FILE_DEVICE_BUS_EXTENDER        = 16_0000002A;


(* Macro definition for defining IOCTL and FSCTL function control
   codes. Note that function codes 0-2047 are reserved for Microsoft
   Corporation, and 2048-4095 are reserved for customers.

| #define CTL_CODE( DeviceType, Function, Method, Access ) (                 \
|     ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method) \
| )
*)

TYPE
  CTL_CODE = RECORD
    Method    : BITS  2 FOR [METHOD_BUFFERED..METHOD_NEITHER];
    Function  : BITS 12 FOR [0..4095];
    Access    : BITS  2 FOR [0..3];
    DeviceType: BITS 16 FOR WORD;
  END;

CONST
  IOCTL_USER_CODES = 16_0800;

(* Define the method codes for how buffers are passed for I/O and FS
   controls *)
CONST
  METHOD_BUFFERED    = 0;
  METHOD_IN_DIRECT   = 1;
  METHOD_OUT_DIRECT  = 2;
  METHOD_NEITHER     = 3;

(* Define the access check value for any access.

   The "FILE_READ_ACCESS" and "FILE_WRITE_ACCESS" constants are also
   defined in "ntioapi.h" as "FILE_READ_DATA" and "FILE_WRITE_DATA".
   The values for these constants *MUST* always be in sync. *)

CONST
  FILE_ANY_ACCESS   = 0;
  FILE_READ_ACCESS  = 1;    (* file & pipe *)
  FILE_WRITE_ACCESS = 2;    (* file & pipe *)

  FILE_READ_DATA    = FILE_READ_ACCESS;
  FILE_WRITE_DATA   = FILE_WRITE_ACCESS;

(* end_ntddk end_nthal end_ntifs *)



(* "IoControlCode" values for disk devices. *)
CONST
  IOCTL_DISK_BASE                = FILE_DEVICE_DISK;
  IOCTL_DISK_GET_DRIVE_GEOMETRY  = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0000,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_GET_PARTITION_INFO  = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0001,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_SET_PARTITION_INFO  = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0002,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS + FILE_WRITE_ACCESS };
  IOCTL_DISK_GET_DRIVE_LAYOUT    = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0003,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_SET_DRIVE_LAYOUT    = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0004,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS + FILE_WRITE_ACCESS };
  IOCTL_DISK_VERIFY              = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0005,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_FORMAT_TRACKS       = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0006,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS + FILE_WRITE_ACCESS };
  IOCTL_DISK_REASSIGN_BLOCKS     = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0007,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS + FILE_WRITE_ACCESS };
  IOCTL_DISK_PERFORMANCE         = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0008,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_IS_WRITABLE         = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0009,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_LOGGING             = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_000A,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_FORMAT_TRACKS_EX    = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_000B,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS + FILE_WRITE_ACCESS };
  IOCTL_DISK_HISTOGRAM_STRUCTURE = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_000C,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_HISTOGRAM_DATA      = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_000D,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_HISTOGRAM_RESET     = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_000E,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_REQUEST_STRUCTURE   = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_000F,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  IOCTL_DISK_REQUEST_DATA        = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0010,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };

(* The following device control codes are common for all class
   drivers. The functions codes defined here must match all of the
   other class drivers. *)

CONST
  IOCTL_DISK_CHECK_VERIFY     = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0200,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_MEDIA_REMOVAL    = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0201,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_EJECT_MEDIA      = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0202,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_LOAD_MEDIA       = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0203,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_RESERVE          = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0204,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_RELEASE          = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0205,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_FIND_NEW_DEVICES = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0206,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_REMOVE_DEVICE    = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0207,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_ACCESS };
  IOCTL_DISK_GET_MEDIA_TYPES  = CTL_CODE{
    DeviceType:= IOCTL_DISK_BASE, Function:= 16_0300,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS  };

(* Define the partition types returnable by known disk drivers. *)

CONST
  PARTITION_ENTRY_UNUSED = 16_00; (* Entry unused *)
  PARTITION_FAT_12       = 16_01; (* 12-bit FAT entries *)
  PARTITION_XENIX_1      = 16_02; (* Xenix *)
  PARTITION_XENIX_2      = 16_03; (* Xenix *)
  PARTITION_FAT_16       = 16_04; (* 16-bit FAT entries *)
  PARTITION_EXTENDED     = 16_05; (* Extended partition entry *)
  PARTITION_HUGE         = 16_06; (* Huge partition MS-DOS V4 *)
  PARTITION_IFS          = 16_07; (* IFS Partition *)
  PARTITION_PREP         = 16_41; (* PowerPC Ref. Platform Boot Partition *)
  PARTITION_UNIX         = 16_63; (* Unix *)
			 	   
  VALID_NTFT             = 16_C0; (* NTFT uses high order bits *)

(* The high bit of the partition type code indicates that a partition
   is part of an NTFT mirror or striped array. *)

CONST PARTITION_NTFT     = 16_80; (* NTFT partition *)

(* Define the media types supported by the driver. *)

TYPE
  PMEDIA_TYPE = UNTRACED REF MEDIA_TYPE;
  MEDIA_TYPE = {
    Unknown,                (* Format is unknown *)
    F5_1Pt2_512,            (* 5.25", 1.2MB,  512 bytes/sector *)
    F3_1Pt44_512,           (* 3.5",  1.44MB, 512 bytes/sector *)
    F3_2Pt88_512,           (* 3.5",  2.88MB, 512 bytes/sector *)
    F3_20Pt8_512,           (* 3.5",  20.8MB, 512 bytes/sector *)
    F3_720_512,             (* 3.5",  720KB,  512 bytes/sector *)
    F5_360_512,             (* 5.25", 360KB,  512 bytes/sector *)
    F5_320_512,             (* 5.25", 320KB,  512 bytes/sector *)
    F5_320_1024,            (* 5.25", 320KB,  1024 bytes/sector *)
    F5_180_512,             (* 5.25", 180KB,  512 bytes/sector *)
    F5_160_512,             (* 5.25", 160KB,  512 bytes/sector *)
    RemovableMedia,         (* Removable media other than floppy *)
    FixedMedia              (* Fixed hard disk media *)
  };

(* Define the input buffer structure for the driver, when
   it is called with IOCTL_DISK_FORMAT_TRACKS. *)

TYPE
  PFORMAT_PARAMETERS = UNTRACED REF FORMAT_PARAMETERS;
  FORMAT_PARAMETERS = RECORD
    MediaType: MEDIA_TYPE;
    StartCylinderNumber: DWORD;
    EndCylinderNumber: DWORD;
    StartHeadNumber: DWORD;
    EndHeadNumber: DWORD;
  END;

(* Define the "BAD_TRACK_NUMBER" type. An array of elements of this type is
   returned by the driver on "IOCTL_DISK_FORMAT_TRACKS" requests, to indicate
   what tracks were bad during formatting. The length of that array is
   reported in the `Information' field of the I/O Status Block. *)

TYPE
  BAD_TRACK_NUMBER = WORD;
  PBAD_TRACK_NUMBER = UNTRACED REF BAD_TRACK_NUMBER;

(* Define the input buffer structure for the driver, when
   it is called with "IOCTL_DISK_FORMAT_TRACKS_EX". *)

TYPE
  PFORMAT_EX_PARAMETERS = UNTRACED REF FORMAT_EX_PARAMETERS;
  FORMAT_EX_PARAMETERS = RECORD
    MediaType : MEDIA_TYPE;
    StartCylinderNumber: DWORD;
    EndCylinderNumber: DWORD;
    StartHeadNumber: DWORD;
    EndHeadNumber: DWORD;
    FormatGapLength: WORD;
    SectorsPerTrack: WORD;
    SectorNumber: ARRAY [0..0] OF WORD;
  END;

(* The following structure is returned on an "IOCTL_DISK_GET_DRIVE_GEOMETRY"
   request and an array of them is returned on an "IOCTL_DISK_GET_MEDIA_TYPES"
   request. *)

TYPE
  PDISK_GEOMETRY = UNTRACED REF DISK_GEOMETRY;
  DISK_GEOMETRY = RECORD
    Cylinders: WinNT.LARGE_INTEGER;
    MediaType: MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack  : DWORD;
    BytesPerSector   : DWORD;
  END;

(* The following structure is returned on an "IOCTL_DISK_GET_PARTITION_INFO"
   and an "IOCTL_DISK_GET_DRIVE_LAYOUT" request.  It is also used in a request
   to change the drive layout, "IOCTL_DISK_SET_DRIVE_LAYOUT". *)

TYPE
  PPARTITION_INFORMATION = UNTRACED REF PARTITION_INFORMATION;
  PARTITION_INFORMATION = RECORD
    StartingOffset: WinNT.LARGE_INTEGER;
    PartitionLength: WinNT.LARGE_INTEGER;
    HiddenSectors: DWORD;
    PartitionNumber: DWORD;
    PartitionType: BYTE;
    BootIndicator: WinNT.WBOOLEAN;
    RecognizedPartition: WinNT.WBOOLEAN;
    RewritePartition: WinNT.WBOOLEAN;
  END;

(* The following structure is used to change the partition type of a
   specified disk partition using an "IOCTL_DISK_SET_PARTITION_INFO"
   request. *)

TYPE
  PSET_PARTITION_INFORMATION = UNTRACED REF SET_PARTITION_INFORMATION;
  SET_PARTITION_INFORMATION = RECORD
    PartitionType: BYTE;
  END;

(* The following structures is returned on an "IOCTL_DISK_GET_DRIVE_LAYOUT"
   request and given as input to an "IOCTL_DISK_SET_DRIVE_LAYOUT" request. *)

TYPE
  PDRIVE_LAYOUT_INFORMATION = UNTRACED REF DRIVE_LAYOUT_INFORMATION;
  DRIVE_LAYOUT_INFORMATION = RECORD
    PartitionCount: DWORD;
    Signature: DWORD;
    PartitionEntry: ARRAY [0..0] OF PARTITION_INFORMATION;
  END;

(* The following structure is passed in on an "IOCTL_DISK_VERIFY" request.
   The offset and length parameters are both given in bytes. *)

TYPE
  PVERIFY_INFORMATION = UNTRACED REF VERIFY_INFORMATION;
  VERIFY_INFORMATION = RECORD
    StartingOffset: WinNT.LARGE_INTEGER;
    Length: DWORD;
  END;

(* The following structure is passed in on an "IOCTL_DISK_REASSIGN_BLOCKS"
   request. *)

TYPE
  PREASSIGN_BLOCKS = UNTRACED REF REASSIGN_BLOCKS;
  REASSIGN_BLOCKS = RECORD
    Reserved: WORD;
    Count: WORD;
    BlockNumber: ARRAY [0..0] OF DWORD;
  END;

(* "IOCTL_DISK_MEDIA_REMOVAL" disables the mechanism on a SCSI device
   that ejects media. This function may or may not be supported on
   SCSI devices that support removable media. "PreventMediaRemove"
   is "TRUE" if and only if media should be prevented from being
   removed. *)

TYPE
  PPREVENT_MEDIA_REMOVAL = UNTRACED REF PREVENT_MEDIA_REMOVAL;
  PREVENT_MEDIA_REMOVAL = RECORD
    PreventMediaRemoval: WinNT.WBOOLEAN;
  END;

(* The following structures define disk performance statistics:
   specifically the locations of all the reads and writes which have
   occured on the disk.

   To use these structures, you must issue an "IOCTL_DISK_HIST_STRUCTURE"
   (with a "DISK_HISTOGRAM") to obtain the basic histogram information.
   The number of buckets which must allocated is part of this structure.
   Allocate the required number of buckets and call an "IOCTL_DISK_HIST_DATA"
   to fill in the data. *)

CONST HIST_NO_OF_BUCKETS  = 24;

TYPE
  PHISTOGRAM_BUCKET = UNTRACED REF HISTOGRAM_BUCKET;
  HISTOGRAM_BUCKET = RECORD
    Reads : DWORD;
    Writes: DWORD;
  END;

CONST HISTOGRAM_BUCKET_SIZE = BYTESIZE(HISTOGRAM_BUCKET);

TYPE
  PDISK_HISTOGRAM = UNTRACED REF DISK_HISTOGRAM;
  DISK_HISTOGRAM = RECORD
    DiskSize: WinNT.LARGE_INTEGER;
    Start: WinNT.LARGE_INTEGER;
    End: WinNT.LARGE_INTEGER;
    Average: WinNT.LARGE_INTEGER;
    AverageRead: WinNT.LARGE_INTEGER;
    AverageWrite: WinNT.LARGE_INTEGER;
    Granularity: DWORD;
    Size: DWORD;
    ReadCount: DWORD;
    WriteCount: DWORD;
    Histogram: PHISTOGRAM_BUCKET;
  END;

CONST DISK_HISTOGRAM_SIZE = BYTESIZE(DISK_HISTOGRAM);

(* The following structures define disk debugging capabilities. The
   IOCTLs are directed to one of the two disk filter drivers.

   "DISKPERF" is a utilty for collecting disk request statistics.

   "SIMBAD" is a utility for injecting faults in IO requests to disks. *)

(* The following structure is exchanged on an "IOCTL_DISK_GET_PERFORMANCE"
   request. This ioctl collects summary disk request statistics used in
   measuring performance. *)

TYPE
  PDISK_PERFORMANCE = UNTRACED REF DISK_PERFORMANCE;
  DISK_PERFORMANCE = RECORD
    BytesRead: WinNT.LARGE_INTEGER;
    BytesWritten: WinNT.LARGE_INTEGER;
    ReadTime: WinNT.LARGE_INTEGER;
    WriteTime: WinNT.LARGE_INTEGER;
    ReadCount: DWORD;
    WriteCount: DWORD;
    QueueDepth: DWORD;
  END;

(* This structure defines the disk logging record. When disk logging
   is enabled, one of these is written to an internal buffer for each
   disk request. *)

TYPE
  PDISK_RECORD = UNTRACED REF DISK_RECORD;
  DISK_RECORD = RECORD
   ByteOffset: WinNT.LARGE_INTEGER;
   StartTime: WinNT.LARGE_INTEGER;
   EndTime: WinNT.LARGE_INTEGER;
   VirtualAddress: WinNT.PVOID;
   NumberOfBytes: DWORD;
   DeviceNumber: BYTE;
   ReadRequest: WinNT.WBOOLEAN ;
  END;

(* The following structure is exchanged on an "IOCTL_DISK_LOG" request.
   Not all fields are valid with each function type. *)

TYPE
  PDISK_LOGGING = UNTRACED REF DISK_LOGGING;
  DISK_LOGGING = RECORD
    Function: BYTE;
    BufferAddress: WinNT.PVOID;
    BufferSize: DWORD;
  END;

(* Disk logging functions *)

(* Start disk logging. Only the "Function" and "BufferSize" fields are
valid. *)
CONST DISK_LOGGING_START   = 0;

(* Stop disk logging. Only the "Function" field is valid. *)
CONST DISK_LOGGING_STOP    = 1;

(* Return disk log. All fields are valid. Data will be copied from
   internal buffer to buffer specified for the number of bytes requested. *)
CONST DISK_LOGGING_DUMP    = 2;

(* DISK BINNING

   "DISKPERF" will keep counters for IO that falls in each of these ranges.
   The application determines the number and size of the ranges. Joe Lin
   wanted me to keep it flexible as possible, for instance, IO sizes are
   interesting in ranges like 0-4096, 4097-16384, 16385-65536, 65537+. *)

CONST DISK_BINNING        = 3;

(* Bin types *)
TYPE BIN_TYPES = { RequestSize, RequestLocation };

(* Bin ranges *)
TYPE
  PBIN_RANGE = UNTRACED REF BIN_RANGE;
  BIN_RANGE = RECORD
    StartValue: WinNT.LARGE_INTEGER;
    Length: WinNT.LARGE_INTEGER;
  END;

(* Bin definition *)
TYPE
  PPERF_BIN = UNTRACED REF PERF_BIN;
  PERF_BIN = RECORD
    NumberOfBins: DWORD;
    TypeOfBin: DWORD;
    BinsRanges: ARRAY [0..0] OF BIN_RANGE;
  END;

(* Bin count *)
TYPE
  PBIN_COUNT = UNTRACED REF BIN_COUNT;
  BIN_COUNT = RECORD
    BinRange: BIN_RANGE;
    BinCount: DWORD;
  END;

(* Bin results *)
TYPE
  PBIN_RESULTS = UNTRACED REF BIN_RESULTS;
  BIN_RESULTS = RECORD
    NumberOfBins: DWORD;
    BinCounts: ARRAY[0..0] OF BIN_COUNT;
  END;

CONST
  IOCTL_SERIAL_LSRMST_INSERT = CTL_CODE{
    DeviceType:= FILE_DEVICE_SERIAL_PORT, Function:= 31,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };

(* The following values follow the escape designator in the
   data stream if the "LSRMST_INSERT" mode has been turned on. *)

CONST SERIAL_LSRMST_ESCAPE     : BYTE = 0;

(* Following this value is the contents of the line status
   register, and then the character in the RX hardware when
   the line status register was encountered. *)

CONST SERIAL_LSRMST_LSR_DATA   : BYTE = 1;

(* Following this value is the contents of the line status
   register.  No error character follows. *)

CONST SERIAL_LSRMST_LSR_NODATA : BYTE = 2;

(* Following this value is the contents of the modem status
   register. *)

CONST
  SERIAL_LSRMST_MST : BYTE = 3;

  FSCTL_LOCK_VOLUME       = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:=  6,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  FSCTL_UNLOCK_VOLUME     = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:=  7,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  FSCTL_DISMOUNT_VOLUME   = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:=  8,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  FSCTL_MOUNT_DBLS_VOLUME = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:= 13,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  FSCTL_GET_COMPRESSION   = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:= 15,
    Method:= METHOD_BUFFERED, Access:= FILE_ANY_ACCESS };
  FSCTL_SET_COMPRESSION   = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:= 16,
    Method:= METHOD_BUFFERED, Access:= FILE_READ_DATA + FILE_WRITE_DATA };
  FSCTL_READ_COMPRESSION  = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:= 17,
    Method:= METHOD_NEITHER,  Access:= FILE_READ_DATA  };
  FSCTL_WRITE_COMPRESSION = CTL_CODE{
    DeviceType:= FILE_DEVICE_FILE_SYSTEM, Function:= 18,
    Method:= METHOD_NEITHER,  Access:= FILE_WRITE_DATA };

PROCEDURE IsRecognizedPartition (partitionType: DWORD): BOOLEAN;
(*
//
// Routine Description:
//
//     This macro is used to determine to which partitions drive letters
//     should be assigned.
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is recognized,
//     otherwise FALSE is returned.
//
*)

END WinIoctl.
