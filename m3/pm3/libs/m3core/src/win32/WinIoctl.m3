(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Aug 19 10:39:56 PDT 1996 by heydon     *)
(*      modified on Mon Aug 12 11:52:00 CST 1996 by kalsow     *)

MODULE WinIoctl;

IMPORT Word;
FROM WinDef IMPORT DWORD;

PROCEDURE IsRecognizedPartition (ptype: DWORD): BOOLEAN =
(*
| #define IsRecognizedPartition( PartitionType ) (          \
|      ((PartitionType & PARTITION_NTFT) &&                 \
|       ((PartitionType & ~16_C0) == PARTITION_FAT_12)) ||  \
|      ((PartitionType & PARTITION_NTFT) &&                 \
|       ((PartitionType & ~16_C0) == PARTITION_FAT_16)) ||  \
|      ((PartitionType & PARTITION_NTFT) &&                 \
|       ((PartitionType & ~16_C0) == PARTITION_IFS))    ||  \
|      ((PartitionType & PARTITION_NTFT) &&                 \
|       ((PartitionType & ~16_C0) == PARTITION_HUGE))   ||  \
|      ((PartitionType & ~PARTITION_NTFT) == PARTITION_FAT_12) ||  \
|      ((PartitionType & ~PARTITION_NTFT) == PARTITION_FAT_16) ||  \
|      ((PartitionType & ~PARTITION_NTFT) == PARTITION_IFS)    ||  \
|      ((PartitionType & ~PARTITION_NTFT) == PARTITION_HUGE) )
*)
  BEGIN
    IF Word.And (ptype, PARTITION_NTFT) # 0
      THEN ptype := Word.And (ptype, Word.Not (16_C0));
      ELSE ptype := Word.And (ptype, Word.Not (PARTITION_NTFT));
    END;
    RETURN (ptype = PARTITION_FAT_12)
        OR (ptype = PARTITION_FAT_16)
        OR (ptype = PARTITION_IFS)
        OR (ptype = PARTITION_HUGE);
  END IsRecognizedPartition;

BEGIN
END WinIoctl.

