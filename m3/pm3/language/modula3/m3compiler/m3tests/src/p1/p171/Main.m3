(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(********************************
Return-Path: muller@src.dec.com
Received: by jumbo.pa.dec.com; id AA18321; Mon, 24 Aug 92 13:34:19 -0700
Received: by procope.pa.dec.com; id AA08982; Mon, 24 Aug 92 13:34:18 -0700
Resent-Message-Id: <9208242034.AA08982@procope.pa.dec.com>
Return-Path: veach 
Delivery-Date: Mon, 24 Aug 92 12:09:50 PDT
Return-Path: veach
Received: by flimflam.pa.dec.com; id AA03128; Mon, 24 Aug 92 12:09:48 -0700
Received: by madrone.pa.dec.com; id AA17117; Mon, 24 Aug 92 12:10:00 -0700
Message-Id: <9208241910.AA17117@madrone.pa.dec.com>
To: muller
Cc: veach
Subject: M3 compiler bug
Date: Mon, 24 Aug 92 12:09:59 -0700
From: veach
X-Mts: smtp
Resent-To: kalsow
Resent-Date: Mon, 24 Aug 92 13:34:17 PDT
Resent-From: Eric Muller <muller>


This example produces a syntax error in the C code generated by m3:

*******************************)

MODULE Main;

TYPE
  Node = RECORD
    key: INTEGER;
    children := ARRAY [0..1] OF REF Node{ NIL, NIL };
  END;

BEGIN
  EVAL BITSIZE (Node);
END Main.

(*********************************
It tries to insert a "typedef" in the middle of a structure declaration.
I can work around this by declaring the "array" type explicitly.

Eric
********************************)
