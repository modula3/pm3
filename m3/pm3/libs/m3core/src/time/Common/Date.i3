(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Dec  9 09:56:34 PST 1993 by mcjones *)

(* A "Date.T" is a moment in time, expressed according to the standard
   (Gregorian) calendar, as observed in some time zone.  A
   "Date.TimeZone" (or just a time zone) is an object that encapsulates
   the rules for converting from UTC (universal coordinated time,
   sometimes known as Greenwich mean time) to local time within a
   particular jurisdiction, taking into account daylight time when
   appropriate.
   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>calendar date</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>local time</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>time</SPAN>
<SPAN CLASS=INDEX.KEY>local</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>time zone</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>time</SPAN>
<SPAN CLASS=INDEX.KEY>zone</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>universal coordinated time</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>time</SPAN>
<SPAN CLASS=INDEX.KEY>UTC (universal coordinated time)</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>Greenwich mean time</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>time</SPAN>
<SPAN CLASS=INDEX.KEY>Greenwich mean time</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>time</SPAN>
<SPAN CLASS=INDEX.KEY>Date interface</SPAN>
<SPAN CLASS=INDEX.TEXT>&lt;TT&gt;Date&lt;/TT&gt; interface</SPAN>
</SPAN>

*)

INTERFACE Date;

IMPORT Time;

TYPE
  T = RECORD
    year: CARDINAL; (* e.g., 1992 *)
    month: Month;
    day: [1 .. 31];
    hour: [0 .. 23];
    minute: [0 .. 59];
    second: [0 .. 59];
    offset: INTEGER;
    zone: TEXT;
    weekDay: WeekDay
  END;
  Month = {Jan, Feb, Mar, Apr, May, Jun, Jul,
            Aug, Sep, Oct, Nov, Dec};
  WeekDay = {Sun, Mon, Tue, Wed, Thu, Fri, Sat};

(* A date's "offset" field specifies the difference in the readings of
   two clocks, one set to UTC and one set to local time, at the moment
   the date occurred, and thus reflects daylight time when
   appropriate.  This difference is specified in seconds, with
   positive values corresponding to local zones behind (west of) UTC.
   A date's "zone" field specifies a name (often a three-letter
   abbreviation) for the time zone in which the date is observed, for
   example, ``PDT'' for Pacific Daylight Time. *)

TYPE TimeZone <: REFANY;

VAR Local, UTC: TimeZone;
(* "Local" is initialized to the time zone in which the computer
   running this program is located.  "UTC" is initialized to the time
   zone for universal coordinated time.  *)

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): T;
(* Return the date corresponding to "t", as observed in the time zone
   "z". If "z" is "NIL", "Local" is used. *)

EXCEPTION Error;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error};
(* Return the time corresponding to the date "d", using the field
   "offset" rather than "zone" and ignoring the field "weekDay". Raise
   "Error" if "d" cannot be represented as a "Time.T". *)

END Date.

(* On POSIX systems, "FromTime(t, Local)" calls "localtime(3)".  On
   Win32 systems, it calls "GetTimeZoneInformation".  Some systems
   keep local time instead of UTC, and typically don't record the
   identity of the local time zone.  On such a system, "FromTime(t,
   Local)" always returns a result with "offset" equal to zero and
   "zone" equal to <TT>&#42;[Unknown zone]&#42;</TT>, and "UTC" is
   "NIL".
*)
