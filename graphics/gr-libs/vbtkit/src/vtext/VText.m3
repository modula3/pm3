(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:13 PST 1993 by meehan                   *)
(*      modified On Tue Jun 16 13:12:32 PDT 1992 by muller                   *)
(*      modified On Wed Mar 25 23:18:37 1992 by steveg                       *)
(*      modified On Fri Mar 20 17:24:47 PST 1992 by jdd                      *)
(*      modified On Tue May 15 17:33:12 PDT 1990 by mcjones                  *)

(* Visible text: the part of the text editor that maintains a consistent
   display of an editable text. It also provides highlighting,
   scrolling, window-splitting, and the insertion caret. *)

MODULE VText;

IMPORT Font, MText, Point, Rd, Rect, Thread, VBT, VT, VTBase, VTCaret, VTDef,
       VTInterval, VTMarker, VTPounce, VTReal, VTView, VTVirtual, VTextRegion;

TYPE
  LineNo = VTDef.LineNo;
  VirtualStart = VTDef.VirtualStart;
  
(************************************************************************)
(*			        Creation 				*)
(************************************************************************)


PROCEDURE New (         mtext   : MText.T;
                        vbt     : VBT.T;
               READONLY rect    : Rect.T;
               READONLY vOptions: VOptions ): T
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR vtext: T;
  BEGIN
    IF (mtext = NIL) OR (vbt = NIL) OR (vOptions.vFontxxx = NIL) THEN
      RAISE VTDef.Error (ErrorCode.IsNil);
    END;
    vtext := NEW (T);
    vtext.mtext := mtext;       (* old-style *)
    vtext.vbt := vbt;           (* old-style *)
    vtext.font := vOptions.vFontxxx.vFont.font; (* old-style *)
    vtext.vOptions := vOptions; (* semi old-style *)
    vtext.vt := VT.New (mtext);
    vtext.west := rect.west;    (* old-style *)
    vtext.north := rect.north;  (* old-style *)
    vtext.width := rect.east - rect.west; (* old-style *)
    vtext.height := rect.south - rect.north; (* old-style *)
    vtext.regionMax := 0;       (* old-style *)
    vtext.closed := FALSE;
    VTextRegion.SetupRegion (
      vtext, 0, rect.north, rect.south - rect.north, 0);
    vtext.lineSpacing :=
      vtext.region [0].view.vScreenFont.vScreenFont.box.south
        - vtext.region [0].view.vScreenFont.vScreenFont.box.north
        + vtext.leading;
    (* old-style *)
    vtext.lineOffset :=
      -vtext.region [0].view.vScreenFont.vScreenFont.box.north;
    (* old-style *)
    vtext.caretState := vtext.vt.caret.state; (* old-style *)
    vtext.dividersDirty := FALSE;
    RETURN vtext;
  END New;


PROCEDURE ExplodeVText (READONLY     vtext   : T;
                        VAR (* OUT*) mtext   : MText.T;
                        VAR (* OUT*) vbt     : VBT.T;
                        VAR (* OUT*) rect    : Rect.T;
                        VAR (* OUT*) vOptions: VOptions ) RAISES {} =
  BEGIN
    mtext := vtext.mtext;
    vbt := vtext.vbt;
    rect := Rect.FromEdges(vtext.west, vtext.west + vtext.width,
                           vtext.north, vtext.north + vtext.height);
    vOptions := vtext.vOptions;
  END ExplodeVText;



PROCEDURE MakeVFont (         font     : Font.T;
                     READONLY printable: SET OF CHAR;
                              whiteTabs: BOOLEAN      ): VFont
  RAISES {VTDef.Error} =
  BEGIN
    RETURN VTView.MakeVFont(font, printable, whiteTabs);
  END MakeVFont;


PROCEDURE ExplodeVFont (READONLY     vFont    : VFont;
                        VAR (* OUT*) font     : Font.T;
                        VAR (* OUT*) printable: SET OF CHAR;
                        VAR (* OUT*) whiteTabs: BOOLEAN      ) RAISES {} =
  BEGIN
    font := vFont.vFont.font;
    printable := vFont.vFont.printable;
    whiteTabs := vFont.vFont.whiteTabs;
  END ExplodeVFont;



PROCEDURE MakeVOptions (vFont: VFont;
                        leftMargin, rightMargin, turnMargin, topMargin,
                          leading: Points;
                        whiteBlack, whiteStroke: ColorScheme;
                        leftOffset             : Points;
                        wrap                   : BOOLEAN;
                        eob                    : BOOLEAN;
                        intervalStylePrecedence:
                            IntervalStylePrecedence := NIL):
  VOptions RAISES {} =
  BEGIN
    RETURN VTView.MakeVOptions(vFont, leftMargin, rightMargin, turnMargin,
                               topMargin, leading, whiteBlack, whiteStroke,
                               leftOffset, wrap, eob,
                               intervalStylePrecedence);
  END MakeVOptions;



PROCEDURE ExplodeVOptions (READONLY     vOptions: VOptions;
                           VAR (* OUT*) vFont   : VFont;
                           VAR (* OUT*) leftMargin, rightMargin, turnMargin,
                                        topMargin, leading: Points;
                           VAR (* OUT*) whiteBlack, whiteStroke: ColorScheme;
                           VAR (* OUT*) leftOffset: Points;
                           VAR (* OUT*) wrap: BOOLEAN;
                           VAR (* OUT*) eob: BOOLEAN;
                           VAR (* OUT*) intervalStylePrecedence:
                                            IntervalStylePrecedence)
  RAISES {} =
  BEGIN
    vFont := vOptions.vFontxxx;
    leftMargin := vOptions.leftMarginPts;
    rightMargin := vOptions.rightMarginPts;
    turnMargin := vOptions.turnMarginPts;
    topMargin := vOptions.topMarginPts;
    leading := vOptions.leadingPts;
    whiteBlack := vOptions.whiteBlack;
    whiteStroke := vOptions.whiteStroke;
    leftOffset := vOptions.leftOffsetPts;
    wrap := vOptions.wrap;
    eob := vOptions.eob;
    intervalStylePrecedence := vOptions.intervalStylePrecedence;
  END ExplodeVOptions;


PROCEDURE ChangeVOptions (vtext: T; READONLY vOptions: VOptions)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR start: Index;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      vtext.vOptions := vOptions;
      vtext.font := vtext.vOptions.vFontxxx.vFont.font; (* old-style *)
      WITH vo = vtext.vOptions DO
        VTView.SetPixelOptions (vo, vtext.vbt);
        vtext.leftMargin := vo.leftMargin;
        vtext.rightMargin := vo.rightMargin;
        vtext.turnMargin := vo.turnMargin;
        vtext.topMargin := vo.topMargin;
        vtext.leading := vo.leading
      END;
      vtext.left := vtext.west + vtext.leftMargin + vtext.turnMargin;
      vtext.lineSpacing :=
        vtext.region [0].view.vScreenFont.vScreenFont.box.south
          - vtext.region [0].view.vScreenFont.vScreenFont.box.north
          + vtext.leading;
      (* old-style *)
      vtext.lineOffset :=
        -vtext.region [0].view.vScreenFont.vScreenFont.box.north;
      (* old-style *)

      FOR r := 0 TO vtext.regionMax DO
        start := vtext.region [r].view.virtual.start.at;
        VTView.Close (vtext.region [r].view);
        VTextRegion.SetupRegion (
          vtext, r, vtext.region [r].north, vtext.region [r].height, start);
      END;
      vtext.dividersDirty := TRUE;

    END;
  END ChangeVOptions;

PROCEDURE Close (vtext: T) RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed);  END;
      VT.Close (vtext.vt);
      vtext.closed := TRUE;
    END;
  END Close;
  
(************************************************************************)
(*				 Regions				*)
(************************************************************************)

PROCEDURE SplitRegion (vtext : T;
                       r     : Region;
                       v     : Pixels;
                       scroll: BOOLEAN  := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      VTextRegion.SplitRegion(vtext, r, v, scroll);
    END;
  END SplitRegion;


PROCEDURE MergeRegion (vtext: T; i, j: Region; scroll: BOOLEAN := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      VTextRegion.MergeRegion(vtext, i, j, scroll);
    END;
  END MergeRegion;

(************************************************************************)
(* Moving *)
(************************************************************************)


PROCEDURE Move (         vtext             : T;
                READONLY newRect, savedRect: Rect.T;
                READONLY dividers          : ARRAY OF Pixels;
                         scroll            : BOOLEAN          := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      VTextRegion.Move(vtext, newRect, savedRect, dividers, scroll);
    END;
  END Move;


PROCEDURE Rescreen (vtext: T; READONLY cd: VBT.RescreenRec) RAISES {} =
  VAR view := vtext.vt.views;
  BEGIN
    WHILE view # NIL DO VTView.Rescreen (view, cd); view := view.next; END;
    WITH vo = vtext.vOptions DO
      VTView.SetPixelOptions (vo, vtext.vbt);
      vtext.leftMargin := vo.leftMargin;
      vtext.rightMargin := vo.rightMargin;
      vtext.turnMargin := vo.turnMargin;
      vtext.topMargin := vo.topMargin;
      vtext.leading := vo.leading;

      vtext.left := vtext.west + vtext.leftMargin + vtext.turnMargin;
      vtext.lineSpacing :=
        vtext.region [0].view.vScreenFont.vScreenFont.box.south
          - vtext.region [0].view.vScreenFont.vScreenFont.box.north
          + vtext.leading;      (* old-style *)
      vtext.lineOffset :=
        -vtext.region [0].view.vScreenFont.vScreenFont.box.north;
      (* old-style *)
    END
  END Rescreen;

(************************************************************************)
(* Drawing *)
(************************************************************************)


PROCEDURE Update (vtext: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    ConcurrentUpdate (vtext);
    Quiesce (vtext);
  END Update;

PROCEDURE ConcurrentUpdate (vtext: T)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      VTextRegion.UpdateDividers (vtext);
      VTReal.Update (vtext.vt);
    END;
  END ConcurrentUpdate;

PROCEDURE Quiesce (vtext: T) RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed);  END;
    END;
  END Quiesce;

PROCEDURE Bad (vtext: T; READONLY where: Rect.T) RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed);  END;
      VTextRegion.Bad (vtext, where);
    END
  END Bad;
(************************************************************************)
(*			        Editing   				*)
(************************************************************************)

PROCEDURE Replace (vtext: T; begin, end: Index; newText: TEXT)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF begin > vtext.vt.length THEN begin := vtext.vt.length; END;
      IF end > vtext.vt.length THEN end := vtext.vt.length; END;
      IF begin > end THEN RAISE VTDef.Error(ErrorCode.IllegalIndex); END;
      VT.Replace(vtext.vt, begin, end, newText);
    END;
  END Replace;


PROCEDURE ReplaceChars (         vtext     : T;
                                 begin, end: Index;
                        READONLY str       : ARRAY OF CHAR)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF begin > vtext.vt.length THEN begin := vtext.vt.length; END;
      IF end > vtext.vt.length THEN end := vtext.vt.length; END;
      IF begin > end THEN RAISE VTDef.Error (ErrorCode.IllegalIndex); END;
      VT.ReplaceChars (vtext.vt, begin, end, str);
    END;
  END ReplaceChars;


PROCEDURE ReplaceFile (vtext     : T;
                       begin, end: Index;
                       file      : Rd.T;
                       start     : Index   := 0;
                       numChars  : Index   := LAST(Index))
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF begin > vtext.vt.length THEN begin := vtext.vt.length; END;
      IF end > vtext.vt.length THEN end := vtext.vt.length; END;
      IF begin > end THEN RAISE VTDef.Error(ErrorCode.IllegalIndex); END;
      VT.ReplaceFile(vtext.vt, begin, end, file, start, numChars);
    END;
  END ReplaceFile;


PROCEDURE Invalidate (vtext: T; begin, oldEnd, newEnd: Index)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF begin > vtext.vt.length THEN begin := vtext.vt.length; END;
      IF oldEnd > vtext.vt.length THEN oldEnd := vtext.vt.length; END;
      IF (begin > oldEnd) OR (begin > newEnd) THEN
        RAISE VTDef.Error(ErrorCode.IllegalIndex);
      END;
      VT.Invalidate(vtext.vt, begin, oldEnd, newEnd - begin);
    END;
  END Invalidate;
  
(************************************************************************)
(* The Caret *)
(************************************************************************)


PROCEDURE SwitchCaret (vtext: T; state: OnOffState)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      VTCaret.Switch(vtext.vt, state);
      vtext.caretState := state; (* old-style *)
    END;
  END SwitchCaret;


PROCEDURE MoveCaret (vtext: T; place: Index)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF place > vtext.vt.length THEN place := vtext.vt.length; END;
      VTCaret.Move(vtext.vt, place);
    END;
  END MoveCaret;


PROCEDURE CaretIndex (vtext: T): Index RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      RETURN vtext.vt.caret.index;
    END;
  END CaretIndex;
  
(************************************************************************)
(* Intervals *)
(************************************************************************)


PROCEDURE CreateInterval (         vtext         : T;
                                   indexL, indexR: Index;
                          READONLY options       : IntervalOptions):
  Interval RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF indexL > vtext.vt.length THEN indexL := vtext.vt.length; END;
      IF indexR > vtext.vt.length THEN indexR := vtext.vt.length; END;
      IF indexL > indexR THEN
        RAISE VTDef.Error(ErrorCode.IllegalIndex);
      END;
      RETURN VTInterval.New(vtext.vt, indexL, indexR, options);
    END;
  END CreateInterval;


PROCEDURE ExplodeInterval (READONLY      interval      : Interval;
                           VAR (* OUT *) indexL, indexR: Index;
                           VAR (* OUT *) options       : IntervalOptions;
                           VAR (* OUT *) state         : OnOffState       )
  RAISES {} =
  BEGIN
    VTInterval.ExplodeInterval (interval, indexL, indexR, options, state)
  END ExplodeInterval;

PROCEDURE MakeIntervalOptions (style                  : IntervalStyle;
                               whiteBlack, whiteStroke: ColorScheme;
                               leading                : Tint           ):
  IntervalOptions RAISES {} =
  BEGIN
    RETURN VTInterval.MakeOptions(style, whiteBlack, whiteStroke, leading);
  END MakeIntervalOptions;


PROCEDURE ExplodeIntervalOptions (READONLY intervalOptions: IntervalOptions;
                                  VAR (* OUT*) style: IntervalStyle;
                                  VAR (* OUT*) whiteBlack, whiteStroke: ColorScheme;
                                  VAR (* OUT*) leading: Tint) RAISES {} =
  BEGIN
    style := intervalOptions.style;
    whiteBlack := intervalOptions.whiteBlack;
    whiteStroke := intervalOptions.whiteStroke;
    leading := intervalOptions.leading;
  END ExplodeIntervalOptions;


PROCEDURE SwitchInterval (interval: Interval; state: OnOffState)
  RAISES {VTDef.Error} =
  BEGIN
    VTInterval.Switch (interval, state)
  END SwitchInterval;

PROCEDURE MoveInterval (interval: Interval; indexL, indexR: Index)
  RAISES {VTDef.Error} =
  BEGIN
    VTInterval.Move (interval, indexL, indexR)
  END MoveInterval;


PROCEDURE ChangeIntervalOptions (         interval: Interval;
                                 READONLY options : IntervalOptions)
  RAISES {VTDef.Error} =
  BEGIN
    VTInterval.ChangeOptions (interval, options)
  END ChangeIntervalOptions;

PROCEDURE DeleteInterval (interval: Interval) RAISES {VTDef.Error} =
  BEGIN
    VTInterval.Delete (interval)
  END DeleteInterval;

(************************************************************************)
(*			     Markers					*)
(************************************************************************)

PROCEDURE CreateMarker (vtext: T; at: Index; options: MarkerOptions):
  Marker RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF at > vtext.vt.length THEN at := vtext.vt.length; END;
      RETURN VTMarker.New (vtext.vt, at, options);
    END;
  END CreateMarker;

PROCEDURE ExplodeMarker (READONLY      marker : Marker;
                         VAR (* OUT *) at     : Index;
                         VAR (* OUT *) options: MarkerOptions;
                         VAR (* OUT*)  state  : OnOffState     )
  RAISES {} =
  BEGIN
    at := marker.index;
    options := marker.options;
    state := marker.state;
  END ExplodeMarker;

PROCEDURE MakeMarkerOptions (whichEnd   : WhichEnd;
                             top, bottom: BOOLEAN;
                             stroke     : Tint      ): MarkerOptions
  RAISES {} =
  BEGIN
    RETURN VTMarker.MakeOptions (whichEnd, top, bottom, stroke);
  END MakeMarkerOptions;

PROCEDURE ExplodeMarkerOptions (READONLY markerOptions: MarkerOptions;
                                VAR (* OUT *) whichEnd   : WhichEnd;
                                VAR (* OUT *) top, bottom: BOOLEAN;
                                VAR (* OUT *) stroke     : Tint      )
  RAISES {} =
  BEGIN
    whichEnd := markerOptions.whichEnd;
    top := markerOptions.top;
    bottom := markerOptions.bottom;
    stroke := markerOptions.stroke;
  END ExplodeMarkerOptions;

PROCEDURE SwitchMarker (marker: Marker; state: OnOffState)
  RAISES {VTDef.Error} =
  VAR vt: VTDef.T;
  BEGIN
    IF marker = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    vt := marker.vt;
    IF vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vt.mutex DO
      IF vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF marker.vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
      VTMarker.Switch (marker, state);
    END;
  END SwitchMarker;
(* Sets marker's state := state. *)

PROCEDURE MoveMarker (marker: Marker; index: Index) RAISES {VTDef.Error} =
  VAR vt: VTDef.T;
  BEGIN
    IF marker = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    vt := marker.vt;
    IF vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    LOCK vt.mutex DO
      IF vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed);  END;
      IF marker.vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
      IF index > marker.vt.length THEN index := marker.vt.length;  END;
      VTMarker.Move (marker, index);
    END;
  END MoveMarker;
(* Moves the marker. *)

PROCEDURE ChangeMarkerOptions (marker: Marker; options: MarkerOptions)
  RAISES {VTDef.Error} =
  VAR vt: VTDef.T;
  BEGIN
    IF marker = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    vt := marker.vt;
    IF vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vt.mutex DO
      IF vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF marker.vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
      VTMarker.ChangeOptions (marker, options);
    END;
  END ChangeMarkerOptions;
(* Re-sets marker's options. *)

PROCEDURE DeleteMarker (marker: Marker) RAISES {VTDef.Error} =
  VAR vt: VTDef.T;
  BEGIN
    IF marker = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    vt := marker.vt;
    IF vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
    LOCK vt.mutex DO
      IF vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed);  END;
      IF marker.vt = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil);  END;
      VTMarker.Close (marker);
    END;
  END DeleteMarker;
(* Sets marker's state = Off and then deletes marker from the set of
   markers associated with the VText. *)

(************************************************************************)
(*		                Scrolling				*)
(************************************************************************)

PROCEDURE Scroll (vtext: T; r: Region; displacement: INTEGER)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      WITH z_144 = vtext.region [r] DO
        WITH z_145 = z_144.view^ DO
          IF displacement = 0 THEN RETURN; END;
          IF displacement > 0 THEN
            VTVirtual.SetStart (
              z_144.view, MIN (VTBase.Down (
                                 z_144.view, z_145.virtual.start.at,
                                 displacement), z_145.vt.length));
          ELSE
            VTVirtual.SetStart (
              z_144.view, z_145.virtual.start.at, -displacement);
          END;
        END;
      END;
    END;
  END Scroll;

PROCEDURE SetStart (vtext  : T;
                    r      : Region;
                    place  : Index;
                    upLines: CARDINAL := 0;
                    force  : BOOLEAN  := FALSE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error(ErrorCode.IllegalRegion);
      END;
      IF place > vtext.vt.length THEN place := vtext.vt.length; END;
      VTVirtual.SetStart(vtext.region[r].view, place, upLines, force);
    END;
  END SetStart;

PROCEDURE LinesBetween (vtext     : T;
                        begin, end: Index;
                        max       : CARDINAL;
                        avail     : Pixels      := UseCurrentWidth; ):
  INTEGER RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* Compute the number of lines that would be displayed between indexL and
     end, with "avail" horizontal pixels available. 0 if on same line, 1 if
     adjacent, etc. Will quit computing and return "max" if there are more
     lines than that between begin and end. *)
  VAR
    index, by: INTEGER;
    lineCount: INTEGER;
    turned   : BOOLEAN;
    l0, l1   : INTEGER;
    start    : VirtualStart;
    width    : Pixels;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext^.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF avail = UseCurrentWidth THEN
        avail := vtext^.region[0].view.lineWidth;
      END;
      IF begin > vtext.vt.length THEN begin := vtext.vt.length; END;
      IF end > vtext.vt.length THEN end := vtext.vt.length; END;
      IF begin > end THEN RAISE VTDef.Error(ErrorCode.IllegalIndex); END;
      (* See if we can give a simple answer *)
      IF avail = vtext^.region[0].view.lineWidth THEN
        FOR r := 0 TO vtext^.regionMax DO
          IF NOT vtext^.region[r].view.virtual.bodyDirty THEN
            l0 := VTBase.UnsafeLocateLine(vtext^.region[r].view, begin);
            IF l0 >= 0 THEN
              l1 := VTBase.UnsafeLocateLine(vtext^.region[r].view, end);
              IF l1 >= 0 THEN RETURN MIN(MAX(l1 - l0, -1), max); END;
            END;
          END;
        END;
      END;
      (* do it the hard way *)
      VTBase.Up(vtext.region[0].view, avail, begin, 0, start);
      index := start.at;
      lineCount := -1;
      WHILE (index <= end) AND (lineCount < max) DO
        index := VTBase.ComputeLine(
                   vtext^.region[0].view, avail, index, by, turned, width);
        lineCount := lineCount + 1;
      END;
      RETURN lineCount;
    END;
  END LinesBetween;

PROCEDURE ComputeLine (              vtext : T;
                                     from  : Index;
                       VAR (* OUT *) max   : Index;
                       VAR (* OUT *) turned: BOOLEAN;
                       VAR (* OUT *) width : Pixels    ): Index
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR i, m: INTEGER;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF from > vtext.vt.length THEN from := vtext.vt.length; END;
      i := VTBase.ComputeLine (
             vtext.region [0].view, vtext.region [0].view.lineWidth, from, m,
             turned, width);
      max := m;
      RETURN MIN (i, vtext.vt.length);
    END;
  END ComputeLine;

PROCEDURE UpLines (vtext: T; place: Index; n: CARDINAL; r: Region := 0):
  Index RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR start: VirtualStart;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error(ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error(ErrorCode.Closed); END;
      IF place > vtext.vt.length THEN place := vtext.vt.length; END;
      VTBase.Up(vtext.region[r].view, vtext.region[r].view.lineWidth,
                place, n, start);
      RETURN start.at;
    END;
  END UpLines;

(************************************************************************)
(* Locations *)
(************************************************************************)

PROCEDURE StartIndex (vtext: T; r: Region): Index RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      WITH z_148 = vtext.region [r] DO
        RETURN z_148.view.virtual.start.at;
      END;
    END;
  END StartIndex;

PROCEDURE LineIndex (vtext: T; r: Region; n: CARDINAL): Index
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      WITH z_149 = vtext.region [r] DO
        WITH z_150 = z_149.view^ DO
          IF z_150.virtual.dirty THEN
            VTVirtual.UpdateView (z_149.view);
          END;
          RETURN MIN (z_150.virtual.line [
                        MIN (n, z_150.virtual.lines)].virtualLine.from,
                      z_150.vt.length);
        END;
      END;
    END;
  END LineIndex;

PROCEDURE CharsInRegion (vtext: T; r: Region): CARDINAL
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      WITH z_151 = vtext.region [r] DO
        WITH z_152 = z_151.view^ DO
          IF z_152.virtual.dirty THEN
            VTVirtual.UpdateView (z_151.view);
          END;
          RETURN MIN (z_152.virtual.line [
                        z_152.virtual.lines].virtualLine.from,
                      z_152.vt.length)
                   - z_152.virtual.line [0].virtualLine.from;
        END;
      END;
    END;
  END CharsInRegion;

PROCEDURE Locate (              vtext: T;
                                r    : Region;
                                place: Index;
                  VAR (* OUT *) h, v : INTEGER )
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR p: Point.T;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      IF place > vtext.vt.length THEN place := vtext.vt.length; END;
      WITH z_153 = vtext.region [r] DO
        IF z_153.view.virtual.dirty THEN
          VTVirtual.UpdateView (z_153.view);
        END;
        VTBase.UnsafeLocatePoint (z_153.view, place, p);
        IF p.v >= 0 THEN
          h := p.h;
          v := p.v + -z_153.view.vScreenFont.vScreenFont.box.north;
        ELSE
          v := p.v;
        END;
      END;
    END;
  END Locate;

PROCEDURE InRegion (vtext: T; r: Region; place: Index): BOOLEAN
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR p: Index;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      IF place > vtext.vt.length THEN place := vtext.vt.length; END;
      p := place;
      WITH z_154 = vtext.region [r] DO
        WITH z_155 = z_154.view^ DO
          IF p < z_155.virtual.start.at THEN RETURN FALSE; END;
          IF z_155.virtual.dirty THEN
            VTVirtual.UpdateView (z_154.view);
          END;
          RETURN
            p < z_155.virtual.line [z_155.virtual.lines].virtualLine.from;
        END;
      END;
    END;
  END InRegion;

PROCEDURE WhichLine (vtext: T; r: Region; v: Pixels): CARDINAL
  RAISES {VTDef.Error} =
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      WITH z_156 = vtext.region [r] DO
        RETURN
          MAX (
            MIN ((v - z_156.view.vOptions.topMargin)
                   DIV z_156.view.lineSpacing, z_156.view.nLines - 1), 0);
      END;
    END;
  END WhichLine;

PROCEDURE Pounce (              vtext                 : T;
                                r                     : Region;
                                p                     : Point.T;
                                mode                  : SelectionMode;
                  VAR (* OUT *) indexL, indexM, indexR: Index;
                  VAR (* OUT *) cage                  : Rect.T         ):
  WhichEnd RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    c         : CHAR;
    iL, iM, iR: INTEGER;
    whichEnd  : WhichEnd;
    lineNo    : LineNo;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      VTPounce.Locate (vtext.region [r].view, p, iL, iR, lineNo, c);
      VTPounce.Extend (vtext.region [r].view, iL, iR, lineNo, c, mode);
      whichEnd :=
        VTPounce.Encage (vtext.region [r].view, p, iL, iM, iR, cage);
      <* ASSERT iR <= vtext.vt.length *> (* past the end of the mtext *)
      indexL := iL;
      indexM := iM;
      indexR := iR;
      RETURN whichEnd;
    END;
  END Pounce;

PROCEDURE PounceLocate (              vtext         : T;
                                      r             : Region;
                                      p             : Point.T;
                        VAR (* OUT *) indexL, indexR: Index;
                        VAR (* OUT *) lineNumber    : CARDINAL;
                        VAR (* OUT *) c             : CHAR      )
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    iL, iR: INTEGER;
    lineNo: LineNo;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      VTPounce.Locate (vtext.region [r].view, p, iL, iR, lineNo, c);
      indexL := iL;
      indexR := iR;
      lineNumber := lineNo;
    END;
  END PounceLocate;

PROCEDURE PounceExtend (                vtext         : T;
                                        r             : Region;
                        VAR (* INOUT *) indexL, indexR: Index;
                                        lineNumber    : CARDINAL;
                                        c             : CHAR;
                                        mode          : SelectionMode)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR iL, iR: INTEGER;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      IF indexL > vtext.vt.length THEN indexL := vtext.vt.length; END;
      IF indexR > vtext.vt.length THEN indexR := vtext.vt.length; END;
      IF indexL > indexR THEN
        RAISE VTDef.Error (ErrorCode.IllegalIndex);
      END;
      iL := indexL;
      iR := indexR;
      VTPounce.Extend (vtext.region [r].view, iL, iR, lineNumber, c, mode);
      <* ASSERT iR <= vtext.vt.length *> (* past the end of the mtext *)
      indexL := iL;
      indexR := iR;
    END;
  END PounceExtend;

PROCEDURE PounceEncage (              vtext : T;
                                      r     : Region;
                                      p     : Point.T;
                                      indexL: Index;
                        VAR (* OUT *) indexM: Index;
                                      indexR: Index;
                        VAR (* OUT *) cage  : Rect.T   ): WhichEnd
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure,
                Thread.Alerted} =
  VAR
    iM      : INTEGER;
    whichEnd: WhichEnd;
  BEGIN
    IF vtext = NIL THEN RAISE VTDef.Error (ErrorCode.IsNil); END;
    LOCK vtext.vt.mutex DO
      IF vtext.vt.closed THEN RAISE VTDef.Error (ErrorCode.Closed); END;
      IF r > vtext.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      IF indexL > vtext.vt.length THEN indexL := vtext.vt.length; END;
      IF indexR > vtext.vt.length THEN indexR := vtext.vt.length; END;
      IF indexL > indexR THEN
        RAISE VTDef.Error (ErrorCode.IllegalIndex);
      END;
      whichEnd := VTPounce.Encage (
                    vtext.region [r].view, p, indexL, iM, indexR, cage);
      indexM := iM;
      RETURN whichEnd;
    END;
  END PounceEncage;

BEGIN
END VText.
