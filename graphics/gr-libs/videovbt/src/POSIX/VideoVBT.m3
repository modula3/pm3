(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Freeman *)
(* Last modified on Wed Mar 22 18:21:09 PST 1995 by msm      *)
(*      modified on Mon Oct 25 13:52:17 PDT 1993 by sfreeman *)

UNSAFE MODULE VideoVBT EXPORTS VideoVBT, VideoVBTRep;

IMPORT Atom, AtomList, Axis, BasicCtypes, IO, JVBuffer, JVConverter,
       JVFromDecomp, JVDecomp, JVDecompPool, Jvs, JvsBuffer, JVSink,
       OSError, Picture, Point, RdUtils, Rect, Region, ScreenType, Stdio,
       Thread, Tick, Time, Trestle, TrestleClass, TrestleComm, TrestleOnX,
       VBT, VBTClass, Word, XScreenType, XScrnCmap(*, Fmt *);

<*PRAGMA LL*>

(* {{{ -- Buffer -- *)

REVEAL
  Buffer = BufferPublic BRANDED OBJECT
             st           : VBT.ScreenType        := NIL;
             width, height: CARDINAL              := 0;
             pic          : Picture.T             := NIL;
             shmInfo      : Picture.SharedMemInfo := NIL;
             next         : Buffer                := NIL;
             fac          : Factory               := NIL;
             lastUse      : CARDINAL              := 0;
           OVERRIDES
             init    := BInit;
             picture := BPicture;
           END;

PROCEDURE BInit (b: Buffer; shmid: BasicCtypes.int; address: ADDRESS):
  JVBuffer.T RAISES {OSError.E} =
  BEGIN
    TRY
      EVAL JVFromDecomp.T.init(b, shmid, address);
      IF b.st # NIL THEN
        <* ASSERT b.addr # NIL *>
        LOCK b DO
          b.shmInfo :=
            NEW(Picture.SharedMemInfo, id := b.shmid, address := address);
          b.pic := Picture.New(b.st, b.width, b.height);
          Picture.AttachData(b.pic, address, b.shmInfo);
        END;
      END;
    EXCEPT
    | Picture.TrestleFail =>
        RAISE
          OSError.E(AtomList.List2(Atom.FromText("VideoVBT.Init"),
                                   Atom.FromText("Picture.TrestleFail")));
    | Picture.ScreenTypeNotSupported =>
        RAISE OSError.E(
                AtomList.List2(Atom.FromText("VideoVBT.Init"),
                               Atom.FromText("Picture.BadScreenType")));
    END;
    RETURN b;
  END BInit;

PROCEDURE BPicture (b: Buffer): Picture.T =
  BEGIN
    RETURN b.pic;
  END BPicture;

(* }}} *)
(* {{{ -- factory -- *)

REVEAL
  Factory = PublicFactory BRANDED OBJECT
              st           : VBT.ScreenType := NIL;
              width, height: CARDINAL;
            OVERRIDES
              preInit := FPreInit;
              reset   := FReset;
              newBuf  := FNewBuf;
              destroy := FDontDestroy;
              make    := FMake;
            END;

VAR
  fmu               := NEW(MUTEX);
  thread : Thread.T := NIL;
  bufList: Buffer   := NIL;

PROCEDURE FDontDestroy (fac: JVBuffer.Factory; buf: JVBuffer.T) =
  VAR b: Buffer := buf;
  BEGIN
    LOCK fmu DO
      IF thread = NIL THEN
        thread := Thread.Fork(NEW(Thread.Closure, apply := Cleaner))
      END;
      INC(fladds);
      b.next := bufList;
      bufList := buf;
      b.lastUse := 0;
      b.fac := fac
    END
  END FDontDestroy;

VAR fladds, flsubs, flcleans := 0;

PROCEDURE Cleaner (<* UNUSED *> cl: Thread.Closure): REFANY =
  VAR
    useless                            := 0;
    cleanse, cprev, prev, next: Buffer := NIL;
  BEGIN
    LOOP
      cprev := NIL;
      prev := NIL;
      next := NIL;
      cleanse := NIL;
      Thread.Pause(3.0D0);
(*          IO.Put(
              Fmt.F("VideoVBT: adds: %s, subs: %s, cleans: %s\n",
                    Fmt.Int(fladds), Fmt.Int(flsubs), Fmt.Int(flcleans)),
              Stdio.stderr);
*)
      LOCK fmu DO
        IF bufList = NIL THEN
          INC(useless);
          IF useless > 15 THEN thread := NIL; RETURN NIL END
        ELSE
          useless := 0;
          next := bufList;
          WHILE next # NIL DO
            INC(next.lastUse);
            IF next.lastUse <= 2 THEN
              cleanse := NIL
            ELSIF cleanse = NIL THEN
              cleanse := next;
              cprev := prev
            END;
            prev := next;
            next := next.next
          END;
          IF cleanse # NIL THEN
            IF cprev = NIL THEN bufList := NIL ELSE cprev.next := NIL END
          END
        END
      END;
      WHILE cleanse # NIL DO
        next := cleanse.next;
        TRY
          INC(flcleans);
          LOCK cleanse DO FDestroy(cleanse.fac, cleanse) END
        EXCEPT
          OSError.E, Thread.Alerted =>
        END;
        cleanse := next
      END
    END
  END Cleaner;

PROCEDURE FMake (f: Factory; wait := TRUE; subtype: CARDINAL): JVBuffer.T
  RAISES {Thread.Alerted, OSError.E} =

  VAR prev, cur: Buffer := NIL;
  BEGIN
    LOCK fmu DO
      cur := bufList;
      WHILE cur # NIL AND (cur.st # f.st OR cur.width # f.width
                             OR cur.height # f.height 
			     OR f.subtype # cur.subtype) DO
        prev := cur;
        cur := cur.next
      END;
      IF cur # NIL THEN
        INC(flsubs);
        IF prev = NIL THEN
          bufList := cur.next
        ELSE
          prev.next := cur.next
        END;
        cur.next := NIL;
        cur.fac := NIL;
        RETURN cur
      END;
    END;
    RETURN PublicFactory.make(f, wait, subtype)
  END FMake;

PROCEDURE FPreInit (f: Factory; st: VBT.ScreenType; width, height: CARDINAL)
  RAISES {Picture.ScreenTypeNotSupported, OSError.E} =
  VAR params: Jvs.DcmpParams;
  BEGIN
    IF NOT Picture.Supported(st) THEN
      RAISE Picture.ScreenTypeNotSupported;
    END;

    (* get nearest output width and height *)
    params.inX := width;
    params.inY := height;
    params.reqX := width;
    params.reqY := height;
    TRY
      WITH jvs = NEW(Jvs.T).init() DO
        EVAL jvs.setDecompress(params);
        jvs.close();
      END;
    EXCEPT
    | Thread.Alerted =>          (* skip *)
    END;

    f.st := st;
    f.width := params.outX;
    f.height := params.outY;
  END FPreInit;

PROCEDURE FNewBuf (f: Factory): JvsBuffer.T =
  BEGIN
    RETURN NEW(Buffer, st := f.st, width := f.width, height := f.height);
  END FNewBuf;

PROCEDURE FReset (f: JVBuffer.Factory; b: JVBuffer.T) =
  BEGIN
    (* reset the dimensions of the XImage structure.  The shared memory
       buffers all have a fixed size. *)
    WITH fac   = NARROW(f, Factory),
         buf   = NARROW(b, Buffer),
         image = Picture.Image(buf.pic) DO
      buf.width := fac.width;
      buf.height := fac.height;
      image.width := fac.width;
      image.height := fac.height;
    END;
  END FReset;

PROCEDURE FDestroy (fac: JVBuffer.Factory; buf: JVBuffer.T)
  RAISES {Thread.Alerted, OSError.E} =
  BEGIN
    (* drop the Picture data structures, but keep the shared memory
       segment *)
    TYPECASE buf OF
    | Buffer (b) =>
        TRY
          Picture.DetachData(b.pic);
        EXCEPT
        | Picture.TrestleFail =>
            IO.Put(
              "VideoVBT.Picture.DetachData.TrestleFail\n", Stdio.stderr);
        END;
    ELSE
    END;
    JVFromDecomp.Factory.destroy(fac, buf);
  END FDestroy;

(* }}} *)
(* {{{ -- the T -- *)

CONST DefaultMaxBuffers = 2;

REVEAL
  T = Public BRANDED OBJECT
        offset                         := Point.Origin;
        sourceHost: TEXT;
        quality   : JVSink.Quality;
        dparams                        := Jvs.DefaultDecompress;
        cmap      : Jvs.ColormapInfo;
        sinkBuffers, decompBuffers: CARDINAL := DefaultMaxBuffers;
        synchronous                          := FALSE;
        fixedSize                            := FALSE;
        minFrameMSecs             : CARDINAL := 0;
        minFrameSecs              : LONGREAL := 0.0d0;

        (* start a thread when nothing running and reshape from empty to
           not empty and suitable screen type.  a thread when reshape to
           empty.  stop a thread when rescreen.  Thread cleans up fields in
           v when it is alerted *)
        starting                         := FALSE;
        input      : JVBuffer.Pool       := NIL;
        decomp     : JVDecomp.T          := NIL;
        thread     : Thread.T            := NIL;
        pauseEvent : Thread.Condition;
        paused                           := FALSE;
        statistics : JVDecomp.Statistics := NIL;
        stopEvent  : Thread.Condition;
        startThread: Thread.T            := NIL;
        (* used to avoid locking the VBT tree when starting up a new
           connection *)
        vis := TrestleOnX.VisibilityUnobscured;
      OVERRIDES
        init     := Init;
        repaint  := Repaint;
        reshape  := Reshape;
        rescreen := Rescreen;
        shape    := Shape;
        discard  := Discard;
        misc     := Misc;

        setQuality       := SetQuality;
        setSize          := SetSize;
        setMinFrameMSecs := SetMinFrameMSecs;
        setSynchronous   := SetSynchronous;
        setFixedSize     := SetFixedSize;

        getDecomp  := GetDecomp;
        getSize    := GetSize;
        setPaused  := SetPaused;
        startStats := StartStats;
        stopStats  := StopStats;
        getStats   := GetStats;
      END;

(* }}} *)
(* {{{ -- methods -- *)

PROCEDURE Init (v            : T;
                sourceHost   : TEXT;
                quality      : JVSink.Quality;
                ncolours     : CARDINAL         := 0;
                width        : CARDINAL         := 320;
                height       : CARDINAL         := 240;
                synchronous                     := FALSE;
                fixedSize                       := FALSE;
                minFrameMSecs: CARDINAL         := 0      ): T =
  BEGIN
    v.sourceHost := sourceHost;
    v.quality := quality;
    v.cmap.nColors := ncolours;
    v.dparams.reqX := width;
    v.dparams.outX := width;
    v.dparams.reqY := height;
    v.dparams.outY := height;
    v.synchronous := synchronous;
    v.fixedSize := fixedSize;
    v.minFrameMSecs := minFrameMSecs;

    IF minFrameMSecs > 0 THEN
      v.minFrameSecs := FLOAT(minFrameMSecs, LONGREAL) / 1000.0d0;
    END;

    v.pauseEvent := NEW(Thread.Condition);
    v.stopEvent := NEW(Thread.Condition);
    RETURN v;
  END Init;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  VAR buff: Buffer := NIL;
  BEGIN
    IF v.st # NIL AND v.input # NIL THEN
      LOCK v DO
        buff := NARROW(v.input.getCurrentBuffer(), Buffer);
        IF buff = NIL THEN RETURN; END;
      END;
      TRY
        IF buff.pic # NIL THEN
          Picture.Paint(v, buff.pic, rgn.r, delta := v.offset);
        END;
      EXCEPT
      | Thread.Alerted =>        (*skip *)
      END;
      <* ASSERT buff # NIL *>
      buff.free();
    END;
  END Repaint;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    LOCK v DO v.offset := Rect.NorthWest(v.domain); END;

    IF Rect.Congruent(cd.prev, cd.new) OR v.st = NIL THEN
      (* there's no video work to do *)
      VBT.Leaf.reshape(v, cd);
      RETURN;
    END;

    IF cd.new = Rect.Empty THEN
      (* think about closing down the thread *)
      Stop(v);
    ELSE
      IF cd.prev = Rect.Empty AND v.thread = NIL THEN
        (* try to start a new connection *)
        TYPECASE v.st OF
        | XScreenType.T (xst) =>
            WITH trsl = Trestle.ScreenOf(v, Point.Origin).trsl,
                 id   = XScrnCmap.ColorMapID(xst.cmap.standard()) DO
              IF trsl # NIL AND id # Jvs.IdNone THEN
                LOCK v DO
                  IF NOT v.fixedSize THEN
                    v.dparams.reqX := Rect.HorSize(cd.new);
                    v.dparams.outX := v.dparams.reqX;
                    v.dparams.reqY := Rect.VerSize(cd.new);
                    v.dparams.outY := v.dparams.reqY;
                  END;
                  v.cmap.id := id;
                  v.cmap.displayName := trsl.trestleId();
                  v.starting := FALSE;
                END;
                Start(v);
              END;
            END;
        ELSE                     (* we don't understand this type *)
        END;

      ELSE
        IF NOT v.fixedSize THEN
          (* change the size of the image *)
          Stop(v);
          LOCK v DO
            v.dparams.reqX := Rect.HorSize(cd.new);
            v.dparams.outX := v.dparams.reqX;
            v.dparams.reqY := Rect.VerSize(cd.new);
            v.dparams.outY := v.dparams.reqY;
          END;
          Start(v);
        END;
        IF cd.prev = Rect.Empty THEN Thread.Signal(v.pauseEvent); END;
      END;
    END;
    VBT.Leaf.reshape(v, cd);
  END Reshape;

<* FATAL TrestleComm.Failure *>

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    Stop(v);
    (* if we need to restart it will be done in the reshape *)
    IF cd.st # NIL THEN LOCK v DO v.starting := TRUE; END; END;
    VBT.Leaf.rescreen(v, cd);
  END Rescreen;

PROCEDURE Shape (v: T; ax: Axis.T; <*UNUSED*> n: CARDINAL): VBT.SizeRange =
  VAR res := VBT.DefaultShape;
  BEGIN
    IF v.st # NIL THEN
      LOCK v DO
        IF v.fixedSize THEN
          CASE ax OF
          | Axis.T.Hor =>
              (* the JVboard wastes 12 pixels on the end of each line *)
              res.pref := MAX(v.dparams.outX - Jvs.linePadding, 0);
          | Axis.T.Ver => res.pref := v.dparams.outY;
          END;
        END;
      END;
    END;
    RETURN res;
  END Shape;

PROCEDURE Discard (v: T) =
  BEGIN
    Stop(v);
    VBT.Leaf.discard(v);
  END Discard;

PROCEDURE Misc (v: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
      Discard(v);
    END;
    IF cd.type = TrestleOnX.Visibility THEN
      v.vis := cd.detail[0];
      Thread.Signal(v.pauseEvent);
    END;
    VBT.Leaf.misc(v, cd);
  END Misc;

PROCEDURE SetQuality (v: T; quality: JVSink.Quality) =
  BEGIN
    SetPictureParams(v, v.sourceHost, quality, v.dparams, v.cmap);
  END SetQuality;

PROCEDURE SetSize (v: T; width, height: CARDINAL) =
  VAR dparams := v.dparams;
  BEGIN
    dparams.reqX := width;
    dparams.reqY := height;
    SetPictureParams(v, v.sourceHost, v.quality, dparams, v.cmap);
  END SetSize;

PROCEDURE SetMinFrameMSecs (v: T; msecs: CARDINAL) =
  BEGIN
    Stop(v);
    LOCK v DO
      v.minFrameMSecs := msecs;
      v.minFrameSecs := FLOAT(msecs, LONGREAL) / 1000.0d0;
    END;
    IF v.st # NIL THEN
      TYPECASE v.st OF XScreenType.T => Start(v) ELSE END
    END
  END SetMinFrameMSecs;

PROCEDURE SetSynchronous (v: T; synchronous: BOOLEAN) =
  BEGIN
    LOCK v DO v.synchronous := synchronous; END;
  END SetSynchronous;

PROCEDURE SetFixedSize (v: T; fixedSize: BOOLEAN) =
  BEGIN
    LOCK v DO v.fixedSize := fixedSize; END;
    VBT.NewShape(v);
  END SetFixedSize;

PROCEDURE GetDecomp (v: T): JVDecomp.T =
  BEGIN
    RETURN v.decomp;
  END GetDecomp;

PROCEDURE GetSize (v: T; VAR width, height: CARDINAL) =
  BEGIN
    LOCK v DO width := v.dparams.outX; height := v.dparams.outY; END;
  END GetSize;

PROCEDURE SetPaused (v: T; paused := FALSE) =
  BEGIN
    LOCK v DO v.paused := paused; END;
    Thread.Signal(v.pauseEvent);
  END SetPaused;

(* }}} *)
(* {{{ -- statistics -- *)

PROCEDURE StartStats (t: T) =
  BEGIN
    LOCK t DO
      IF t.statistics = NIL THEN
        t.statistics := NEW(JVDecomp.Statistics);
      END;
      WITH s = t.statistics DO
        s.framesStarted := 0;
        s.framesProcessed := 0;
        s.timesBlocked := 0;
        s.cumLatency := 0;
      END;
    END;
  END StartStats;

PROCEDURE StopStats (t: T) =
  BEGIN
    LOCK t DO t.statistics := NIL; END;
  END StopStats;

PROCEDURE GetStats (t: T): JVDecomp.Statistics =
  BEGIN
    RETURN t.statistics;
  END GetStats;

(* }}} *)
(* {{{ -- exported in VideoVBTRep.i3 -- *)

PROCEDURE SetPictureParams (         v         : T;
                                     sourceHost: TEXT;
                                     quality   : JVSink.Quality;
                            READONLY dparams   : Jvs.DcmpParams;
                            READONLY cmap      : Jvs.ColormapInfo) =
  VAR
    name: TEXT;
    id  : Jvs.Id;
  BEGIN
    Stop(v);

    LOCK v DO
      v.sourceHost := sourceHost;
      v.quality := quality;
      v.dparams := dparams;
      v.cmap := cmap;
    END;
    IF v.st # NIL THEN
      TYPECASE v.st OF
      | XScreenType.T (xst) =>
          WITH trsl = Trestle.ScreenOf(v, Point.Origin).trsl DO
            IF trsl # NIL THEN
              name := trsl.trestleId();
              id := XScrnCmap.ColorMapID(xst.cmap.standard());
              IF id # Jvs.IdNone THEN
                LOCK v DO v.cmap.id := id; v.cmap.displayName := name; END;
                IF NOT v.fixedSize THEN
                  VBT.NewShape(v);
                ELSE
                  Start(v);
                END;
              END;
            END;
          END;
      ELSE                       (* skip *)
      END;
    END;
  END SetPictureParams;

PROCEDURE GetPictureParams (    v         : T;
                            VAR sourceHost: TEXT;
                            VAR quality   : JVSink.Quality;
                            VAR dparams   : Jvs.DcmpParams;
                            VAR cmap      : Jvs.ColormapInfo) =
  BEGIN
    LOCK v DO
      sourceHost := v.sourceHost;
      quality := v.quality;
      dparams := v.dparams;
      cmap := v.cmap;
    END;
  END GetPictureParams;

(* }}} *)
(* {{{ -- local procedures -- *)

PROCEDURE Start (v: T) =
  VAR oldt := v.startThread;
  BEGIN
    IF oldt # NIL THEN Thread.Alert(oldt); END;
    LOCK v DO v.startThread := Thread.Fork(NEW(StartClosure, v := v)); END;
  END Start;

PROCEDURE Stop (v: T) =
  VAR oldstart := v.startThread;
  BEGIN
    IF oldstart # NIL THEN Thread.Alert(oldstart); END;
    (* v.thread is set to NIL at the end of Apply() *)
    IF v.thread # NIL THEN
      LOCK v DO
        WHILE v.thread # NIL DO
          Thread.Alert(v.thread);
          Thread.Wait(v, v.stopEvent);
        END;
      END;
    END;
  END Stop;

(* }}} *)
(* {{{ -- Picture.Paint callback -- *)

PROCEDURE FreeProc (param: REFANY) =
  BEGIN
    NARROW(param, Buffer).free();
  END FreeProc;

(* }}} *)
(* {{{ -- paint thread -- *)

TYPE Closure = Thread.Closure OBJECT v: T;  OVERRIDES apply := Apply; END;

PROCEDURE Apply (cl: Closure): REFANY =
  VAR
    buff            : Buffer := NIL;
    v                        := cl.v;
    input                    := v.input;
    inDecomp, paused         := FALSE;
    lastFrameTime            := Time.Now();
  BEGIN
    TRY
      v.decomp.join();
      inDecomp := TRUE;
      input.join();

      TRY
        LOOP
          IF v.paused OR v.domain = Rect.Empty
               OR v.vis = TrestleOnX.VisibilityFullyObscured THEN
            v.decomp.setPaused(TRUE);
            paused := TRUE;

            LOCK v DO
              IF v.statistics # NIL THEN
                INC(v.statistics.timesBlocked);
              END;
              WHILE v.paused OR v.domain = Rect.Empty
                      OR v.vis = TrestleOnX.VisibilityFullyObscured DO
                Thread.AlertWait(v, v.pauseEvent);
              END;
            END;
            v.decomp.setPaused(FALSE);
            paused := FALSE;
          END;

          IF v.minFrameMSecs > 0 THEN
            WITH waitTime = v.minFrameSecs + lastFrameTime - Time.Now() DO
              IF waitTime > 0.0d0 THEN Thread.AlertPause(waitTime); END;
            END;
          END;

          IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;

          buff := NARROW(input.waitForChange(), Buffer);
          lastFrameTime := Time.Now();
          IF buff.pic # NIL THEN
            LOCK v DO
              IF v.statistics # NIL THEN
                INC(v.statistics.framesStarted);
                v.statistics.cumLatency :=
                  Word.Plus(v.statistics.cumLatency,
                            Word.Minus(Tick.Now(), buff.localTime));
              END;
            END;
            IF buff.ready # NIL THEN
              buff.ready.endToEnd := TRUE;
            END;
            IF v.synchronous THEN
              Picture.Paint(v, buff.pic, delta := v.offset);
              buff.free();
            ELSE
              Picture.Paint(v, buff.pic, freeProc := FreeProc,
                            freeParam := buff, delta := v.offset);
            END;
          END;
          buff := NIL;
        END;
      EXCEPT
      | Thread.Alerted, JVBuffer.Closed => (* skip *)
      END;

      IF buff # NIL AND v.synchronous THEN buff.free(); END;
      IF input # NIL THEN input.leave(); END;
      IF paused THEN v.decomp.setPaused(FALSE); END;
      LOCK v DO
        (* clean up vbt fields *)
        v.thread := NIL;
        v.input := NIL;
        IF inDecomp AND v.decomp # NIL THEN v.decomp.leave(); END;
      END;
    EXCEPT
    | JVConverter.Error (e) =>
        VAR etext := "";
        BEGIN
          IF e # NIL AND e.head # NIL THEN
            etext := RdUtils.FailureText(e);
          END;
          JVConverter.ReportError("VideoVBT.Apply " & etext);
        END;
    | Thread.Alerted =>          (*skip *)
    END;
    LOCK v DO v.decomp := NIL; v.input := NIL; v.thread := NIL; END;
    Thread.Signal(v.stopEvent);
    RETURN NIL;
  END Apply;

(* }}} *)
(* {{{ -- Start thread -- *)

TYPE
  StartClosure =
    Thread.Closure OBJECT v: T;  OVERRIDES apply := StartApply; END;

PROCEDURE StartApply (cl: StartClosure): REFANY =
  VAR
    v                   := cl.v;
    decomp : JVDecomp.T;
    jvs    : Jvs.T;
    factory: Factory;
    failed              := TRUE;
    subtype := JvsBuffer.Subtype(v.dparams.outX, v.dparams.outY);
  BEGIN
    TRY
      decomp :=
        JVDecompPool.GetDecomp(
          v.sourceHost, v.quality, v.dparams, v.cmap, FALSE, v.sinkBuffers,
          v.decompBuffers, delay := v.minFrameMSecs, subtype := subtype);
      IF decomp = NIL THEN
        jvs := NEW(Jvs.T).init();
        factory := NEW(Factory);
        factory.preInit(v.st, v.dparams.reqX, v.dparams.reqY);
        EVAL factory.init(jvs);
        decomp :=
          JVDecompPool.GetDecomp(
            v.sourceHost, v.quality, v.dparams, v.cmap, TRUE,
            v.sinkBuffers, v.decompBuffers, factory, jvs, v.minFrameMSecs,
	    subtype := subtype);
	factory.subtype := subtype
      END;

      LOCK v DO
        v.decomp := decomp;
        v.input := decomp.getOutput();
        <* ASSERT v.input # NIL *>
        v.thread := Thread.Fork(NEW(Closure, v := v));
      END;
      failed := FALSE;
    EXCEPT
    | OSError.E (e) =>
        VAR etext := "";
        BEGIN
          IF e # NIL AND e.head # NIL THEN
            etext := RdUtils.FailureText(e);
          END;
          JVConverter.ReportError("VideoVBT.Start " & etext);
        END;
    | Picture.ScreenTypeNotSupported =>
        JVConverter.ReportError("VideoVBT.Start.BadScreenType");
    | Thread.Alerted =>          (* skip *)
    END;

    LOCK v DO v.startThread := NIL; END;
    IF failed THEN LOCK v DO v.decomp := NIL; v.input := NIL; END; END;

    RETURN NIL;
  END StartApply;

(* }}} *)
BEGIN
  EVAL JVConverter.RegisterErrorReporter(JVConverter.toStderr);
END VideoVBT.
