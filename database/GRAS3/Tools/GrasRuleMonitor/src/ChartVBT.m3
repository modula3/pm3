MODULE ChartVBT;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:02  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT RectsVBT, VBT, TextSeq, IntSeq, PaintOp, Rect, Point, Fmt, Region,
       VBTClass;

CONST
  SouthMargin = 20;
  WestMargin  = 20;

REVEAL
  T = Public BRANDED OBJECT
        names      : TextSeq.T;
        heights    : IntSeq.T;
        maxHeight  : CARDINAL;
        rectWidth  : REAL;
        deltaHeight: REAL;
        noOfCharts : CARDINAL;
      OVERRIDES
        init      := Init;
        redisplay := Redisplay;
        repaint   := Repaint;
      END;


PROCEDURE Init (vbt          : T;
                noOfCharts   : CARDINAL;
                initialheight: CARDINAL   := 100): T =
  VAR color := PaintOp.FromRGB(0.0, 0.0, 0.0);
  BEGIN
    EVAL RectsVBT.T.init(vbt);
    vbt.maxHeight := initialheight;
    RectsVBT.SetN(vbt, noOfCharts);
    vbt.noOfCharts := noOfCharts;
    (* leave some room at the bottom to print axes *)
    RectsVBT.SetMargin(
      vbt, FLOAT(WestMargin), FLOAT(SouthMargin), 0.0, 0.0);
    vbt.rectWidth := MIN(1.0 / FLOAT(vbt.noOfCharts), 1.0 / 20.0);
    Ydim(vbt, 0);
    vbt.names := NEW(TextSeq.T).init();
    vbt.heights := NEW(IntSeq.T).init();
    FOR i := 1 TO noOfCharts DO
      RectsVBT.Color(vbt, i, color);
      vbt.names.addhi("");
      vbt.heights.addhi(0);
    END;
    RETURN vbt;
  END Init;

PROCEDURE Redisplay (v: T) =
  BEGIN
    RectsVBT.T.redisplay(v);
    Repaint(v, Region.Full)
  END Redisplay;

PROCEDURE Repaint (vbt: T; READONLY rgn: Region.T) =
  BEGIN
    RectsVBT.T.repaint(vbt, rgn);
    RedrawAxes(vbt);
  END Repaint;

PROCEDURE SetChartName (vbt: T; no: CARDINAL; name: TEXT) =
  BEGIN
    vbt.names.put(no - 1, name);
    RedrawAxes(vbt);
  END SetChartName;

PROCEDURE SetChart (vbt: T; no, height: CARDINAL) =

  PROCEDURE East (rect: CARDINAL): REAL =
    BEGIN
      RETURN FLOAT(rect) * vbt.rectWidth - 0.1 * vbt.rectWidth;
    END East;

  PROCEDURE West (rect: CARDINAL): REAL =
    BEGIN
      RETURN FLOAT(rect - 1) * vbt.rectWidth + 0.1 * vbt.rectWidth;
    END West;

  PROCEDURE North (count: CARDINAL): REAL =
    BEGIN
      RETURN vbt.deltaHeight * FLOAT(count);
    END North;

  BEGIN
    vbt.heights.put(no-1, height);
    IF height >= vbt.maxHeight THEN
      (* we reached the current limit *)
      vbt.maxHeight := MAX(100, 2 * vbt.maxHeight);
      Ydim(vbt, vbt.maxHeight);
      FOR i := 0 TO vbt.noOfCharts - 1 DO
        RectsVBT.Position(vbt, i + 1, West(i + 1), 0.0, East(i + 1),
                          North(vbt.heights.get(i)), TRUE);
      END;
      RedrawAxes(vbt);
    ELSE
      RectsVBT.Position(vbt, no, West(no), 0.0, East(no), North(height));
      RectsVBT.Draw(vbt, no);
    END;
  END SetChart;

PROCEDURE RedrawAxes (vbt: T) =
  BEGIN
    WITH number   = vbt.noOfCharts,
         dom      = VBT.Domain(vbt),
         domwidth = dom.east - dom.west,
         width    = domwidth DIV MAX(number, 20) DO
      IF NOT Rect.IsEmpty(dom) THEN
        (* print types along x-axis *)
        FOR i := 0 TO vbt.noOfCharts - 1 DO
          VBT.PaintText(
            vbt, pt := Point.FromCoords(dom.west + WestMargin + width DIV 2
                                          + i * width, dom.south - 5),
            t := vbt.names.get(i), op := PaintOp.BgFg);
        END;
        (* print max on top of y-axis *)
        VBT.PaintText(
          vbt, pt := Point.FromCoords(dom.west + 5, dom.north + 10),
          t := Fmt.Int(vbt.maxHeight), op := PaintOp.BgFg);
      END;
    END;
  END RedrawAxes;

PROCEDURE Ydim (vbt: T; num: CARDINAL) =
  BEGIN
    vbt.maxHeight := num;
    IF vbt.maxHeight # 0 THEN
      vbt.deltaHeight := 1.0 / FLOAT(vbt.maxHeight);
    ELSE
      vbt.deltaHeight := 0.0;
    END;
  END Ydim;

BEGIN
END ChartVBT.
