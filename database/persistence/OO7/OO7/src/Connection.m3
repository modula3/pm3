MODULE Connection;

IMPORT CRandom, GenParams, Globals, OO7;
FROM IO IMPORT Put, PutInt;

REVEAL T = OO7.Connection BRANDED "Connection.T" OBJECT
OVERRIDES
  init := Init;
END;

PROCEDURE Init(self: T; fromPart: OO7.AtomicPart; toPart: OO7.AtomicPart):
  OO7.Connection =
  BEGIN
    IF Globals.debugMode THEN
      Put("Connection::Connection(fromId = "); PutInt(fromPart.id);
      Put(", toId = "); PutInt(toPart.id); Put(")\n");
    END;

    (* initialize the simple stuff *)
    VAR typeNo := CRandom.random() MOD GenParams.NumTypes;
    BEGIN self.type := Globals.types[typeNo]; END;
    self.length := CRandom.random() MOD GenParams.XYRange;

    self.from := fromPart;   (* establish pointer back to "from" part *)
    self.to := toPart;       (* establish forward pointer to "to" part *)
    fromPart.to.addhi(self); (* establish pointer to connection *)
    toPart.from.addhi(self); (* establish pointer to connection *)
    RETURN self;
  END Init;

BEGIN
END Connection.
