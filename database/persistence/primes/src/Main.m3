MODULE Main;

IMPORT IO, Database, Transaction, RTParams;

<* FATAL Transaction.InProgress *>
<* FATAL Transaction.NotInProgress *>
<* FATAL Database.Opened *>
<* FATAL Database.Exists *>
<* FATAL Database.NotFound *>

TYPE
  Prime = REF RECORD
    number: INTEGER;
    next: Prime;
  END;

CONST
  group = 10;

VAR
  tr := NEW(Transaction.T);
  head, p : Prime;
  i, n: INTEGER;
  divisor: BOOLEAN;
  db: Database.T;
BEGIN
  TRY
    db := Database.Open("primes");
  EXCEPT
  | Database.NotFound =>
    Database.Create("primes");
    db := Database.Open("primes");
  END;

  tr.begin();

  head := db.getRoot();
  IF head = NIL OR RTParams.IsPresent("init") THEN
    head := NEW(Prime, number := 2);
    db.setRoot(head);
  ELSE
    IO.Put("Primes from the persistent store:\n");
  END;

  n := head.number; IO.PutInt(n); IO.Put("\n");
  i:= 0; p := head.next;
  WHILE p # NIL DO
    n := p.number;
    IO.PutInt(n); IO.Put(" ");
    INC(i);
    IF i MOD group = 0 THEN IO.Put("\n"); END;
    p := p.next;
  END;
  IO.Put("\n");

  i := 0;
  IO.Put("New primes:\n");
  LOOP
    INC(n);
    divisor := FALSE;
    p := head;

    LOOP
      IF n MOD p.number = 0 THEN EXIT END;
      IF p.next = NIL THEN
        p.next := NEW(Prime, number := n);
        INC(i);
        IO.PutInt(n); IO.Put(" ");
        IF i MOD group = 0 THEN tr.chain(); IO.Put("#\n"); END;
        EXIT;
      END;
      p := p.next;
    END;
  END;
END Main.
