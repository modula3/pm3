MODULE TasteCode ;

IMPORT Stdio, Wr, Lex, Fmt, Thread ;

<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  Byte = [0 .. 255] ;

VAR
  code : ARRAY Address OF Byte ;

PROCEDURE Emit1(op : OpCode) =
BEGIN
  code[pc] := VAL(ORD(op), Byte) ;
  INC(pc)
END Emit1 ;

PROCEDURE Emit2(op : OpCode ; num : Number) =
BEGIN
  code[pc + 0] := VAL(ORD(op),          Byte) ;
  code[pc + 1] := VAL(ORD(num) DIV 256, Byte) ;
  code[pc + 2] := VAL(ORD(num) MOD 256, Byte) ;
  INC(pc, 3)
END Emit2 ;

PROCEDURE Emit3(op : OpCode ; level : Level ; adr : Address) =
BEGIN
  code[pc + 0] := VAL(ORD(op),          Byte) ;
  code[pc + 1] := VAL(ORD(level),       Byte) ;
  code[pc + 2] := VAL(ORD(adr) DIV 256, Byte) ;
  code[pc + 3] := VAL(ORD(adr) MOD 256, Byte) ;
  INC(pc, 4)
END Emit3 ;

PROCEDURE Fixup(adr : Address) =
BEGIN
  code[adr + 0] := VAL(ORD(pc) DIV 256, Byte) ;
  code[adr + 1] := VAL(ORD(pc) MOD 256, Byte)
END Fixup ;

PROCEDURE Interpret() =
VAR
  stack     : ARRAY [0 .. 1000] OF Number ;
  top       : Address ;
  base      : Address ;
  val,a,lev : INTEGER ;
  ok        : BOOLEAN ;

  PROCEDURE GetOp() : OpCode =
  BEGIN
    INC(pc) ;
    RETURN VAL(code[pc - 1], OpCode)
  END GetOp ;

  PROCEDURE GetLevel() : Level =
  BEGIN
    INC(pc) ;
    RETURN VAL(code[pc - 1], Level)
  END GetLevel ;

  PROCEDURE GetNum() : Number =
  VAR n : Number ;
  BEGIN
    n := (VAL(code[pc], Number) * 256) + VAL(code[pc + 1], Number) ;
    INC(pc, 2) ;
    RETURN n
  END GetNum ;

  PROCEDURE GetAdr() : Address =
  VAR a : Address ;
  BEGIN
    a := (VAL(code[pc], Address) * 256) + VAL(code[pc + 1], Address) ;
    INC(pc, 2) ;
    RETURN a
  END GetAdr ;

  PROCEDURE Push(num : Number) =
  BEGIN
    stack[top] := num ;
    INC(top)
  END Push ;

  PROCEDURE Pop() : Number =
  BEGIN
    DEC(top) ;
    RETURN stack[top]
  END Pop ;

  PROCEDURE Up(level : Level) : Address =
  VAR b : Address := base ;
  BEGIN
    WHILE (level > 0) DO
      b := stack[b] ;
      DEC(level)
    END ;
    RETURN b
  END Up ;

BEGIN
  Wr.PutText(Stdio.stdout, "Interpreting...\n") ;
  pc := progStart ;
  base := 0 ;
  top := 3 ;
  LOOP
    CASE GetOp() OF
      OpCode.Load  => lev := GetLevel() ;
                      a   := GetAdr() ;
                      Push(stack[Up(lev) + a])
    | OpCode.Lit   => Push(GetNum())
    | OpCode.Sto   => lev := GetLevel() ;
                      a   := GetAdr() ;
                      stack[Up(lev) + a] := Pop()
    | OpCode.Add   => Push(Pop() + Pop())
    | OpCode.Sub   => Push(-Pop() + Pop())
    | OpCode.Div   => val := Pop() ;
                      Push(Pop() DIV val)
    | OpCode.Mul   => Push(Pop() * Pop())
    | OpCode.Equ   => Push(ORD(Pop() = Pop()))
    | OpCode.Lss   => Push(ORD(Pop() > Pop()))
    | OpCode.Gtr   => Push(ORD(Pop() < Pop()))
    | OpCode.Call  => Push(Up(GetLevel())) ;
                      Push(base) ;
                      Push(pc+2) ;
                      pc := GetAdr() ;
                      base := top - 3
    | OpCode.Ret   => top := base ;
                      base := stack[top + 1] ;
                      pc := stack[top + 2]
    | OpCode.Res   => INC(top, GetAdr())
    | OpCode.Jmp   => pc := GetAdr()
    | OpCode.FJmp  => a := GetAdr() ;
                      IF (Pop() = 0) THEN
                        pc := a
                      END
    | OpCode.Halt  => EXIT
    | OpCode.Neg   => Push(-Pop())
    | OpCode.Read  => lev := GetLevel() ;
                      a   := GetAdr() ;
                      ok  := FALSE ;
                      WHILE (NOT ok) DO
                        Wr.PutText(Stdio.stdout, "? ") ;
                        Wr.Flush(Stdio.stdout) ;
                        TRY
                          val := Lex.Int(Stdio.stdin) ;
                          stack[Up(lev) + a] := val ;
                          ok := TRUE
                        EXCEPT
                        ELSE
                          Wr.PutText(Stdio.stdout, "invalid number!\n")
                        END
                      END
    | OpCode.Write => Wr.PutText(Stdio.stdout, Fmt.F("%6s\n", Fmt.Int(Pop()))) ;
                      Wr.Flush(Stdio.stdout)
    ELSE
      Wr.PutText(Stdio.stderr, "Illegal opcode!\n") ;
      EXIT
    END
  END
END Interpret ;

BEGIN
  pc := 1
END TasteCode.
