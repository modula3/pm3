INTERFACE TasteCode ;

TYPE
  OpCode = { Add, Sub, Mul, Div, Equ, Lss, Gtr, Load, Lit, Sto,
             Call, Ret, Res, Jmp, FJmp, Halt, Neg, Read, Write } ;
  Address = [0 .. 65535] ;
  Number  = [0 .. 65535] ;
  Level   = [0 .. 255] ;

VAR
  progStart : Address ;     (* address of first instruction of main program *)
  pc        : Address ;     (* program counter *)

PROCEDURE Emit1(op : OpCode) ;

PROCEDURE Emit2(op : OpCode ; num : Number) ;

PROCEDURE Emit3(op : OpCode ; level : Level ; adr : Address) ;

PROCEDURE Fixup(adr : Address) ;

PROCEDURE Interpret() ;

END TasteCode.
