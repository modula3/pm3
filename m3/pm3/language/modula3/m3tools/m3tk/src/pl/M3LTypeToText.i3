INTERFACE M3LTypeToText;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)


CONST
(* Used in type strings *)
  IntegerCh         = 'I';
  CardinalCh        = 'C';
  CharCh            = 'H';
  WideCharCh        = 'W';
  BooleanCh         = 'B';
  RealCh            = 'R';
  LongRealCh        = 'L';
  ExtendedCh        = 'X';
  RefAnyCh          = 'F';
  AddressCh         = 'A';
  NullCh            = 'N';
  RootCh            = 'O';
  UntracedRootCh    = 'U';
  VoidCh            = 'V';

  EnumerationCh     = 'E';

  SubrangeCh        = 'i'; (* like smaller INTEGER hence i (best I could do) *)
  ArrayCh           = 'a';
  RecordCh          = 'r';
  BitsCh            = 'b';
  SetCh             = 's';
  RefCh             = 'f';
  UntracedRefCh     = 'u';
  ProcedureCh       = 'p';
  ObjectCh          = 'o';
  OpaqueCh          = 'q';
  MethodCh          = 'm'; (* introduces the object type for a method *)

  TypeIndexOneCh    = '#';
  TypeIndexTwoCh    = '$';
  TypeIndexThreeCh  = '%';
  TypeIndexManyCh   = '&';

  TypeIndexChars    = SET OF CHAR {
                          TypeIndexOneCh, TypeIndexTwoCh,
                          TypeIndexThreeCh, TypeIndexManyCh};

  UserBrandCh       = '+';
  CompilerBrandCh   = '|';
  RaisesAnyCh       = '*';
  DefaultCh         = '=';
  VarCh             = '@';
  ReadonlyCh        = '!';
  PropagateCh       = '.';
  EndSeqCh          = ';';


  FirstDigitCh = '0';
  LastDigitCh = '9';
  FirstDigitValue = 0;
  LastDigitValue = ORD(LastDigitCh) - ORD(FirstDigitCh) + FirstDigitValue;
  FirstLowerCh = 'a';
  LastLowerCh = 'z';
  FirstLowerValue = LastDigitValue + 1;
  LastLowerValue = ORD(LastLowerCh) - ORD(FirstLowerCh) + FirstLowerValue;
  FirstUpperCh = 'A';
  LastUpperCh = 'Z';
  FirstUpperValue = LastLowerValue + 1;
  LastUpperValue = ORD(LastUpperCh) - ORD(FirstUpperCh) + FirstUpperValue;
  LastSmallNumber = LastUpperValue;
  FirstBigNumber = LastSmallNumber + 1;
  BigNumberBraCh = '(';
  BigNumberKetCh = ')';

(* Small numbers are represented as follows. A digit is '0'..'9', 'a'..'z' or
'A'..'Z' which represent the range FirstSmallNumber to LastSmallNumber. A
number is either a single digit or a bracketed sequence of digits e.g.
  A      36
  (10)   62
Such small numbers are used to represent the lengths of identifiers in types;
the bracketed form will rarely be used as such identifiers tend to be fairly
short. Small numbers use only the alphanumeric characters and brackets and do
not clash with any of the type string "punctuation" characters. *)

  TypeIndexFirstDigitCh = '0';
  TypeIndexBase = 64;
  TypeIndexLastDigitCh  =
      VAL(ORD(TypeIndexFirstDigitCh) + TypeIndexBase - 1, CHAR);

(* Component types are represented by indexes into an array. This array may be
large so the indexes are not represented in small number format. The numbers
are represented by a sequence of "digits" in the range:
  TypeIndexFirstDigitCh..TypeIndexLastDigitCh
A 1 digit number is represented by 'TypeIndexOneCh' followed by 1 digit
A 2 digit number is represented by 'TypeIndexTwoCh' followed by 2 digits.
A 3 digit number is represented by 'TypeIndexThreeCh' followed by 3 digits.
Larger numbers are represented by a sequence of digits enclosed by occurences
of 'TypeIndexManyCh' *)

END M3LTypeToText.
