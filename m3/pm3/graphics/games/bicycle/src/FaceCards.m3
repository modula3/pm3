(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* Copyright 1990 David Lemke and Network Computing Devices             *)
(* Copyright (c) 1989, Donald R. Woods and Sun Microsystems, Inc.       *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Sun Mar  1 17:44:30 1992 by msm     *)
(*      modified on Wed Sep 18 01:01:30 1991 by kalsow  *)


MODULE FaceCards;

IMPORT Pixmap, PixmapFromXData, Point, CardSuit;

FROM Card IMPORT Suit, Value;

VAR 
  mu := NEW(MUTEX);
  inited := FALSE;
  pix: ARRAY Suit OF ARRAY FaceRank OF Pixmap.T;
  pixcenter, xipcenter: ARRAY Suit OF ARRAY FaceRank OF Point.T;

TYPE
  SuitData = ARRAY FaceRank OF PixmapFromXData.T;
  SuitDelta = ARRAY FaceRank OF Point.T;
  Pt = Point.T;

CONST
  Width = 47;
  Height = 92;

PROCEDURE Pix(s: Suit; r: FaceRank): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN pix[s][r]
    END
  END Pix;

PROCEDURE Init() =
  VAR PW := CardSuit.PixWidth; PH := CardSuit.PixHeight; BEGIN
    FOR s := FIRST(Suit) TO LAST(Suit) DO
      FOR r := FIRST(FaceRank) TO LAST(FaceRank) DO
        pix[s][r] := PixmapFromXData.P(Data[s][r]);
        IF r = Value.King THEN
          pixcenter[s][r].h := Delta[s][r].h + PW[s] DIV 2;
          xipcenter[s][r].h := Width - Delta[s][r].h + 1 - (PW[s]+1) DIV 2
        ELSE
          pixcenter[s][r].h := Width - Delta[s][r].h - (PW[s]+1) DIV 2;
          xipcenter[s][r].h := Delta[s][r].h + 1 + PW[s] DIV 2
        END;
        pixcenter[s][r].v := Delta[s][r].v + PH[s] DIV 2;
        xipcenter[s][r].v := Height - Delta[s][r].v - (PH[s] + 1) DIV 2;
      END
    END;
    inited := TRUE
  END Init;

PROCEDURE PixCenter(s: Suit; r: FaceRank): Point.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN pixcenter[s][r]
    END
  END PixCenter;

PROCEDURE XipCenter(s: Suit; r: FaceRank): Point.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN xipcenter[s][r]
    END
  END XipCenter;

TYPE AT = ARRAY OF TEXT;

PROCEDURE Cat(READONLY a: AT): TEXT =
  VAR res := ""; BEGIN
    FOR i := 0 TO LAST(a) DO res := res & " " & a[i] END;
    RETURN res
  END Cat;

VAR ClubKing := PixmapFromXData.T{47, 92, Cat(AT{
  "00 b0 20 08 82 06 00 e0 20 08 82 03",
  "00 c0 db b6 ed 01 00 80 21 08 c2 00",
  "00 00 ff ff 7f 00 00 00 56 55 35 00",
  "00 00 ac aa 1a 00 00 00 fc ff 0f 00",
  "00 00 fc ff 0f 00 00 00 04 a0 0a 08",
  "00 00 3c ae 0a 1c 00 00 74 a6 0a 3e",
  "00 00 7c be 0a 3a 00 00 24 a0 0a 2a",
  "00 00 24 a0 0a 3a 00 00 e4 a1 0a 2a",
  "00 00 44 a0 0a 3a 00 00 b4 a1 3a 2a",
  "00 00 47 a0 2e 3a 00 80 e4 b8 2b 2a",
  "00 80 06 a7 3a 3a 00 e0 e0 ac 1e 2a",
  "00 f8 f1 21 32 3a 00 fe 9f ff 6b 2a",
  "80 ff 3f 22 e2 3a e0 ff bf 88 f8 2b",
  "f8 df 7f 22 be 3b fe bf fe 88 cf 2b",
  "ff f7 f5 e3 63 3a 7f 36 eb ff 31 2b",
  "3e 36 f6 3e 78 3a be f7 ea 00 1c 6b",
  "de e3 d5 38 ce 7b fe 9c ef 38 9e 6b",
  "7e 7f d7 d6 c6 7b be eb ae ee f3 6b",
  "de dd d5 d6 e3 7b ff aa ab 10 f7 6b",
  "7b 77 d7 38 f3 7b 7d 2a eb 87 f9 6b",
  "fc c9 9d 8c f9 7b f8 ff 86 90 fb 0f",
  "fc 67 b6 b6 f9 78 fe d7 32 a6 fd 0e",
  "cf cf 82 a0 f9 7e d8 ef de bd fb 0d",
  "b0 df bd 7b f7 1b 7e 9f 05 41 f3 f3",
  "70 bf 65 4c eb 7f 1e 9f 6d 6d e6 3f",
  "f0 df 09 61 ff 1f de 9f 31 b9 93 3f",
  "d6 9f e1 d7 54 be de cf 1c eb ee de",
  "d6 ef 08 d5 55 ff de c7 6b ab bb 7b",
  "d6 cf 77 75 d7 7d de 63 6b eb fe 7e",
  "d6 79 1c f7 39 7f de 73 1c ab c7 7b",
  "d6 38 00 57 ef 7d 5c 1e 7c 6f 6c 7c",
  "d4 8c ff d7 6c fe 5c c6 c7 af ef ff",
  "d4 f3 11 7f fd 7f dc 7d 44 fe fb 1f",
  "d4 1f 11 fd ff 07 5c 47 44 fc ff 01",
  "54 d6 ff f9 7f 00 5c 4c 84 8f 1f 00",
  "54 78 35 07 07 00 5c 5c e5 60 01 00",
  "54 d4 1d 27 01 00 5c 74 05 e2 00 00",
  "54 5c 85 2d 00 00 5c 50 05 22 00 00",
  "54 50 85 27 00 00 5c 50 05 24 00 00",
  "54 50 05 24 00 00 5c 50 7d 3e 00 00",
  "7c 50 65 2e 00 00 38 50 75 3c 00 00",
  "10 50 05 20 00 00 00 f0 ff 3f 00 00",
  "00 f0 ff 3f 00 00 00 58 55 35 00 00",
  "00 ac aa 6a 00 00 00 fe ff ff 00 00",
  "00 43 10 84 01 00 80 b7 6d db 03 00",
  "c0 41 10 04 07 00 60 41 10 04 0d 00"})};

VAR DiamondKing := PixmapFromXData.T{47, 92, Cat(AT{
  "00 2c 22 22 a2 01 00 58 55 55 d5 00",
  "00 30 22 22 e2 08 00 e0 88 88 48 04",
  "00 c0 55 55 75 06 00 80 89 88 28 45",
  "00 00 ff 1f 32 65 00 00 fe ff 95 24",
  "00 00 02 fc 93 6a 00 00 f2 f0 5f 76",
  "00 00 0a d1 df 1d 00 00 7a 50 df 1d",
  "00 00 b2 50 55 76 00 00 79 50 b5 6a",
  "00 00 01 a0 aa 24 00 80 22 a0 2a 65",
  "00 80 4d 40 55 65 00 00 31 5c 55 26",
  "00 00 02 66 55 64 00 00 0e 5b 55 68",
  "0c 00 06 55 d5 20 d6 40 41 a5 aa 60",
  "aa a0 aa fa ff 7f 56 5d 55 3f c6 48",
  "6a bf aa 6a ad 65 d4 7a ff c7 18 73",
  "b8 34 fe ff ff 7f 9c 25 de 03 00 7f",
  "b2 4d ac ff ff 41 33 cf dc 31 c6 5b",
  "e6 d8 fd 5b eb 57 36 de 7d 8d b9 4e",
  "8c 97 9d fd 75 5f e4 2d b9 ac aa 7a",
  "f2 28 d8 56 cb 7d 59 2d 99 eb cf 6a",
  "2c 2e 7b 35 a7 77 56 ad af 2a 25 2b",
  "8b 5e 5b 35 74 5e 55 5f aa aa aa 2a",
  "22 5f f8 df aa 74 d5 5b aa fa cb 29",
  "e8 59 fb df 9a 52 95 db 6f 53 9a 32",
  "2a 5f 9b 1c 3d 67 27 5e fd 5f 3d 72",
  "4e bc fa bf 7a e4 e6 bc 38 d9 fa 54",
  "4c 59 ca f6 db a9 4a 59 fb df 9a 17",
  "94 d3 5f 55 da ab 2e 55 fb 1f fa 44",
  "54 55 55 55 fa aa 7a 2e ac da 7a d1",
  "d4 a4 54 f5 b5 6a ee e5 ac de 74 34",
  "56 f3 d7 99 b4 9a be d3 6a 1b 14 4f",
  "5e 55 35 9d b4 27 fa ae bf b9 e9 31",
  "72 9d b1 be 7b 6c ea d7 da bf 1b 67",
  "da 63 8c 3b f3 cc 82 ff ff 35 b2 4d",
  "fe 00 c0 7b a4 39 fe ff ff 7f 2c 1d",
  "ce 18 e3 ff 5e 2b a6 b5 56 55 fd 56",
  "12 63 fc aa ba 6a fe ff 5f 55 05 55",
  "06 55 a5 82 02 6b 04 ab aa 60 00 30",
  "16 aa da 70 00 00 26 aa 66 40 00 00",
  "64 aa 3a 8c 00 00 a6 aa 02 b2 01 00",
  "a6 54 05 44 01 00 24 55 05 80 00 00",
  "56 ad 0a 9e 00 00 6e aa 0a 4d 00 00",
  "b8 fb 0a 5e 00 00 b8 fb 8b 50 00 00",
  "6e fa 0f 4f 00 00 56 c9 3f 40 00 00",
  "24 a9 ff 7f 00 00 a6 4c f8 ff 00 00",
  "a2 14 11 91 01 00 60 ae aa aa 03 00",
  "20 12 11 11 07 00 10 47 44 44 0c 00",
  "00 ab aa aa 1a 00 80 45 44 44 34 00"})};

VAR HeartKing := PixmapFromXData.T{47, 92, Cat(AT{
  "00 08 00 00 80 00 00 b0 6d db 76 00",
  "00 60 55 55 55 00 00 c0 28 8a 22 03",
  "00 80 10 04 a1 04 00 00 11 04 f1 02",
  "00 00 fe ff 9f 3e 00 00 aa aa ca 6a",
  "00 00 fc ff 2f 2a 00 00 04 a0 ca 2a",
  "00 00 1a af 8a 2a 00 00 a6 a0 fa 56",
  "00 00 3e 4f 55 02 00 00 3a 4c 95 45",
  "00 00 3e 4e 35 7e 00 00 a2 80 2a 02",
  "00 00 62 80 2a 7f 00 00 42 40 55 01",
  "00 00 03 40 d5 7f 00 c0 b2 21 d5 3c",
  "00 20 43 d0 54 1e 00 90 e2 50 2a 4f",
  "00 f0 04 a8 9f 27 00 58 ab aa c9 33",
  "00 46 55 75 ea 69 c0 2f 0b 6a ff 2c",
  "78 56 e6 f8 66 1a ae f5 bc 6f f5 4b",
  "95 d9 f3 e3 1d 67 4a 5a 0d 58 6d 7c",
  "a5 f4 f5 d7 87 71 b2 59 5d 5d 47 46",
  "49 b2 d5 f5 a6 38 97 55 5f 7d 15 4d",
  "3a ed f6 b7 a7 44 74 b2 84 30 55 56",
  "f9 2c 55 55 ad 52 f2 af a7 f2 96 5b",
  "74 59 44 51 97 59 b1 3a 8f 50 e6 5d",
  "38 1d 50 b9 96 44 b5 1a 9f d8 c5 72",
  "72 1c 50 99 42 3c b8 1a 1f a8 62 0f",
  "f5 2e 90 78 f2 63 8e e7 cf f9 f3 38",
  "1c cf 9f f3 e7 71 c6 4f 1e 09 74 af",
  "f0 46 15 f8 58 1d 3c 42 99 0a 38 4e",
  "4e a3 1b f9 58 ad 22 69 9d 0a b8 1c",
  "ba 67 0a f1 5c 8d 9a e9 8a 22 9a 2e",
  "da 69 4f e5 f5 4f 4a b5 aa aa 34 9f",
  "6a aa 0c 21 4d 2e 22 e5 ed 6f b7 5c",
  "b2 a8 be fa aa e9 1c 65 af ab 4d 92",
  "62 e2 ba ba 9a 4d 8e e1 eb af 2f a5",
  "3e b6 1a b0 5a 52 e6 b8 c7 cf 9b a9",
  "d2 af f6 3d af 75 58 66 1f 67 6a 1e",
  "34 ff 56 d0 f4 03 96 57 ae aa 62 00",
  "cc 93 55 d5 1a 00 e4 f9 15 20 0f 00",
  "f2 54 0a 47 09 00 78 2a 0b c2 04 00",
  "3c ab 84 4d 03 00 fe ab 02 c0 00 00",
  "80 aa 02 42 00 00 fe 54 01 46 00 00",
  "40 54 01 45 00 00 7e ac 72 7c 00 00",
  "a2 a9 32 5c 00 00 40 aa f2 7c 00 00",
  "6a 5f 05 65 00 00 54 51 f5 58 00 00",
  "54 53 05 20 00 00 54 f4 ff 3f 00 00",
  "56 53 55 55 00 00 7c f9 ff 7f 00 00",
  "40 8f 20 88 00 00 20 85 20 08 01 00",
  "c0 44 51 14 03 00 00 aa aa aa 06 00",
  "00 6e db b6 0d 00 00 01 00 00 10 00"})};

VAR SpadeKing := PixmapFromXData.T{47, 92, Cat(AT{
  "00 a0 10 04 41 02 00 c0 39 8e 63 01",
  "00 80 7d df f7 00 00 00 7d df 67 00",
  "00 00 12 04 21 00 00 00 fc ff 3f 00",
  "00 00 fc ff 3f 00 00 00 54 01 20 20",
  "00 00 54 79 3e 70 00 00 54 05 21 58",
  "00 00 54 b9 2f 0c 00 00 54 9d 26 04",
  "00 00 54 b9 2f 24 00 00 54 01 21 24",
  "00 00 54 01 21 14 00 00 54 81 21 14",
  "00 00 54 b5 36 14 00 00 54 1d 3c 24",
  "00 00 54 61 23 24 00 00 54 81 20 24",
  "00 00 56 c1 e1 24 00 00 55 03 60 15",
  "00 00 55 ad 5a 15 00 80 ab d3 a5 14",
  "00 e0 ff ff 7f 34 00 bc 00 00 c4 35",
  "00 b3 ff ff 47 35 c0 07 55 55 af 66",
  "38 67 ff ff ad 66 76 6e 01 00 a1 66",
  "e3 4c 31 b8 52 45 c7 1d 3a 84 56 45",
  "8f d3 f2 87 56 45 9c d2 e2 4f 28 45",
  "78 9d da 4f a9 46 b1 22 42 54 ab 46",
  "73 27 24 52 ab 66 97 af 05 40 94 66",
  "be be fd bf 54 67 5c 3c fd bf 55 7f",
  "7c 78 ac aa 55 11 5f 7c a8 2a 5e 7f",
  "e8 5e 5b 55 62 0a bf 4f 3b d6 a2 7e",
  "68 47 7a d7 5e 0b bf 42 c8 09 a1 7e",
  "7e 85 90 13 42 fd d0 7a eb 5e e2 16",
  "7e 45 6b dc f2 fd 50 46 aa da 7a 17",
  "fe 7a 54 15 3e fa 88 aa 55 35 1e 3e",
  "fe aa fd bf 3c 3a e6 2a fd bf 7d 7d",
  "66 29 02 a0 f5 e9 66 d5 4a 24 e4 ce",
  "62 d5 2a 42 44 8d 62 95 f2 5b b9 1e",
  "a2 14 f2 47 4b 39 a2 6a e1 4f cb f1",
  "a2 6a 21 5c b8 e3 a2 4a 1d 8c 32 c7",
  "66 85 00 80 76 6e 66 b5 ff ff e6 1c",
  "66 f5 aa aa e0 03 ac e2 ff ff cd 00",
  "ac 23 00 00 3d 00 2c fe ff ff 07 00",
  "28 a5 cb d5 01 00 a8 5a b5 aa 00 00",
  "a8 06 c0 aa 00 00 24 87 83 6a 00 00",
  "24 04 81 2a 00 00 24 c4 86 2a 00 00",
  "24 3c b8 2a 00 00 28 6c ad 2a 00 00",
  "28 84 81 2a 00 00 28 84 80 2a 00 00",
  "24 84 80 2a 00 00 24 f4 9d 2a 00 00",
  "20 64 b9 2a 00 00 30 f4 9d 2a 00 00",
  "1a 84 a0 2a 00 00 0e 7c 9e 2a 00 00",
  "04 04 80 2a 00 00 00 fc ff 3f 00 00",
  "00 fc ff 3f 00 00 00 84 20 48 00 00",
  "00 e6 fb be 00 00 00 ef fb be 01 00",
  "80 c6 71 9c 03 00 40 82 20 08 05 00"})};

VAR ClubQueen := PixmapFromXData.T{47, 92, Cat(AT{
  "c0 ec 92 02 00 00 e0 ef 29 03 00 00",
  "e1 af 44 02 00 00 c3 a6 92 02 00 00",
  "02 f6 29 03 00 00 66 b6 cf 03 00 00",
  "f4 d7 20 02 00 00 f4 57 ef 03 00 00",
  "64 73 e6 02 00 00 04 5b ae 07 00 00",
  "b4 5b 40 0a 00 00 fe 4b 68 12 00 00",
  "fa 79 30 27 00 00 f2 2d 20 4f 00 00",
  "cf 2d 20 9d 00 00 d8 24 04 39 01 00",
  "f0 7e bc 73 02 00 e0 f6 b0 67 02 00",
  "70 f2 c1 ef 04 00 70 5b 43 dc 04 00",
  "38 0f 7f f1 04 00 b8 29 54 e4 04 00",
  "9c fc 00 dd 04 01 cc 84 b7 c3 83 02",
  "ee 8f 7c 62 c3 06 26 da 6d 37 26 09",
  "33 73 d7 1d cf 36 bb 3f ba f8 9b 2a",
  "e9 fb d7 af 31 25 6c 55 ef f4 f9 15",
  "f6 ee 39 ac 19 3a 5e 55 11 dc f9 45",
  "bb bb e2 ac f1 33 55 55 e3 f8 63 4f",
  "ee ee 5a 6b b7 2e 55 55 bb 3b 5e 59",
  "bb bb 5b 2b af 37 55 55 43 38 5b 68",
  "ee ee e2 18 b0 53 f5 57 05 18 70 64",
  "1f b8 87 18 e8 29 03 c0 c5 99 d9 6b",
  "01 03 a7 9a a9 2b 1b 03 75 17 50 57",
  "1e 30 2a 2a e0 2c 0d 30 9e 3c 06 58",
  "1a 60 3c 79 0c b0 34 07 54 54 0c 78",
  "ea 0a e8 ae c0 d8 d4 95 59 e5 c0 80",
  "d6 9b 99 a3 03 c0 94 17 18 e1 1d f8",
  "26 0e 18 a0 ea af ca 0d 18 47 77 77",
  "16 da 1c c2 aa aa ec f5 d4 da dd dd",
  "9a 7a dc dd aa aa 74 ed d6 5a 77 77",
  "f2 c6 1f c7 aa aa cc 8f 35 47 dd dd",
  "a2 9f 3b 88 aa 7a 5c 98 35 9c 77 6f",
  "a8 9f 2f f7 aa 36 a4 8c f5 eb df 97",
  "54 d9 1f 5d fc dd 6c f3 b8 eb ce cc",
  "90 64 ec b6 5b 64 60 c3 46 3e f1 77",
  "40 c1 c3 ed 21 33 80 20 bb 00 3f 39",
  "00 20 27 2a 94 1d 00 20 8f fe f0 1c",
  "00 20 3b c2 da 0e 00 20 f7 83 4f 0e",
  "00 40 e6 0d 6f 07 00 40 ce 3d 7e 0f",
  "00 80 9c 20 24 1b 00 00 b9 04 b4 f3",
  "00 00 f2 04 b4 4f 00 00 e4 0c 9e 5f",
  "00 00 48 16 d2 7f 00 00 50 02 da 2d",
  "00 00 e0 75 da 20 00 00 40 67 ce 26",
  "00 00 c0 f7 ea 2f 00 00 40 04 eb 2f",
  "00 00 c0 f3 6d 66 00 00 c0 94 6f 40",
  "00 00 40 49 65 c3 00 00 40 22 f5 87",
  "00 00 c0 94 f7 07 00 00 40 49 37 03"})};

VAR DiamondQueen := PixmapFromXData.T{47, 92, Cat(AT{
  "04 f1 49 01 00 00 c8 b7 94 01 00 00",
  "08 79 22 03 00 00 88 58 41 05 00 00",
  "e8 bf 80 09 00 00 88 ec 77 0b 00 00",
  "48 5e 88 17 00 00 f8 97 ef 27 00 00",
  "48 0f 73 4f 00 00 24 1b d7 5f 00 00",
  "fc 17 10 9f 00 00 a4 1d 20 bf 00 00",
  "c2 17 ac 7d 01 00 ff 0e 90 7a 02 00",
  "c0 09 80 fd 02 00 40 1b 9f fa 02 00",
  "e0 38 cc f5 05 00 a0 62 c0 ea 05 08",
  "f0 c8 e0 f5 09 1c d0 a3 21 eb 0b 2a",
  "f8 9f 9e dc 0b 77 a8 ff a2 ff 13 2a",
  "dc fd ff df 17 18 f4 ea ff ab 17 2c",
  "fe 77 7f f7 1f 2a ea af aa fa 3b 55",
  "5f ff dd 7f fd 52 fd fa ff af 5f 23",
  "df d5 ff d5 ae 5e ce af aa fa 97 35",
  "6f f2 d5 ff d7 62 a7 f2 7f 3e 57 55",
  "37 f2 4f 06 b7 08 f3 2f 4f 6e 6e 55",
  "5b 31 dc 6c 3e 22 49 55 9b 8c 5f 55",
  "9d 51 9b fe aa 08 bc 4e f8 ab 4e 55",
  "2e b9 af fa 75 22 5e c5 ea 0f 8c 55",
  "6f d5 1f f0 aa 08 5f c5 e0 9f 8a 55",
  "6f b9 3e 40 71 23 5f 9d 82 5f 9d 54",
  "6b a3 fa 5a a3 4e 35 ab da ad 6a 56",
  "6a 56 b5 5b d5 ac 72 c5 5a 5f c5 d6",
  "2a b9 fa 41 b9 fa c4 8e 02 7c 9d f6",
  "aa 51 f9 07 a3 fa 10 55 0f f8 ab f6",
  "aa 31 f0 57 a3 7a 44 ae 5f f5 9d 74",
  "aa 72 d5 1f 72 3d 10 55 7f d9 8a b9",
  "aa fa 31 d9 aa 92 44 7c 36 3b 8c da",
  "aa 76 76 f2 f4 cf 10 ed 60 f2 4f ec",
  "aa ea 7c fe 4f e5 46 eb ff ab 4f f6",
  "ac e9 5f 55 f5 73 7a 75 ab ff ab fb",
  "c4 fa f5 ff 5f bf 4a bf fe bb ff fa",
  "aa dc 5f 55 f5 57 54 f8 ef fe ee 7f",
  "34 e8 d5 ff 57 2f 18 e8 fb ff bf 3b",
  "54 c8 ff 45 ff 15 ee d0 3b 79 f9 1f",
  "54 d0 d7 84 c5 0b 38 90 af 07 13 0f",
  "10 a0 57 03 46 05 00 a0 af 33 1c 07",
  "00 40 5f f9 d8 02 00 40 bf 01 90 03",
  "00 40 5e 09 70 ff 00 80 be 35 e8 43",
  "00 00 fd 04 b8 25 00 00 f9 08 e8 3f",
  "00 00 fa eb d8 24 00 00 f2 ce f0 12",
  "00 00 e4 f7 e9 1f 00 00 e8 11 7a 12",
  "00 00 d0 ee 37 11 00 00 90 01 fd 17",
  "00 00 a0 82 1a 11 00 00 c0 44 9e 10",
  "00 00 80 29 ed 13 00 00 80 92 8f 20"})};

VAR HeartQueen := PixmapFromXData.T{47, 92, Cat(AT{
  "00 54 05 20 00 00 00 ac ce 20 00 00",
  "00 56 cd 11 00 00 00 ae da 11 00 00",
  "00 16 15 10 00 00 00 0e 3a 13 00 00",
  "00 75 3e 17 00 00 00 45 70 27 00 00",
  "00 7d 6e 20 00 00 00 75 cc 2c 00 00",
  "80 7e 9e 5d 00 00 80 46 80 5d 00 00",
  "80 86 00 83 00 00 80 8e 02 37 01 00",
  "40 8f 81 ff 01 00 40 0f 81 0d 00 00",
  "40 0f c4 3f 00 00 40 97 c3 7a 00 08",
  "a0 13 e3 ff 00 36 a0 3b 61 f5 03 2a",
  "a0 2f f0 ff 0f 5d a0 57 58 cd 3f 2a",
  "d0 ad af ce 1f 36 d0 5d 55 fb 0f 48",
  "d0 f2 ea f1 0f 7e 50 91 3f fb 35 37",
  "28 8e 24 b6 f0 20 98 7c e4 b1 d4 2b",
  "48 e6 1f 38 67 76 24 e2 03 18 7a 52",
  "24 f2 67 5b 5d 2a 32 d3 af fe 67 52",
  "29 51 4f ad 3b 62 20 39 9d ec 25 73",
  "54 e9 9e ce 3a 79 58 d9 3b ce 2a 3d",
  "12 b9 39 76 35 5f 9c b9 7b 67 2d 2f",
  "94 68 77 6b 35 57 84 7c 67 bf 2a 2b",
  "8e 64 f9 b7 36 15 95 6c e7 b5 2a 0b",
  "a4 74 9d 59 3d 25 88 a4 77 5e 2f 13",
  "c9 bc dc 79 33 11 5e dc 73 e7 1d 3d",
  "bc b8 e7 ce 3b 7a 88 cc 9e 3b 3d 93",
  "c8 f4 7a ee 25 11 a4 bc 9a b9 2e 25",
  "d0 54 ad e7 36 a9 a8 6c ed 9f 26 71",
  "d4 54 fd e6 3e 21 ea ac d6 ee 16 29",
  "f4 b4 e6 de 9d 39 fa ac 6e 9c 9d 48",
  "bc 54 73 dc 9b 1a 9e 5c 73 79 97 2a",
  "ce a4 37 b9 9c 04 46 dc b5 f2 8a 94",
  "4a e6 7f f5 cb 4c 54 ba da e6 4f 24",
  "4a 5e 18 c0 47 24 6e e6 1c f8 67 12",
  "d4 2b 8d 27 3e 19 04 0f 6d 24 71 14",
  "ec ac df fc 89 0a 7e f0 8f 57 4f 0b",
  "12 f0 df aa ba 0b 6c f8 73 f5 b5 0b",
  "54 fc b3 1a ea 05 ba f0 ff 0f f4 05",
  "54 c0 af 86 dc 05 6c 00 ff c7 c8 05",
  "10 00 5e c3 e9 02 00 00 fc 23 f0 02",
  "00 00 b0 81 f0 02 00 80 ff 81 f1 02",
  "00 80 ec 40 71 01 00 00 c1 00 61 01",
  "00 00 ba 01 62 01 00 00 ba 79 7e 01",
  "00 00 34 33 ae 00 00 00 04 76 be 00",
  "00 00 e4 0e a2 00 00 00 e8 7c ae 00",
  "00 00 c8 5c 70 00 00 00 08 a8 68 00",
  "00 00 88 5b 75 00 00 00 88 b3 6a 00",
  "00 00 04 73 35 00 00 00 04 a0 2a 00"})};

VAR SpadeQueen := PixmapFromXData.T{47, 92, Cat(AT{
  "00 96 b4 10 00 00 00 4e 19 08 00 00",
  "00 26 b2 0b 00 00 04 15 34 0b 00 00",
  "04 0d b8 0a 00 00 0a 75 7c 08 00 00",
  "0a 45 62 0f 00 00 0a 7d 7e 0e 00 00",
  "11 5d cc 0d 00 00 8a 7a dc 10 00 00",
  "8e 4a 80 1f 00 00 84 8a c1 1d 00 00",
  "84 8a c0 1f 00 00 8c 0a a0 03 00 00",
  "86 9a 63 01 00 00 4c 9d d1 01 00 00",
  "46 2d 30 03 00 00 4c 2d a8 02 00 00",
  "46 55 44 04 00 00 4c af ab 0a 00 00",
  "e6 13 11 11 00 00 ec ab aa 2e 00 00",
  "66 46 44 26 00 00 7c ae aa 41 00 00",
  "16 1c 91 94 00 00 bc a8 6a 8c 01 04",
  "36 4b 24 8a 02 0e 7c b3 1e 80 02 15",
  "f6 e4 47 61 87 3b fc a1 c2 b0 6f 15",
  "e6 91 b4 dc 7d 4e cc 93 04 f6 dc 25",
  "96 8f 08 6d b6 39 3c 8f c8 32 6b 7f",
  "66 ce b9 bd c5 2b cc fc df be a5 30",
  "86 cd 69 a7 55 31 1c 8d 98 a3 b5 4d",
  "36 8b e8 29 2b 13 2c 9a e4 6c 16 35",
  "46 bf 74 ce 0c 56 8c a3 3e 80 30 17",
  "86 ff ff ff 7f 30 ac 45 44 44 84 10",
  "a6 ff ff ff ff 3f 8c ab aa aa ea 18",
  "18 57 55 55 d5 31 fc ff ff ff ff 65",
  "08 21 22 22 a2 35 0c fe ff ff ff 61",
  "e8 0c 01 7c c5 31 6a 30 73 2e fd 62",
  "ac 68 36 27 59 34 c8 d4 94 17 d1 6c",
  "b2 ad c5 19 b1 38 8c aa e5 96 b3 61",
  "0c a5 7d fb 3f 33 d4 a3 bd 9d 73 66",
  "fe d6 4c 13 f1 3c 9c 6d b6 10 f1 69",
  "a4 3b 6f 20 c9 33 72 be 3b 2d 89 67",
  "a8 f6 0d 43 85 3f dc e1 86 e2 27 6f",
  "a8 40 01 78 cd 3e 70 40 51 24 d2 6c",
  "20 80 31 56 15 3d 00 00 29 89 38 68",
  "00 00 82 55 75 3e 00 00 64 22 62 66",
  "00 00 74 55 d5 37 00 00 88 88 c8 67",
  "00 00 50 d5 f5 32 00 00 20 22 aa 62",
  "00 00 40 15 b4 32 00 00 c0 0c b4 62",
  "00 00 80 8b b9 32 00 00 80 c6 59 61",
  "00 00 c0 05 50 31 00 00 f8 03 51 21",
  "00 00 b8 83 51 21 00 00 f8 01 52 71",
  "00 00 08 3b 5e 51 00 00 b0 33 ba 88",
  "00 00 70 7e be 50 00 00 f0 46 a2 50",
  "00 00 10 3e ae 50 00 00 50 1d b0 20",
  "00 00 d0 2c a8 20 00 00 d0 4d 64 00",
  "00 00 10 98 72 00 00 00 08 2d 69 00"})};

VAR ClubJack := PixmapFromXData.T{47, 92, Cat(AT{
  "60 ab 6a 6b 03 00 c0 d6 56 b5 01 00",
  "80 ad 6a db 00 00 81 db 56 6d 00 00",
  "83 f6 7e 37 00 00 46 0e 00 18 00 00",
  "4d fd ff 0f 00 00 9b fd ff 0f 00 00",
  "b7 04 40 0d 00 00 60 04 40 15 00 00",
  "ff 1c 5f 15 00 00 04 a4 40 15 00 00",
  "04 1e 4f 15 00 00 04 4c 46 15 00 00",
  "04 5c 4e 15 00 00 04 24 40 15 00 00",
  "04 e4 40 75 00 00 04 44 40 5d 00 00",
  "04 04 42 57 00 00 04 e8 41 55 00 00",
  "0f c8 60 35 00 00 12 18 70 7c 00 00",
  "27 37 dc ff 01 00 e4 e9 07 de 07 00",
  "e7 1c 9f d7 1e 00 f4 19 fa e9 7a 40",
  "ef dc 62 c4 ea 61 d4 19 06 d6 7e 37",
  "bc 1c 9e c3 f6 1e 7c d5 fe eb 7e 15",
  "ec 1a fa c1 ea 12 d4 1d e2 d5 6a 19",
  "bc cf 46 c0 fe 11 6c 0f dd fa f6 10",
  "f4 0a 3b d0 7e 16 bc 6d 55 cf 7a 10",
  "b4 07 1b d4 26 18 bc 85 ad d3 a1 15",
  "9c b5 0a 65 14 14 dc 82 ed 94 11 1a",
  "dc c2 4a 09 6c 1a cc 5a 3d 35 0b 1d",
  "6c c1 4a 82 06 1d 6c 61 d5 6d b5 1e",
  "6c ad ba b2 86 1e bc 60 6d 5b 83 1e",
  "78 c1 da b6 06 3d 78 61 4d 5d b5 36",
  "78 ad b6 ab 86 36 b8 60 41 52 83 36",
  "b8 d0 ac bc 5a 33 58 36 90 52 43 3b",
  "58 88 29 b7 41 3b 28 28 a6 50 ad 39",
  "a8 85 cb b5 a1 3d 18 64 2b d8 e0 2d",
  "08 5e f3 aa b6 3d 68 7e 0b dc 50 2f",
  "08 6f 5f bb f0 36 88 7f 03 62 f3 3d",
  "98 56 ab 47 b8 2b 48 57 83 5f 58 37",
  "a8 7e d7 7f ab 3e 78 6f c3 79 38 3d",
  "ec 7e 6b 60 98 2b 86 57 23 46 3b f7",
  "02 5e 97 5f 98 2f 00 78 eb f9 38 e7",
  "00 e0 7b e0 97 27 00 80 ff 3b ec e4",
  "00 00 3e 0e 18 48 00 00 ac 06 13 f0",
  "00 00 aa 82 17 20 00 00 ea 42 20 20",
  "00 00 ba 02 22 20 00 00 ae 02 27 20",
  "00 00 a8 02 24 20 00 00 a8 72 3a 20",
  "00 00 a8 62 32 20 00 00 a8 f2 78 20",
  "00 00 a8 02 25 20 00 00 a8 fa 38 ff",
  "00 00 a8 02 20 06 00 00 b0 02 20 ed",
  "00 00 f0 ff bf d9 00 00 f0 ff bf b2",
  "00 00 18 00 70 62 00 00 ec 7e 6f c1",
  "00 00 b6 6a db 81 00 00 db 56 b5 01",
  "00 80 ad 6a 6b 03 00 c0 d6 56 d5 06"})};

VAR DiamondJack := PixmapFromXData.T{47, 92, Cat(AT{
  "00 ce ed 76 0e 00 00 9c ed 36 07 00",
  "00 38 0c 86 03 00 01 f0 ff ff 01 00",
  "03 f0 ff ff 00 00 07 b0 02 c0 00 00",
  "06 a8 42 c0 00 00 05 a8 3e fc 00 00",
  "06 a8 82 c2 00 00 05 aa 7e fa 00 00",
  "06 ab 3a ba 00 00 85 a9 22 a2 00 00",
  "46 a9 02 a2 00 00 a5 a8 02 a4 00 00",
  "9e a8 02 a7 00 00 47 a8 02 a0 00 00",
  "22 ae 22 a0 00 00 10 a9 c2 d7 03 40",
  "0d af 83 91 04 60 07 61 06 28 05 70",
  "85 1e 1c 44 0c 30 c4 64 f0 c7 37 10",
  "66 99 03 38 d3 50 f7 66 fc c7 dc 73",
  "ce 8d 03 38 de 5d ac 3b fc 87 bf 1d",
  "17 d7 03 f8 be 3b b4 ee fe ef 76 77",
  "57 dc bb bb 76 3e b4 ba ee ee f6 1c",
  "17 69 bb bb f6 59 b4 da ee ee d6 73",
  "4f ec bd bb f6 57 ac da eb ee 16 1f",
  "1d e9 b6 bb f6 3f af da ee ee 16 78",
  "55 ec db bb f6 3f bc da bb ee 16 10",
  "16 e9 6c bb f6 5f af 7a d7 ee 16 78",
  "76 7c ab bd f6 57 ac 9a 5d eb 56 1c",
  "35 ed a2 b6 f6 37 ef ee 59 ed b6 7b",
  "b5 73 db da 5e 37 2c bd 46 b1 5e 1a",
  "58 7a 8d 62 bd 34 ec 7a 5b db ce ad",
  "de 6d b7 9a 77 f7 ec 6f 6d 45 b7 ac",
  "38 6a d7 ba 59 35 ea 6f bd d5 3e 6e",
  "1e 68 77 eb 5e f5 fa 6f dd 36 97 68",
  "08 68 77 dd 5b 3d fc 6f dd db 37 aa",
  "1e 68 77 77 5b f5 fc 6f dd 6d 97 b8",
  "f8 68 77 d7 5b 35 ea 6f dd bd 37 f2",
  "ce 6b 77 77 5b 2d 9a 6f dd dd 96 e8",
  "38 6f 77 77 5d 2d 7c 6e dd dd 3b ea",
  "ee 6e f7 7f 77 2d dc 7d 1f c0 eb e8",
  "b8 fd e1 3f dc 35 ba 7b 1c c0 b1 73",
  "ce 3b e3 3f 66 ef 0a cb 1c c0 99 66",
  "08 ec e3 0f 26 23 0c 30 22 38 78 a1",
  "0e a0 14 60 86 e0 06 20 89 c1 f5 b0",
  "02 c0 eb 43 95 08 00 00 05 44 75 44",
  "00 00 05 40 15 e2 00 00 e5 40 15 79",
  "00 00 25 40 15 a5 00 00 45 40 95 62",
  "00 00 45 44 95 a1 00 00 5d 5c d5 60",
  "00 00 5f 7e 55 a0 00 00 43 41 15 60",
  "00 00 3f 7c 15 a0 00 00 03 42 15 60",
  "00 00 03 40 0d e0 00 00 ff ff 0f c0",
  "00 80 ff ff 0f 80 00 c0 61 30 1c 00",
  "00 e0 6c b7 39 00 00 70 6e b7 73 00"})};

VAR HeartJack := PixmapFromXData.T{47, 92, Cat(AT{
  "60 ca 64 ca 00 00 c0 d4 6a 65 00 00",
  "81 c9 64 32 00 00 03 d3 6a 19 00 00",
  "17 c6 60 0c 00 00 37 fc ff 0f 00 00",
  "57 f8 ff 0f 00 00 97 50 05 08 00 00",
  "37 51 e5 05 00 00 3f 51 c5 04 00 00",
  "6a 51 a5 04 00 00 aa 51 c5 09 00 00",
  "6a a9 02 08 00 00 3f a9 32 10 00 00",
  "97 a8 12 1b 00 00 57 56 e1 04 00 00",
  "37 59 01 04 00 09 97 56 81 07 00 0f",
  "87 5a 07 0e 04 06 07 a7 1a 08 09 0d",
  "c7 fe 75 14 f6 0b f7 ff ff 3f 88 06",
  "3f 07 00 c0 84 01 38 8f 44 91 41 02",
  "e7 1e ff 7f a6 0e de 3f 80 00 9c 1a",
  "b6 73 fe 7f 7e 17 d9 f3 44 22 f3 0a",
  "ed ee 89 91 73 07 76 fc 53 ca dd 06",
  "3b 39 97 e9 7f 1f 9d 3c af 75 d6 7a",
  "4e ee 2e 74 6e 6b 27 f7 5f ba d7 6a",
  "93 af 59 fa 7b 5b c9 d5 b9 9d dd 32",
  "e4 fa b7 9d 6b 6b 72 1f 7f ee d7 5a",
  "f9 26 66 fe 7f 37 5c f3 67 66 dc 6e",
  "af 09 6e 66 b2 5d f6 fd 7f f6 67 3b",
  "ed fa 7f ff cf 76 da 4a 24 12 d9 2d",
  "ed 8a 94 94 a8 57 da 6a 4f 79 ab 2d",
  "b4 d5 9e f2 56 5b ea 15 29 29 51 b7",
  "b4 9b 48 24 52 5b 6e f3 ff fe 5f b7",
  "dc e6 6f fe bf 6f ba 4d 66 76 90 f5",
  "76 3b 66 e6 cf 3a ec fe 7f 66 64 9f",
  "5a eb 77 fe f8 4e d6 d6 b9 ed 5f 27",
  "4c bb b9 9d ab 93 da de 5f 9a f5 c9",
  "56 eb 5d fa ef e4 d6 76 2e 74 77 72",
  "5e 6b ae f5 3c b9 f8 fe 97 e9 9c dc",
  "60 bb 53 ca 3f 6e e0 ce 89 91 77 b7",
  "50 cf 44 22 cf 9b e8 7e fe 7f ce 6d",
  "58 39 00 01 fc 7b 70 65 fe ff 78 e7",
  "40 82 89 22 f1 1c 80 21 03 00 e0 fc",
  "60 11 fc ff ff ef d0 6f 28 ae 7f e3",
  "b0 90 10 58 e5 e0 60 20 70 e0 5a e1",
  "f0 00 e0 81 6a e9 90 00 20 80 9a ec",
  "00 00 20 87 6a ea 00 00 d8 48 15 e9",
  "00 00 08 4c 95 fc 00 00 10 40 95 56",
  "00 00 90 a3 8a 55 00 00 20 a5 8a 56",
  "00 00 20 a3 8a fc 00 00 a0 a7 8a ec",
  "00 00 10 a0 0a e9 00 00 f0 ff 1f ea",
  "00 00 f0 ff 3f ec 00 00 30 06 63 e8",
  "00 00 98 56 cb c0 00 00 4c 26 93 81",
  "00 00 a6 56 2b 03 00 00 53 26 53 06"})};

VAR SpadeJack := PixmapFromXData.T{47, 92, Cat(AT{
  "b8 ba ee ea 00 00 70 75 d7 75 00 00",
  "e4 ba ee 3a 00 00 ca 75 d7 1d 00 00",
  "8e ff ff 0f 00 00 0e ff ff 0f 00 00",
  "0a 01 a8 0a 00 00 0a f1 a8 0a 00 00",
  "0e 09 ab 0a 00 00 0e 79 a8 0a 00 00",
  "0a b1 a8 0a 00 00 0e 79 a8 0a 00 00",
  "8a 00 a8 0a 00 00 95 03 a8 0a 00 00",
  "0a 99 a8 0a 00 00 0e 21 a9 0a 00 00",
  "0a df a8 0a 00 00 0e 0e a8 0a 00 00",
  "0e 02 e8 0e 00 00 0a 02 bc 3b 00 00",
  "0a 06 eb 2e 00 00 0e fc bf 3b 00 00",
  "9e 03 00 f8 01 00 b1 6d db 06 07 00",
  "7e 02 00 74 19 00 d9 ff ff 77 ed 00",
  "ce b4 bb 27 67 01 55 f9 ee 06 33 07",
  "32 b0 fb 77 99 09 39 fe bf 77 cd 3c",
  "4d 26 a8 24 67 66 e6 4a 72 06 31 3f",
  "b3 f1 ac 75 dd 13 98 b1 71 74 b7 2c",
  "ef d2 db 24 89 21 a0 f6 04 04 27 33",
  "7c 3e f9 ff 89 2d 26 4e 5a d5 22 21",
  "92 93 ae ea 88 33 cb a4 db 5d 22 2d",
  "67 c9 e8 ae 98 21 70 32 5a 35 b2 72",
  "bf 8c a8 9a 9c 3c 38 25 fa 2f a4 31",
  "4c da 99 0c 57 3a 9c 4f 69 4b f9 1c",
  "38 9f d2 96 f2 39 5c ea 30 99 5b 32",
  "8c 25 f4 5f a4 1c 3c 39 59 15 31 fd",
  "4e 4d ac 5a 4c 0e 84 19 75 17 93 e6",
  "b4 44 ba db 25 d3 cc 11 57 75 c9 49",
  "84 44 ab 5a 72 64 b4 91 ff 9f 7c 3e",
  "cc e4 20 20 6f 05 84 91 24 db 4b f7",
  "34 ed 2e 8e 8d 19 c8 bb ae 35 8f cd",
  "fc 8c 60 4e 52 67 66 e6 24 15 64 b2",
  "3c b3 ee fd 7f 9c 90 99 ee df 0d 4c",
  "e0 cc 60 77 9f aa 80 e6 e4 dd 2d 73",
  "00 b7 ee ff ff 9b 00 98 2e 00 40 7e",
  "00 e0 60 db b6 8d 00 80 1f 00 c0 79",
  "00 00 dc fd 3f 70 00 00 74 d7 60 50",
  "00 00 dc 3d 40 50 00 00 70 17 40 70",
  "00 00 50 15 70 70 00 00 50 15 fb 50",
  "00 00 50 95 84 70 00 00 50 15 99 50",
  "00 00 50 15 c0 a9 00 00 50 15 00 51",
  "00 00 50 15 9e 70 00 00 50 15 8d 50",
  "00 00 50 15 9e 70 00 00 50 d5 90 70",
  "00 00 50 15 8f 50 00 00 50 15 80 50",
  "00 00 f0 ff ff 70 00 00 f0 ff ff 71",
  "00 00 b8 eb ae 53 00 00 5c 77 5d 27",
  "00 00 ae eb ae 0e 00 00 57 77 5d 1d"})};

VAR
  Clubs := SuitData{ClubJack, ClubQueen, ClubKing};
  Diamonds := SuitData{DiamondJack, DiamondQueen, DiamondKing};
  Hearts := SuitData{HeartJack, HeartQueen, HeartKing};
  Spades := SuitData{SpadeJack, SpadeQueen, SpadeKing};
  Data := ARRAY Suit OF SuitData{Spades, Hearts, Diamonds, Clubs};
  
CONST
  ClubDelta = SuitDelta{Pt{1,1}, Pt{1,1}, Pt{1,1}};
  DiamondDelta = ClubDelta;
  HeartDelta = SuitDelta{Pt{2,2}, Pt{1,1}, Pt{1,5}};
  SpadeDelta = SuitDelta{Pt{1,-1}, Pt{2,3}, Pt{2,1}};
  Delta = ARRAY Suit OF 
    SuitDelta{SpadeDelta, HeartDelta, DiamondDelta, ClubDelta};

BEGIN
END FaceCards.
