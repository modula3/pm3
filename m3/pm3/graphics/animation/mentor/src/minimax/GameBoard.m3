(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

MODULE GameBoard EXPORTS GameBoard, GameBoardPrivate;

IMPORT MoveList;

REVEAL
  T =
    GameBoardPrivate BRANDED OBJECT
      cells: ARRAY [0 .. BoardSize - 1], [0 .. BoardSize - 1] OF PlayerId;
      (* The array keeping track of what piece is in what cell of the
         board *)
      numPieces: ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF
                   [0 .. BoardSize];
      (* How many pieces each player has left *)
      numMove     : CARDINAL;    (* The number of the move *)
      movingPlayer: PlayerId;    (* The player who is moving *)
      parentBoard : T;
      key         : INTEGER;
    OVERRIDES
      Init           := DoInit;
      squareContents := DoSquareContents;
      moveNumber     := DoMoveNumber;
      toMove         := DoToMove;
      legalMoves     := DoLegalMoves;
      finished       := DoFinished;
      Move           := DoMove;
      previous       := DoPrevious;
      getKey         := DoGetKey;
    END;

(* Opponent[player] gives the opponent of player *)
CONST
  Opponent = ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF
               [PlayerId.PlayerA .. PlayerId.PlayerB]{
               PlayerId.PlayerB, PlayerId.PlayerA};

(* HomeRow[player] gives the row number which player began on *)
CONST
  HomeRow = ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF
              [0 .. BoardSize - 1]{0, 2};

(* Forward[player] gives the delta of y which represents player's pieces
   moving forward *)
CONST
  Forward = ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF
              [-1 .. 1]{1, -1};

PROCEDURE ComputeKey (cells: ARRAY [0 .. BoardSize - 1],
                               [0 .. BoardSize - 1] OF
                               PlayerId): INTEGER =
  VAR res: INTEGER := 0;
  BEGIN
    FOR x := 0 TO BoardSize - 1 DO
      FOR y := 0 TO BoardSize - 1 DO
        res := 3 * res + ORD(cells[x, y]);
      END;
    END;
    RETURN res;
  END ComputeKey;

PROCEDURE DoInit (self: T): T =
  BEGIN
    self.cells :=
      ARRAY [0 .. BoardSize - 1] OF
        ARRAY [0 .. BoardSize - 1] OF PlayerId{
        ARRAY [0 .. BoardSize - 1] OF
          PlayerId{PlayerId.PlayerA, PlayerId.Neither, PlayerId.PlayerB},
        ..};
    self.numPieces := ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF
                        [0 .. BoardSize]{3, 3};
    self.numMove := 0;
    self.movingPlayer := PlayerId.PlayerA;
    self.parentBoard := NIL;
    self.key := ComputeKey(self.cells);
    RETURN self;
  END DoInit;

PROCEDURE DoSquareContents (self: T; square: Square): PlayerId =
  BEGIN
    RETURN self.cells[square.x, square.y];
  END DoSquareContents;

PROCEDURE DoMoveNumber (self: T): CARDINAL =
  BEGIN
    RETURN self.numMove;
  END DoMoveNumber;

PROCEDURE DoToMove (self: T): PlayerId =
  BEGIN
    RETURN self.movingPlayer;
  END DoToMove;

(* This is a helping procedure, testing whether the game is over because
   one side has no pieces left, or has pieces on the opponents end of the
   board *)

PROCEDURE GameObviouslyOver (board: T; VAR (*OUT*) winner: PlayerId):
  BOOLEAN =
  BEGIN
    FOR player := PlayerId.PlayerA TO PlayerId.PlayerB DO
      IF board.numPieces[player] = 0 THEN
        winner := Opponent[player];
        RETURN TRUE;
      END;
    END;
    FOR player := PlayerId.PlayerA TO PlayerId.PlayerB DO
      FOR x := 0 TO BoardSize - 1 DO
        IF board.cells[x, HomeRow[Opponent[player]]] = player THEN
          winner := player;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END GameObviouslyOver;

PROCEDURE DoLegalMoves (self: T): MoveList.T =
  VAR
    winner  : PlayerId;
    movelist: MoveList.T := NIL;
  BEGIN
    IF GameObviouslyOver(self, winner) THEN RETURN NIL; END;
    FOR x := 0 TO BoardSize - 1 DO
      FOR y := 0 TO BoardSize - 1 DO
        IF self.cells[x, y] = self.movingPlayer THEN
          PieceMoves(self, self.movingPlayer, x, y, movelist);
        END;
      END;
    END;
    RETURN movelist;
  END DoLegalMoves;

(* PieceMoves adds to movelist the moves that mover can make which involve
   moving the piece in square (x,y). *)

PROCEDURE PieceMoves (    board   : T;
                          mover   : PlayerId;
                          x, y    : [0 .. BoardSize - 1];
                      VAR movelist: MoveList.T            ) =
  VAR newY: [0 .. BoardSize - 1] := y + Forward[mover];
  BEGIN
    IF board.cells[x, newY] = PlayerId.Neither THEN
      movelist :=
        MoveList.Cons(Move{Square{x, y}, Square{x, newY}}, movelist);
    END;
    FOR newX := x - 1 TO x + 1 BY 2 DO
      IF newX >= 0 AND newX < BoardSize THEN
        IF board.cells[newX, newY] = Opponent[mover] THEN
          movelist := MoveList.Cons(
                        Move{Square{x, y}, Square{newX, newY}}, movelist);
        END;
      END;
    END;
  END PieceMoves;

PROCEDURE DoFinished (self: T; VAR (*OUT*) winner: PlayerId): BOOLEAN =
  BEGIN
    IF GameObviouslyOver(self, winner) THEN RETURN TRUE; END;
    IF self.legalMoves() = NIL THEN
      winner := Opponent[self.movingPlayer];
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END DoFinished;

PROCEDURE DoPrevious (self: T): T =
  BEGIN
    RETURN self.parentBoard;
  END DoPrevious;

PROCEDURE DoGetKey (self: T): INTEGER =
  BEGIN
    RETURN self.key;
  END DoGetKey;

PROCEDURE DoMove (self: T; move: Move): T =
  PROCEDURE UpdateCells ():
    ARRAY [0 .. BoardSize - 1], [0 .. BoardSize - 1] OF PlayerId =
    VAR res: ARRAY [0 .. BoardSize - 1], [0 .. BoardSize - 1] OF PlayerId;
    BEGIN
      res := self.cells;
      res[move.fromSquare.x, move.fromSquare.y] := PlayerId.Neither;
      res[move.toSquare.x, move.toSquare.y] := self.movingPlayer;
      RETURN res;
    END UpdateCells;

  PROCEDURE UpdateNumPieces ():
    ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF [0 .. BoardSize] =
    VAR
      res: ARRAY [PlayerId.PlayerA .. PlayerId.PlayerB] OF [0 .. BoardSize];
    BEGIN
      res := self.numPieces;
      IF self.cells[move.toSquare.x, move.toSquare.y]
           = Opponent[self.movingPlayer] THEN
        res[Opponent[self.movingPlayer]] :=
          res[Opponent[self.movingPlayer]] - 1;
      END;
      RETURN res;
    END UpdateNumPieces;
  VAR
    newCells: ARRAY [0 .. BoardSize - 1], [0 .. BoardSize - 1] OF PlayerId;
  BEGIN
    newCells := UpdateCells();
    RETURN NEW(T, cells := newCells, numPieces := UpdateNumPieces(),
               numMove := self.numMove + 1,
               movingPlayer := Opponent[self.movingPlayer],
               parentBoard := self, key := ComputeKey(newCells));
  END DoMove;

BEGIN
END GameBoard.
