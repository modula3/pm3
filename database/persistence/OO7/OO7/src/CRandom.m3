UNSAFE MODULE CRandom;

IMPORT Ctypes;

PROCEDURE State (VAR state: ARRAY[1..256] OF CHAR): Ctypes.void_star =
  BEGIN
    RETURN ADR(state[1]);
  END State;

BEGIN
END CRandom.
