INTERFACE Test;

IMPORT Text;

VAR
  errors:   INTEGER := 0;
  warnings: INTEGER := 0;

PROCEDURE msg (t: Text.T);
PROCEDURE msgB (b: BOOLEAN);
PROCEDURE msgI (i: INTEGER);
PROCEDURE msgC (c: CHAR);
PROCEDURE msgR (r: REAL);

PROCEDURE check (b: BOOLEAN);
PROCEDURE checkB (b, shouldBe: BOOLEAN);
PROCEDURE checkI (i, shouldBe: INTEGER);
PROCEDURE checkC (i, shouldBe: CHAR);
PROCEDURE checkR (r, shouldBe: REAL);
PROCEDURE checkL (r, shouldBe: LONGREAL);
PROCEDURE checkX (r, shouldBe: EXTENDED);

PROCEDURE warn (b: BOOLEAN);

PROCEDURE done ();

END Test.

