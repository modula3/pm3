      SUBROUTINE DGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI,                  VS, LDVS, WORK, LWORK, BWORK, INFO )
      CHARACTER          JOBVS, SORT
      INTEGER            INFO, LDA, LDVS, LWORK, N, SDIM
      LOGICAL            BWORK( * )
      DOUBLE PRECISION   A( LDA, * ), VS( LDVS, * ), WI( * ), WORK( * ),                   WR( * )
      LOGICAL            SELECT
      EXTERNAL           SELECT
*  JOBVS   (input)
*  SORT    (input)
*  SELECT  (input)
*  N       (input)
*  A       (input/output)
*  LDA     (input)
*  SDIM    (output)
*  WR      (output)
*  WI      (output)
*  VS      (output)
*  LDVS    (input)
*  WORK    (workspace/output)
*  LWORK   (input)
*  BWORK   (workspace)
*  INFO    (output)

