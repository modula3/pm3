      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,                  INFO )
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( LWORK )
*  TRANS   (input)
*  M       (input)
*  N       (input)
*  NRHS    (input)
*  A       (input/output)
*  LDA     (input)
*  B       (input/output)
*  LDB     (input)
*  WORK    (workspace/output)
*  LWORK   (input)
*  INFO    (output)

