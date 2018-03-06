      DIMENSION A(10)
 1    FORMAT (I3/(F6.3))
      READ 1 N, (A(J), J = 1,N)
      SUM = 0.0
      DO 5 I = 1, N
      IF (A(I)) 4, 2, 2
 2    SUM = SUM + A(I)
      PRINT 1 A(I)
      CONTINUE
 3    FORMAT (21HNegative number found)
 4    PRINT 3
 5    CONTINUE
 6    FORMAT (3HHi F6.3)
      PRINT 6 SUM / N
