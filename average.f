C     PROGRAM FOR FINDING THE AVERAGE OF A
C    X    SET OF NUMBERS
      DIMENSION A(999)
    1 FORMAT (I3/(F6.3))
      READ 1 N, (A(J), J = 1, N)
      SUM = 0.0
      DO 6 I = 1, N
      IF (A(I)) 5, 2, 2
    2 SUM = SUM + A(I)
    3 FORMAT (F6.3)
      PRINT 3 A(I)
      CONTINUE
    4 FORMAT (21HNEGATIVE NUMBER FOUND)
    5 PRINT 4
    6 CONTINUE
    7 FORMAT (9HAVERAGE: F6.3)
      PRINT 7 SUM / N
