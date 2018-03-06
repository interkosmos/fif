      DIMENSION NS(100)
 1    FORMAT (I6)
      READ 1 N, (NS(I), I = 1,N)
      PRINT 1 (NS(N + 1 - I), I = 1,N)
