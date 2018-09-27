  
  
  
  
       real FUNCTION DDOT(N,D,I1,S,I2)
c!#   $Id: ddot.F,v 1.1 2000/11/07 20:16:33 mcc Exp $
c!#   $Log: ddot.F,v $
c!#   Revision 1.1  2000/11/07 20:16:33  mcc
c!#   Routine added to build minimize.f90dir lib. Initial revision
c!#
c!#   Revision 1.1  1997/02/10 14:52:21  leidner
c!#   Initial revision
c!#
C
C      -------------------------------------------------------
C      THIS FUNCTION COMPUTES THE INNER PRODUCT OF TWO VECTORS
C      -------------------------------------------------------
C
       real D(N),S(N),PROD
       INTEGER I1,I2
C
        PROD=0.0D0
        DO 10 I=1,N 
 10     PROD= PROD+D(I)*S(I)
C
        DDOT= PROD
C
       RETURN
       END
