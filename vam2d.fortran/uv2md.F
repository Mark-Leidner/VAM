c!#   $Id: uv2md.F,v 1.2 2005/08/09 19:26:08 leidner Exp $
c!#   $Log: uv2md.F,v $
c!#   Revision 1.2  2005/08/09 19:26:08  leidner
c!#   Fixed minor typo which caused a compile failure on the Portland Group
c!#   compilers.
c!#
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      SUBROUTINE UV2MD(U, V, ZMAG, THETA)
C***********************************************************************
C     THIS ROUTINE CALCULATES WIND DIRECTION FROM U,V COMPONENTS
C
C
      IF(U .EQ. 0.0 .AND. V .EQ. 0.0) RETURN
      ZMAG=SQRT(U**2 + V**2)
      PI=3.1415926
      IF(V.EQ.0.0)GO TO 12
      IF(U.EQ.0.0.AND.V.GT.0.0)THETA=PI
      IF(U.EQ.0.0.AND.V.LT.0.0)THETA=2.*PI
      IF(U.NE.0.0.AND.V.NE.0.0)GO TO 15
      GO TO 13
C
   12 IF(U.LT.0.0)THETA=PI/2.
      IF(U.GT.0.0)THETA=3.*PI/2.
      GO TO 13
   15 IF(U.GT.0.)THETA=3.*PI/2.-ATAN(V/U)
      IF(U.LT.0.) THETA=PI/2.+ATAN(V/ABS(U))
   13 THETA=THETA*180./PI
C     WRITE(6,11) THETA
   11 FORMAT(1X,F12.4)
      RETURN
      END
