c!#   $Id: gridx.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: gridx.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      SUBROUTINE GRIDX (XHAT,XS,DELX,IPER,XMIN,XMAX,I,X,*)
#include "vam.h"
C***********************************************************************
C-----SPECIFICATIONS
C-----XREFS:
C-----FORT : ..AMOD ...... ...... ...... ...... ...... ...... ......
C*****GRIDX TRANSFORMS XHAT TO GRID COORDINATES I+X FOR SUBSEQUENT
C*****BILINEAR INTERPOLATION.  PERIODICITY IS ALLOWED FOR.
      LOGICAL L
C     DOUBLE PRECISION AMOD
C     AMOD(DUMMY)=DMOD(DUMMY)
      X=(XHAT-XS)/DELX
      IF (IPER.EQ.0) GO TO 100
      XPER=IPER
      X=AMOD(X,XPER)
  100 X=X+1
      IF (X.LT.XMIN) X=X+IPER
      L = (X.LT.XMIN) .OR. (X.GT.XMAX)
      I=X
      X=X-I
      IF (L) RETURN 1
      RETURN
      END
