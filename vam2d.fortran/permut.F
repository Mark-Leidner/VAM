c!#   $Id: permut.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: permut.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      SUBROUTINE PERMUT(X,W,IPERM,IDIM,N)
#include "vam.h"
C-----SPECIFICATIONS
      DIMENSION X(IDIM,N),W(N),IPERM(N)
C-----COPY X INTO W(ORK) AREA
      DO 100 I=1,N
  100 W(I)=X(1,I)
C-----SORT BACK INTO X USING IPERM
      DO 200 I=1,N
      J=IPERM(I)
  200 X(1,I)=W(J)
      RETURN
      END
