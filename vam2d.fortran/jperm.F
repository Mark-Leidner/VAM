c!#   $Id: jperm.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: jperm.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      SUBROUTINE JPERM (IX,IW,IPERM,IDIM,N)
#include "vam.h"
      DIMENSION IX(IDIM,N),IW(N),IPERM(N)
C-----COPY IX INTO IW(ORK) AREA
      DO 100 I=1,N
  100 IW(I)=IX(1,I)
C-----SORT BACK INTO IX USING IPERM
      DO 200 I=1,N
      J=IPERM(I)
  200 IX(1,I)=IW(J)
      RETURN
      END
