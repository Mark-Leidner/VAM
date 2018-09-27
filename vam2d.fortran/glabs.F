c!#   $Id: glabs.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: glabs.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      SUBROUTINE GLABS (N,L,S,M)
#include "vam.h"
C-----------------------------------------------------------------------
C-----XREFS:
C-----FORT : ...MOD ...... ...... ...... ...... ...... ...... ......
      INTEGER N,M,R
      REAL L,S
      IF (N.GT.4) GO TO 10
      M=1
      RETURN
   10 M=(S/L)*(N-1)+0.99
   20 R=MOD(N-1,M)
      IF (R.EQ.0) RETURN
      IF (R.GE.(M+1)/2) RETURN
      IF (M.GE.N/2) GO TO 30
      M=M+1
      GO TO 20
   30 M=N/2
      RETURN
      END
