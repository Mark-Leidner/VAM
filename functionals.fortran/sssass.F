      SUBROUTINE SSSASS (SS,SWGT,DELSSU,DELSSV,U,V,IDIM)
c!#   $Id: sssass.F,v 1.1 2000/11/13 13:47:56 mcc Exp $
c!#   $Log: sssass.F,v $
c!#   Revision 1.1  2000/11/13 13:47:56  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.3  1997/04/09 15:08:58  rnh
c!#   Added 12 point bicubic interpolation,
c!#   controlled by parameter interp in /gparm/.
c!#
c!#	Revision 1.2  1997/02/12  20:19:52  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  14:15:09  leidner
c!#	Initial revision
c!#
#include "vam.h"
C***********************************************************************
C*****SSSASS CALCULATES SUMS OF SQUARES AND GRADIENT DUE TO SASS WINDS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
#include "ptdta.h"
#include "gparm.h"
C     DOUBLE PRECISION EXP
C     EXP(DUMMY)=DEXP(DUMMY)
cJA   IF (NPTS.LE.0) CALL ERRSEA (513,6HSSSASS)
C-----LOOP THROUGH POINTS
      DO 100 N=1,NPTS
      IF (WGT(N).LE.0) GO TO 100
      IF (NWINDS(N).EQ.0) GO TO 100
c
      call uvinterp
     c   ( idim, 1,
     c     ill(n), jll(n), xcoord(n), ycoord(n),
     i     u, v,
     o     ui5, vi5 )
c
C-----CALCULATE D2MAX
      D2MAX=DFAC*VBAR(N)**2
C-----CALCULATE LOSS AND PARTIAL WRT UI, VI
      S=1
      DSDUI=0
      DSDVI=0
      NA=NWINDS(N)
      DO 50 IA=1,NA
      D2=(ui5-UOBS(IA,N))**2 + (vi5-VOBS(IA,N))**2
C-----AVOID UNDERFLOWS WHICH MAY OCCUR IF PERSS IF WAY OFF AND FIRST
C-----STEP TAKEN BY ZXCGR GOES WAY OFF
      IF (D2.GT.170*D2MAX) GO TO 50
      T=1-EXP(-D2/D2MAX)
      S=S*T
C-----AVOID DIVIDE CHECK - WHEN T = 0, S = 0 AND DSDUI, DSDVI ARE ZERO
      IF (T.EQ.0) GO TO 50
      DSDUI=DSDUI + (1/T - 1)*(ui5-UOBS(IA,N))
      DSDVI=DSDVI + (1/T - 1)*(vi5-VOBS(IA,N))
   50 CONTINUE
      DSDUI=S*DSDUI
      DSDVI=S*DSDVI
      S=D2MAX*S
C-----INCREMENT SUMS AND GRADIENTS
   75 SS=SS + WGT(N)*S
      SWGT=SWGT + WGT(N)
c
      ui = wgt(n)*dsdui
      vi = wgt(n)*dsdvi
c
      call uvinterpad
     c   ( idim, 1,
     c     ill(n), jll(n), xcoord(n), ycoord(n),
     i     delssu, delssv,
     o     ui, vi )
c
  100 CONTINUE
      RETURN
      END
