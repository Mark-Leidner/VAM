      SUBROUTINE SSFORE (SS,SWGT,DELSSU,DELSSV,U,V,IDIM)
c!#   $Id: ssfore.F,v 1.1 2000/11/13 13:47:48 mcc Exp $
c!#   $Log: ssfore.F,v $
c!#   Revision 1.1  2000/11/13 13:47:48  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.3  1998/04/02 15:24:04  stanr
c!#   change type of SS to real*8, matching sscgr
c!#
c!#   Revision 1.2  1997/02/12 20:19:52  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  14:15:09  leidner
c!#	Initial revision
c!#
#include "vam.h"
C***********************************************************************
C*****SSFORE CALCULATES
C*****SUM OF SQUARES AND GRADIENT DUE TO (ANALYSIS - FORECAST)
C-----U AND V PASSED TO THIS ROUTINE MUST BE DELTAS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
#include "gparm.h"
C-----DEFINE NUMERICAL CONSTANTS
      REAL NINETY
      data NINETY/90D0/
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HSSFORE)
      DO 1000 L=LMIN,LMAX
C-----CHECK FOR POLE
      IF ( .NOT. ((L.EQ.1 .AND. YS.EQ.-NINETY)  .OR.
     +      (L.EQ.NY.AND. YF.EQ.+NINETY)) ) GO TO 800
C-----POLE POINT FOR THIS L
      SS=SS+APOLE*(U(1,L)**2 + V(1,L)**2)
      DELSSU(1,L)=DELSSU(1,L)+APOLE*U(1,L)
      DELSSV(1,L)=DELSSV(1,L)+APOLE*V(1,L)
      GO TO 1000
  800 DO 900 K=KMIN,KMAX
      SS=SS+BUNC(K)*AUNC(L)*(U(K,L)**2 + V(K,L)**2)
      DELSSU(K,L)=DELSSU(K,L)+BUNC(K)*AUNC(L)*U(K,L)
      DELSSV(K,L)=DELSSV(K,L)+BUNC(K)*AUNC(L)*V(K,L)
  900 CONTINUE
 1000 CONTINUE
      SWGT=AREA
      RETURN
      END
