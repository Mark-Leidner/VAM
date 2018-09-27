      SUBROUTINE SSFILT (SS,SWGT,DELSSU,DELSSV,U,V,IDIM)
c!#   $Id: ssfilt.F,v 1.1 2000/11/13 13:47:45 mcc Exp $
c!#   $Log: ssfilt.F,v $
c!#   Revision 1.1  2000/11/13 13:47:45  mcc
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
C*****SSFILT CALCULATES
C*****SUM OF SQUARES AND GRADIENT DUE TO J2 FILTER
C-----U AND V PASSED TO THIS ROUTINE MUST BE DELTAS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
      real SU,SV
#include "gparm.h"
C-----DEFINE NUMERICAL CONSTANTS
      REAL NINETY
      data NINETY/90D0/
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HSSFILT)
C-----CHECK THAT INTEGRATION AREA IS INTERIOR TO GRID EXCEPT AT POLES
      IF(KMIN.EQ.1 .OR. KMAX.EQ.NX) CALL ERRSEA (514,6HSSFILT)
      IF ((LMIN.EQ.1  .AND. YS.NE.-NINETY) .OR.
     +    (LMAX.EQ.NY .AND. YF.NE.+NINETY)) CALL ERRSEA (514,6HSSFILT)
C-----IF POLES PRESENT ALL LONGITUDES MUST BE PRESENT
      IF ( ((LMIN.EQ.1 .AND. YS.EQ.-NINETY)  .OR.
     +      (LMAX.EQ.NY.AND. YF.EQ.+NINETY)) .AND.
     +     (IPER.GT.NX) ) CALL ERRSEA (514,6HSSFILT)
      DO 1000 L=LMIN,LMAX
C-----CHECK FOR POLE
      IF ( .NOT. ((L.EQ.1 .AND. YS.EQ.-NINETY)  .OR.
     +      (L.EQ.NY.AND. YF.EQ.+NINETY)) ) GO TO 800
C-----POLE POINT FOR THIS L
      IF (L.EQ.1) IPOLE=-1
      IF (L.EQ.NY) IPOLE=1
      L1=L-IPOLE
      SU=-IPER*U(1,L)
      SV=-IPER*V(1,L)
      DO 250 I=1,IPER
      SU=SU+ALPHA(I)*U(I,L1)-IPOLE*BETA(I)*V(I,L1)
      SV=SV+IPOLE*BETA(I)*U(I,L1)+ALPHA(I)*V(I,L1)
  250 CONTINUE
      SS=SS+APOLE*EJ2**2*(SU**2 + SV**2)
      DELSSU(1,L)=DELSSU(1,L)-APOLE*EJ2**2*SU*IPER
      DELSSV(1,L)=DELSSV(1,L)-APOLE*EJ2**2*SV*IPER
      DO 500 I=1,IPER
      DELSSU(I,L1)=DELSSU(I,L1)
     +    +APOLE*EJ2**2*(ALPHA(I)*SU+IPOLE*BETA(I)*SV)
      DELSSV(I,L1)=DELSSV(I,L1)
     +    +APOLE*EJ2**2*(-IPOLE*BETA(I)*SU+ALPHA(I)*SV)
  500 CONTINUE
      GO TO 1000
C-----NON POLAR L
  800 DO 900 K=KMIN,KMAX
      SU=AJ2(L)*U(K,L+1)+BJ2(L)*U(K,L-1)+CJ2(L)*U(K,L)+
     +   DJ2(L)*(U(K+1,L)+U(K-1,L))
      SV=AJ2(L)*V(K,L+1)+BJ2(L)*V(K,L-1)+CJ2(L)*V(K,L)+
     +   DJ2(L)*(V(K+1,L)+V(K-1,L))
      SS=SS+BUNC(K)*AUNC(L)*(SU**2 + SV**2)
      DELSSU(K,L+1)=DELSSU(K,L+1)+BUNC(K)*AUNC(L)*SU*AJ2(L)
      DELSSU(K,L-1)=DELSSU(K,L-1)+BUNC(K)*AUNC(L)*SU*BJ2(L)
      DELSSU( K, L)=DELSSU( K, L)+BUNC(K)*AUNC(L)*SU*CJ2(L)
      DELSSU(K+1,L)=DELSSU(K+1,L)+BUNC(K)*AUNC(L)*SU*DJ2(L)
      DELSSU(K-1,L)=DELSSU(K-1,L)+BUNC(K)*AUNC(L)*SU*DJ2(L)
      DELSSV(K,L+1)=DELSSV(K,L+1)+BUNC(K)*AUNC(L)*SV*AJ2(L)
      DELSSV(K,L-1)=DELSSV(K,L-1)+BUNC(K)*AUNC(L)*SV*BJ2(L)
      DELSSV( K, L)=DELSSV( K, L)+BUNC(K)*AUNC(L)*SV*CJ2(L)
      DELSSV(K+1,L)=DELSSV(K+1,L)+BUNC(K)*AUNC(L)*SV*DJ2(L)
      DELSSV(K-1,L)=DELSSV(K-1,L)+BUNC(K)*AUNC(L)*SV*DJ2(L)
  900 CONTINUE
 1000 CONTINUE
      SWGT=AREA
      RETURN
      END
