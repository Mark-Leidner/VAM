      SUBROUTINE SSDTZ (SS,SWGT,DELSSU,DELSSV,U,V,C,VEL,DTZ0,IDIM,INIT)
c!#   $Id: ssdtz.F,v 1.1 2000/11/13 13:47:40 mcc Exp $
c!#   $Log: ssdtz.F,v $
c!#   Revision 1.1  2000/11/13 13:47:40  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.4  1998/04/02 19:41:52  stanr
c!#   change type of SS to real*8, matching sscgr
c!#
c!#   Revision 1.3  1997/07/08 16:52:24  leidner
c!#   Added new vorticity tendency constraint, controlled
c!#   by init_dtz0 in /gparm/.
c!#
c!#	Revision 1.2  1997/02/12  20:19:52  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  14:15:09  leidner
c!#	Initial revision
c!#
#include "vam.h"
C***********************************************************************
C*****SSDTZ  CALCULATES SUMS OF SQUARES
C*****AND GRADIENT DUE TO TIME RATE OF CHANGE OF VORTICITY
C-----U AND V PASSED TO THIS ROUTINE MUST NOT BE DELTAS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
C-----FORT : ..SQRT ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
      DIMENSION VEL(IDIM,1),C(IDIM,1),DTZ0(IDIM,1)
      INTEGER INIT
#include "gparm.h"
C-----DEFINE NUMERICAL CONSTANTS
      REAL NINETY
      data NINETY/90D0/
C-----DEFINE PHYSICAL CONSTANT: ROTATION RATE OF EARTH (RADIANS/SEC)
      DATA OMEGA/7.292D-5/
C-----DEFINE CONVERSION FACTOR: DEGREES TO RADIANS
      DATA FACTOR/1.74532925199432946D-2/
C-----DEFINE PHYSICAL CONSTANT: RADIUS OF EARTH (METERS)
      DATA REARTH/6371D3/
C     DOUBLE PRECISION SQRT
C     SQRT(DUMMY)=sqrt(DUMMY)
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HSSDTZ )
C-----CHECK THAT INTEGRATION AREA IS INTERIOR TO GRID EXCEPT AT POLES
      IF(KMIN.EQ.1 .OR. KMAX.EQ.NX) CALL ERRSEA (514,6HSSDTZ )
      IF ((LMIN.EQ.1  .AND. YS.NE.-NINETY) .OR.
     +    (LMAX.EQ.NY .AND. YF.NE.+NINETY)) CALL ERRSEA (514,6HSSDTZ )
C-----IF POLES PRESENT ALL LONGITUDES MUST BE PRESENT
      IF ( ((LMIN.EQ.1 .AND. YS.EQ.-NINETY)  .OR.
     +      (LMAX.EQ.NY.AND. YF.EQ.+NINETY)) .AND.
     +     (IPER.GT.NX) ) CALL ERRSEA (514,6HSSDTZ )
C-----GLOBAL TEMPORARIES
      RY=1/(2*REARTH*FACTOR*DELY)
      RYY=(2*RY)**2
C-----PRECALCULATE VEL AND C/VEL
      DO 100 J=1,NY
      DO 100 I=1,NX
      VEL(I,J)=SQRT(U(I,J)**2 + V(I,J)**2)
      IF (VEL(I,J).GT.0) C(I,J)=C(I,J)/VEL(I,J)
  100 CONTINUE
C-----IF POLES ARE PRESENT USE R.M.S. OF VALUES ONE LATITUDE AWAY.
C-----TO DO THIS FIX VALUES OF AUNC
      IF ((LMIN.EQ.1).AND.(YS.EQ.-NINETY))
     +   AUNC(2)=AUNC(2)+APOLE/(KMAX-KMIN)
      IF ((LMAX.EQ.NX).AND.(YS.EQ. NINETY))
     +   AUNC(NX-1)=AUNC(NX-1)+APOLE/(KMAX-KMIN)
      DO 1000 L=LMIN,LMAX
C-----CHECK FOR POLE
      IF ( .NOT. ((L.EQ.1 .AND. YS.EQ.-NINETY)  .OR.
     +      (L.EQ.NY.AND. YF.EQ.+NINETY)) ) GO TO 800
C=====CALL ERRSEA (529,6HSSDTZ )
      GO TO 1000
C-----NON POLAR L
  800 CONTINUE
C-----LATITUDE DEPENDENT TEMPORARIES
      F=2*OMEGA*AZ(L)
      T=AZ(L)/(REARTH*BZ(L))
      BETAP=2*OMEGA*BZ(L)/REARTH - F*T
      RX=1/(2*REARTH*FACTOR*DELX*BZ(L))
      RXX=(2*RX)**2
      RXY=RX*RY
      DO 900 K=KMIN,KMAX
C-----POINT DEPENDENT TEMPORARIES
      U0=U(K,L)
      V0=V(K,L)
      UX=RX*(U(K+1,L)-U(K-1,L))
      VX=RX*(V(K+1,L)-V(K-1,L))
      UY=RY*(U(K,L+1)-U(K,L-1))
      VY=RY*(V(K,L+1)-V(K,L-1))
      VXX=RXX*(V(K+1,L)-2*V(K,L)+V(K-1,L))
      VXY=RXY*(V(K+1,L+1)-V(K+1,L-1)-V(K-1,L+1)+V(K-1,L-1))
      UXY=RXY*(U(K+1,L+1)-U(K+1,L-1)-U(K-1,L+1)+U(K-1,L-1))
      UYY=RYY*(U(K,L+1)-2*U(K,L)+U(K,L-1))
C-----
      T1=UX+VY
      T2=VX-UY
      T3=VXX-UXY
      T4=VXY-UYY
      T5=2*T*V0-(UX+VY)
      T6=2*T*U0+F+(VX-UY)
      T7=  T*U0+F+(VX-UY)
C-----
      DTZ = T2*T1 + U0*V0/(REARTH**2) + T*(U0*(2*UX + VY) + 2*V0*UY)
     +      + U0*T3 + V0*T4 + F*T1 + BETAP*V0
     F      - RX*(C(K+1,L)*VEL(K+1,L)**2*V(K+1,L)
     F          - C(K-1,L)*VEL(K-1,L)**2*V(K-1,L) )
     F      + RY*(C(K,L+1)*VEL(K,L+1)**2*U(K,L+1)
     F          - C(K,L-1)*VEL(K,L-1)**2*U(K,L-1) )
     F      - T*C(K,L)*VEL(K,L)**2*U0
C-----CONTINUATIONS WITH F ARE FOR FRICTIONAL EFFECTS
      IF (INIT.EQ.1) THEN
        DTZ0(K,L) = DTZ
      ELSEIF (INIT.EQ.0) THEN
        DTZ0(K,L) = 0.
      ENDIF
      T8=BUNC(K)*AUNC(L)*(DTZ - DTZ0(K,L))
      SS=SS+T8*(DTZ - DTZ0(K,L))
      DELSSU(K  ,L  )=DELSSU(K  ,L  )+T8*
     +                ( (1/(REARTH**2)+2*RYY)*V0 + T*(2*UX + VY) + T3
     F                - T*C(K,L)*(VEL(K,L)**2+U0**2) )
      DELSSU(K+1,L  )=DELSSU(K+1,L  )+T8*( RX*T6
     F                - RX*C(K+1,L)*U(K+1,L)*V(K+1,L) )
      DELSSU(K-1,L  )=DELSSU(K-1,L  )+T8*(-RX*T6
     F                + RX*C(K-1,L)*U(K-1,L)*V(K-1,L) )
      DELSSU(K  ,L+1)=DELSSU(K  ,L+1)+T8*( RY*T5 - RYY*V0
     F                + RY*C(K,L+1)*(VEL(K,L+1)**2+U(K,L+1)**2) )
      DELSSU(K  ,L-1)=DELSSU(K  ,L-1)+T8*(-RY*T5 - RYY*V0
     F                - RY*C(K,L-1)*(VEL(K,L-1)**2+U(K,L-1)**2) )
      DELSSU(K+1,L+1)=DELSSU(K+1,L+1)+T8*(-RXY*U0 )
      DELSSU(K-1,L-1)=DELSSU(K-1,L-1)+T8*(-RXY*U0 )
      DELSSU(K+1,L-1)=DELSSU(K+1,L-1)+T8*( RXY*U0 )
      DELSSU(K-1,L+1)=DELSSU(K-1,L+1)+T8*( RXY*U0 )
      DELSSV(K  ,L  )=DELSSV(K  ,L  )+T8*
     +                ( (1/(REARTH**2)-2*RXX)*U0 + 2*T*UY + BETAP + T4
     F                - T*C(K,L)*U0*V0 )
      DELSSV(K+1,L  )=DELSSV(K+1,L  )+T8*( RX*T1 + RXX*U0
     F                - RX*C(K+1,L)*(VEL(K+1,L)**2 + V(K+1,L)**2) )
      DELSSV(K-1,L  )=DELSSV(K-1,L  )+T8*(-RX*T1 + RXX*U0
     F                + RX*C(K-1,L)*(VEL(K-1,L)**2 + V(K-1,L)**2) )
      DELSSV(K  ,L+1)=DELSSV(K  ,L+1)+T8*( RY*T7
     F                + RY*C(K,L+1)*U(K,L+1)*V(K,L+1) )
      DELSSV(K  ,L-1)=DELSSV(K  ,L-1)+T8*(-RY*T7
     F                - RY*C(K,L-1)*U(K,L-1)*V(K,L-1) )
      DELSSV(K+1,L+1)=DELSSV(K+1,L+1)+T8*( RXY*V0 )
      DELSSV(K-1,L-1)=DELSSV(K-1,L-1)+T8*( RXY*V0 )
      DELSSV(K+1,L-1)=DELSSV(K+1,L-1)+T8*(-RXY*V0 )
      DELSSV(K-1,L+1)=DELSSV(K-1,L+1)+T8*(-RXY*V0 )
  900 CONTINUE
 1000 CONTINUE
      SWGT=AREA
C-----RESTORE VALUES OF AUNC
      IF ((LMIN.EQ.1).AND.(YS.EQ.-NINETY))
     +   AUNC(2)=AUNC(2)-APOLE/(KMAX-KMIN)
      IF ((LMAX.EQ.NX).AND.(YS.EQ. NINETY))
     +   AUNC(NX-1)=AUNC(NX-1)-APOLE/(KMAX-KMIN)
C-----FIX C
      DO 1100 J=1,NY
      DO 1100 I=1,NX
      IF (VEL(I,J).GT.0) C(I,J)=C(I,J)*VEL(I,J)
 1100 CONTINUE
      RETURN
      END
