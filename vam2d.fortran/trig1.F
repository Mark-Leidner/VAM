c!#   $Id: trig1.F,v 1.3 2005/08/09 19:26:08 leidner Exp $
c!#   $Log: trig1.F,v $
c!#   Revision 1.3  2005/08/09 19:26:08  leidner
c!#   Fixed minor typo which caused a compile failure on the Portland Group
c!#   compilers.
c!#
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE TRIG1(IDEBUG)
#include "vam.h"
C***********************************************************************
C*****TRIG1 CALCULATE TRIGONOMETRIC FACTORS IN GPARM COMMON AREA
C-----SPECIFICATIONS
C-----XREFS:
C-----FORT :   COS    SIN    TAN IBCOM#
#include "gparm.h"
#include "grdta.h"
C-----DEFINE NUMERICAL CONSTANTS
      DATA ONE/1D0/
C-----DEFINE NUMERICAL CONSTANTS
      REAL NINETY
      data NINETY/90D0/
C-----DEFINE CONVERSION FACTOR: DEGREES TO RADIANS
      DATA FACTOR/1.74532925199432946D-2/
C-----DEFINE PHYSICAL CONSTANT: RADIUS OF EARTH (METERS)
      DATA REARTH/6371D3/
C     DOUBLE PRECISION COS
C     COS(DUMMY)=DCOS(DUMMY)
C     DOUBLE PRECISION SIN
C     SIN(DUMMY)=DSIN(DUMMY)
C     DOUBLE PRECISION TAN
C     TAN(DUMMY)=DTAN(DUMMY)
C-----
C-----IN TERMS OF RADIANS
      Y0=FACTOR*YS
      DY=FACTOR*DELY
      DX=FACTOR*DELX
C-----CALCULATE J2 AND DTZ COEFFICIENTS
      DO 10 L=1,NY
C-----CHECK IF L IS A POLAR LATITUDE
      IF ( .NOT. ((L.EQ.1 .AND. YS.EQ.-NINETY)  .OR.
     +      (L.EQ.NY.AND. YF.EQ.+NINETY)) ) GO TO 5
      AZ(L)=0
      BZ(L)=0
      AJ2(L)=0
      BJ2(L)=0
      CJ2(L)=0
      DJ2(L)=0
      GO TO 10
    5 PHI=Y0+(L-1)*DY
      AZ(L)=SIN(PHI)
      BZ(L)=COS(PHI)
      AJ2(L)=1/(REARTH*DY)**2 - (AZ(L)/BZ(L))/(2*REARTH**2*DY)
      BJ2(L)=2/(REARTH*DY)**2 - AJ2(L)
      DJ2(L)=1/(REARTH*BZ(L)*DX)**2
      CJ2(L)=-(AJ2(L)+BJ2(L)+2*DJ2(L))
   10 CONTINUE
      EJ2=4/(IPER*(REARTH*DY)**2)
C-----CALCULATE DIVERGENCE COEFFICIENTS
      DO 20 L=1,NY
      PHI=Y0+(L-ONE/2)*DY
      ADIV(L)=1/(2*REARTH*COS(PHI)*DX)
      BDIV(L)=1/(2*REARTH*DY) - TAN(PHI)/(4*REARTH)
      CDIV(L)=1/(REARTH*DY) - BDIV(L)
   20 CONTINUE
C-----CALCULATE MAP FACTORS
      DO 30 I=1,NX
      PHI=(I-1)*DX
      ALPHA(I)=COS(PHI)
      BETA(I)=SIN(PHI)
   30 CONTINUE
C-----CALCULATE INTEGRATING FACTORS
      AREA=REARTH**2*(KMAX-KMIN)*DX*
     +   2*SIN((LMAX-LMIN)*DY/2)*COS(Y0+(LMAX+LMIN-2)*DY/2)
      T=REARTH*SIN(DY/2)
      APOLE=(KMAX-KMIN)*DX*T**2/2
      ACL1=0
      DO 40 L=1,NY
      IF (LMIN.LE.L .AND. L.LT.LMAX) AC(L)=T/ADIV(L)
      IF (.NOT.(LMIN.LE.L .AND. L.LT.LMAX)) AC(L)=0
      IF (LMIN.LE.L .AND. L.LE.LMAX) AUNC(L)=(AC(L)+ACL1)/2
      IF (.NOT.(LMIN.LE.L .AND. L.LE.LMAX)) AUNC(L)=0
      ACL1=AC(L)
   40 CONTINUE
      DO 50 K=1,NX
      IF (K.LT.KMIN .OR. K.GT.LMAX) BUNC(K)=0
      IF (K.EQ.KMIN .OR. K.EQ.KMAX) BUNC(K)=ONE/2
      IF (K.GT.KMIN .AND. K.LT.KMAX) BUNC(K)=1
   50 CONTINUE
C-----DEBUG OUTPUT
      IF (IDEBUG.EQ.0) RETURN
      WRITE (6,101) REARTH,AREA,APOLE,EJ2,
     +   (L,AC(L),AUNC(L),AJ2(L),BJ2(L),CJ2(L),DJ2(L),ADIV(L),BDIV(L),
     +    CDIV(L),L=1,NY)
  101 FORMAT (//'0GEOMETRIC CONSTANTS'/
     +   '0RADIUS OF THE EARTH',T49,E14.6/
     +   '0TOTAL AREA',T49,E14.6/
     +   '0POLAR CAP AREA/4',T49,E14.6/
     +   '0J2 E COEFFICIENT',T49,E14.6///
     +   '0        INTEGRATING FACTORS',T38,'J2 COEFFICIENTS',T94,
     +   'DIVERGENCE COEFFICIENTS'/
     +   '0   L    A(CENTERED)   A(UNCENTERED)',T38,
     +   'A',13X,'B',13X,'C',13X,'D',13X,'A',13X,'B',13X,'C'//
     +   (1X,I4,1X,9E14.6))
      WRITE (6,102) (I,ALPHA(I),BETA(I),BUNC(I),I=1,NX)
  102 FORMAT (//'0   I    ALPHA',9X,'BETA',10X,'B(UNCENTERED)'//
     +   (1X,I4,1X,3E14.6))
      WRITE (6,103) (L,AZ(L),BZ(L),L=1,NY)
  103 FORMAT (//'0   L   SIN(L)',7X,'COS(L)'//
     +   (1X,I4,1X,2E14.6))
      RETURN
      END
