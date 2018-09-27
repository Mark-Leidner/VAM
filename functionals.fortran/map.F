c!#   $Id: map.F,v 1.1 2000/11/13 13:47:29 mcc Exp $
c!#   $Log: map.F,v $
c!#   Revision 1.1  2000/11/13 13:47:29  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE MAP (X,U,V,IDIM)
#include "vam.h"
C***********************************************************************
C*****MAP MAPS THE VECTOR OF ACTIVE GRID VALUES INTO THE (U,V) GRID
C*****    ARRAYS
C-----SPECIFICATIONS
C-----XREFS: ..NONE
      DIMENSION X(1),U(IDIM,1),V(IDIM,1)
C-----DEFINE DOUBLE PRECISION ACCUMULATORS
      real*8 SU,SV
#include "gparm.h"
C-----
      N=0
C-----PROCESSES S.P. IF PRESENT
      IF (.NOT.LSP) GO TO 100
      N=N+1
      U(1,1)=X(N)
      V(1,1)=X(N+NGRDPT)
C-----PROCESS ACTIVE NON POLAR REGION
  100 DO 200 J=JMIN,JMAX
      DO 200 I=IMIN,IMAX
      N=N+1
      U(I,J)=X(N)
      V(I,J)=X(N+NGRDPT)
  200 CONTINUE
C-----PROCESSES N.P. IF PRESENT
      IF (.NOT.LNP) GO TO 300
      N=N+1
      U(1,NY)=X(N)
      V(1,NY)=X(N+NGRDPT)
C***********************************************************************
C*****EXTEND EXTENDS GRID ARRAY AT POLES IF PRESENT AND WRAPS GRID
C*****AROUND IF THE ACTIVE REGION INCLUDES ALL LONGITUDES.
      ENTRY EXTEND (U,V,IDIM)
  300 IF (.NOT.LSP) GO TO 400
      DO 350 I=2,NX
      U(I,1)=ALPHA(I)*U(1,1)-BETA(I)*V(1,1)
      V(I,1)=BETA(I)*U(1,1)+ALPHA(I)*V(1,1)
  350 CONTINUE
  400 IF (.NOT.LIPER) GO TO 600
      IF (.NOT.(IMIN.GT.1)) GO TO 500
      ITEMP=IMIN-1
      DO 450 J=JMIN,JMAX
      DO 450 I=1,ITEMP
      U(I,J)=U(I+IPER,J)
      V(I,J)=V(I+IPER,J)
  450 CONTINUE
  500 IF (.NOT.(IMAX.LT.NX)) GO TO 600
      ITEMP=IMAX+1
      DO 550 J=JMIN,JMAX
      DO 550 I=ITEMP,NX
      U(I,J)=U(I-IPER,J)
      V(I,J)=V(I-IPER,J)
  550 CONTINUE
  600 IF (.NOT.LNP) GO TO 700
      DO 650 I=2,NX
      U(I,NY)=ALPHA(I)*U(1,NY)+BETA(I)*V(1,NY)
      V(I,NY)=-BETA(I)*U(1,NY)+ALPHA(I)*V(1,NY)
  650 CONTINUE
  700 RETURN
C***********************************************************************
C*****CHAIN APPLIES CHAIN RULE TO GRADIENT OF S WITH RESPECT TO ACTIVE
C*****GRID POINTS.  (U,V) ARE ALTERED BUT IF EXTENDED RESULT IN A MAP
C*****OF X. NOTE ON INPUT (U,V) CONTAINS GRADIENT OF S WITH RESPECT
C*****TO ALL GRID POINTS.
      ENTRY CHAIN (X,U,V,IDIM)
C-----PROCESSES S.P. IF PRESENT
      IF (.NOT.LSP) GO TO 1100
C-----TAKE ADVANTAGE OF FACT THAT ALPHA(I)=1 AND BETA(I)=0 WHEN I=1
      SU=U(1,1)
      SV=V(1,1)
      DO 1050 I=2,NX
      SU=SU+ALPHA(I)*U(I,1)+BETA(I)*V(I,1)
      SV=SV-BETA(I)*U(I,1)+ALPHA(I)*V(I,1)
 1050 CONTINUE
      U(1,1)=SU
      V(1,1)=SV
C-----PROCESSES ACTIVE NON POLAR REGION
 1100 IF (.NOT.LIPER) GO TO 1300
      IF (.NOT.(IMIN.GT.1)) GO TO 1200
      ITEMP=IMIN-1
      DO 1150 J=JMIN,JMAX
      DO 1150 I=1,ITEMP
      U(I+IPER,J)=U(I+IPER,J)+U(I,J)
      V(I+IPER,J)=V(I+IPER,J)+V(I,J)
 1150 CONTINUE
 1200 IF (.NOT.(IMAX.LT.NX)) GO TO 1300
      ITEMP=IMAX+1
      DO 1250 J=JMIN,JMAX
      DO 1250 I=ITEMP,NX
      U(I-IPER,J)=U(I-IPER,J)+U(I,J)
      V(I-IPER,J)=V(I-IPER,J)+V(I,J)
 1250 CONTINUE
C-----PROCESSES N.P. IF PRESENT
 1300 IF (.NOT.LNP) GO TO 1400
      SU=U(1,NY)
      SV=V(1,NY)
      DO 1350 I=2,NX
      SU=SU+ALPHA(I)*U(I,NY)-BETA(I)*V(I,NY)
      SV=SV+BETA(I)*U(I,NY)+ALPHA(I)*V(I,NY)
 1350 CONTINUE
      U(1,NY)=SU
      V(1,NY)=SV
C***********************************************************************
C*****IMAP REVERSES ACTION OF MAP, MAPPING THE ACTIVE GRID POINTS IN
C*****THE ARRAY ONTO THE VECTOR X.
      ENTRY IMAP(X,U,V,IDIM)
 1400 N=0
      IF (.NOT.LSP) GO TO 1500
      N=N+1
      X(N)=U(1,1)
      X(N+NGRDPT)=V(1,1)
 1500 DO 1600 J=JMIN,JMAX
      DO 1600 I=IMIN,IMAX
      N=N+1
      X(N)=U(I,J)
      X(N+NGRDPT)=V(I,J)
 1600 CONTINUE
      IF (.NOT.LNP) GO TO 1700
      N=N+1
      X(N)=U(1,NY)
      X(N+NGRDPT)=V(1,NY)
 1700 RETURN
      END
