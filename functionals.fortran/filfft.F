c!#   $Id: filfft.F,v 1.1 2000/11/13 13:47:26 mcc Exp $
c!#   $Log: filfft.F,v $
c!#   Revision 1.1  2000/11/13 13:47:26  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.2  1997/02/12 20:31:36  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE FILFFT (U,D,IDIM)
#include "vam.h"
C***********************************************************************
C*****FILFFT FFT FILTERS AROUND LATITUDE CIRCLES NEAR THE POLES
C-----SINGLE PRECISION ONLY
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
C-----NCAR : .RFFTB .RFFTF .RFFTI ...... ...... ...... ...... ......
C-----FORT : IBCOM# ...... ...... ...... ...... ...... ...... ......
      REAL U(IDIM,1),D(1)

#include "gparm.h"

      INTEGER IFIRST
      data IFIRST/1/
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HFILTER)
      IMIN1=IMIN-1
      IMAX1=IMAX+1
C-----INITIALIZE WORK AREA FOR FFT
      CALL RFFTI (NI,D)
      DO 1000 J=JMIN,JMAX
C-----CALCULATE FIRST TRUNCATED WAVE NUMBER
      MT=NI*BZ(J)*DELX/DELY + 1
      ISTART=IMIN-1+2*MT
      IF (ISTART.GT.IMAX) GO TO 1000
      IF (IFIRST.EQ.1) WRITE (6,101) J,MT,ISTART
  101 FORMAT (' FOURIER FILTER IN USE AT LATITUDE ',I5,
     +   '  TRUNCATION STARTS AT WAVE NUMBER',I5,'  ISTART IS ',I5)
C-----FFT FORWARDS
      CALL RFFTF(NI,U(IMIN,J),D)
C-----FILTER
      DO 500 I=ISTART,IMAX
      U(I,J)=0
  500 CONTINUE
C-----FFT BACKWARDS
      CALL RFFTB(NI,U(IMIN,J),D)
C-----NORMALIZE
      DO 600 I=IMIN,IMAX
  600 U(I,J)=U(I,J)/NI
C-----EXTEND IF NECESSARY
      IF (.NOT.LIPER) GO TO 1000
      DO 700 I=1,IMIN1
  700 U(I,J)=U(I+IPER,J)
      DO 800 I=IMAX1,NX
  800 U(I,J)=U(I-IPER,J)
 1000 CONTINUE
      IFIRST=0
      RETURN
      END
