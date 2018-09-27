c!#   $Id: residc.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: residc.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE RESIDC
#include "vam.h"
C***********************************************************************
C*****RESIDC CALCULATES RESIDUALS AND PRINTS OUTLIERS
C*****FOR CONVENTIONAL DATA
C
C  10/21/91 - modified to not qc atlas buoys. J. Ardizzone
c
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : .BILIN ERRSEA ...... ...... ...... ...... ...... ......
C-----FORT : ...ABS ...MOD ..SQRT FRDNL# FWRNL# IBCOM# ...... ......
#include "gparm.h"
#include "grdta.h"
#include "sldta.h"
C-----DEFINE NUMERICAL CONSTANTS
      DATA FILL/-777D20/
      LOGICAL L1,L2,L3,L4
C-----
      DIMENSION GAMMA(3)
      DATA GAMMA/1D0,1D0,5D-1/,VCALM/1D0/
      DATA MCOUNT,NCOUNT/20,0/,IZERO/1/,MLINES/100/,LINCNT/80/
      REAL LAT,LONG
      integer itag(MAXTAG)
      NAMELIST /input/ ICOUNT,MCOUNT,NCOUNT,GAMMA,VCALM,
     +  I0,I1,I2,I3,I4,ILINES,MLINES,IZERO,LINCNT,itag
C     DOUBLE PRECISION ABS
C     ABS(DUMMY)=DABS(DUMMY)
C     DOUBLE PRECISION SQRT
C     SQRT(DUMMY)=sqrt(DUMMY)
      ILINES=0
      I0=0
      I1=0
      I2=0
      I3=0
      I4=0
      ICOUNT=0
      call isetcon(MAXTAG,itag,ALLTYPE)
      WRITE (6,101)
      WRITE (6,input)
      READ (IUNML,input,END=1520,ERR=1520)
      WRITE (6,input)
  101 FORMAT ('0RESIDUALS FOR CONVENTIONAL DATA.')
  102 FORMAT ('0',33('====')/'  1 2 3 4    N TYPE I   J     X     Y',
     +   '     LAT    LONG       W       U       V    VELP      UI',
     +   '      VI    VELI    VAVG   DELV2  COS(A)'/' ',33('----'))
cJA   IF (NPTC.LE.0) CALL ERRSEA (513,6HRESIDC)

      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG

C-----LOOP THROUGH POINTS
      DO 300 N=1,NPTC

      if (ntag.ne.0 .and. isrch(ntag,itag,iconv(n)).eq.0) go to 300

      ICOUNT=ICOUNT+1
      L4=(ICOUNT.LE.NCOUNT).AND.(MOD(ICOUNT,MCOUNT).EQ.0)
C-----INTERPOLATE GRID TO CURRENT LOCATION
      I=ILC(N)
      J=JLC(N)
      UI=BILIN (XCONV(N),YCONV(N),U(I,J),IDIM)
      VI=BILIN (XCONV(N),YCONV(N),V(I,J),IDIM)
      DELV2=(UI-UCONV(N))**2 + (VI-VCONV(N))**2
      VELI=SQRT(UI**2 + VI**2)
      VELP=SQRT(UCONV(N)**2 + VCONV(N)**2)
      VAVG=(VELI+VELP)/2
C-----CAN TESTS BE DONE
      IF ((VELI.GT.VCALM).AND.(VELP.GT.VCALM)) GO TO 150
      L1=.FALSE.
      L3=.FALSE.
      COSANG=FILL
      IF ((VELI.GT.VCALM).OR.(VELP.GT.VCALM)) GO TO 160
      L2=.FALSE.
      GO TO 180
  150 L1=DELV2.GT.GAMMA(1)**2*VAVG**2
      COSANG=(UCONV(N)*UI+VCONV(N)*VI)/(VELP*VELI)
      L3=COSANG.LT.GAMMA(3)
  160 L2=ABS(VELP-VELI).GT.GAMMA(2)*VAVG
      IF (L1) I1=I1+1
      IF (L2) I2=I2+1
      IF (L3) I3=I3+1
  180 CONTINUE
      IF (L4) I4=I4+1
      IF (ILINES .GE. MLINES) GO TO 250
C-----OUTPUT
      IF (.NOT. (L1.OR.L2.OR.L3.OR.L4) ) GO TO 300
      IF (MOD(ILINES,LINCNT).EQ.0) WRITE (6,102)
      LAT=YS+(J-1+YCONV(N))*DELY
      LONG=XS+(I-1+XCONV(N))*DELX
      WRITE (6,201) L1,L2,L3,L4,N,ICONV(N),I,J,XCONV(N),YCONV(N),LAT,
     +   LONG,WCONV(N),UCONV(N),VCONV(N),VELP,UI,VI,VELI,VAVG,DELV2,
     +   COSANG
  201 FORMAT (' ',4L2,I5,I3,2I4,2F6.3,10F8.3,F8.0,F8.3)
      ILINES=ILINES+1
C-----SET NEGATIVE WEIGHTS FOR OUTLIERS IF IZERO.EQ.1
  250 IF (.NOT.(L1.OR.L2.OR.L3)) GO TO 300
      I0=I0+1
      IF (IZERO.EQ.1 .AND. WCONV(N).GT.0) WCONV(N)=-WCONV(N)
      sl_qc(n) = ior(sl_qc(n),2**ipass)
  300 CONTINUE
      WRITE (6,input)
      RETURN
 1520 CALL ERRSEA (520,6HRESIDC)
      RETURN
      END
