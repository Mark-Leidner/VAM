c!#   $Id: resid.F,v 1.3 1998/05/20 14:29:57 leidner Exp $
c!#   $Log: resid.F,v $
c!#   Revision 1.3  1998/05/20 14:29:57  leidner
c!#   added dualqc option
c!#   controlled by namelist parameter dualqc (default is FALSE)
c!#
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE RESID
#include "vam.h"
C***********************************************************************
C*****RESID CALCULATES RESIDUALS AND PRINTS OUTLIERS FOR SASS WINDS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : .BILIN ERRSEA ...... ...... ...... ...... ...... ......
C-----FORT : ...ABS ...MOD ..SQRT FRDNL# FWRNL# IBCOM# ...... ......
#include "gparm.h"
#include "grdta.h"
#include "ptdta.h"
C-----DEFINE NUMERICAL CONSTANTS
      DATA FILL/-777D20/
      LOGICAL L1,L2,L3,L4
      logical dualqc
C-----
      DIMENSION GAMMA(3)
      DATA GAMMA/1D0,1D0,5D-1/,VCALM/1D0/
      DATA MCOUNT,NCOUNT/20,0/,IZERO/1/,MLINES/100/,LINCNT/80/
      REAL LAT,LONG
      integer itag(MAXTAG)
      NAMELIST /input/ ICOUNT,MCOUNT,NCOUNT,GAMMA,VCALM,
     +  I0,I1,I2,I3,I4,ILINES,MLINES,IZERO,LINCNT,itag
     +  ,dualqc

      pi = 4.*atan(1.D0)
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
      dualqc = .false.
      ndualqc = 0
      ndual = 0
      call isetcon(MAXTAG,itag,ALLTYPE)
      WRITE (6,101)
      WRITE (6,input)
      READ (IUNML,input,END=1520,ERR=1520)
      WRITE (6,input)
  101 FORMAT ('0RESIDUALS FOR SASS WINDS.')
  102 FORMAT ('0',33('====')/'  1 2 3 4    N NA   I   J     X     Y',
     +   '     LAT    LONG       W       U       V    VELP      UI',
     +   '      VI    VELI    VAVG   DELV2  COS(A)'/' ',33('----'))
cJA   IF (NPTS.LE.0) CALL ERRSEA (513,6HRESID )

      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG

C-----LOOP THROUGH POINTS
      DO 300 N=1,NPTS

      if (ntag.ne.0 .and. isrch(ntag,itag,iobs(n)).eq.0) go to 300

      if (dualqc) goto 290

      ICOUNT=ICOUNT+1
      L4=(ICOUNT.LE.NCOUNT).AND.(MOD(ICOUNT,MCOUNT).EQ.0)
C-----INTERPOLATE GRID TO CURRENT LOCATION
      I=ILL(N)
      J=JLL(N)
      UI=BILIN (XCOORD(N),YCOORD(N),U(I,J),IDIM)
      VI=BILIN (XCOORD(N),YCOORD(N),V(I,J),IDIM)
C-----
      NA=NWINDS(N)
      DELV2=(UI-UOBS(1,N))**2 + (VI-VOBS(1,N))**2
      M=1
      IF (NA.LE.1) GO TO 75
C-----DETERMINE CLOSEST ALIAS
      DO 50 IA=2,NA
      D=(UI-UOBS(IA,N))**2 + (VI-VOBS(IA,N))**2
      IF (D.GT.DELV2) GO TO 50
      DELV2=D
      M=IA
   50 CONTINUE
C-----
   75 CONTINUE
      VELI=SQRT(UI**2 + VI**2)
      VELP=SQRT(UOBS(M,N)**2 + VOBS(M,N)**2)
      VAVG=(VELI+VELP)/2
C-----CAN TESTS 1 AND 3 BE DONE
      IF ((VELI.GT.VCALM).AND.(VELP.GT.VCALM).AND.(NA.GT.0))
     +   GO TO 150
      L1=.FALSE.
      IF (NA.EQ.0) DELV2=FILL
      L3=.FALSE.
      COSANG=FILL
C-----CAN TEST 2 BE DONE
      IF ((VELI.GT.VCALM).OR.(VELP.GT.VCALM)) GO TO 160
      L2=.FALSE.
      GO TO 180
  150 L1=DELV2.GT.GAMMA(1)**2*VAVG**2
      COSANG=(UOBS(M,N)*UI+VOBS(M,N)*VI)/(VELP*VELI)
      L3=COSANG.LT.GAMMA(3)
  160 L2=ABS(VELP-VELI).GT.GAMMA(2)*VAVG
  180 CONTINUE
      IF (L1) I1=I1+1
      IF (L2) I2=I2+1
      IF (L3) I3=I3+1
      IF (L4) I4=I4+1
      IF (ILINES .GE. MLINES) GO TO 250
C-----OUTPUT
      IF (.NOT. (L1.OR.L2.OR.L3.OR.L4) ) GO TO 300
      IF (MOD(ILINES,LINCNT).EQ.0) WRITE (6,102)
      LAT=YS+(J-1+YCOORD(N))*DELY
      LONG=XS+(I-1+XCOORD(N))*DELX
      WRITE (6,201) L1,L2,L3,L4,N,NA,I,J,XCOORD(N),YCOORD(N),LAT,LONG,
     +   WGT(N),UOBS(M,N),VOBS(M,N),VELP,UI,VI,VELI,VAVG,DELV2,COSANG
  201 FORMAT (' ',4L2,I5,I3,2I4,2F6.3,10F8.3,F8.0,F8.3)
      ILINES=ILINES+1
C-----NEGATIVE WEIGHTS FOR OUTLIERS IF IZERO.EQ.1
  250 IF (.NOT.(L1.OR.L2.OR.L3)) GO TO 300
      I0=I0+1
      IF (IZERO.EQ.1 .AND. WGT(N).GT.0) WGT(N)=-WGT(N)
      pt_qc(n) = ior(pt_qc(n),2**ipass)

      goto 300

c!#   Dual ambiguity QC
 290  continue
      if ( nwinds(n) .ne. 2 ) goto 300 ! if there are not 2 ambigs, skip
      ndual = ndual + 1
      call winds( uobs(1,n), vobs(1,n), speed1, dir1, .false. )
      call winds( uobs(2,n), vobs(2,n), speed2, dir2, .false. )
      del = pi - abs( pi - abs( dir1 - dir2 ) )
      if ( del .lt. (3.*pi/4.) ) then
         wgt(n)=-wgt(n)
         ndualqc = ndualqc + 1
      endif

  300 CONTINUE
      write(6,input)

      if (dualqc) write (6,301) ndual, ndualqc,
     &     (float(ndualqc)/float(ndual))*100.
 301  FORMAT(/'Of ',i6,' dual ambiguity WVCs, ',i6,'(',f6.2,
     &     ' %) failed dual QC.'/)

      RETURN
 1520 CALL ERRSEA (520,6HRESID )
      RETURN
      END
