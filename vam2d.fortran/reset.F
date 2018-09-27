c!#   $Id: reset.F,v 1.5 1997/06/23 17:47:31 leidner Exp $
c!#   $Log: reset.F,v $
c!#   Revision 1.5  1997/06/23 17:47:31  leidner
c!#   changed ns_kp to 3-parameter kp
c!#
c!#	Revision 1.4  1997/04/17  13:24:35  leidner
c!#	bug fix for NSCAT data weights
c!#
c!#	Revision 1.3  1997/02/21  21:58:24  leidner
c!#	added reset code for nscat data
c!#
c!#	Revision 1.2  1997/02/12  18:26:09  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE RESET
#include "vam.h"
C***********************************************************************
C*****RESET RESETS WEIGHTS AND ALIAS COUNTERS OF SASS AND
C*****CONVENTIONAL DATA WHICH MAY HAVE BEEN ALTERED BY CALLS
C*****TO RESID, RESIDC, COMPAR, FIXAL
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
C-----FORT : ...ABS IBCOM# ...... ...... ...... ...... ...... ......
#include "gparm.h"
#include "ptdta.h"
#include "sldta.h"
#include "er_bscat.h"
#include "ns_bscat.h"
c
C-----DEFINE NUMERICAL CONSTANTS
      DATA FILL/-777D20/
C     DOUBLE PRECISION ABS
C     ABS(DUMMY)=DABS(DUMMY)
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HRESET )
      IF (NPTS.LE.0) GO TO 500
C-----RESET SASS POINTS
      DO 400 N=1,NPTS
      WGT(N)=ABS(WGT(N))
      IF (NWINDS(N).EQ.0) GO TO 400
C-----FOR NON NADIR LOCATIONS COUNT ALIASES
      DO 300 IA=2,4
      IF (UOBS(IA,N).EQ.FILL) GO TO 350
  300 CONTINUE
      IA=5
  350 NWINDS(N)=IA-1
  400 CONTINUE
C-----
      WRITE (6,101)
  101 FORMAT ('0WEIGTS AND ALIAS COUNTS OF SASS DATA ARE RESET.')
  500 IF (NPTC.LE.0) GO TO 900
C-----RESET CONVENTIONAL POINTS
      DO 800 N=1,NPTC
      WCONV(N)=ABS(WCONV(N))
  800 CONTINUE

      WRITE (6,102)
  102 FORMAT ('0WEIGTS OF CONVENTIONAL DATA ARE RESET.')

  900 if (er_nobs .le. 0) go to 1000
c     
c     Reset ERS1 weights.
c     ===================
c
      do j = 1,ER_MBEAM
         do i = 1,er_nobs
            er_kp(i,j) = abs(er_kp(i,j))
         end do
      end do
C-----
      WRITE (6,103)
  103 FORMAT ('0WEIGTS OF ERS1 DATA ARE RESET.')

 1000 if (ns_nobs .le. 0) go to 1500
c     
c     Reset NSCAT weights.
c     ====================
c
         do i = 1,ns_nobs
            ns_kpa(i) = abs(ns_kpa(i))
            ns_kpb(i) = abs(ns_kpb(i))
            ns_kpc(i) = abs(ns_kpc(i))
         end do
C-----
      WRITE (6,104)
  104 FORMAT ('0WEIGTS OF NSCAT DATA ARE RESET.')
C-----
 1500 RETURN
      END
