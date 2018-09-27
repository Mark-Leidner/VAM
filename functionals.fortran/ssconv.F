      SUBROUTINE SSCONV (SS,SWGT,DELSSU,DELSSV,U,V,IDIM)
c!#   $Id: ssconv.F,v 1.1 2000/11/13 13:47:34 mcc Exp $
c!#   $Log: ssconv.F,v $
c!#   Revision 1.1  2000/11/13 13:47:34  mcc
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
C*****SSCONV CALCULATES
C*****SUMS OF SQUARES AND GRADIENT DUE TO CONVENTIONAL WIND DATA
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
#include "gparm.h"
#include "sldta.h"
cJA   IF (NPTC.LE.0) CALL ERRSEA (513,6HSSCONV)
C-----LOOP THROUGH POINTS
      DO 100 N=1,NPTC
      IF (WCONV(N).LE.0) GO TO 100
c
      call uvinterp
     c   ( idim, 1,
     c     ilc(n), jlc(n), xconv(n), yconv(n),
     i     u, v,
     o     ui5, vi5 )
c
C-----INCREMENT SUMS AND GRADIENTS
      SS=SS +WCONV(N)*((ui5-UCONV(N))**2 +(vi5-VCONV(N))**2)
      SWGT=SWGT + WCONV(N)
c
      ui = wconv(n)*(ui5-uconv(n))
      vi = wconv(n)*(vi5-vconv(n))
c
      call uvinterpad
     c   ( idim, 1,
     c     ilc(n), jlc(n), xconv(n), yconv(n),
     i     delssu, delssv,
     o     ui, vi )
c
  100 CONTINUE
      RETURN
      END
