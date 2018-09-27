      SUBROUTINE SSVELM (SS,SWGT,DELSSU,DELSSV,U,V,IDIM)
c!#   $Id: ssvelm.F,v 1.1 2000/11/13 13:48:01 mcc Exp $
c!#   $Log: ssvelm.F,v $
c!#   Revision 1.1  2000/11/13 13:48:01  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.4  1998/02/11 20:12:45  leidner
c!#   added check to skip calculation if there are no ambiguities
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
C*****SSVELM CALCULATES SUMS OF SQUARES AND GRADIENT
C*****DUE TO VELOCITY MAGNITUDE OF SASS WINDS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
C-----FORT : ..SQRT ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
#include "ptdta.h"
#include "gparm.h"
C-----USE DOUBLE PRECISION ACCUMULATOR FOR SWGT
      real*8 DSWGT
C     DOUBLE PRECISION SQRT
C     SQRT(DUMMY)=sqrt(DUMMY)
      DSWGT=0
cJA   IF (NPTS.LE.0) CALL ERRSEA (513,6HSSVELM)
C-----LOOP THROUGH POINTS
      DO 100 N=1,NPTS
      IF (WGT(N).LE.0) GO TO 100
      IF (NWINDS(N).EQ.0) GO TO 100
c
      call uvinterp
     c   ( idim, 1,
     c     ill(n), jll(n), xcoord(n), ycoord(n),
     i     u, v,
     o     ui5, vi5 )
c
      VELI=SQRT(ui5**2 + vi5**2)
C-----INCREMENT SUMS AND GRADIENTS
      H=WGT(N)/(1+VBAR(N)*RA)
      DSWGT=DSWGT + H
      SS=SS + H*(VELI-VBAR(N))**2
C-----IF (VELI.EQ.0) GRADIENT IS SET TO ZERO
      IF (VELI.EQ.0) GO TO 95
      H=H*(VELI-VBAR(N))
c
      ui = h*(ui5/veli)
      vi = h*(vi5/veli)
c
      call uvinterpad
     c   ( idim, 1,
     c     ill(n), jll(n), xcoord(n), ycoord(n),
     i     delssu, delssv,
     o     ui, vi )
c
   95 CONTINUE
  100 CONTINUE
      SWGT=DSWGT
      RETURN
      END
