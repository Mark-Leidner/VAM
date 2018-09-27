      SUBROUTINE SSVORT  (SS,SWGT,DELSSU,DELSSV,U,V,IDIM)
c!#   $Id: ssvort.F,v 1.1 2000/11/13 13:48:03 mcc Exp $
c!#   $Log: ssvort.F,v $
c!#   Revision 1.1  2000/11/13 13:48:03  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.3  1998/04/27 16:14:44  stanr
c!#   fix errsea call
c!#
c!#   Revision 1.2  1997/02/12 20:19:52  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  14:15:09  leidner
c!#	Initial revision
c!#
#include "vam.h"
C***********************************************************************
C*****SSVORT CALCULATES
C*****SUM OF SQUARES AND GRADIENT DUE TO VORTICITY
C-----CALCULATION IS THE SAME AS FOR DIVERGENCE EXCEPT THAT V REPLACES
C-----U AND -U REPLACES V
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......
      real*8 SS
      DIMENSION DELSSU(IDIM,1),DELSSV(IDIM,1),U(IDIM,1),V(IDIM,1)
#include "gparm.h"
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HSSVORT)
      LMAX1=LMAX-1
      KMAX1=KMAX-1
      DO 1000 L=LMIN,LMAX1
      DO 1000 K=KMIN,KMAX1
      S=ADIV(L)*(V(K+1,L+1)+V(K+1,L)-V(K,L+1)-V(K,L)) -
     +   BDIV(L)*(U(K+1,L+1)+U(K,L+1)) + CDIV(L)*(U(K+1,L)+U(K,L))
C-----INCREMENT SUMS AND GRADIENTS
      SS=SS + AC(L)*S**2
      DELSSU(K,L)=DELSSU(K,L) + AC(L)*S*CDIV(L)
      DELSSU(K+1,L)=DELSSU(K+1,L) + AC(L)*S*CDIV(L)
      DELSSU(K,L+1)=DELSSU(K,L+1) - AC(L)*S*BDIV(L)
      DELSSU(K+1,L+1)=DELSSU(K+1,L+1) - AC(L)*S*BDIV(L)
      DELSSV(K,L)=DELSSV(K,L) - AC(L)*S*ADIV(L)
      DELSSV(K+1,L)=DELSSV(K+1,L) + AC(L)*S*ADIV(L)
      DELSSV(K,L+1)=DELSSV(K,L+1) - AC(L)*S*ADIV(L)
      DELSSV(K+1,L+1)=DELSSV(K+1,L+1) + AC(L)*S*ADIV(L)
 1000 CONTINUE
      SWGT=AREA
      RETURN
      END
