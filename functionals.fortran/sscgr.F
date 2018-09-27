c!#   $Id: sscgr.F,v 1.1 2000/11/13 13:47:32 mcc Exp $
c!#   $Log: sscgr.F,v $
c!#   Revision 1.1  2000/11/13 13:47:32  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.7  1997/11/03 18:49:12  leidner
c!#   added fft filter of small-scale noise in gradient (removed in r1.4)
c!#
c!#   Revision 1.6  1997/07/25 17:14:19  leidner
c!#   Added penalty and departure terms to ss calc for scatterometer backscatter
c!#
c!#	Revision 1.5  1997/07/08  16:52:24  leidner
c!#	Added new vorticity tendency constraint, controlled
c!#	by init_dtz0 in /gparm/.
c!#
c!#	Revision 1.4  1997/04/17  18:44:35  rnh
c!#	Added ssmi los wind cost function
c!#
c!#	Revision 1.3  1997/02/24  19:20:07  leidner
c!#	added call to ssnscat
c!#
c!#	Revision 1.2  1997/02/12  20:19:52  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  14:15:09  leidner
c!#	Initial revision
c!#
        SUBROUTINE SSCGR (NDIM,X,SOS,DELSS)
#include "vam.h"
C***********************************************************************
C*****SSCGR PROVIDES LINKAGE TO IMSL ROUTINES
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ...MAP ..IMAP .CHAIN .SSDIV .SSDTZ EXTEND FILFFT PLGRAD
C-----       PLWING SSCONV SSFILT SSFORE SSSASS SSVELM SSVORT ......
C-----FORT : .sqrt IBCOM# ...... ...... ...... ...... ...... ......
      DIMENSION X(NDIM), DELSS(NDIM)
#include "gparm.h"
#include "grdta.h"
#include "weight.h"
#include "ssmidata.h"
      real*8 SS
      real*8 DDSS,DDSOS,DDSOS_nopenalty
      real*8 Jvelm, Jlosm, ns_Jdepart, ns_Jpsd, er_Jdepart, er_Jpsd
      dimension TEMP(NSOS)
      INTEGER ITITLE(5,NSOS+2)
      data ITITLE / 'PREV','IOUS',' FOR','ECAS','T   ',
     +   'CONV','ENTI','ONAL',' DAT','A   ',
     +   'ALIA','SED ','WIND','S DA','TA  ',
     +   'VELO','CITY',' MAG','NITU','DE  ',
     +   'J2 F','ILTE','R OF',' DEL','TAS ',
     +   'DIVE','RGEN','CE O','F DE','LTAS',
     +   'VORT','ICIT','Y OF',' DEL','TAS ',
     +   '(D/D','T) O','F VO','RTIC','ITY ',
     +   'ERS1',' DAT','A   ','    ','    ',
     +   'NSCA','T DA','TA  ','    ','    ',
     +   'SSMI',' DAT','A   ','    ','    ',
     +   'ALL ','EFFE','CTS ','    ','    ',
     +   5*'===='/
C-----SET PLOT FLAG
      IPLOT=-1
      GO TO 50
      ENTRY SSCGR1(NDIM,X,SOS,DELSS,idum)
      iplot=idum
   50 iteration=iteration+1
C*****MAPS X INTO U AND V
      CALL MAP (X,U,V,IDIM)
C*****ZEROS OUT DELSS ARRAY FOR SUBSEQUENT ADDS
      DDSOS=0
      DDSOS_nopenalty=0
      DO 100 I=1,NDIM
  100 DELSS(I)=0
C-----SWAP AND CALCULATE DELTAS
      DO 200 J=1,NY
      DO 200 I=1,NX
      T=U(I,J)-U0(I,J)
      U(I,J)=U0(I,J)
      U0(I,J)=T
      T=V(I,J)-V0(I,J)
      V(I,J)=V0(I,J)
      V0(I,J)=T
  200 CONTINUE
C-----CONTROL SMALL SCALE NOISE IN DELTAS
      CALL FILFFT (U0,VEL,IDIM)
      CALL FILFFT (V0,VEL,IDIM)
C-----CONSTRUCT FILTERED ANALYSIS
      DO 300 J=1,NY
      DO 300 I=1,NX
      U(I,J)=U(I,J)+U0(I,J)
      V(I,J)=V(I,J)+V0(I,J)
  300 CONTINUE
C-----
C*****CALCULATES SUM OF SQUARES
      DO 1000 K=1,NSOS
      IF (LAMDA(K).EQ.0) GO TO 1000
C-----INITIALIZES ACCUMULATORS
      SS=0
      NORM(K)=0
      DO 500 J=1,NY
      DO 500 I=1,NX
      DELSSU(I,J)=0
      DELSSV(I,J)=0
  500 CONTINUE
      IF (K.EQ.1) CALL SSFORE (SS,NORM(K),DELSSU,DELSSV,U0,V0,IDIM)
      IF (K.EQ.2) CALL SSCONV (SS,NORM(K),DELSSU,DELSSV, U, V,IDIM)
      IF (K.EQ.3) CALL SSSASS (SS,NORM(K),DELSSU,DELSSV, U, V,IDIM)
      IF (K.EQ.4) CALL SSVELM (SS,NORM(K),DELSSU,DELSSV, U, V,IDIM)
      IF (K.EQ.5) CALL SSFILT (SS,NORM(K),DELSSU,DELSSV,U0,V0,IDIM)
      IF (K.EQ.6) CALL SSDIV  (SS,NORM(K),DELSSU,DELSSV,U0,V0,IDIM)
      IF (K.EQ.7) CALL SSVORT (SS,NORM(K),DELSSU,DELSSV,U0,V0,IDIM)
      IF (K.EQ.8)
     +    CALL SSDTZ (SS,NORM(K),DELSSU,DELSSV,U,V,CD,VEL,DTZ0,IDIM,-1)
      IF (K.EQ.9)
     +  CALL SSERS1 ( SS, NORM(K), DELSSU, DELSSV, U, V, IDIM,
     +      er_Jdepart, er_Jpsd)
      IF (K.EQ.10)
     +  CALL SSNSCAT ( SS, NORM(K), DELSSU, DELSSV, U, V, IDIM,
     +      ns_Jdepart, ns_Jpsd)
      IF (K.EQ.11) then
        norm(k) = nssmi
        CALL SSSSMI (idim,
     i      u, v,
     o      delssu, delssv,
     o      ss, Jvelm, Jlosm )
      endif
      If (K .lt. 9) SS = SS/2
      SSQ(K)=LSCALE(K)*SS
c
c     Don't include Penalty terms in SSQ
c
      if (K .eq. 9) SSQ(K)=LSCALE(K)*er_Jdepart
      if (K .eq. 10) SSQ(K)=LSCALE(K)*ns_Jdepart
      DDSOS=DDSOS+LAMDA(K)*LSCALE(K)*SS
      DDSOS_nopenalty=DDSOS_nopenalty+LAMDA(K)*SSQ(K)
      CALL CHAIN (X,DELSSU,DELSSV,IDIM)
C-----X NOW CONTAINS THE K(TH) GRADIENT
      W=LAMDA(K)*LSCALE(K)
      DDSS=0
      DO 600 I=1,NDIM
      DELSS(I)=DELSS(I)+W*X(I)
      DDSS=DDSS+X(I)**2
  600 CONTINUE
      TEMP(K)=W*sqrt(DDSS/NDIM)
 1000 CONTINUE
      SOS=DDSOS
C-----RESTORE U0 AND V0
      DO 1200 J=1,NY
      DO 1200 I=1,NX
      U0(I,J)=U(I,J)-U0(I,J)
      V0(I,J)=V(I,J)-V0(I,J)
 1200 CONTINUE
C-----RECONSTRUCT X SINCE IT HAS BEEN USED AS A WORK SPACE
      CALL IMAP (X,U,V,IDIM)
C-----
C-----PLOTS OF DELSSU,DELSSV BEYOND THE ACTIVE REGION ARE LOST FOR
C-----TOTAL GRADIENT - PUT ZEROS THERE
      DO 1500 J=1,NY
      DO 1500 I=1,NX
      DELSSU(I,J)=0
      DELSSV(I,J)=0
 1500 CONTINUE
      CALL MAP (DELSS,DELSSU,DELSSV,IDIM)
C-----CONTROL SMALL SCALE NOISE IN GRADIENT
      CALL FILFFT (DELSSU,VEL,IDIM)
      CALL FILFFT (DELSSV,VEL,IDIM)
      CALL IMAP (DELSS,DELSSU,DELSSV,IDIM)
 2000 IF (IPLOT.EQ.0) RETURN
C-----CALCULATE RMS OF GRADIENT
      DDSS=0
      DO 2200 I=1,NDIM
      DDSS=DDSS+DELSS(I)**2
 2200 CONTINUE
      DDSS=sqrt(DDSS/NDIM)
      IF (IPLOT.LT.0) GO TO 2400
      WRITE (6,201) ((ITITLE(I,K),I=1,5),LAMDA(K),TEMP(K)
     +   ,SSQ(K),NORM(K),K=1,NSOS)
      if (lamda(9).gt.0 .and. er_Jpsd.ne.0) write (6,204) er_Jpsd
      if (lamda(10).gt.0 .and. ns_Jpsd.ne.0) write (6,205) ns_Jpsd
      if (lamda(11).gt.0) write (6,206) Jvelm, Jlosm
      WRITE (6,202) iteration,DDSS,DDSOS_nopenalty
      RETURN
 2400 WRITE (6,203) iteration,DDSS,DDSOS_nopenalty
  201 FORMAT (1X,25('====')/' SUM OF SQUARES DUE TO',
     +   T30,'LAMDA',T50,'*RMS(GRAD)',T70,'SSQ',T90,'SWGT'/
     +   1X,25('----')/(1X,5A4,T22,E18.7,3E20.7))
  202 FORMAT (1X,25('----')/' ALL EFFECTS ',T30,'CALL',I6,2E20.7/
     +   1X,25('----'))
  203 FORMAT (T30,I10,2E20.7)
  204 format (' Jpenalty (ERS1): ',t60,e20.7)
  205 format (' Jpenalty (NSCAT): ',t60,e20.7)
  206 format (' Jvelm, Jlosm (SSMI): ',t40,2e20.7)
      RETURN
      END
