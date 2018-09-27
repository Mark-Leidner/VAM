c!#   $Id: solve.F,v 1.4 1997/12/18 22:21:43 leidner Exp $
c!#   $Log: solve.F,v $
c!#   Revision 1.4  1997/12/18 22:21:43  leidner
c!#   added option to write out minimization results to Splus file
c!#
c!#   Revision 1.3  1997/04/08 16:33:12  rhoffman
c!#   Changed standard minimization to use routine VA15AD.
c!#
c!#	Revision 1.2  1997/02/12  19:19:28  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      subroutine solve (minimize)
#include "vam.h"
C***********************************************************************
C*****Solve - find sum of squares or minimum
C-----XREFS:                                                            
C-----USER : ...MAP ..CKWK ..IMAP .SSCGR ERRSEA SSCGR1 ...... ......
C-----???? : VA15AD ...... ...... ...... ...... ...... ...... ......    
c!!!!!       change double precision to real for CRAY in VA15AD
C-----FORT : FRDNL# FWRNL# IBCOM# ...... ...... ...... ...... ......    
C-----SPECIFICATIONS                                                    
      logical minimize
c
c	Local Variables
c     ---------------
c
#include "gparm.h"
#include "grdta.h"
#include "ptdta.h"
#include "weight.h"
C-----DEFINE NUMERICAL CONSTANTS
      COMMON /WORK/ WORK(WBF)
      integer iprint(2)
      character*132 lamda_minimize_splus, lamda_background_splus
      SAVE SS,Eps1,IPLOT,MAXFN,
     &      Eps, Mdim, Iprint
      NAMELIST /input/ SS,Eps1,IPLOT,MAXFN,
     &      Eps, Mdim, Iprint,acc,perss,
     &      LAMDA,LSCALE,SCALE,RA,DFAC,
     &      lamda_minimize_splus, lamda_background_splus

C-----SET DEFAULTS FOR NAMELIST
      data SS/0/,Eps1/1d-2/,IPLOT/1/,MAXFN/25/
      data Eps/1d-6/, Mdim/6/, Iprint/-1,0/
      data lamda_minimize_splus/' '/
      data lamda_background_splus/' '/

C-----NOTE: LAMDA, LSCALE, SCALE initialized in MAIN
C-----NOTE: RA, DFAC INITIALIZED IN RPTDTA
C-----
      WRITE (6,241) NAME0,NAME
  241 FORMAT ('0FORECAST GRID IS ',A8,10X,'1ST GUESS GRID IS ',A8/)
C-----READ IN NAMELIST
      READ (IUNML,input,END=1520,ERR=1520)
      WRITE (6,input)
C-----
      NDIM=2*NGRDPT
C-----CHECK IF WORK AREA IS SUFFICIENTLY LARGE
      CALL CKWK(1,NDIM)
      CALL IMAP(WORK(1),U,V,IDIM)
C-----
      IF (.not.minimize .or. SS.eq.0) then
        CALL CKWK(2,NDIM)
        CALL SSCGR1 (NDIM,WORK(1),SS,WORK(1+NDIM),IPLOT)
c
c     write out ssq's after minimization by calling "SUM OF SQUARES"
c
        if (iteration.gt.1 .and. lamda_minimize_splus.ne.' ') then
           open (91,file=lamda_minimize_splus,form='formatted')
           write(91,2100) (k,lamda(k),norm(k),scale(k),ssq(k),
     &          lscale(k),k=1,nsos)
           close(91)
        endif
 2000   format('nsos ',34x,'lamda ',34x,'norm  ',34x,'scale ',
     &       34x,'ssq   ',34x,'lscale')
 2100   format(i3,5f40.20)
      endif
C-----
      if (minimize) then
        ipass = ipass + 1
        NAME='MINIMIZE'
        PER=PERSS
C-----CHECK IF WORK AREA IS SUFFICIENTLY LARGE
        CALL CKWK(2,NDIM)
        I=1
        CALL SSCGR1 (NDIM,WORK(1),SS,WORK(1+NDIM),I)
c
c     write out ssq's before minimization
c
        if (lamda_background_splus.ne.' ') then
           open (90,file=lamda_background_splus,form='formatted')
           write(90,2000)
           write(90,2100) (k,lamda(k),norm(k),scale(k),ssq(k),
     &          lscale(k),k=1,nsos)
           close(90)
        endif

  265   SSOLD=SS
        CALL CKWK(5+2*Mdim,NDIM)
        Ifun=0
        Iflag=0
c     While (Iflag .eq. 1 .or. (Iflag.eq.0 .and. Ifun.eq.0))
  275   if ((Iflag .eq. 1 .and. Ifun .le. MaxFn)
     &      .or. (Iflag.eq.0 .and. Ifun.eq.0)) then
          call SSCGR(Ndim,Work(1),SS,Work(1+Ndim))
          call VA15AD(Ndim, Mdim, Work(1), SS, Work(1+Ndim),
     &        .FALSE., Work(1+2*Ndim), Iprint, Eps,
     &        Work(1+3*Ndim), Work(1+(3+Mdim)*Ndim), Ipoint,
     &        Work(1+(3+2*Mdim)*Ndim), Iflag,iter)
          ifun = ifun + 1
          go to 275
        endif
C-----STORE RESULT
        CALL MAP (WORK(1),U,V,IDIM)
        CALL SSCGR1 (NDIM,WORK(1),SS,WORK(1+NDIM),I)
C-----PLOT MAP IF DESIRED
        IF (IFLAG.LT.0) then
          PER=100*(SSOLD-SS)/SSOLD
          IF ((IFLAG.EQ.-4 .or. IFLAG.eq.-5)
     &        .AND. PER.GT.EPS1) GO TO 265
        endif
      endif
c
      return
C-----NAMELIST ERROR
 1520 CALL ERRSEA (520,6HSOLVE3)
      RETURN
      END
