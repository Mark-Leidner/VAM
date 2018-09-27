c!#   $Id: resids.F,v 1.4 1998/04/06 17:52:44 stanr Exp $
c!#   $Log: resids.F,v $
c!#   Revision 1.4  1998/04/06 17:52:44  stanr
c!#   change type of Jdepart and Jpsd to real*8, matching sscgr
c!#
c!#   Revision 1.3  1997/07/25 17:02:42  leidner
c!#   Added penalty and departure terms to ss calc for scatterometer backscatter
c!#
c!#	Revision 1.2  1997/02/12  19:19:28  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine resids
c**********************************************************************
c English Name: ERS1 Residual
c -------------
c
c Purpose: Performs quality control on the ERS-1 backscatter data.
c --------
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c     Namelist - "input"
c     -----------------
c
c     integer iprint,iver
c     real gamma(ER_MBEAM)
c     namelist /input/ iver,iprint,gamma
c
c     iprint - controls frequency of diagnostic output from this 
c              routine:
c
c                      iprint = 0 (no output)
c                               1 (every ob failing QC is printed)
c                               2 (every other ob failing QC is printed)
c                               .
c                               .
c     iver - specifies which field to use as a reference for determining
c            the quality of the ERS1 observations:
c
c                      iver = 0 (background used)
c                             1 (current analysis used)
c
c     gamma - threshold or maximum displacement that a given ERS1 obs.
c             may have from the reference field according to the 
c             following formula:
c
c                      displacement = ((s0obs - s0calc) / s0sd)**2
c
c             An average of the displacement is calculated over all
c             "valid" antenna measurements and the result is compared
c             to the threshold as follows:
c
c                      if (avgdisp > gamma(nbeam)) report fails QC
c
c              where "nbeam" is the number of antenna measurements used
c              in the average.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 16, 1993 - created
c ---------
c**********************************************************************
#include "vam.h"
      integer ANALYSIS,BACKGROUND
      parameter (ANALYSIS = 1,BACKGROUND = 0)
c
c     Common Block - "grdta.h" (Gridded Data)
c     --------------------------
c
#include "gparm.h"
#include "grdta.h"
c
c     Common Block - "er_bscat.h" (ERS1 Backscatter)
c     -----------------------------
c
#include "er_bscat.h"
c
c     Local Variables
c     --------------- 
c
      real*8 ss
      integer ntag
      real j1,delta(ER_MBEAM)
c
c     Namelist - "input" 
c     -----------------
c
      real gamma(ER_MBEAM)
      integer iprint,iver,itag(MAXTAG)
      namelist /input/ iver,iprint,gamma,itag

c     Jdepart   Cost function due to departures
c     Jpsd      Cost function due to penalty term
      real*8 Jdepart, Jpsd
c
      data iprint/0/
      data iver/ANALYSIS/
      data gamma/ER_MBEAM*10.0/
c
c     Read Namelist
c     ============= 
c
      call isetcon(MAXTAG,itag,ALLTYPE)
      read(IUNML,input,end=900,err=900)
      write(6,input)

      print *, ' '
      if (iver .eq. BACKGROUND) then 
         print *, 'RESIDUALS FOR ERS1 DATA: USING BACKGROUND'
      else
         print *, 'RESIDUALS FOR ERS1 DATA: USING CURRENT ANALYSIS'
      endif
      print *, ' '
c
c     Calculate trajectory values for all observations.
c     =================================================
c
      call ssers1 (ss,xnorm,delssu,delssv, u, v,idim, Jdepart, Jpsd)
c
c     Begin Quality Control
c     =====================
c
      itoss = 0
      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG

      do n = 1,er_nobs

         if (ntag.eq.0 .or. isrch(ntag,itag,er_tag(n)).ne.0) then 
c
c        Measure the departure from the verification source.
c        ---------------------------------------------------
c
         nsig = 0
         sum = 0.0
         do ib = 1,ER_MBEAM
            if (er_kp(n,ib) .gt. 0.0) then 
         
               nsig = nsig + 1

               if (iver .eq. BACKGROUND) then 
                  s0calc = er_s0bg(n,ib)
               else
                  s0calc = er_s05(n,ib)
               endif

               delta(ib) = ((er_s0obs(n,ib)-s0calc) / er_s0sd(n,ib))**2
               sum = sum + delta(ib)

            endif
         end do
c
c        Determine if report is acceptable.
c        ----------------------------------
c
         if (nsig .gt. 0) then 

            j1 = sum / nsig
            if (j1 .gt. gamma(nsig)) then 

               itoss = itoss + 1
               er_qc(n) = ior(er_qc(n),2**ipass)
               call showqc(itoss,iprint,n,j1,delta)

               do ib = 1,ER_MBEAM
                  er_kp(n,ib) = -abs(er_kp(n,ib))
               end do

            endif

         endif

         endif

      end do

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      call errsea(520,'RESIDS')
      go to 999

 999  continue

      write(*,100) itoss,er_nobs
 100  format(1x,i8,' REPORTS OUT OF ',i8,' FAILED QUALITY CONTROL')

      return
      end
