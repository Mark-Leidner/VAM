c!#   $Id: showqc.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: showqc.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine showqc(ncnt,iprint,ptr,j1,delta)
c**********************************************************************
c English Name: Show Quality Control
c -------------
c
c Purpose: Prints out information concerning the quality of an ERS1
c -------- report as determined by "RESIDS" routine.
c
c Notes: 1) This routine is called from "RESIDS" routine and should not 
c ------    be called directly by the user.
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 16, 1993 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer ncnt,iprint,ptr
      real j1,delta(ER_MBEAM)
c
c     Common Block - "er_bscat.h" (ERS1 Backscatter)
c     -----------------------------
c
#include "er_bscat.h"
c
c     Initialize
c     ==========
c
      if (iprint .le. 0) go to 999
      if (mod(ncnt,iprint) .ne. 0) go to 999
      if (ncnt .eq. iprint) write(*,100)
c
c     Print QC info for all valid antenna measurements.
c     =================================================
c
      nsig = 0
      do ib = 1,ER_MBEAM

         if (er_kp(ptr,ib) .gt. 0.0) then 

            nsig = nsig + 1
            if (nsig .eq. 1) then 
               write(*,110) er_qc(ptr),
     &                      er_lat(ptr),er_lon(ptr),er_ic(ptr),
     &                      er_jc(ptr),er_xc(ptr),er_yc(ptr),j1,
     &                      ib,er_s0sd(ptr,ib),er_s0obs(ptr,ib),
     &                      er_s0bg(ptr,ib),er_s05(ptr,ib),delta(ib)
            else
               write(*,120) ib,er_s0sd(ptr,ib),er_s0obs(ptr,ib),
     &                      er_s0bg(ptr,ib),er_s05(ptr,ib),delta(ib)
            endif

         endif

      end do

 100  format(1x,/,1x,128('-'),/,1x,' Q ',
     &      '  LAT   ','  LON   ','  I ',' J ',
     &      '  X   ','  Y   ','       J1      ','ANT','    S0SD       ',
     &      '    S0OBS      ','    S0BG       ','    S05        ',
     &      '    DELTA      ',/,1x,131('-'))
 110  format(1x,i3,2f8.3,i4,i3,2f6.3,e15.6,i3,5e15.6)
 120  format(1x,53x,i3,5e15.6)

 999  return
      end
