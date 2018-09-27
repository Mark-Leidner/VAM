c!#   $Id: sl_grosscheck.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: sl_grosscheck.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function SL_grosscheck(istart,istop,minspeed,maxspeed)
c**********************************************************************
c English Name: Gross Check (SL - Standard Level Data)
c -------------
c
c Purpose: Performs gross quality control check. This routine
c -------- operates on the VAM conventional data buffer for the index
c          range specified (see "sldta.h").
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c
c     Argument List
c     -------------
c
c     real minspeed,maxspeed
c     integer SL_grosscheck,istart,istop
c
c     SL_grosscheck (O) - function return value: number of reports after
c                   grosscheck.
c     istart (I) - index pointer into conventional data buffer 
c                  signifying where to begin grosschecking.
c     istop  (I) - index pointer into conventional data buffer 
c                  signifying where to stop grosschecking.
c     minspeed (I) - minimum wind speed threshold for accepting reports.
c
c                      speed <= minspeed (report eliminated)
c
c     maxspeed (I) - maximum wind speed threshold for accepting reports.
c
c                      speed >= maxspeed (report eliminated)
c
c Programmer: Joseph V. Ardizzone
c ----------- (NASA Goddard Space Flight Center)
c             (Satellite Data Utilization Office)
c
c Modified: April 4, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer istart,istop
      real minspeed,maxspeed
c
c     Common Blocks
c     -------------
c
#include "sldta.h"
c
c     Local Variables
c     ---------------
c
      real wspeed
      integer i,ncnt,ipt
c
c     Eliminate reports failing gross check.
c     ======================================
c
      ncnt = 0
      do i = istart,istop

         wspeed = sqrt(uconv(i)**2 + vconv(i)**2)
         if (wspeed .gt. minspeed .and. wspeed .lt. maxspeed) then 

            ipt  = istart + ncnt
            ncnt = ncnt + 1
            xconv(ipt)  = xconv(i)
            yconv(ipt)  = yconv(i)
            iconv(ipt)  = iconv(i)
            wconv(ipt)  = wconv(i)
            ilc(ipt)    = ilc(i)
            jlc(ipt)    = jlc(i)
            sl_qc(ipt)  = sl_qc(i)
            uconv(ipt)  = uconv(i)
            vconv(ipt)  = vconv(i)

         endif

      end do

      SL_grosscheck = ncnt
      write(*,100) istop-istart+1,ncnt
 100  format(/,1x,'iovam(SL_grosscheck): ',i6,' obs reduced to ',i6)

      return
      end
