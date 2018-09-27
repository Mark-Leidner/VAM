c!#   $Id: sl_superob.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: sl_superob.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function SL_superob(istart,istop)
c**********************************************************************
c English Name: Superob (SL - Standard Level Data)
c -------------
c
c Purpose: Eliminates multiple reports by averaging all observations
c -------- reporting at the same location into one report. This routine
c          operates on the VAM conventional data buffer for the index
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
c     integer SL_superob,istart,istop
c
c     SL_superob (O) - function return value: number of reports after
c                   superob.
c     istart (I) - index pointer into conventional data buffer 
c                  signifying where to begin superobing.
c     istop  (I) - index pointer into conventional data buffer 
c                  signifying where to stop superobing.
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
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "sldta.h"
c
c     Local Variables
c     ---------------
c
      logical sameob
      integer i,j,navg,ncnt,ipt
      real usum,vsum,ilat,ilon,jlat,jlon,fill

      data fill/9999.0/
c
c     Average Reports at same location.
c     =================================
c
      ncnt = 0
      do 10 i = istart,istop

         if (FTEST(xconv(i),fill)) go to 10
         ilat = ys + (jlc(i) - 1 + yconv(i)) * dely
         ilon = xs + (ilc(i) - 1 + xconv(i)) * delx

         navg = 1
         usum = uconv(i)
         vsum = vconv(i)

         do 5 j = i+1,istop

            if (FTEST(xconv(j),fill)) go to 5
            jlat = ys + (jlc(j) - 1 + yconv(j)) * dely
            jlon = xs + (ilc(j) - 1 + xconv(j)) * delx

            sameob = (FTEST(ilat,jlat))
            sameob = (sameob .and. FTEST(ilon,jlon))
            sameob = (sameob .and. iconv(i) .eq. iconv(j))

            if (sameob) then 
               navg = navg + 1
               usum = usum + uconv(j)
               vsum = vsum + vconv(j)
               xconv(j) = fill
            endif

 5       continue

         ipt  = istart + ncnt
         ncnt = ncnt + 1
         xconv(ipt)  = xconv(i)
         yconv(ipt)  = yconv(i)
         iconv(ipt)  = iconv(i)
         sl_qc(ipt)  = sl_qc(i)
         wconv(ipt)  = wconv(i)
         ilc(ipt)    = ilc(i)
         jlc(ipt)    = jlc(i)
         uconv(ipt)  = usum / float(navg)
         vconv(ipt)  = vsum / float(navg)

 10   continue

      SL_superob = ncnt
      write(*,100) istop-istart+1,ncnt
 100  format(/,1x,'iovam(SL_superob): ',i6,' obs reduced to ',i6)

      return
      end
