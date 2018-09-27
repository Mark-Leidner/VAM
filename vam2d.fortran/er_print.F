c!#   $Id: er_print.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: er_print.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine ER_print(istart,istop,iprint)
c**********************************************************************
c English Name: Print (ER - Backscatter)
c -------------
c
c Purpose: Samples data according to specified frequency by printing
c -------- buffer contents. This routine operates on the VAM
c          Backscatter data buffer for the index range specified
c          (see "er_bscat.h").
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
c     integer istart,istop,iprint
c
c     istart (I) - index pointer into SASS data buffer 
c                  signifying where to begin sampling.
c     istop  (I) - index pointer into SASS data buffer 
c                  signifying where to stop sampling.
c     iprint (I) - sampling frequency (every "iprint" reports are
c                  printed):
c
c              iprint > 0 (print)
c                    <= 0 (no printing)
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
      integer istart,istop,iprint
c
c     Common Blocks
c     -------------
c
#include "er_bscat.h"
c
c     Local Variables
c     ---------------
c
      integer i,ib,nsig
c
c     Print sample data.
c     ==================
c
      if (iprint .le. 0) return
      write(*,100)

      do i = istart,istop,iprint
         nsig = 0
         do ib = 1,ER_MBEAM
   
            if (er_kp(i,ib) .gt. 0.0) then
   
               nsig = nsig + 1
               if (nsig .eq. 1) then
                  write(*,110) er_lat(i),er_lon(i),er_ic(i),
     &                         er_jc(i),er_xc(i),er_yc(i),
     &                         er_velobs(i),er_dirobs(i),
     &                         ib,er_theta(i,ib),er_azim(i,ib),
     &                         er_kp(i,ib),er_s0sd(i,ib),
     &                         er_s0obs(i,ib)
               else
                  write(*,120) ib,er_theta(i,ib),er_azim(i,ib),
     &                         er_kp(i,ib),er_s0sd(i,ib),
     &                         er_s0obs(i,ib)
               endif
   
            endif
   
         end do

      end do

      write(*,130) istop-istart+1

 100  format(/,1x,'Backscatter Data Inventory',/,1x,26('-'),
     &       //,1x,127('-'),/,1x,'  LAT   ','  LON   ','  I ',' J ',
     &      '  X   ','  Y   ','  VEL  ','  DIR  ',
     &      'ANT','   THETA       ',
     &      '   AZIMUTH     ','      KP       ','    SD         ',
     &      '      S0       ',/,1x,127('-'))

 110  format(1x,2f8.3,i4,i3,2f6.3,2f7.2,i3,5e15.6)
 120  format(1x,49x,i3,5e15.6)
 130  format(/,1x,'Total Number of Reports: ',i6)

      return
      end
