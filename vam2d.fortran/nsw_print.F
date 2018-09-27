c!#   $Id: nsw_print.F,v 1.2 1998/03/18 16:29:56 leidner Exp $
c!#   $Log: nsw_print.F,v $
c!#   Revision 1.2  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.1  1997/04/14 19:13:54  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine NSW_print(istart,istop,iprint)
c**********************************************************************
c English Name: Print (NSW - NSCAT winds)
c -------------
c
c Purpose: Samples data according to specified frequency by printing
c -------- buffer contents. This routine operates on the VAM
c          NSCAT winds data buffer for the index range specified
c          (see "ns_winds.h").
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer istart,istop,iprint
c
c     istart (I) - index pointer into conventional data buffer 
c                  signifying where to begin sampling.
c     istop  (I) - index pointer into conventional data buffer 
c                  signifying where to stop sampling.
c     iprint (I) - sampling frequency (every "iprint" reports are
c                  printed):
c
c              iprint > 0 (print)
c                    <= 0 (no printing)
c
c Programmer: Mark Leidner
c ----------- Atmospheric and Environmental Research, Inc.
c
c Modified: April 11, 1997 - created (after J. Ardizzone's ER_print).
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
#include "ns_winds.h"
c
c     Local Variables
c     ---------------
c
      integer i
c
c     Print sample data.
c     ==================
c
      if (iprint .le. 0) return
      write(*,100)

      do i=istart,istop,iprint
c
c     Find rev which corresponds to the i-th data in storage
c
        isum = 0
        do 50 j=1,NS_MAXREVS
          isum = isum + nsw_nwvc_rev(j)
          if (isum .lt. i) goto 50
          if (isum .ge. i) then
            n=j
            goto 51
          endif
  50    continue
  51    continue
c
c     Print WVC information and the 1st or zeroth ambiguity
c
        if (nsw_nambigs(i) .eq. 0) then
          write(*,110) nsw_revs(n),nsw_row(i),nsw_col(i),nsw_lat(i),
     &        nsw_lon(i),nsw_ic(i),nsw_jc(i),nsw_xc(i),nsw_yc(i),
     &        nsw_time(i),nsw_tb(i),nsw_al(i),nsw_dt(i),0
        else
          write(*,111) nsw_revs(n),nsw_row(i),nsw_col(i),nsw_lat(i),
     &        nsw_lon(i),nsw_ic(i),nsw_jc(i),nsw_xc(i),nsw_yc(i),  
     &        nsw_time(i),nsw_tb(i),nsw_al(i),nsw_dt(i),
     &        1,nsw_u(i,1),nsw_v(i,1),nsw_mle(i,1)
        endif
c
c    If more than one ambiguity, print out others
c
        if (nsw_nambigs(i) .gt. 1) then
          do j = 2,nsw_nambigs(i)
            write(*,115) j,nsw_u(i,j),nsw_v(i,j),nsw_mle(i,j)
          end do
        endif
c
      end do
c
      write(*,120) istop-istart+1
c
c   Rev |  Row | Cell |   Lat  |   Lon  |  Ic |  Jc |  Xc   |  Yc   |  Time  |   GOES tb    |   GOES al    |   GOES dt    |amb|    U   |    V   |  mle  |
cxiiiiix|xiiiix|xxiixx|xfff.ffx|xfff.ffx|xiiix|xiiix|xf.fffx|xf.fffx|xff.fffx|xee.eeeee+Eeex|xee.eeeee+Eeex|xee.eeeee+Eeex|xix|xfff.ffx|xfff.ffx|xfff.fx|
  100 format(/1x,'NSCAT Winds Data Inventory',/,1x,29('-'),
     &    //,1x,135('-'),
     &     /,'   Rev |  Row | Cell |   Lat  |   Lon  |  Ic |',
     &    '  Jc |  Xc   |  Yc   |  Time  |   GOES tb    |   GOES al',
     &    '    |   GOES dt    |amb|    U   |    V   |  MLE  |',
     &     /,1x,135('-'))
  110 format(1x,i5,' | ',i4,' |  ',i2,'  | ',2(f6.2,' | '),
     &    2(i3,' | '),2(f5.3,' | '),f6.3,' | ',3(e12.5,' | '),
     &    i1,' | ',2('    NA',' | '),'   NA',' |')
  111 format(1x,i5,' | ',i4,' |  ',i2,'  | ',2(f6.2,' | '),
     &    2(i3,' | '),2(f5.3,' | '),f6.3,' | ',3(e12.5,' | '),
     &    i1,' | ',2(f6.2,' | '),f5.1,' |')
  115 format('       |      |      |        |        |     |',
     &    '     |       |       |        |              |          ',
     &    '    |              ',
     &    '| ',i1,' | ',2(f6.2,' | '),f5.1,' |')
  120 format(/,1x,'Total Number of Reports: ',i6)
c
      return
      end
