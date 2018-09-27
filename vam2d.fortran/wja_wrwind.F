c!#   $Id: wja_wrwind.F,v 1.4 1998/05/20 14:32:02 leidner Exp $
c!#   $Log: wja_wrwind.F,v $
c!#   Revision 1.4  1998/05/20 14:32:02  leidner
c!#   added write of position of selected ambiguity
c!#
c!#   Revision 1.3  1998/03/18 16:28:31  leidner
c!#   data weight now written
c!#
c!#   Revision 1.2  1998/03/04 14:19:12  leidner
c!#   changed write format to accomodate 4-digit row #'s (25km data)
c!#
c!#   Revision 1.1  1997/08/12 16:13:44  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function WJA_wrwind(idate,itime)
c**********************************************************************
c English Name: Write JPL winds (WNA - Writes JPL winds in Ascii format)
c -------------
c
c Purpose: Writes JPL NSCAT U and V (see "ns_winds.h") to output data
c          set in ASCII format.
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
c     integer WJA_wrwind,idate,itime
c
c     WJA_wrwind (O) - function return value:
c
c          WJA_wrwind =  0 (normal)
c                       -1 (error returned opening data set)
c                       -2 (error reading namelist)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c
c Programmer: Mark Leidner
c ----------- (AER, Inc.)
c
c Modified: Apr 22, 1997 (created)
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer WJA_wrwind
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
#include "ns_winds.h"
#include "ptdta.h"
c
c     Namelist
c     --------
c
      integer iu
      character*24 header_fmt, header_fmt2, header_fmt_revs, 
     &    data_fmt, data_fmt2
      namelist /input/ iu, header_fmt, header_fmt_revs, data_fmt
      data header_fmt/'(i10)'/
      data header_fmt2/'(f10.2)'/
      data header_fmt_revs/'(15i6)'/
      data data_fmt/'(10f10.3)'/
      data data_fmt2/'(10i5)'/
c
c     Local Variables
c     ---------------
c 

c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      WJA_wrwind = 0

      read(IUNML,input,end=902,err=902)
      write(*,input)

c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     Write header from gparm and ns_winds.h
c     ========================================
c
      write(iu,header_fmt) idate
      write(iu,header_fmt) itime
      write(iu,header_fmt2) lognum
      write(iu,header_fmt_revs) (nsw_revs(i),i=1,NS_MAXREVS)
      write(iu,header_fmt_revs) (nsw_nwvc_rev(i),i=1,NS_MAXREVS)
      write(iu,header_fmt_revs) nsw_nwvc
c
c     Write JPL WVC data to ascii file
c     ==================================
c     
c     convert longitudes to (-180,180)lon coord
      do i=1,nsw_nwvc
         if (nsw_lon(i).gt.180) nsw_lon(i) = nsw_lon(i) - 360
      enddo
c
c     convert all missing values to -999 before writing out
      do i=1,nsw_nwvc
        if (nsw_nambigs(i) .lt. 4) then
          do j=nsw_nambigs(i)+1,4
            nsw_u(i,j) = -999.
            nsw_v(i,j) = -999.
            nsw_mle(i,j) = -999.
          end do
        endif
      end do

      write(iu,data_fmt) (nsw_lat(n),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_lon(n),n=1,nsw_nwvc)
      write(iu,data_fmt2) (nsw_nambigs(n),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_u(n,1),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_v(n,1),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_mle(n,1),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_u(n,2),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_v(n,2),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_mle(n,2),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_u(n,3),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_v(n,3),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_mle(n,3),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_u(n,4),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_v(n,4),n=1,nsw_nwvc)
      write(iu,data_fmt) (nsw_mle(n,4),n=1,nsw_nwvc)
      write(iu,data_fmt2)(nsw_row(n),n=1,nsw_nwvc)
      write(iu,data_fmt2)(nsw_col(n),n=1,nsw_nwvc)
      write(iu,data_fmt) (WGT(n),n=1,nsw_nwvc)
      write(iu,data_fmt2) (Sel(n),n=1,nsw_nwvc)
c
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WJA_wrwind = -1
      go to 999

 902  continue
      WJA_wrwind = -2
      print *, 'vam(WJA_wrwind): error reading namelist'
      go to 999

 999  return
      end
