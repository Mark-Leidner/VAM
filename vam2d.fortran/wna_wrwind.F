c!#   $Id: wna_wrwind.F,v 1.4 1998/05/14 20:07:41 leidner Exp $
c!#   $Log: wna_wrwind.F,v $
c!#   Revision 1.4  1998/05/14 20:07:41  leidner
c!#   added handling of WVC info from NSCAT sig0 and/or NSCAT amb.winds
c!#   data structures
c!#
c!#   Revision 1.3  1998/03/04 14:19:12  leidner
c!#   changed write format to accomodate 4-digit row #'s (25km data)
c!#
c!#   Revision 1.2  1997/04/08 20:26:16  leidner
c!#   added lognum
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WNA_wrwind(idate,itime)
c**********************************************************************
c English Name: Write NSCAT (WNA - Writes NSCAT winds in Ascii format)
c -------------
c
c Purpose: Writes analysis U and V interpolated to NSCAT WVC locations
c -------- (see "ns_bscat.h") to output data set in ASCII format.
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
c     integer WNA_wrwind,idate,itime
c
c     WNA_wrwind (O) - function return value:
c
c          WNA_wrwind =  0 (normal)
c                       -1 (error returned opening data set)
c                       -2 (error reading namelist)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c
c Programmer: Mark Leidner
c ----------- (AER, Inc.)
c
c Modified: Jan 3, 1997 (created)
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer WNA_wrwind,idate,itime
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
#include "ns_bscat.h"
#include "ns_winds.h"
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
c!#   Local variables for holding sig0 or amb. winds WVC infomation
      integer nout
      integer revsout(NS_MAXREVS), n_by_rev(NS_MAXREVS)
      integer ic(NS_MWVC), jc(NS_MWVC)
      real xc(NS_MWVC), yc(NS_MWVC), lon(NS_MWVC), lat(NS_MWVC)
      integer rowsout(NS_MWVC), colsout(NS_MWVC)

      real ubuf(NS_MWVC),vbuf(NS_MWVC)

c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      WNA_wrwind = 0
      nout = 0

      read(IUNML,input,end=901,err=901)
      write(*,input)

c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 902

c!#   Determine if sig0 or ambiguous winds WVC info is stored
c!#   and hold in local variables

c!#   sig0 WVC info
      if (ns_nWVC .gt. 0) then
         do i=1,NS_MAXREVS
            revsout(i)  = revs(i)
            n_by_rev(i) = ns_nWVC_rev(i)
         enddo
         nout = ns_nWVC
         do n=1,nout
            ic(n) = WVC_ic(n)
            jc(n) = WVC_jc(n)
            xc(n) = WVC_xc(n)
            yc(n) = WVC_yc(n)
            lat(n) = WVC_Lat(n)
            lon(n) = WVC_Lon(n)
            rowsout(n) = WVC_row(n)
            colsout(n) = WVC_col(n)
         enddo

c!#   amb. winds WVC info
      else if (nsw_nwvc .gt. 0) then
         do i=1,NS_MAXREVS
            revsout(i)  = nsw_revs(i)
            n_by_rev(i) = nsw_nwvc_rev(i)
         enddo
         nout = nsw_nwvc
         do n=1,nout
            ic(n) = nsw_ic(n)
            jc(n) = nsw_jc(n)
            xc(n) = nsw_xc(n)
            yc(n) = nsw_yc(n)
            lat(n) = nsw_Lat(n)
            lon(n) = nsw_Lon(n)
            rowsout(n) = nsw_row(n)
            colsout(n) = nsw_col(n)
         enddo
      
c!#   Dummy check for no data
      else if ( nout.eq.0 ) then
         go to 903
      endif

c!#   If both are present, check to make sure they agree
      if ( ( ns_nWVC.gt.0 .and. nsw_nwvc.gt.0 ) .and.
     &     ( ns_nWVC.ne.nsw_nwvc ) ) go to 904

c     Write header from gparm and scatterometer data structure
      write(iu,header_fmt) idate
      write(iu,header_fmt) itime
      write(iu,header_fmt2) lognum
      write(iu,header_fmt_revs) (revsout(i),i=1,NS_MAXREVS)
      write(iu,header_fmt_revs) (n_by_rev(i),i=1,NS_MAXREVS)
      write(iu,header_fmt_revs) nout

c     Interpolate u,v to NSCAT WVC locations
      do n = 1,nout
         ii      = ic(n)
         jj      = jc(n)
         ubuf(n) = bilin (xc(n),yc(n),u(ii,jj),idim)
         vbuf(n) = bilin (xc(n),yc(n),v(ii,jj),idim)
      end do

c     Write NSCAT WVC data to ascii file
c     convert longitudes to (-180,180)lon coord
      do n=1,nout
         if (lon(n).gt.180) lon(n) = lon(n) - 360
      enddo

      write(iu,data_fmt) (lat(n), n=1,nout)
      write(iu,data_fmt) (lon(n), n=1,nout)
      write(iu,data_fmt) (ubuf(n), n=1,nout)
      write(iu,data_fmt) (vbuf(n), n=1,nout)
      write(iu,data_fmt2)(rowsout(n), n=1,nout)
      write(iu,data_fmt2)(colsout(n), n=1,nout)
c
      go to 999
c
c     End of Routine / Error Control
c     ==============================

 901  continue
      print *, 'vam(WNA_wrwind): error reading namelist'
      WNA_wrwind = -1
      go to 999

 902  continue
      print *, 'vam(WNA_wrwind): error opening unit', iu
      WNA_wrwind = -2
      go to 999

 903  continue
      print *, 'vam(WNA_wrwind): no data present in ns_bscat or ',
     &     'ns_winds data structures'
      WNA_wrwind = -3
      go to 999

 904  continue
      print *, 'vam(WNA_wrwind): sig0 and amb. wind WVC''s do ',
     &     'not agree'
      WNA_wrwind = -4
      go to 999

 999  return
      end

