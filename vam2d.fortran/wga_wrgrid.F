c!#   $Id: wga_wrgrid.F,v 1.3 1998/03/11 17:53:09 stanr Exp $
c!#   $Log: wga_wrgrid.F,v $
c!#   Revision 1.3  1998/03/11 17:53:09  stanr
c!#   fix sometimes-unset xs_new
c!#
c!#   Revision 1.2  1997/04/08 20:23:38  leidner
c!#   added lognum and changed starting lat to be in the range (-180,180)
c!#
c!#	Revision 1.1  1997/02/18  20:04:10  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WGA_wrgrid(idate,itime)
c**********************************************************************
c English Name: Write Grid (WGA - Writes Grid in Ascii format)
c -------------
c
c Purpose: Writes U and V gridded wind fields from VAM data buffer
c -------- (see "grdta.com") to output data set in ASCII format.
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
c     integer WGA_wrgrid,idate,itime
c
c     WGA_wrgrid (O) - function return value:
c
c          WGA_wrgrid =  0 (normal)
c                       -1 (error returned opening data set)
c                       -2 (error reading namelist)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c
c Programmer: Ross N Hoffman
c ----------- (AER, Inc.)
c
c Modified: Dec 5, 1996 (created)
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer WGA_wrgrid,idate,itime
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
c
c     Namelist
c     --------
c
      integer iu
      character*24 header_fmt, header_fmt2, data_fmt
      namelist /input/ iu, header_fmt, data_fmt
      data header_fmt/'(i10,2f10.3)'/
      data header_fmt2/'(f10.2)'/
      data data_fmt/'(10f10.3)'/
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      WGA_wrgrid = 0

      read(IUNML,input,end=902,err=902)
      write(*,input)

c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     Write header from gparm
c     =======================
c
      xs_new = xs
      if (xs .gt. 180.) xs_new = xs - 360.
      write(iu,header_fmt) idate
      write(iu,header_fmt) itime
      write(iu,header_fmt2) lognum
      write(iu,header_fmt) nx,xs_new,delx
      write(iu,header_fmt) ny,ys,dely
c
c     Write data from grdta (u, then v)
c     =================================
c     
      write(iu,data_fmt) ((u(i,j),i=1,nx),j=1,ny)
      write(iu,data_fmt) ((v(i,j),i=1,nx),j=1,ny)
c
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WGA_wrgrid = -1
      go to 999

 902  continue
      WGA_wrgrid = -2
      print *, 'vam(WGA_wrgrid): error reading namelist'
      go to 999

 999  return
      end
