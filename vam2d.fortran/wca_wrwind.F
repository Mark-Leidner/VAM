c!#   $Id: wca_wrwind.F,v 1.1 1997/02/21 23:45:10 leidner Exp $
c!#   $Log: wca_wrwind.F,v $
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function WCA_wrwind(idate,itime)
c**********************************************************************
c English Name: Write Conv (WCA - Writes Conv winds in Ascii format)
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
c Modified: Jan 7, 1997 (created)
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer WCA_wrwind,idate,itime
c
c     Common Blocks
c     -------------
c
#include "sldta.h"
#include "gparm.h"
#include "grdta.h"
c
c     Namelist
c     --------
c
      integer iu
      character*24 header_fmt, data_fmt
      namelist /input/ iu, header_fmt, data_fmt
      data header_fmt/'(i10)'/
      data data_fmt/'(10f10.3)'/
c
c     Local Variables
c     ---------------
c 
      real Latbuf(SLMAX), Lonbuf(SLMAX)

c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      WCA_wrwind = 0

      read(IUNML,input,end=902,err=902)
      write(*,input)

c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     Write header from gparm and sldta.h
c     =====================================
c
      write(iu,header_fmt) idate
      write(iu,header_fmt) itime
      write(iu,header_fmt) nptc
c
c
c     Store lat/lon data locations in buffers
c     ========================================
c
      do n = 1,nptc

         ii   = ilc(n)
         jj   = jlc(n)
         Latbuf(n) = ys + (jj - 1 + yconv(n)) * dely
         Lonbuf(n) = xs + (ii - 1 + xconv(n)) * delx
c     convert longitudes to (-180,180)lon coord
         if (Lonbuf(n).gt.180) Lonbuf(n) = Lonbuf(n) - 360

      end do
c
c
c     Write NSCAT WVC data to ascii file
c     ==================================
c     
      write(iu,data_fmt) (Latbuf(n),n=1,nptc)
      write(iu,data_fmt) (Lonbuf(n),n=1,nptc)
      write(iu,data_fmt) (uconv(n), n=1,nptc)
      write(iu,data_fmt) (vconv(n), n=1,nptc)
c
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WCA_wrwind = -1
      go to 999

 902  continue
      WCA_wrwind = -2
      print *, 'vam(WCA_wrwind): error reading namelist'
      go to 999

 999  return
      end
