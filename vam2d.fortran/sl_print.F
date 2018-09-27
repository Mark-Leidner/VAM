c!#   $Id: sl_print.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: sl_print.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine SL_print(istart,istop,iprint)
c**********************************************************************
c English Name: Print (SL - Standard Level Data)
c -------------
c
c Purpose: Samples data according to specified frequency by printing
c -------- buffer contents. This routine operates on the VAM
c          conventional data buffer for the index range specified
c          (see "sldta.h").
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
#include "sldta.h"
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

      do i = istart,istop,iprint

         write(*,110) xconv(i),yconv(i),ilc(i),jlc(i),iconv(i),
     &                wconv(i),uconv(i),vconv(i)

      end do

      write(*,120) istop-istart+1

 100  format(/,1x,'Conventional Data Inventory',/,1x,29('-'),
     &       //,1x,'   XCONV     |','   YCONV     |','    ILC      |',
     &             '    JLC      |','   ICONV     |','   WCONV     |',
     &             '   UCONV     |','   VCONV     |',/,1x,112('-'))

 110  format(1x,2(e13.5,'|'),3(i13,'|'),3(e13.5,'|'))
 120  format(/,1x,'Total Number of Reports: ',i6)

      return
      end
