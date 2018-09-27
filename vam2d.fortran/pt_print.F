c!#   $Id: pt_print.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: pt_print.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine PT_print(istart,istop,iprint)
c**********************************************************************
c English Name: Print (PT - Satellite Obs or Point Data)
c -------------
c
c Purpose: Samples data according to specified frequency by printing
c -------- buffer contents. This routine operates on the VAM
c          SASS data buffer for the index range specified
c          (see "ptdta.h").
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
#include "ptdta.h"
c
c     Local Variables
c     ---------------
c
      integer i,k
c
c     Print sample data.
c     ==================
c
      if (iprint .le. 0) return
      write(*,100)

      do i = istart,istop,iprint

         write(*,110) xcoord(i),ycoord(i),ill(i),jll(i),iobs(i),
     &                wgt(i),vbar(i),nwinds(i),uobs(1,i),vobs(1,i)

         do k = 2,nwinds(i)
            write(*,115) uobs(k,i),vobs(k,i)
         end do

      end do

      write(*,120) istop-istart+1

 100  format(/,1x,'SASS Data Inventory',/,1x,21('-'),
     &       //,1x,'   XCOORD    |','   YCOORD    |',' ILL  |',
     &             ' JLL  |','    IOBS     |','    WGTS     |',
     &             '     VBAR    |','   NWINDS    |','    UOBS     |',
     &             '     VOBS    |',/,1x,126('-'))

 110  format(1x,2(e13.5,'|'),2(i6,'|'),1(i13,'|'),2(e13.5,'|'),
     &          1(i13,'|'),2(e13.5,'|'))
 115  format(1x,2(13x,'|'),2(6x,'|'),1(13x,'|'),2(13x,'|'),
     &          1(13x,'|'),2(e13.5,'|'))
 120  format(/,1x,'Total Number of Reports: ',i6)

      return
      end
