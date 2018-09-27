c!#   $Id: ioclose.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: ioclose.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine ioclose(iu)
c**********************************************************************
c English Name: I/O Close
c -------------
c
c Purpose: Disconnects I/O link with data set associated with the
c -------- specified unit number (see "ioopen").
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
c     integer iu
c
c     iu (I) - fortran unit number
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: March 31, 1995 - created. 
c ---------
c**********************************************************************
#include "iosubs.h"
c
c     Argument List
c     -------------
c
      integer iu
c
c     Common Blocks
c     -------------
c
#include "iolist.h"
c
c     Case 1: Illegal unit number.
c     ============================
c
      if (iu .le. 0 .or. iu .gt. MAXUNIT) return
c
c     Case 2: Specified unit number already closed.
c     =============================================
c
      if (iomap(iu) .ge. 0) return
c
c     Case 3: Specified unit number is reserved.
c     ==========================================
c
      if (iomap(iu) .eq. IOFILL) return
c
c     Case 4: Close unit and increment data set pointer.
c     ==================================================
c
      close (unit=iu)
      iomap(iu) = -iomap(iu) + 1
      if (iomap(iu) .gt. ionum) iomap(iu) = 0

      return
      end
