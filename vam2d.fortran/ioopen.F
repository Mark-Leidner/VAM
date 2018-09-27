c!#   $Id: ioopen.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: ioopen.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function ioopen(iu)
c**********************************************************************
c English Name: I/O Open
c -------------
c
c Purpose: Established I/O link with data set associated with the
c -------- specified unit number (see "ioload" and "ioclose").
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
c     integer ioopen,iu
c
c     ioopen (O) - function return value:
c
c               ioopen = 0 (data set opened or already connected).
c                        2 (data exhausted for specified unit number).
c                       -1 (error connecting unit for I/O).
c                       -2 (illegal unit number specified).
c                       -3 (requested unit number is reserved).
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
      if (iu .le. 0 .or. iu .gt. MAXUNIT) then 

         ioopen = -2
         print *,'iosubs(ioopen): illegal unit number: ',iu
         return
c
c     Case 2: Requested unit number is reserved.
c     ==========================================
c
      elseif (iomap(iu) .eq. IOFILL) then 

         ioopen = -3
         print *,'iosubs(ioopen): reserved unit number: ',iu
         return
c
c     Case 3: Requested unit number already in use.
c     =============================================
c         
      elseif (iomap(iu) .lt. 0) then 

         ioopen = 0
         return
c
c     Case 4: Requested unit number has been closed.
c     ==============================================
c
      elseif (iomap(iu) .eq. 0) then 

         ioopen = 2
         return
c
c     Case 5: No more data sets associated with requested unit number.
c     ================================================================
c
      elseif (iounit(iodata(iomap(iu))) .ne. iu) then 

         ioopen = 2
         iomap(iu) = 0
         return
c
c     Case 6: Connect unit number to new data set.
c     ============================================
c
      else

         ioopen = -1
         if (iolink(iodata(iomap(iu))) .ne. 0) return
         ioopen = 0
         iomap(iu) = -iomap(iu)

      endif

      return
      end
