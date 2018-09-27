c!#   $Id: iodisable.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: iodisable.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine iodisable(iu1,iu2)
c**********************************************************************
c English Name: I/O Disable
c -------------
c
c Purpose: Flags the specified unit numbers as inelligible for I/O
c -------- via the "iosubs" library routines.
c
c Notes: 1) This routine must be called before "ioload".
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 3, 1995 - created.
c ---------
c**********************************************************************
#include "iosubs.h"
c
c     Argument List
c     -------------
c
      integer iu1,iu2
c
c     Local Variables
c     ---------------
c
      integer i
c
c     Common Blocks
c     -------------
c
#include "iolist.h"

      if (iu1 .gt. iu2) return
      if (iu1 .le. 0 .or. iu2 .le. 0) return
      if (iu2 .gt. MAXUNIT) return

      do i = iu1,iu2
         iomap(i) = IOFILL
      end do

      return
      end
