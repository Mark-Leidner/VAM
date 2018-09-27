c!#   $Id: ioload.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: ioload.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function ioload(iu)
c**********************************************************************
c English Name: I/O Load
c -------------
c
c Purpose: Reads data set containing I/O parameters for data sets to
c -------- be accessed during execution. Parameters are saved in common
c          block, "iolist" (see "iolist.h" for more documentation).
c
c Notes: 1) Input data set is a formatted data set with each record
c ------    having the following format:
c
c                  'unit number!filename!format'
c
c                ex.) '12!test.data!-F f77 -N ieee'
c
c           To specify multiple data sets for the same unit number,
c           please list in sequential order:
c
c                ex.) '12!test.data1!-F f77 -N ieee'
c                     '12!test.data2!-F f77 -N ieee'
c                     '12!test.data3!-F f77 -N ieee'
c               
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer iu
c
c     ioload (O) - function return value:
c
c            ioload = 0 (normal)
c                    -1 (max data sets exceeded).
c                    -2 (max unit numbers exceeded).
c                    -3 (attempt to access reserved unit number).
c
c     iu (I) - unit number linked to input data set.
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
c     Local Variables
c     ---------------
c
      integer i,nu
c
c     Common Blocks
c     -------------
c
#include "iolist.h"

      ioload = 0
c
c     Load I/O parameter list into data buffer.
c     =========================================
c
      i     = 0
      ionum = 0
      do while (.true.)

         i = i + 1
         if (i .gt. MAXDSET) go to 901

         read(iu,fmt='(a132)',end=999,err=999) iodata(i)

         ionum = i
         nu = iounit(iodata(i))
         if (nu .le. 0 .or. nu .gt. MAXUNIT) go to 902

         if (iomap(nu) .eq. IOFILL) go to 903
         if (iomap(nu) .eq. 0) iomap(nu) = i

      end do
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      ioload = -1
      print *, 'iosubs(ioload): max data sets exceeded.'
      go to 999

 902  continue
      ioload = -2
      print *, 'iosubs(ioload): illegal unit number: ',nu
      go to 999

 903  continue
      ioload = -3
      print *, 'iosubs(ioload): unit reserved: ',nu
      go to 999

 999  return
      end
