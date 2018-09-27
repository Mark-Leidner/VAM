c!#   $Id: iounit.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: iounit.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function iounit(iocntl)
c**********************************************************************
c English Name: I/O Unit Number
c -------------
c
c Purpose: Extracts the unit number from the specified I/O control
c -------- string.
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
c     integer iounit
c     character*(*) iocntl
c
c     iounit (O) - function return value:
c
c           iounit = -1 (no unit number in control string)
c                  > 0 (extracted unit number)
c
c     iocntl (I) - I/O control string with exclamation separated fields:
c
c           iocntl = 'unit number!filename!format'
c
c           ex.) '12!test.data!-F f77 -N ieee'
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
      character*(*) iocntl
c
c     Local Variables
c     ---------------
c
      integer iu,ilen
      character*6 cfmt
      character*10 cunit
      
      data cfmt/'(i   )'/
c
c     Extract the unit number field from the I/O control string.
c     ==========================================================
c      
      iounit = -1
      ilen = ioparse(iocntl,1,cunit)
      if (ilen .eq. 0) return

      write(unit=cfmt(3:5),fmt='(i3)') ilen
      read(unit=cunit(1:ilen),fmt=cfmt) iu
      iounit = iu

      return
      end
