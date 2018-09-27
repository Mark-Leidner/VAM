c!#   $Id: ioparse.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: ioparse.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function ioparse(iocntl,ifield,cfield)
c**********************************************************************
c English Name: I/O Parse
c -------------
c
c Purpose: Extracts designated character string field from I/O control
c -------- string containing exclamation separated fields.
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
c     integer ifield
c     character*(*) iocntl,cfield
c
c     ioparse (O) - function return value:
c
c          ioparse = 0 (requested field not found)
c                  > 0 (length of extracted field)
c
c     iocntl (I) - I/O control string with exclamation separated fields:
c
c           iocntl = 'unit number!filename!format'
c
c           ex.) '12!test.data!-F f77 -N ieee'
c
c     ifield (I) - desired field.
c
c     cfield (O) - extracted field:
c
c           ex.) ifield = 1,cfield='12'
c                       = 2,cfield='test.data'
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
      integer ifield
      character*(*) iocntl,cfield
c
c     Local Variables
c     ---------------
c
      integer i,j,ilen,lastchar,nfield
c
c     Extract desired field.
c     ======================
c
      i      = 0
      j      = 1
      ilen   = 0
      nfield = ifield
      lastchar = iolen(iocntl)

      do while (nfield .gt. 0 .and. j .le. lastchar)

         if (iocntl(j:j) .eq. '!') then 

            ilen   = i
            i      = 0
            nfield = nfield - 1

         elseif (j .eq. lastchar) then 

            i      = i + 1
            ilen   = i
            nfield = nfield - 1
            cfield(i:i) = iocntl(j:j)

         else

            i = i + 1
            cfield(i:i) = iocntl(j:j)

         endif

         j = j + 1

      end do

      if (nfield .ne. 0) ilen = 0
      ioparse = ilen

      return
      end
