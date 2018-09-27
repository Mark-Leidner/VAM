c!#   $Id: leapyr.f,v 1.1 1999/10/04 14:00:01 mcc Exp $
c!#   $Log: leapyr.f,v $
c!#   Revision 1.1  1999/10/04 14:00:01  mcc
c!#   Initial revision
c!#
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function leapyr(year)
c**********************************************************************
c English name: Leap Year
c -------------
c
c Purpose: Determines if "year" is a leap year.
c --------
c
c Notes: A year is a leap year if it is divisible by 4 but not a 100
c ------ except if it is divisible by 400.
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     logical leapyr
c     integer year
c
c     year (i) - year to be checked in yyyy format (ex. 1978).
c     leapyr (o) - function return value:
c                             ( .true.) - "year" is a leap year.
c                             ( .false.) - "year" is not a leap year.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddard Space Flight Center)
c
c Modified: September 30, 1992 - documented.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      logical leapyr
      integer year

      if ( (mod(year,4) .eq. 0 .and. mod(year,100) .ne. 0) .or.
     .                                      mod(year,400) .eq. 0) then

         leapyr = .true.
      else
         leapyr = .false.
      endif

      return
      end
