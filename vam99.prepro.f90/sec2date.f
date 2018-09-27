c!#   $Id: sec2date.f,v 1.1 1999/10/04 13:59:44 mcc Exp $
c!#   $Log: sec2date.f,v $
c!#   Revision 1.1  1999/10/04 13:59:44  mcc
c!#   Initial revision
c!#
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine sec2date(refyr,seconds,idate,itime)
c**********************************************************************
c English Name: Seconds to Date/Time
c -------------
c
c Purpose: Calculates the date/time associated with the number of
c -------- seconds from the beginning of some reference year.
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
c     integer refyr,seconds,idate,itime
c
c     refyr (I) - reference year. The number of seconds specified will
c                 be interpreted as the number of seconds from Jan. 1
c                 of the reference year. Please note that "refyr" is
c                 the absolute year (ex. 1978 ; not 78).
c     seconds (I) - number of seconds from "reference year".
c     idate (O) - returned date associated with specified seconds in
c                 yymmdd format.
c     itime (O) - returned time associated with specified seconds in
c                 hhmmss format.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: December 30, 1992 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      integer refyr,seconds,idate,itime
c
c     Local Variables
c     ---------------
c
      integer totsec,days(12)

      data days/31,28,31,30,31,30,31,31,30,31,30,31/
c
c     Determine the year.
c     ===================
c
      isec  = seconds
      iyear = refyr
      do while (isec .ge. totsec(iyear))
         isec = isec - totsec(iyear)
         iyear = iyear + 1
      end do
      if (iyear .lt. 2000) then
         iyear = iyear - 1900
      else
         iyear = iyear - 2000
      endif
c
c     Determine the month.
c     ====================
c
      days(2) = 28
      if (totsec(iyear) .gt. 365*86400) days(2) = 29

      imon = 1
      do while (isec .ge. days(imon)*86400)
         isec = isec - days(imon)*86400
         imon = imon + 1
      end do
c
c     Determine the day.
c     ==================
c
      iday = 1
      do while (isec .ge. 86400)
         isec = isec - 86400
         iday = iday + 1
      end do
c
c     Determine the hour.
c     ===================
c
      ihour = 0
      do while (isec .ge. 3600)
         isec = isec - 3600
         ihour = ihour + 1
      end do
c
c     Determine the minutes
c     =====================
c
      imin = 0
      do while (isec .ge. 60)
         isec = isec - 60
         imin = imin + 1
      end do
c
c     Pack the date/time.
c     ===================
c
      idate = iyear*10000 + imon*100 + iday
      itime = ihour*10000 + imin*100 + isec

      return
      end
