c!#   $Id: inctime2.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: inctime2.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine inctime2(indate,intime,inchrs,incmin,incsec)
c**********************************************************************
c English name: Increment Time
c -------------
c
c Purpose: Increments date/time stored as "yymmdd" and "hhmmss"
c -------- respectively.
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
c     integer indate,intime,inchrs,incmin,incsec
c
c     indate (io) - on input, the date in "yymmdd" format. on output,
c                   the incremented date in "yymmdd" format. date
c                   format may also be "yyyymmdd" where the full year
c                   information is given (ex. 1978). if full year info.
c                   is not give (ex. 78), the century is assumed to be
c                   1900.
c     intime (io) - on input, the time in "hhmmss" format. on output,
c                   the incremented time in "hhmmss" format.
c     inchrs (i) - hours to increment the date/time.
c     incmin (i) - minutes to increment the date/time.
c     incsec (i) - seconds to increment the date/time.
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
      integer indate,intime,inchrs,incmin,incsec
c
c     Local Variables
c     ---------------
c
      logical leapyr
      integer year,month,day,hour,min,sec
      integer baseyr,nfeb,ndays(12)

      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/
      equivalence (nfeb,ndays(2))

      ierr   = 0
      baseyr = 1900
c
c     Separate Time Variables
c     =======================
c
      call decode(indate,year,month,day)
      call decode(intime,hour,min  ,sec)
      if (year .gt. 99) baseyr = 0
c
c     Increment Time
c     ==============
c
      sec = sec + incsec
      do while (sec .ge. 60)
         min = min + 1
         sec = sec - 60
      end do

      min = min + incmin
      do while (min .ge. 60)
         hour = hour + 1
         min = min - 60
      end do

      hour = hour + inchrs
      do while (hour .ge. 24)
         day = day + 1
         hour = hour - 24
      end do

      nfeb = 28
      if (leapyr(year+baseyr)) nfeb = 29
      do while (day .gt. ndays(month))
         day = day - ndays(month)
         month = month + 1
         if (month .gt. 12) then
            month = 1
            year = year + 1
            nfeb = 28
            if (leapyr(year+baseyr)) nfeb = 29
         endif
      end do
c
c     Pack the new date and time.
c     ===========================
c
      indate = year * 10000 + month * 100 + day
      intime = hour * 10000 + min   * 100 + sec

      return
      end
