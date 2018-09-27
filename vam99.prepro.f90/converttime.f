c!#   $Id: converttime.f,v 1.1 1999/10/04 14:00:16 mcc Exp $
c!#   $Log: converttime.f,v $
c!#   Revision 1.1  1999/10/04 14:00:16  mcc
c!#   Initial revision
c!#
c!#   Revision 1.2  1998/01/28 14:34:17  leidner
c!#   added check for leap year
c!#
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#

**********************************************************
      subroutine converttime(timetag,juldate,iymd,ihms,ihr,imn,isc)
C THIS SUBROUTINE CONVERTS THE Julian day and year TO
C year month and day, SUCH THAT Julian day 215 in 96
C IS 960915 (September 15, 1996).
      integer day,daycnt,juldate, year
      logical found, leapyr
      character*24 timetag
      dimension mo(12)
      data mo /31,28,31,30,31,30,31,31,30,31,30,31/
C
      read (TimeTag(1:4),'(i4)') iyear
c
c     IF LEAP YEAR, CHANGE 28 TO 29 IN DATA MO
c
      if (leapyr(iyear)) mo(2) = 29
c
c  Convert Julian date and year to YYMMDD
c
      read (TimeTag(3:4),'(i2)') iyear
      read (TimeTag(6:8),'(i3)') juldate
c
c     find the month
c
      daycnt = 0
      found  = .false.
      do 1 m=1,12
         daycnt = daycnt + mo(m)
         if (daycnt .ge. juldate) then
            month = m
            goto 2
         endif
1     continue
      if (.not.found) then
         print *, 'could not find julian date ', juldate
         print *,' stopping'
         stop 1
      endif
2     continue
c
c     find the day
c
      day = mo(month) - (daycnt-juldate)
      iymd = iyear*10000 + month*100 + day 
c
c  Convert time to HHMMSS
c
      read (TimeTag(10:11),'(i2)') ihr
      read (TimeTag(13:14),'(i2)') imn
      read (TimeTag(16:17),'(i2)') isc
      ihms = ihr*10000 + imn*100 + isc

      return
      end
