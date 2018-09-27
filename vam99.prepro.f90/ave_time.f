c!#
c!#   CSU IDENTIFICATION : ave_time

c!##  PURPOSE : computes the average (central) time for one orbit 
c!#             of scatterometer data and the bracketing analysis
c!#             times.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran 90

c!#   CSU DESIGN : adapted from nscat_ave_time.f 

c!##  INPUT/OUTPUT INTERFACE :

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      program ave_time
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c!#   Inputs: 
c!#   Beginning and ending times of orbit as command line
c!#   arguments "-b" and "-e".  Times must be in the format 

c!#   YYYY-DDDThh:mm:ss.sss - 21 byte character field
c!#   where:
c!#       YYYY is the year, DDD is day of year (Julian),
c!#       hh is hour (UTC), mm is minutes and ss.sss is seconds

c!#   (see Merida, 1996).

c!#   The rev number and analysis 'interval' are supplied at the
c!#   command line.

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!##  REFERENCES :

c!#   Merida, S. and K. Fry, "NASA Scatterometer Science Data
c!#   Processing System SCLK_UTC File Format", JPL D-12990, 1996.

c!#   "NASA Scatterometer Science Data Product User's Manual;
c!#   Overview & Geophysical Data Products", JPL D-12985 v1.1, 1997.

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c
c $Id: ave_time.f,v 1.2 2001/08/07 16:56:05 mcc Exp $
c 
c $Log: ave_time.f,v $
c Revision 1.2  2001/08/07 16:56:05  mcc
c Incorporated changes to account for leap year.
c
c Revision 1.1  1999/10/25 16:35:49  mcc
c Initial revision
c
c
c!##  GLOBAL AND SHARED DATA :
      USE grid_hdr_typ_m   !#
      USE delta_time_m

c!##  LOCAL DATA ELEMENTS :

      implicit none

c!#    Command line processing variables:
c!#~   argv      command line argument holder
c!#~   btime     date/time at beginning of data in JPL format
c!#~   etime     date/time at end of data in JPL format
c!#~   iarg      counter for command line arguments
c!#~   iargc     external function for handling command line args
c!#~   inlenb    length of beginning time character string
c!#~   inlene    length of ending time character string
c!#~   inlenr    length of rev number character string
c!#~   intvc     analysis interval in hours (character string)
c!#~   interval  analysis interval in hours (integer)
c!#~   nargs     number of arguments in command line
c!#~   rev       rev number for this orbit (integer)
c!#~   revc      rev number for this orbit (character string)

      character*256 :: argv
      character*24 :: btime, etime, revc, intvc
      character (len=14) :: janone_tim, central_tim
      character (len=14) :: prev_native_tim, next_native_tim
      integer :: iarg, iargc, inlenb, inlene, inlenr, nargs, rev

c!#    Date/time variables:
c!#~   idate     date of central time of data in yyyymmdd
c!#~   ihmsb     beginning time of data hhmmss
c!#~   ihmse     ending time of data hhmmss
c!#~   ihrb      beginning time of data hours
c!#~   ihre      ending time of data hours
c!#~   imnb      beginning time of data minutes
c!#~   imne      ending time of data minutes
c!#~   iscb      beginning time of data seconds
c!#~   isce      ending time of data seconds
c!#~   itime     central time of data in hhmmss
c!#~   iymdb     beginning date of data yymmdd
c!#~   iymde     ending date of data yymmdd
c!#~   jdb       julian day at beginning of data
c!#~   jde       julian day at end of data
c!#~   avetim    central time of data in seconds from the start of yyyy
c!#~   sb        beginning time of data in seconds from the start of yyyy;
c!#~   sb        (also used as temporary dummy time variable wrt yyyy)
c!#~   sb_hold   temporary time of data in seconds from the start of yyyy
c!#~   se        ending time of data in seconds from the start of yyyy

      integer :: ierr
      integer :: idate, ihmsb, ihmse, ihrb, ihre, imnb, imne, iscb
      integer :: isce, itime, iymdb, iymde, jdb, jde, avetim, sb, se

c!#    Parameters:
c!#~   secd      seconds in 1 day
c!#~   sech      seconds in 1 hour
c!#~   secm      seconds in 1 minute

      integer, parameter :: secm=60
      integer, parameter :: sech=60*secm
      integer, parameter :: secd=24*sech
      integer, parameter :: lunout=10

      integer :: date1, date2, time1, time2
      integer :: rem, s_next, s_prev, ints, interval 
      logical :: leapyr

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

      
c!#   process command line arguments

      nargs = iargc()
      do iarg = 1, nargs
         call getarg (iarg, argv)
         if (argv(1:2) == '-R') then
            revc = argv(3:)
            inlenr = index(revc,' ') - 1
         else if (argv(1:2) == '-B') then
            btime = argv(3:)
            inlenb = index(btime,' ') - 1
         else if (argv(1:2) == '-E') then
            etime = argv(3:)
            inlene = index(etime,' ') - 1
         else if (argv(1:2) == '-T') then
            intvc = argv(3:)
            inlene = index(intvc,' ') - 1
         else
            print *, 'Unknown argument: ', argv(1:index(argv,' ')-1)
            print *, 'legal arguments: -Rrev -Bbegin_time -Eend_time'
            stop
         endif
      enddo

c!#   exit if required command-line inputs missing

      if (inlenr < 0) then
          print *, 'Error: missing -Rrev'
          stop
      endif
      if (inlenb < 0) then
          print *, 'Error: missing -Bbegin_time'
          stop
      endif
      if (inlene < 0) then
          print *, 'Error: missing -Eend_time'
          stop
      endif

      read (revc, '(i8)') rev
      read (intvc, '(i8)') interval

c!#   determine if begin time occurs in a leap year
      read (btime(1:4),'(i4.4)') iymdb
      leapyr = ((mod(iymdb,4) .eq. 0 .and. mod(iymdb,100) .ne. 0)
     &           .or. mod(iymdb,400) .eq. 0)

c!#   convert jpl time format to (julday, yymmdd, hhmmss, hh, mm, ss).
c!#   beginning and ending times in seconds from the start of yyyy.

      read (btime(06:08),'(i3.3)') jdb
      read (btime(10:11),'(i2.2)') ihrb
      read (btime(13:14),'(i2.2)') imnb
      read (btime(16:17),'(i2.2)') iscb
      sb = (jdb-1)*secd + ihrb*sech + imnb*secm + iscb
      if (leapyr .and. (jdb .gt. 59)) sb = sb - secd

c!#   determine if end time occurs in a leap year
      read (etime(1:4),'(i4.4)') iymde
      leapyr = ((mod(iymde,4) .eq. 0 .and. mod(iymde,100) .ne. 0)
     &           .or. mod(iymde,400) .eq. 0)

      read (etime(06:08),'(i3.3)') jde
      read (etime(10:11),'(i2.2)') ihre
      read (etime(13:14),'(i2.2)') imne
      read (etime(16:17),'(i2.2)') isce
      se = (jde-1)*secd + ihre*sech + imne*secm + isce
      if (leapyr .and. (jde .gt. 59)) se = se - secd

c!#   determine central time and date.
c!#   check to see if rev crosses into next year.

      if ( se .lt. sb ) then
         print*,' rev time crosses into next year ',jdb,' ',jde
         if ( jdb .le. jde ) stop 
     &       'begin julday le end julday at year change'
         se = se + jdb * 86400
      endif
      avetim = ( sb + se ) / 2

c!#   extract yyyy from header of first native file to set time in 
c!#   seconds from start of january 1, yyyy. 

      janone_tim(1:14) = btime(1:4)//'0101000000'
      call delta_time(janone_tim,-1,avetim,central_tim,ierr)    
      read (central_tim(01:08),'(i8.8)') idate
      read (central_tim(09:14),'(i6.6)') itime

c!#   compute bracketing analysis times 

      ints = interval * 3600
      rem = mod(avetim,ints)

c!#   compute seconds from start of yyyy to previous and next
c!#   analysis times

      s_prev = avetim - rem
      s_next = avetim + (ints - rem)

      call delta_time(janone_tim,-1,s_prev,prev_native_tim,ierr)    
      read (prev_native_tim(01:08),'(i8.8)') date1
      read (prev_native_tim(09:14),'(i6.6)') time1

      call delta_time(janone_tim,-1,s_next,next_native_tim,ierr)    
      read (next_native_tim(01:08),'(i8.8)') date2
      read (next_native_tim(09:14),'(i6.6)') time2

c!#   write date/time and central time to output file

      open (unit=lunout, file='ave_time.dat', form='formatted')
      write(lunout,100) 
     & rev, idate, itime, date1, time1, date2, time2, interval
      close (lunout)

 100  format(i8,3(i10,i8),i3)

      end
c

