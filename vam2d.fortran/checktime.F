c!#   $Id: checktime.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: checktime.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function checktime()
c**********************************************************************
c English Name: Check Time
c -------------
c
c Purpose: Checks current time against clock time and returns flag
c -------- indicating status. Clock time and current time are set
c          by "setclock" and "settime" entries within this function.
c
c Notes: 1) Please forgive the use of "entries". This is probably one
c ------    of the few instances where it seems appropriate.
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer checktime,idate,itime,izone
c
c     checktime (O) - function return value:
c
c         checktime = -1 (current time earlier than clock time or
c                         current time same as clock time but current
c                         time zone not equal clock time zone).
c
c                      0 (current time and zone match clock time and
c                         zone).
c
c                      1 (current time later than clock time).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 6, 1995 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      integer checktime
c
c     Local Variables
c     ---------------
c
      logical early,ontime,wrongzone
      integer tdate,ttime,tzone,cdate,ctime,czone,idate,itime,izone

      data tdate,ttime,tzone/0,0,0/
      data cdate,ctime,czone/0,0,0/
c
c     Check Time and return zone.
c     ===========================
c
      early     = (tdate .lt. cdate .or.
     &            (tdate .eq. cdate .and. ttime .lt. ctime))
      ontime    = (tdate .eq. cdate .and. ttime .eq. ctime)
      wrongzone = (tzone .ne. czone)

      checktime = 1
      if (ontime) checktime = 0
      if (early .or. (ontime .and. wrongzone)) checktime = -1

      return
c**********************************************************************
      entry setclock(idate,itime,izone)
c**********************************************************************
c English Name: Set Clock
c -------------
c
c Purpose: Sets clock time (see "checktime" routine).
c --------
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
c     integer idate,itime,izone
c
c     idate (I) - date in YYMMDD format.
c     itime (I) - time in HHMMSS format.
c     izone (I) - time zone flag (may be arbitrary qualifier).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 6, 1995 - created.
c ---------
c**********************************************************************

      cdate = idate
      ctime = itime
      czone = izone

      return
c**********************************************************************
      entry settime(idate,itime,izone)
c**********************************************************************
c English Name: Set Time
c -------------
c
c Purpose: Sets current time (see "checktime" routine).
c --------
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
c     integer idate,itime,izone
c
c     idate (I) - date in YYMMDD format.
c     itime (I) - time in HHMMSS format.
c     izone (I) - time zone flag (may be arbitrary qualifier).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 6, 1995 - created.
c ---------
c**********************************************************************

      tdate = idate
      ttime = itime
      tzone = izone

      return
      end
