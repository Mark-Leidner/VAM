c!#   $Id: date2tau.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: date2tau.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function date2tau(idate,itime)
c**********************************************************************
c English Name: Date to Tau
c -------------
c
c Purpose: Converts conventional date/time to tau time.
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
c     real date2tau
c     integer idate,itime
c
c     date2tau (O) - function return value:
c
c          date2tau = (tau time)
c
c     idate (I) - date in YYMMDD format.
c     itime (I) - time in HHMMSS format.
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
      real date2tau
      integer idate,itime
c
c     Local Variables
c     ---------------
c
      integer ierr,ndate(4)
c
c     Convert date/time to tau.
c     =========================
c
      call getdat(idate,itime,ndate)
      call taudat(ierr,date2tau,ndate,1,1)

      return
      end
