c!#   $Id: tau2date.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: tau2date.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine tau2date(tau,idate,itime)
c**********************************************************************
c English Name: Tau to Date
c -------------
c
c Purpose: Converts tau time to conventional date/time.
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
c     real tau
c     integer idate,itime
c
c     tau (I) - tau time.
c     idate (O) - returned date in YYMMDD format.
c     itime (O) - returned time in HHMMSS format.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified:
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      real tau
      integer idate,itime
c
c     Local Variables
c     ---------------
c
      integer ierr,ndate(4)
c
c     Convert tau to date/time.
c     =========================
c
      call taudat(ierr,tau,ndate,1,2)
    
      idate = ndate(3)*10000 + ndate(1)*100 + ndate(2)
      itime = ndate(4)*10000

      return
      end
