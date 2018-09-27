c!#   $Id: synoptic.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: synoptic.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function synoptic(inchrs,idate,itime)
c**********************************************************************
c English Name: Synoptic Time
c -------------
c
c Purpose: Calculates the synoptic time by rounding the date/time to
c -------- to nearest synoptic hour that is a multiple of the specified
c          hourly increment.
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
c     real synoptic
c     integer inchrs,idate,itime
c
c     synoptic (O) - function return value: deviation (in hours)
c                    from the bin hour.   
c     inchrs (I) - desired hourly increment for spacing the temporal
c                  bins.
c     idate (IO) - on input, this is the date in yymmdd format. On
c                  output, this is the adjusted date based on the 
c                  assigned hourly bin.
c     itime (IO) - on input, this is the time in hhmmss format. On
c                  output, this is the adjusted time based on the 
c                  assigned hourly bin (mmss = 0000).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: December 1, 1992 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      real synoptic
      integer inchrs,idate,itime
c
c     Local Variables
c     ---------------
c
      real midtime
c
c     Extract temporal components.
c     ============================
c
      call decode(idate,iyear,imon,iday)
      call decode(itime,ihour,imin,isec)
      hour     = float(ihour) + float(imin)/60.0 + float(isec)/3600.0
      midtime  = float(inchrs) / 2.0
      hinc     = float(inchrs)
c
c     Determine the closest hourly bin.
c     =================================
c
      xrem = amod(hour,hinc)
      if (xrem .ge. midtime) then 
         hour = hour + (hinc - xrem)
         synoptic = -(hinc - xrem)
      else
         hour = hour - xrem
         synoptic = xrem
      endif
c
c     Set the new date/time.
c     ======================
c
      idate = iyear*10000 + imon*100 + iday
      itime = int(hour)*10000
      call inctime(idate,itime,0,0,0)

      return
      end
