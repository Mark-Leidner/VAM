c!#   $Id: interp.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: interp.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function interp(rdata1,rdata2,loc1,loc2,newloc)
c**********************************************************************
c English Name: Interpolate (Utility Routine)
c -------------
c
c Purpose: Performs linear interpolation to new location given the data
c -------- and locations of two other points.
c
c Notes: 1) If the logarithm of the locations is passed in, this
c ------    becomes a linear in log interpolation etc..
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     real interp,rdata1,rdata2,loc1,loc2,newloc
c
c      interp (OUTPUT) function return value containing the inter-
c                        polated data at the new location.
c
c      rdata1 (INPUT) data value at location 1 (loc1).
c
c      rdata2 (INPUT) data value at location 2 (loc2).
c
c      loc1 (INPUT) location of rdata1 value.
c
c      loc2 (INPUT) location of rdata2 value.
c
c      newloc (INPUT) destination location that returned value resides
c                    at after interpolation.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Centel Federal Services Corporation)
c
c Modified: 5/9/89 - created
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      real interp,rdata1,rdata2,loc1,loc2,newloc
c
c     Local Variables
c     ---------------
c
      real slope,intercept
c
c     Interpolate
c     -----------
c
      if (loc1 .eq. loc2) then 
         interp = rdata1
      else
         slope = (rdata2-rdata1) / (loc2-loc1)
         intercept = rdata1 - slope*loc1
         interp = slope*newloc + intercept
      endif

      return
      end
