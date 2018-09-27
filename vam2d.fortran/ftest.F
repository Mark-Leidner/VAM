c!#   $Id: ftest.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: ftest.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function FTEST(val1,val2)
c**********************************************************************
c English Name: Floating Point Test
c -------------
c
c Purpose: Compares two floating point numbers and returns "true" if
c -------- the numbers are identical (within "delta" of each other).
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddard Space Flight Center)
c
c Modified: May 13, 1992 - created.
c ---------
c**********************************************************************
      parameter (DELTA = 0.01)
c
c     Argument List
c     -------------
c
      logical FTEST
      real val1,val2

      FTEST = .false.
      if ( abs(val1 - val2) .le. DELTA) FTEST = .true.

      return
      end
