c!#   $Id: isrch.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: isrch.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function isrch(idim,ivec,ival)
c**********************************************************************
c English Name: Integer Search
c -------------
c
c Purpose: Searches for location of specified value in an array of
c -------- integer values.
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
c     integer isrch,ivec,ival
c
c     isrch (O) - function return value:
c
c           isrch = 0 (unable to locate value)
c                  >0 (index of "ivec" where "ival" first located).
c
c     idim (I) - number of values in "ivec".
c     ivec (I) - array of integer values to be searched.
c     ival (I) - integer value to be located in "ivec".
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
      integer isrch,idim,ivec(*),ival
 
      do isrch = 1,idim
         if (ivec(isrch) .eq. ival) return
      end do

      isrch = 0
      return
      end
