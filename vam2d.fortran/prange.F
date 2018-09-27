c!#   $Id: prange.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: prange.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine prange(descr,idim,rdata,fill)
c**********************************************************************
c English Name: Print Range
c -------------
c
c Purpose: Finds the minimum and maximum in an array of "idim"
c -------- contiguous real numbers (excluding fill values).
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Centel Federal Services Corporation) 
c
c Modified: June 1991 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      character*(*) descr
      integer idim
      real rdata(idim),fill
c
c     Local Variables
c     ---------------
c
      integer i,j
      real r
      logical noval
c 
c     Find min/max of data.
c     ---------------------
c
      i     = 0
      nfill = 0
      noval = .true.
      rmin  = fill 
      rmax  = fill

      do while (noval .and. i .lt. idim)

         i = i + 1
         r = rdata(i)
         if (abs(r-fill) .gt. 0.01) then 
            rmin = r
            rmax = r
            noval = .false.
         else
            nfill = nfill + 1
         endif

      enddo

      if (noval) return

      do 5 j = i+1,idim

         r = rdata(j)

         if (abs(r-fill) .le. 0.01) then 
            nfill = nfill + 1
            go to 5
         endif

         if (r .lt. rmin) rmin = r
         if (r .gt. rmax) rmax = r

 5    continue 

      print *, descr,' min = ',rmin,' max = ',rmax,' nfill = ',nfill

      return
      end 
