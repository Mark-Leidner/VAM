c!#   $Id: rmean.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: rmean.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine rmean(ncnt,idim,rdata,ravg,fill)
c**********************************************************************
c English Name: Running Mean
c -------------
c
c Purpose: Computes a running mean by updating an array of averages
c -------- with new data. Checks are made for undefined (fill) values.
c          Once encountered, the average at that location is set to
c          "fill" and will not be updated again.
c
c Notes: 1) no checks made for bad sample size counter ("ncnt"). Must
c ------    never be zero.
c        2) sample size assumed to be the same for each array element.
c        3) data vector containing averages must not contain "fill"
c           values initially.
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer idim,ncnt
c     real rdata(idim),ravg(idim),fill
c
c     ncnt (I) - sample size counter incremented by user (initially
c                this should be set to 1).
c     idim (I) - number of points being averaged.
c     rdata (I) - vector containing new data to be summed into average.
c     ravg (IO) - on input, this vector contains current averages. On
c                 output, this vector contains the updated mean. This
c                 array need not be initialized to zeroes.
c     fill (I) - value representing undefined data.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: September 30, 1992 - created.
c ---------
c**********************************************************************
c
c     Argument List
c     -------------
c
      integer idim,ncnt
      real rdata(idim),ravg(idim),fill
c
c     Local Variables
c     ---------------
c
      logical FTEST
c
c     Update the average.
c     ===================
c
      do 5 i = 1,idim

         if (FTEST(rdata(i),fill)) ravg(i) = fill
         if (FTEST(ravg(i),fill)) go to 5
         rsum = ravg(i) * float(ncnt - 1) + rdata(i)
         ravg(i) = rsum / float(ncnt)

 5    continue

      return
      end
