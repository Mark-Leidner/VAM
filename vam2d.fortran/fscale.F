c!#   $Id: fscale.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: fscale.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine fscale(idim,rdata,scfact,fill)
c**********************************************************************
c English Name:  Scale Floating Point
c -------------
c
c Purpose: Scales data vector values by user specified factor.
c -------- Undefined values are not changed.
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
c     integer idim
c     real rdata(idim),scfact,fill
c
c     idim (I) - dimension of data vector.
c     rdata (IO) - on input, array of values to be scaled. On output,
c                  array of scaled values (value * scfact).
c     scfact (I) - scale factor
c     fill (I) value representing undefined data.
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
      integer idim
      real rdata(idim),scfact,fill
c
c     Local Variables
c     --------------- 
c
      logical FTEST
c
c     Scale the Data
c     ==============
c
      do 5 i = 1,idim
         if (FTEST(rdata(i),fill)) go to 5
         rdata(i) = rdata(i) * scfact
 5    continue

      return
      end
