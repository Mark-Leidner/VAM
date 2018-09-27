c!#   $Id: setfill.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: setfill.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine setfill(idim,rdata,fill)
c**********************************************************************
c English name: Set Fill Value
c -------------
c
c Purpose: Searches input array for one of several possible fill
c -------- values and upon discovery, sets the value to a universal
c          fill value.
c
c Notes: 1) **Warning: it is possible that a real data value could be
c ------    interpreted as a fill value using this routine.
c        2) Be carefull that your data doesn't contain fill values 
c           not accounted for in this routine.
c
c Variable Description:
c ---------------------
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
      real rdata(idim),fill
c
c     Local Variables
c     ---------------
c
      logical FTEST

      data fill1/-999.9/
      data fill2/1.e+10/
      data fill3/1.e+15/

      do 5 i = 1,idim
         rval = rdata(i)
         if (FTEST(rval,fill1)) rdata(i) = fill
         if (FTEST(rval,fill2)) rdata(i) = fill
         if (FTEST(rval,fill3)) rdata(i) = fill
         if (rval .gt. 1.e+12) rdata(i) = fill
 5    continue

      return
      end
