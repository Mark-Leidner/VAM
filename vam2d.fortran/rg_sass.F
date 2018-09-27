c!#   $Id: rg_sass.F,v 1.2 1997/02/12 19:38:56 leidner Exp $
c!#   $Log: rg_sass.F,v $
c!#   Revision 1.2  1997/02/12 19:38:56  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function rg_sass(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
c**********************************************************************
c English Name: Regrid SASS
c -------------
c
c Purpose: Calculates grid locations of the SASS observations based
c -------- on newly specified grid parameters (see "regrid.f").
c
c Notes: 1) This routine assumes that "grparm.f" has been called
c ------    to read in the new grid parameters into "grparm.h".
c
c        2) This routine will eliminate observations not falling on the
c           new grid. These observations cannot be recovered!
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer rg_sass,nx0,iper0,ny0
c     real xs0,delx0,ys0,dely0
c
c     rg_sass (O) - function return value (number of observations
c                   falling within new grid boundaries).
c     xs0 (I) - starting longitude (degrees) of initial grid.
c     delx0 (I) - longitude spacing (degrees) of initial grid.
c     nx0 (I) - longitude dimension of initial grid.
c     iper0 (I) - period of initial grid.
c     ys0 (I) - starting latitude (degrees) of initial grid.
c     dely0 (I) - latitude spacing (degrees) of initial grid.
c     ny0 (I) - latitude dimension of initial grid.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: August 9, 1993 - created.
c ---------
c**********************************************************************
#include "vam.h"
#include "gparm.h"
#include "ptdta.h"
c
c     Argument List
c     -------------
c
      integer rg_sass,nx0,iper0,ny0
      real xs0,delx0,ys0,dely0
c
c     Local Variables
c     ---------------
c
      real lat,long

      rg_sass = 0
      nptsa   = 0
      ipt     = 0
c
c     Regrid SASS data points.
c     ========================
c
      do 5 n = 1,npts
c
c        Calculate lat/lon associated with current grid coordinates.
c        -----------------------------------------------------------
c
         lat =ys0+dely0*(jll(n)-1+ycoord(n))
         long=xs0+delx0*(ill(n)-1+xcoord(n))
c
c        Re-map current lat/lon to the new grid coordinates.
c        ---------------------------------------------------
c
         call gridx (long,xs,delx,iper,xmin,xmax,igrid,xgrid,*5)
         call gridx (lat ,ys,dely,   0,ymin,ymax,jgrid,ygrid,*5) 
c
c        Save all information associated with the current observation.
c        -------------------------------------------------------------
c
         ipt = ipt + 1

         ill(ipt)    = igrid
         xcoord(ipt) = xgrid
         jll(ipt)    = jgrid
         ycoord(ipt) = ygrid
         iobs(ipt)   = iobs(n)
         wgt(ipt)    = wgt(n)
         pt_qc(ipt)  = pt_qc(n)
         wgt(ipt)    = wgt(n)
         nwinds(ipt) = nwinds(n)
         vbar(ipt)   = vbar(n)
      
         do ia = 1,4
            uobs(ia,ipt) = uobs(ia,n)
            vobs(ia,ipt) = vobs(ia,n)
         end do

         if (nwinds(ipt) .gt. 0) nptsa = nptsa + 1

 5    continue

      rg_sass = ipt
      npts    = ipt

      return
      end
