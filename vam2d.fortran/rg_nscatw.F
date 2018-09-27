c!#   $Id: rg_nscatw.F,v 1.2 1998/05/15 18:26:02 stanr Exp $
c!#   $Log: rg_nscatw.F,v $
c!#   Revision 1.2  1998/05/15 18:26:02  stanr
c!#   added missing END
c!#
c!#   Revision 1.1  1997/05/21 14:25:16  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function rg_nscatw(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
c**********************************************************************
c English Name: Regrid NSCAT winds
c -------------
c
c Purpose: Calculates grid locations of the NSCAT observations based
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
c     integer rg_nscatw,nx0,iper0,ny0
c     real xs0,delx0,ys0,dely0
c
c     rg_nscatw (O) - function return value (number of observations
c                   falling within new grid boundaries).
c     xs0 (I) - starting longitude (degrees) of initial grid.
c     delx0 (I) - longitude spacing (degrees) of initial grid.
c     nx0 (I) - longitude dimension of initial grid.
c     iper0 (I) - period of initial grid.
c     ys0 (I) - starting latitude (degrees) of initial grid.
c     dely0 (I) - latitude spacing (degrees) of initial grid.
c     ny0 (I) - latitude dimension of initial grid.
c
c Programmer: S. Mark Leidner
c ----------- (AER, Inc.)
c
c Modified: May 21, 1997 - created (based on rg_ers1.F by JVA)
c ---------
c**********************************************************************
#include "vam.h"
#include "gparm.h"
#include "ns_winds.h"
c
c     Argument List
c     -------------
c
      integer rg_nscatw,nx0,iper0,ny0
      real xs0,delx0,ys0,dely0
c
c     Local Variables
c     ---------------
c
      real lat,long

      rg_nscatw = 0
      ipt       = 0
c
c     Regrid NSCAT data points.
c     =========================
c
      do 3 i = 1, nsw_nwvc
c
c        Calculate WVC lat/lon associated with current grid coordinates.
c        -----------------------------------------------------------
c
         lat =ys0+dely0*(nsw_jc(i)-1+nsw_yc(i))
         long=xs0+delx0*(nsw_ic(i)-1+nsw_xc(i))
c
c        Re-map WVC lat/lon to the new grid coordinates.
c        -----------------------------------------------
c
         call gridx (long,xs,delx,iper,xmin,xmax,igrid,xgrid,*3)
         call gridx (lat ,ys,dely,   0,ymin,ymax,jgrid,ygrid,*3) 

         ipt  = ipt + 1
         nsw_ic  (ipt)   = igrid
         nsw_xc  (ipt)   = xgrid
         nsw_jc  (ipt)   = jgrid
         nsw_yc  (ipt)   = ygrid
         nsw_lat (ipt)   = nsw_lat(i)
         nsw_lon (ipt)   = nsw_lon(i)
         nsw_col (ipt)   = nsw_col(i)
         nsw_nambigs(ipt)= nsw_nambigs(i)
         nsw_qc  (ipt)   = nsw_qc(i)
         nsw_row (ipt)   = nsw_row(i)
         nsw_tag (ipt)   = nsw_tag(i)
         nsw_time(ipt)   = nsw_time(i)
         do j=1,4
            nsw_mle (ipt,j) = nsw_mle(i,j)
            nsw_u   (ipt,j) = nsw_u(i,j)
            nsw_v   (ipt,j) = nsw_v(i,j)
         end do
    3 continue

      rg_nscatw = ipt
      nsw_nwvc  = ipt

      return
      end
