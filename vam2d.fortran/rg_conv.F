c!#   $Id: rg_conv.F,v 1.2 1997/02/12 19:35:51 leidner Exp $
c!#   $Log: rg_conv.F,v $
c!#   Revision 1.2  1997/02/12 19:35:51  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function rg_conv(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
c**********************************************************************
c English Name: Regrid Conventional
c -------------
c
c Purpose: Calculates grid locations of the conventional observations
c -------- based on newly specified grid parameters (see "regrid.f").
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
c     integer rg_conv,nx0,iper0,ny0
c     real xs0,delx0,ys0,dely0
c
c     rg_conv (O) - function return value (number of observations
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
#include "sldta.h"
c
c     Argument List
c     -------------
c
      integer rg_conv,nx0,iper0,ny0
      real xs0,delx0,ys0,dely0
c
c     Local Variables
c     ---------------
c
      real lat,long

      rg_conv = 0
      ipt     = 0
c
c     Regrid conventional data points.
c     ================================
c
      do 5 n = 1,nptc
c
c        Calculate lat/lon associated with current grid coordinates.
c        -----------------------------------------------------------
c
         lat =ys0+dely0*(jlc(n)-1+yconv(n))
         long=xs0+delx0*(ilc(n)-1+xconv(n))
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

         ilc(ipt)   = igrid
         xconv(ipt) = xgrid
         jlc(ipt)   = jgrid
         yconv(ipt) = ygrid
         wconv(ipt) = wconv(n)
         iconv(ipt) = iconv(n)
         sl_qc(ipt) = sl_qc(n)
         uconv(ipt) = uconv(n)
         vconv(ipt) = vconv(n)

 5    continue

      rg_conv = ipt
      nptc    = ipt

      return
      end
