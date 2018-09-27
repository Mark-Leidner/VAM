c!#   $Id: rg_nscat.F,v 1.4 1998/05/15 18:26:02 stanr Exp $
c!#   $Log: rg_nscat.F,v $
c!#   Revision 1.4  1998/05/15 18:26:02  stanr
c!#   added missing END
c!#
c!#   Revision 1.3  1997/06/23 17:51:04  leidner
c!#   changed ns_kp to 3-parameter kp
c!#
c!#	Revision 1.2  1997/05/21  14:31:10  leidner
c!#	bug fix (added resetting ns_nWVC for new grid)
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function rg_nscat(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
c**********************************************************************
c English Name: Regrid NSCAT
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
c     integer rg_nscat,nx0,iper0,ny0
c     real xs0,delx0,ys0,dely0
c
c     rg_nscat (O) - function return value (number of observations
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
c Modified: August 6, 1993 - created.
c ---------
c**********************************************************************
#include "vam.h"
#include "gparm.h"
#include "ns_bscat.h"
c
c     Argument List
c     -------------
c
      integer rg_nscat,nx0,iper0,ny0
      real xs0,delx0,ys0,dely0
c
c     Local Variables
c     ---------------
c
      real lat,long

      rg_nscat = 0
      ipt      = 0
c
c     Regrid NSCAT data points.
c     =========================
c
      do 3 i = 1, ns_nWVC
c
c        Calculate WVC lat/lon associated with current grid coordinates.
c        -----------------------------------------------------------
c
         lat =ys0+dely0*(WVC_jc(i)-1+WVC_yc(i))
         long=xs0+delx0*(WVC_ic(i)-1+WVC_xc(i))
c
c        Re-map WVC lat/lon to the new grid coordinates.
c        -----------------------------------------------
c
         call gridx (long,xs,delx,iper,xmin,xmax,igrid,xgrid,*3)
         call gridx (lat ,ys,dely,   0,ymin,ymax,jgrid,ygrid,*3) 

         ipt  = ipt + 1
         WVC_ic  (ipt)   = igrid
         WVC_xc  (ipt)   = xgrid
         WVC_jc  (ipt)   = jgrid
         WVC_yc  (ipt)   = ygrid
         WVC_lat (ipt)   = WVC_lat(i)
         WVC_lon (ipt)   = WVC_lon(i)

    3 continue

      ns_nWVC  = ipt


      ipt      = 0
      do 5 n = 1, ns_nobs

c
c        Calculate lat/lon associated with current grid coordinates.
c        -----------------------------------------------------------
c
         lat =ys0+dely0*(ns_jc(n)-1+ns_yc(n))
         long=xs0+delx0*(ns_ic(n)-1+ns_xc(n))
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

         ns_time (ipt)   = ns_time(i)
         ns_vel5 (ipt)   = ns_vel5(i)
         ns_dir5 (ipt)   = ns_dir5(i)
         ns_velbg(ipt)   = ns_velbg(i)
         ns_dirbg(ipt)   = ns_dirbg(i)
         ns_ic(ipt)     = igrid
         ns_xc(ipt)     = xgrid
         ns_jc(ipt)     = jgrid
         ns_yc(ipt)     = ygrid
         ns_lat(ipt)    = ns_lat(n)
         ns_lon(ipt)    = ns_lon(n)
         ns_qc(ipt)     = ns_qc(n)
         ns_tag(ipt)    = ns_tag(n)
            ns_ant(ipt)   = ns_ant(n)
            ns_theta(ipt) = ns_theta(n)
            ns_azim(ipt)  = ns_azim(n)
            ns_S0obs(ipt) = ns_S0obs(n)
            ns_kpa(ipt)    = ns_kpa(n)
            ns_kpb(ipt)    = ns_kpb(n)
            ns_kpc(ipt)    = ns_kpc(n)
            ns_s0sd(ipt)  = ns_s0sd(n) 
            ns_s05(ipt)   = ns_s05(n)
            ns_s0bg(ipt)  = ns_s0bg(n)

    5 continue

      rg_nscat = ipt
      ns_nobs  = ipt

      return
      end
