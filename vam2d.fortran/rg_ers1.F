c!#   $Id: rg_ers1.F,v 1.2 1997/02/12 19:37:34 leidner Exp $
c!#   $Log: rg_ers1.F,v $
c!#   Revision 1.2  1997/02/12 19:37:34  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function rg_ers1(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
c**********************************************************************
c English Name: Regrid ERS1
c -------------
c
c Purpose: Calculates grid locations of the ERS1 observations based
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
c     integer rg_ers1,nx0,iper0,ny0
c     real xs0,delx0,ys0,dely0
c
c     rg_ers1 (O) - function return value (number of observations
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
#include "er_bscat.h"
c
c     Argument List
c     -------------
c
      integer rg_ers1,nx0,iper0,ny0
      real xs0,delx0,ys0,dely0
c
c     Local Variables
c     ---------------
c
      real lat,long

      rg_ers1 = 0
      ipt     = 0
c
c     Regrid ERS1 data points.
c     ========================
c
      do 5 n = 1,er_nobs
c
c        Calculate lat/lon associated with current grid coordinates.
c        -----------------------------------------------------------
c
         lat =ys0+dely0*(er_jc(n)-1+er_yc(n))
         long=xs0+delx0*(er_ic(n)-1+er_xc(n))
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

         er_ic(ipt)     = igrid
         er_xc(ipt)     = xgrid
         er_jc(ipt)     = jgrid
         er_yc(ipt)     = ygrid
         er_lat(ipt)    = er_lat(n)
         er_lon(ipt)    = er_lon(n)
         er_velobs(ipt) = er_velobs(n)
         er_dirobs(ipt) = er_dirobs(n)
         er_time(ipt)   = er_time(n)
         er_vel5(ipt)   = er_vel5(n)
         er_dir5(ipt)   = er_dir5(n)
         er_velbg(ipt)  = er_velbg(n)
         er_dirbg(ipt)  = er_dirbg(n)
         er_qc(ipt)     = er_qc(n)
         er_tag(ipt)    = er_tag(n)

         do i = 1,ER_MBEAM

            er_ant(ipt,i)   = er_ant(n,i)
            er_theta(ipt,i) = er_theta(n,i)
            er_azim(ipt,i)  = er_azim(n,i)
            er_S0obs(ipt,i) = er_S0obs(n,i)
            er_kp(ipt,i)    = er_kp(n,i)
            er_s0sd(ipt,i)  = er_s0sd(n,i) 
            er_s05(ipt,i)   = er_s05(n,i)
            er_s0bg(ipt,i)  = er_s0bg(n,i)

         end do

 5    continue

      rg_ers1 = ipt
      er_nobs = ipt

      return
      end
