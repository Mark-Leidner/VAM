c!# CSU IDENTIFICATION : regrid.F
c!#     $Id: regrid.F,v 1.6 1997/07/08 17:29:59 leidner Exp $

c!## PURPOSE : Regrids entire problem. A different area is allowed if
c!# it is included in the old area.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      subroutine regrid                                                 !#

c!# Input/Output: by common blocks

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: regrid.F,v $
c!## CHANGE LOG : Revision 1.6  1997/07/08 17:29:59  leidner
c!## CHANGE LOG : updated documentation style
c!## CHANGE LOG :
c!#	Revision 1.5  1997/07/08  17:05:45  leidner
c!#	Added calculation of background vorticity tendency
c!#
c!#	Revision 1.4  1997/05/21  14:30:06  leidner
c!#	Added grid shifting, controlled by (gshift_dlon, gshift_dlat), and
c!#	regridding of NSCAT winds data
c!#
c!#	Revision 1.3  1997/02/21  21:54:59  leidner
c!#	added regridding for nscat data
c!#
c!#	Revision 1.2  1997/02/12  19:19:28  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "grdta.h"
#include "ptdta.h"
#include "sldta.h"
#include "er_bscat.h"
#include "ns_bscat.h"
#include "ns_winds.h"

c!## LOCAL DATA ELEMENTS :

c!# rg_*      functions for regridding data in the vam data buffers
c!# ss        sum of squared error from ssdtz
c!# swgt      sum of weights from ssdtz
c!# FILL      fill value for missing grid points
      integer rg_sass,rg_conv,rg_ers1,rg_nscat,rg_nscatw                !#
      real*8 ss, swgt                                                   !#
      data FILL/-777D20/                                                !#

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   1.  Initialize
c
      if (ngrdpt.le.0) call errsea (513,6HREGRID)
      write(6,110) 'Before Regridding'
      write(6,100) npts,nptsa,nptc,er_nobs,ns_nobs
c
      xs0   = xs
      delx0 = delx
      nx0   = nx
      iper0 = iper
      ys0   = ys
      dely0 = dely
      ny0   = ny
c
c     ------------------------------------------------------------------
c
c!#   2.  Shift grids
c
      do j = 1,ny
	 do i = 1,nx
	    delssu(i,j)=u(i,j)
	    delssv(i,j)=v(i,j)
	    u(i,j)=u0(i,j)
	    v(i,j)=v0(i,j)
	 end do
      end do
c
c     ------------------------------------------------------------------
c
c!#   3.  Set new grid parameters.
c
      call grparm
c
c     ------------------------------------------------------------------
c
c!#   4.  Interpolate U0 and V0 to new grid.
c
      call ginter (idim,u,xs0+gshift_dlon,delx0,nx0,iper0,
     &                    ys0+gshift_dlat,dely0,ny0,0,
     &             idim,u0,xs,delx,nx,ys,dely,ny,FILL)
      call ginter (idim,v,xs0+gshift_dlon,delx0,nx0,iper0,
     &                    ys0+gshift_dlat,dely0,ny0,0,
     &             idim,v0,xs,delx,nx,ys,dely,ny,FILL)
c
c     ------------------------------------------------------------------
c
c!#   5.  Interpolate U and V to new grid.
c
      call ginter (idim,delssu,xs0+gshift_dlon,delx0,nx0,iper0,
     &                         ys0+gshift_dlat,dely0,ny0,0,
     &             idim,u,xs,delx,nx,ys,dely,ny,FILL)
      call ginter (idim,delssv,xs0+gshift_dlon,delx0,nx0,iper0,
     &                         ys0+gshift_dlat,dely0,ny0,0,
     &             idim,v,xs,delx,nx,ys,dely,ny,FILL)
c
c     ------------------------------------------------------------------
c
c!#   6.  Initialize background vorticity tendency arrray.
c
      call ssdtz ( ss, swgt, delssu, delssv, u0, v0,
     &          cd, vel, dtz0, idim, init_dtz0 )
      write(*,*) 'init_dtz0, ss: ', init_dtz0, ss
c
c     ------------------------------------------------------------------
c
c!#   7.  Interpolate observational data to new grid.
c
      n1 = rg_sass(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
      n2 = rg_conv(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
      n3 = rg_ers1(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
      n4 = rg_nscat(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
      n5 = rg_nscatw(xs0,delx0,nx0,iper0,ys0,dely0,ny0)
c
c     ------------------------------------------------------------------
c
c!#   8.  Summary prints.
c
      write(6,110) 'After Regridding '
      write(6,100) npts,nptsa,nptc,er_nobs,ns_nobs,nsw_nwvc
c
 100  format(1x,'SASS Observations: ',i8,/,
     &       1x,'Non-Nadir SASS   : ',i8,/,
     &       1x,'Conventional     : ',i8,/,
     &       1x,'ERS-1            : ',i8,/,
     &       1x,'NSCAT sig0       : ',i8,/,
     &       1x,'NSCAT winds      : ',i8,/)
 110  format(1x,/,1x,a17,/,1x,17('='),/)
c
c     ------------------------------------------------------------------
c
c!## ERROR HANDLING :
c
      return
      end
