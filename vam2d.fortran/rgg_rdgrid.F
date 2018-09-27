c!# CSU IDENTIFICATION : rgg_rdgrid.F
c!#     $Id: rgg_rdgrid.F,v 1.4 1997/07/08 16:43:12 leidner Exp $

c!## PURPOSE : checks in a global analysis from NCEP

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!# Reads U and V gridded wind fields from input NCEP GDAS
c!# data set into VAM data buffers (see "grdta.h") for the 
c!# specified date/time.

c!## CONSTRAINTS :

c!# The input NCEP data must be in unpacked GRIB format.

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      function RGG_rdgrid                                               !#
     I    (idate,itime)                                                 !#

c!# Input:
c!# idate     desired date in yymmdd format
c!# itime     desired time in hhmmss format
      integer idate, itime                                              !#

c!# Output: Normally 0, otherwise see ERROR HANDLING
      integer RGG_rdgrid                                                !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!# This routine invokes the subroutine "grparm" which will
c!# read important namelist parameters needed by this routine.
c!# The namelist parameters are stored in "gparm.h".

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: rgg_rdgrid.F,v $
c!## CHANGE LOG : Revision 1.4  1997/07/08 16:43:12  leidner
c!## CHANGE LOG : updated documentation style
c!## CHANGE LOG :
c!#	Revision 1.3  1997/07/07  17:48:27  leidner
c!#	Added calculation of background vorticity tendency
c!#
c!#	Revision 1.2  1997/04/14  16:10:52  leidner
c!#	added time check, cleaned up code and documented
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "grname.h"
#include "grdta.h"
      common /work/ usurf(IBF,JBF),vsurf(IBF,JBF),idum(WBF-IBF*JBF*2)

c!## LOCAL DATA ELEMENTS :

c!# fill      fill value for missing grid points
c!# im        x dimension of grid
c!# jnp       y dimension of grid
c!# kgds      grid description section
c!# kpds      product description section
c!# ss        sum of squared error from ssdtz
c!# swgt      sum of weights from ssdtz
      real fill                                                         !#
      real*8 ss, swgt                                                   !#
      integer im,jnp                                                    !#
      integer*4 kpds(100),kgds(200)                                     !#
      data fill   /-777D20/                                             !#

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   1.  Retrieve namelist parameters / initialize
c
      RGG_rdgrid = 0
      call grparm
c
c     ------------------------------------------------------------------
c
c!#   2.  Establish I/O link
c
 5    continue
      ierr = ioopen(iu)
      if (ierr .eq. 2) go to 902
      if (ierr .ne. 0) go to 901
c
c     ------------------------------------------------------------------
c
c!#   3.  Read unpacked GRIB File
c
      call read_gdas
     I    ( iu,
     O    im, jnp, kpds, kgds, u, v)
c
c     ------------------------------------------------------------------
c
c!#   4.  Extract needed information from kpds and kgds
c
      iyr = kpds(8)
      imo = kpds(9)
      idy = kpds(10)
      ihr = kpds(11)
      im  = kgds(2)
      jnp = kgds(3)
c
c     ------------------------------------------------------------------
c
c!#   5.  Check available storage space
c
      if (im .gt. IBF .or. jnp .gt. JBF) go to 904
c
c     ------------------------------------------------------------------
c
c!#   6.  Check date / time
c
      kdate = iyr*10000 + imo*100 + idy
      ktime = ihr*10000
      if (idate .ne. kdate .or. itime .ne. ktime) go to 903
c
c     ------------------------------------------------------------------
c
c!#   7.  Store grid parameters
c 
      nx0   = im
      ny0   = jnp
      jper0 = 0
      xs0   = 0.0
      ys0   = 90.0
      iper0 = im
      delx0 = 360.0 / float(im)
      dely0 = -180.0 / float(jnp-1)
c
c     ------------------------------------------------------------------
c
c!#   8.  Interpolate to desired background resolution
c
      call ginter (im,u,xs0,delx0,nx0,iper0,ys0,dely0,
     &   ny0,0,IBF,u0,xs,delx,nx,ys,dely,ny,fill)
      call ginter (im,v,xs0,delx0,nx0,iper0,ys0,dely0,
     &   ny0,0,IBF,v0,xs,delx,nx,ys,dely,ny,fill)
c
c     ------------------------------------------------------------------
c
c!#   9.  Initialize analysis grid to background grid
c
      do j = 1,ny
         do i = 1,nx
            u(i,j) = u0(i,j)
            v(i,j) = v0(i,j)
         end do
      end do
c
      write (*,100) xs,delx,nx,iper,ys,dely,ny,jper,u(1,1),v(1,1)
c
c!#   9.1.  Initialize background vorticity tendency arrray
c
      call ssdtz ( ss, swgt, delssu, delssv, u0, v0,
     &          cd, vel, dtz0, idim, init_dtz0 )
      write(*,*) 'init_dtz0, ss: ', init_dtz0, ss
      go to 999
c
c     ------------------------------------------------------------------
c
c!#  10.  End of routine / error control
c
c!## ERROR HANDLING :
c
c!# RGG_rdgrid function return value
c!#          =  0 (normal)
c!#          =  2 (end of data; requested date/time not found)
c!#          =  3 (requested date/time not found)
c!#          = -1 (error)
c!#          = -2 (max dimensions exceeded)

 900  continue
      call ioclose(iu)
      go to 5

 901  continue
      RGG_rdgrid = -1
      go to 999

 902  continue
      RGG_rdgrid = 2
      print *, 'vam(RGG_rdgrid): End of Data'
      print *, 'vam(RGG_rdgrid): date/time not found: ',idate,itime
      print *, 'vam(RGG_rdgrid): input file date/time: ',kdate,ktime

 903  continue
      RGG_rdgrid = 3
      print *, 'vam(RGG_rdgrid): date/time not found: ',idate,itime
      print *, 'vam(RGG_rdgrid): input file date/time: ',kdate,ktime
      print *, 'vam(RGG_rdgrid): unit = ',iu
      go to 999

 904  continue
      RGG_rdgrid = -2
      print *, 'vam(RGG_rdgrid): max dimensions exceeded.'
      go to 999

 100  format(/,1x,'Grid Parameters: ',2e18.7,2i6,
     &       /,16x,2e18.7,2i6,
     &       //,1x,'Sample Values: ',2e18.7)

 999  return
      end
