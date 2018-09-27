c!#   $Id: wgp_wrdata.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: wgp_wrdata.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine WGP_wrdata(iu,rlev,uqnt,vqnt,im,jnp,rbuf)
c**********************************************************************
c English cqnt: Write Data  (WGP - Write Grid in Phoenix)
c -------------
c
c Purpose: Writes U and V gridded wind fields from VAM data buffer
c -------- (see "grdta.h") to output data set in Phoenix format.
c
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
c     integer iu,im,jnp
c     character*8 uqnt,vqnt
c     real rlev,rbuf(im,jnp)
c
c     iu (I) - fortran unit number assigned to output "Phoenix" data.
c     im (I) - longitude dimension of wind field.
c     jnp (I) - latitude dimension of wind field.
c     rlev (I) - Phoenix level tag.
c     uqnt (I) - Phoenix 8-character quantity tag for U-component.
c     vqnt (I) - Phoenix 8-character quantity tag for V-component.
c     rbuf (O) - output buffer (must be atleast im*jnp 4byte words).    
c
c Programmer: Joseph V. Awrizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddawr Space Flight Center)
c
c Modified: April 13, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"

      real PHNXFILL
      parameter (PHNXFILL = 1.E+15)
c
c     Argument List
c     -------------
c
      real rlev
      integer iu,im,jnp
      real*4 rbuf(im,jnp)
      character*8 uqnt,vqnt
c
c     Local Variables
c     ---------------
c
      real*4 level
      real rlon,xlon,ylat
      integer i,j,newi,newj
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
c
c     Common Block - "Work Space"
c     ---------------------------
c
      common /work/ usurf(IBF,JBF),vsurf(IBF,JBF),idum(WBF-IBF*JBF*2)
c
c     Initialize
c     ==========
c
      level = rlev

      do j = 1,jnp
         do i = 1,im
            usurf(i,j) = PHNXFILL
            vsurf(i,j) = PHNXFILL
         end do
      end do
c
c     Extract VAM data into standard Phoenix grid.
c     ============================================
c
      ylat = ys + dely
      do j = 2,ny-1

         newj = (ylat + 90.0) / dely + 1.0
         xlon = xs

         do i = 1,nx-3

            rlon = xlon
            if (xlon .ge. 180.0) rlon = xlon - 360.0
            newi = (rlon + 180.0) / delx + 1.0

            usurf(newi,newj) = u(i,j)
            vsurf(newi,newj) = v(i,j)

            xlon = xlon + delx

         end do

         ylat = ylat + dely

      end do
c
c     Write data to output data set.
c     ==============================
c
      do j = 1,jnp
         do i = 1,im
            rbuf(i,j) = usurf(i,j)
         end do
      end do

      write(iu) level,uqnt,rbuf
      print *, 'vam(WGP_wrdata): ',level,' "',uqnt,'" ',im,jnp

      do j = 1,jnp
         do i = 1,im
            rbuf(i,j) = vsurf(i,j)
         end do
      end do

      write(iu) level,vqnt,rbuf
      print *, 'vam(WGP_wrdata): ',level,' "',vqnt,'" ',im,jnp
      

      return
      end
