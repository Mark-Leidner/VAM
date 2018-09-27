c     ******************************************************************

      SUBROUTINE bidimtl

c**** bidim interpolates the analysis to one observation location

c!#   $Id: bidimtl.F,v 1.1 2000/11/06 19:51:41 mcc Exp $

c     PURPOSE

c     Interpolates gridded analysis to an observation point.
c     
c**   INTERFACE

     C     ( nx, ny,
     I     fgrid, i, j,
     O     fi )

      IMPLICIT NONE

c     LINEAR ROUTINE - NO TRAJECTORY VALUES !!!

c!#~  nx        number of longitude grid points
c!#~  ny        number of latitude grid points
c!#~  fgrid     gridpoint values
c!#~  i         longitude index
c!#~  j         latitude index
c!#~  fi        interpolated value at (xobs,yobs)
c     i,j       indicate the lower left-hand corner of grid box
      INTEGER, INTENT(IN) :: nx,ny,i,j
      REAL, INTENT(IN) :: fgrid(nx,ny)
      REAL, INTENT(OUT) :: fi

c!#~   ii,jj    stencil indices
      INTEGER ii,jj

c     ------------------------------------------------------------------

c     1. Interpolate analysis to location of observation.

      fi = 0
      DO ii=1,nw
        DO jj=1,nw
          fi = fi + wi(ii,jj)*fgrid(i-nw/2+ii,j-nw/2+jj)
        END DO
      END DO

      END SUBROUTINE bidimtl
