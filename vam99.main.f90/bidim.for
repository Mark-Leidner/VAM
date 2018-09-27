c     ******************************************************************

<NLM> SUBROUTINE bidim
<LTM> SUBROUTINE bidimtl
<ADJ> SUBROUTINE bidimad

c**** bidim interpolates the analysis to one observation location

c!#   $Id: bidim.for,v 1.4 1999/03/17 22:34:31 rnh Exp $

c     PURPOSE

c     Interpolates gridded analysis to an observation point.
c     
c**   INTERFACE

     C     ( nx, ny,
     I     fgrid, i, j,
     O     fi )

      IMPLICIT NONE

cPER> LINEAR ROUTINE - NO TRAJECTORY VALUES !!!

c!#~  nx        number of longitude grid points
c!#~  ny        number of latitude grid points
c!#~  fgrid     gridpoint values
c!#~  i         longitude index
c!#~  j         latitude index
c!#~  fi        interpolated value at (xobs,yobs)
c     i,j       indicate the lower left-hand corner of grid box
      INTEGER, INTENT(IN) :: nx,ny,i,j
<NLM> REAL, INTENT(IN) :: fgrid(nx,ny)
<NLM> REAL, INTENT(OUT) :: fi
<LTM> REAL, INTENT(IN) :: fgrid(nx,ny)
<LTM> REAL, INTENT(OUT) :: fi
<ADJ> REAL, INTENT(INOUT) :: fgrid(nx,ny)
<ADJ> REAL, INTENT(INOUT) :: fi

c!#~   ii,jj    stencil indices
      INTEGER ii,jj

c     ------------------------------------------------------------------

c     1. Interpolate analysis to location of observation.

<NLM> fi = 0
<LTM> fi = 0
<ADJ> fi = 0
      DO ii=1,nw
        DO jj=1,nw
<NLM>     fi = fi + wi(ii,jj)*fgrid(i-nw/2+ii,j-nw/2+jj)
<LTM>     fi = fi + wi(ii,jj)*fgrid(i-nw/2+ii,j-nw/2+jj)
<ADJ>     fgrid(i-nw/2+ii,j-nw/2+jj) +=
<ADJ>&      wi(ii,jj)*fi
        END DO
      END DO

<NLM> END SUBROUTINE bidim
<LTM> END SUBROUTINE bidimtl
<ADJ> END SUBROUTINE bidimad
