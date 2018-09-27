      MODULE interp_mod

      USE constants, ONLY: pi

      IMPLICIT NONE
      PRIVATE
      PUBLIC gridcell_init,interpgrid,igrid,xgrid,same
      PUBLIC uvinterp,uvinterptl,uvinterpad

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c!#   Interpolation to observation points:
c!#~   lonmin      minimum longitude for data acceptance
c!#~   lonmax      maximum longitude for data acceptance
c!#~   lonper      periodicity in grid units
c!#~   latmin      minimum latitude for data acceptance
c!#~   latmax      maximum latitude for data acceptance
      REAL, PUBLIC, SAVE :: lonmin,lonmax,latmin,latmax,lonper

c!##  LOCAL DATA:
c!#~   interp    interpolation method
c!#~   interp.   (1=>bilinear;2=>quasi-bicubic)
c!#~   nw        size of stencil
c!#~   wi        weights for interpolation 
      INTEGER, SAVE :: interp=0, nw
      REAL, SAVE, ALLOCATABLE :: wi(:,:)

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE gridcell_init (xmin,xmax,ymin,ymax,xs,dx,nx,ys,dy,ny,
     &    interp_method)

      REAL, INTENT(IN) :: xmin,xmax,ymin,ymax,xs,dx,ys,dy

      INTEGER, INTENT(IN) :: nx,ny,interp_method

c     Local variables
      LOGICAL lerr
      INTEGER ierr
c!#~  x0,x1,y0,y1 extreme grid coordinates for interpolation
      REAL :: x0,x1,y0,y1

      interp = interp_method

c     ------------------------------------------------------------------

c     Allocate the weights and
c     set the extreme grid coordinates for interpolation

      ierr = 0
      IF (ALLOCATED(wi)) DEALLOCATE(wi, STAT=ierr)
      IF (ierr.NE.0) STOP 'gridcell_init: DEALLOCATE (wi, ...'

      IF (interp .EQ. 1) THEN
        nw=2; x0=1; x1=nx  ; y0=1; y1=ny
      ELSE IF (interp .EQ. 2) THEN
        nw=4; x0=2; x1=nx-1; y0=2; y1=ny-1
      ELSE
        write (*,*) 'VAM: gridcell_init: illegal value of interp: ',
     &      interp
        stop 'gridcell_init: illegal value of interp'
      END IF
      ALLOCATE(wi(nw,nw), STAT=ierr)
      IF (ierr.NE.0) STOP 'gridcell_init: ALLOCATE (wi(nw,nw), ...'
      
c!#   lonmin, lonmax, lonper, latmin, latmax are all in grid units
      lonmin=xgrid(xmin,xs,dx,0.0,x0,x1,lerr)
      IF (lerr) STOP 'gridcell_init: lonmin error'
      lonmax=xgrid(xmax,xs,dx,0.0,x0,x1,lerr)
      IF (lerr) STOP 'gridcell_init: lonmax error'
      lonper=2*pi/dx

      latmin=xgrid(ymin,ys,dy,0.0,y0,y1,lerr)
      IF (lerr) STOP 'gridcell_init: latmin error'
      latmax=xgrid(ymax,ys,dy,0.0,y0,y1,lerr)
      IF (lerr) STOP 'gridcell_init: latmax error'

      END SUBROUTINE gridcell_init

c     -----------------------------------------------------------------

      SUBROUTINE interpgrid (xs0,dx0,nx0,ys0,dy0,ny0,u0,v0,
     &    xs,dx,nx,ys,dy,ny,u,v)

c     Input grid:
      REAL, INTENT(IN) :: xs0,dx0,ys0,dy0
      INTEGER, INTENT(IN) :: nx0,ny0
      REAL, INTENT(IN), DIMENSION(nx0,ny0) :: u0, v0

c     Output grid:
      REAL, INTENT(IN) :: xs,dx,ys,dy
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(OUT), DIMENSION(nx,ny) :: u, v

c     Local variables:
      INTEGER i, ierr, ioff, j, nxc
      LOGICAL lerr
      REAL tolerance, xper0,xmin,xmax,ymin,ymax,xi(nx),yi(ny), xf0, xsc
      real, dimension(:,:), allocatable :: ucopy, vcopy

c     If the grid descriptions are the same simply copy:
      IF (nx.EQ.nx0 .AND. ny.EQ.ny0 .AND.
     &    same(xs,xs0) .AND. same(ys,ys0) .AND.
     &    same(dx,dx0) .AND. same(dy,dy0)) THEN
        u(:,:)=u0(:,:)
        v(:,:)=v0(:,:)
      ELSE

        xper0=2*pi/dx0

c!#  If the old grid is periodic, ensure longitudes are replicated for
c!#  interpolation at the extreme longitudes: nx0 = xper0 + 3
        tolerance = 10*EPSILON(xper0)*xper0
        if (nx0 .lt. xper0 - tolerance) then !#
c!#        Old grid is not periodic: do a straight copy
           nxc = nx0            !#
        else                    !#
           nxc = nint(xper0) + 3 !#
        endif                   !#

c!#     Allocate work arrays

        ierr = 0 
        if (allocated(ucopy)) deallocate(ucopy, vcopy, stat=ierr)
        if (ierr .ne. 0) stop 'interpgrid: deallocate (ucopy...'
        allocate(ucopy(nxc,ny0), vcopy(nxc,ny0), stat=ierr)
        if (ierr .ne. 0) stop 'interpgrid: allocate (ucopy...'

        SELECT CASE (nxc-nx0) !#
           CASE (3) !#     Old grid has implied periodicity, add 3 columns
              ioff = 1 ; xsc = xs0 - dx0	!#add column on left
              ucopy(1,:) = u0(nx0,:) ; vcopy(1,:) = v0(nx0,:) 
              ucopy(nxc-1,:) = u0(1,:) ; vcopy(nxc-1,:) = v0(1,:) 
              ucopy(nxc,:)   = u0(2,:) ; vcopy(nxc,:)   = v0(2,:) 
           CASE (2) !# Old grid has explicit periodicity, add 2 columns
              ioff = 1 ; xsc = xs0 - dx0	!#add column on left
              ucopy(1,:) = u0(nx0-1,:) ; vcopy(1,:) = v0(nx0-1,:) 
              ucopy(nxc,:)   = u0(2,:) ; vcopy(nxc,:)   = v0(2,:) 
           CASE (1) !# Old grid has 1 extra column (on left), add 1 more on right
              ioff = 0 ; xsc = xs0	!#nothing added on left
              ucopy(nxc,:)   = u0(3,:) ; vcopy(nxc,:)   = v0(3,:) 
           CASE DEFAULT !#no periodicity, or enough columns already - straight copy
              ioff = 0 ; xsc = xs0	!#nothing added on left
        end select !#
c!#     Copy the interior longitudes to the work arrays
        ucopy((ioff+1):(ioff+nx0),:) = u0
        vcopy((ioff+1):(ioff+nx0),:) = v0

        IF (interp .EQ. 1) THEN
          xmin=1; xmax=nxc  ; ymin=1; ymax=ny0
        ELSE IF (interp .EQ. 2) THEN
          xmin=2; xmax=nxc-1; ymin=2; ymax=ny0-1
        END IF

c     Set the coordinates in the old grid system of the new grid points.
        DO i=1,nx
          xi(i)=xgrid(xs+(i-1)*dx,xsc,dx0,xper0,xmin,xmax,lerr)
          IF (lerr) STOP 'interpgrid error: xi'
        END DO
        DO j=1,ny
          yi(j)=xgrid(ys+(j-1)*dy,ys0,dy0,  0.0,ymin,ymax,lerr)
          IF (lerr) STOP 'interpgrid error: yi'
        END DO

c     For each new grid point
        DO j=1,ny
          DO i=1,nx
            call uvinterp(nxc,ny0,xi(i),yi(j),ucopy,vcopy,u(i,j),v(i,j))
          END DO
        END DO

        ierr = 0 
        if (allocated(ucopy)) deallocate(ucopy, vcopy, stat=ierr)
        if (ierr .ne. 0) stop 'interpgrid: deallocate (ucopy...'

      END IF

      END SUBROUTINE interpgrid

c     -----------------------------------------------------------------

      INTEGER FUNCTION igrid (x,xs,dx,iper,imin,imax,lerr)

c     igrid calculates an integer grid index.
c     periodicity is allowed for.
c     no bounds checking if imax < imin.

      REAL, INTENT(IN) :: x,xs,dx
      INTEGER, INTENT(IN) :: iper,imin,imax
      LOGICAL, INTENT(OUT) :: lerr

c     Convert to grid units
      igrid=(x-xs)/dx + 1.5

c     If grid is periodic remove any periodicity in igrid
      IF (iper .GT. 0) then
        igrid=MOD(igrid-1,iper) + 1

c     If igrid is too small increment one period
c     e.g. Grid is 270-360 and data is -60
        IF (igrid.LT.imin) igrid=igrid+iper
      END IF

      lerr=((igrid.LT.imin .OR. igrid.GT.imax) .AND. imin.LT.imax)
     &    .OR. .NOT.same((igrid-1)*dx, x-xs)

      IF (lerr) THEN
        WRITE(*,*) 'VAM: igrid fault:'
        WRITE(*,*) 'Values for igrid,x,xs,dx,iper,imin,imax are: '
        WRITE(*,*) igrid,x,xs,dx,iper,imin,imax
      END IF

      END FUNCTION igrid

c     -----------------------------------------------------------------

      REAL FUNCTION xgrid (x,xs,dx,xper,xmin,xmax,lerr)

c     xgrid calculates a real grid index.
c     periodicity is allowed for.
c     no bounds checking if xmax < xmin.

      REAL, INTENT(IN) :: x,xs,dx,xper,xmin,xmax
      LOGICAL, INTENT(OUT) :: lerr

c     Convert to grid units
      xgrid=(x-xs)/dx + 1

c     If grid is periodic remove any periodicity in xgrid
      IF (xper .GT. 0.5) then
        xgrid=MOD(xgrid-1,xper) + 1

c     If xgrid is too small increment one period
c     e.g. Grid is 270-360 and data is -60
        IF (xgrid.LT.xmin) xgrid=xgrid+xper
      END IF

c     In the exceptional case of a maximum value shave a little bit
c     off the grid coordinate, so no checking will be needed during
c     interpolations, if xmax is integer valued (nominal case).
c     IF (xgrid.EQ.xmax) xgrid=xgrid*(1-10*EPSILON(xgrid))
      IF (ABS(xgrid-xmax)/MAX(1.,ABS(xmax)) .LT. 10*EPSILON(xmax))
     &     xgrid=xmax*(1-10*EPSILON(xmax))

c     Note that EPSILON(x) is an intrinsic that returns a positive 
c     number almost negligible compared to 1.0 for the same type of
c     number as x.

      lerr=(xgrid.LT.xmin .OR. xgrid.GE.xmax) .AND. xmin.LT.xmax

c     Constant extrapolation will be used if error condition exists
c     and no other action is taken:

      IF (lerr) THEN
        IF (xgrid .LT. xmin) xgrid=xmin*(1+10*EPSILON(xmin))
        IF (xgrid .GT. xmax) xgrid=xmax*(1-10*EPSILON(xmax))
      END IF

      END FUNCTION xgrid

c     -----------------------------------------------------------------

      LOGICAL FUNCTION same (x1, x2)

*     Are two radian measures the ``same''

c     Comparisons for equality must be loose.  That is we want 135*(8/3)
c     to be equal to 360, even if 8/3 is not represented perfectly.  For
c     these calculations, agreement to eps is sufficient.

      REAL, INTENT(IN) :: x1, x2
      REAL, PARAMETER :: eps=.00001

      same = ABS(x1-x2) .LT. eps

      END FUNCTION same

c     -----------------------------------------------------------------

#include "uvinterp.F"
#include "uvinterptl.F"
#include "uvinterpad.F"

c     ******************************************************************
 
      SUBROUTINE bidimw
c 
c**** bidimw calculates the interpolation weights 
c 
c     PURPOSE 

c     This subroutine allocates and calculates the weights needed to
c     perform interpolation.

c**   INTERFACE
c     
     I     ( xobs, yobs )

c     xobs       x-coordinate of observation within the grid cell
c     yobs       y-coordinate of observation within the grid cell
c     interp     interpolation method (1=>bilinear;2=>quasi-bicubic)
      REAL, INTENT(IN) :: xobs, yobs

c     ------------------------------------------------------------------

c     1. Calculate the weights.

      IF (interp.EQ.1) THEN
        CALL bidim4w  ( xobs, yobs )
      ELSE
        CALL bidim12w ( xobs, yobs )
      END IF

      END SUBROUTINE bidimw

c     ******************************************************************
 
      SUBROUTINE bidim4w
c 
c**** bidim4w calculates the weights for bilinear interpolation 
c 
c     PURPOSE 

c     This subroutine calculates the weights needed to perform
c     bilinear interpolation.

c**   INTERFACE
c     
     I     ( xobs, yobs )

c     xobs       x-coordinate of observation within the grid cell
c     yobs       y-coordinate of observation within the grid cell
      REAL, INTENT(IN) :: xobs, yobs

c     ------------------------------------------------------------------

c     1. Calculate the weights for bilinear interpolation.

      wi(2,2) = xobs*yobs
      wi(2,1) = xobs-wi(2,2)
      wi(1,2) = yobs-wi(2,2)
      wi(1,1) = 1 - yobs - wi(2,1)

      END SUBROUTINE bidim4w

c     ******************************************************************
 
      SUBROUTINE bidim12w 
c 
c**** bidim12w calculates bidimensional 12 point interpolation weights
c 
c     PURPOSE 

c     This subroutine calculates the weights needed to perform
c     bidimensional 12 point interpolation.

c**   INTERFACE
c     
     I     ( xobs, yobs )

c     xobs       x-coordinate of observation within the grid cell
c     yobs       y-coordinate of observation within the grid cell
      REAL, INTENT(IN) :: xobs, yobs

c     Local variables.
c     a          intermediate factors used in computing the weights
c     b          intermediate factors used in computing the weights
      REAL, DIMENSION(4) :: a, b
      INTEGER i, j

c     ------------------------------------------------------------------

c     1. Calculate the weights for bidim 12 pt interpolation.

c     Use the Lagrange multiplier representation of collocating cubic
c        f(x) = \sum_j f_j (\prod_(i \ne j) (x - x_i))/
c                          (\prod_(i \ne j) (x_j - x_i)) .
c     Taking the points at (-1, 0, 1, 2) to determine the 
c     weights for interpolating cubically to the x and y locations
c     separately.

      a(1) = (xobs * (xobs - 1) * (xobs - 2))/(-6)
      a(2) = ((1 + xobs) * (xobs - 1) * (xobs - 2))/2
      a(3) = ((1 + xobs) * xobs * (xobs - 2))/(-2)
      a(4) = ((1 + xobs) * xobs * (xobs - 1))/6
      b(1) = (yobs * (yobs - 1) * (yobs - 2))/(-6)
      b(2) = ((1 + yobs) * (yobs - 1) * (yobs - 2))/2
      b(3) = ((1 + yobs) * yobs * (yobs - 2))/(-2)
      b(4) = ((1 + yobs) * yobs * (yobs - 1))/6

c     Interpolating in x and then in y,
c         f = b_1 f_1 + b_2 f_2 + ...
c           = b_1 ( (1-x) f_21 + x f_31) + 
c             b_2 ( a_1 f_12 + a_2 f_22 + a_3 f_32 + a_4 f_42) + ...
c     shows that the x and y weights multiply.

      wi(1,1) = 0.
      wi(2,1) = b(1)*(1 - xobs)
      wi(3,1) = b(1)*xobs
      wi(4,1) = 0.
      DO j=2,3
        DO i=1,4
          wi(i,j)=b(j)*a(i)
        END DO
      END DO
      wi(1,4) = 0.
      wi(2,4) = b(4)*(1 - xobs)
      wi(3,4) = b(4)*xobs
      wi(4,4) = 0.

      END SUBROUTINE bidim12w

#include "bidim.F"
#include "bidimtl.F"
#include "bidimad.F"

      END MODULE interp_mod
