c!# CSU IDENTIFICATION : jb_mod
c!# $Id: jb_mod.f,v 1.6 2005/03/21 20:26:23 rnh Exp $

c!##  PURPOSE : Calculate composite background term for 2dVAR.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c     No polar cap in this version.

c!##  LANGUAGE : f90

c!#   CSU DESIGN :

      MODULE jb_mod

      USE types, ONLY: accumulator
      USE constants, ONLY: pi, a

      IMPLICIT NONE
      PRIVATE
      PUBLIC jb

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c!#   Grid description:
c!#~   my       number of latitude grid boxes in integration domain
c!#~   mx       number of longitude grid boxes in integration domain
c!#~   i1       first longitude grid index
c!#~   in       last longitude grid index
c!#~   j1       first latitude grid index
c!#~   jn       last latitude grid index
c!#~   y0       integration boundary latitude
c!#~   dy       latitude increment
c!#~   dx       longitude increment
      INTEGER, SAVE :: my=0, mx=0, i1, in, j1, jn
      REAL, SAVE :: y0, dy, dx

c!#   By convention the grid boxes to be integrated over are numbered
c!#   1:mx in lon and the lon boundary points are numbered 0:mx.  Thus
c!#   | ... |   0 |   1 |   2 |   3 |  ... |   mx | ... |
c!#  i1 ...-1     0     1     2     3    ...   mx   ... in
c!#   1    ...   kmin  ...  kmin+2 ...   ...  kmax ... idim
c!#   And the same convention is used for the lat direction.

c!#   Geometric factors:
c!#   All geometric factors depend on lat either on the grid
c!#   or in the grid cells, except rdy:
c!#~   area       size of grid cell
c!#~   area.      = a^2 dellon del(sin(lat))
c!#~   adiv       constant to calculate divergence in grid cell
c!#~   adiv.      = 1/(2*a*cos(lat)*del(lon))
c!#~   bdiv       constant to calculate divergence in grid cell
c!#~   bdiv.      = 1/(2*a*del(lat)) - tan(lat)/(4*a)
c!#~   cdiv       constant to calculate divergence in grid cell
c!#~   cdiv.      = 1/(2*a*del(lat)) + tan(lat)/(4*a)
c!#~   rdx        reciprocal lon increment for centered difference
c!#~   rdx.       = 1/(2*a*cos(lat)*del(lon))
c!#~   rdy        reciprocal lat increment for centered difference
c!#~   rdy.       = 1/(2*a*del(lat))
c!#~   gamma      tan term in divergence calculation
c!#~   gamma.     = tan(lat)/a
c     These will be DIMENSION(my)
      REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: area, adiv, bdiv, cdiv !#
c     These will be DIMENSION(0:my)
      REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: rdx, gamma !#
      REAL, SAVE :: rdy !#

c!#   Constants needed for the calculation
c!#~   velmin     Minimum wind speed (m/s) (avoid division by zero)
      REAL, PARAMETER :: velmin=1d-8

c!#   Define parameters indexing components of Jb
c!#~   delu      index of Jb component for uBGD
c!#~   delv      index of Jb component for vBGD
c!#~   div       index of Jb component for DIV
c!#~   vor       index of Jb component for VOR
c!#~   lapu      index of Jb component for uLAP
c!#~   lapv      index of Jb component for vLAP
c!#~   dvor      index of Jb component for DVDT
c!#~   ddiv      index of Jb component for DDDT
c!#~   nlamda    number of components in Jb
      INTEGER, PARAMETER :: delu=1, delv=2, div=3, vor=4, !#
     &    lapu=5, lapv=6, dvor=7, ddiv=8, nlamda=8 !#
c!# ***************************************************************************
c!#   NOTE: These must agree with the iback array in 
c!#         solve_mod for proper indexing of the slamdas in ss_back
c!# ***************************************************************************

c!#   Define logicals controlling which components are needed, etc.
c!#~   llamda    is lamda positive.
c!#~   lback     are any background lamdas positive.
c!#~   ldyn      are any dynamics lamdas positive.
      LOGICAL :: llamda(nlamda), lback, ldyn

c!#   Define logical indicating which components are calculated as
c!#   mean squared quantities at the center of the grid cell.
c!#   The others are calculated as divergences.
c!#~   lsquared  Is Jb component calculated as squared quantity.
      LOGICAL, PARAMETER, DIMENSION(nlamda) :: lvalues=
     &    (/ .TRUE., .TRUE., .TRUE., .TRUE.,
     &    .FALSE., .FALSE., .FALSE., .FALSE. /)
      LOGICAL, PARAMETER, DIMENSION(nlamda) :: lsquared=
     &    (/.TRUE., .TRUE., .FALSE., .FALSE.,
     &     .TRUE., .TRUE., .FALSE., .FALSE./)

c!#   Storage of indices for various latitudes.
c     n and s are used for the arrays at two latitudes, while
c     t, c, and b are used for the full arrays.
c!#~   n         index of northern latitude (local arrays)
c!#~   s         index of southern latitude (local arrays)
c!#~   t         index of top latitude (dummy arrays)
c!#~   c         index of center latitude (dummy arrays)
c!#~   b         index of bottom latitude (dummy arrays)
      INTEGER :: n, s, t, c, b, lat

c     --------t---------- top
c
c     --------c---------- center    --------n----------   north
c            lat                           lat
c     --------b---------- bottom    --------s----------   south

c!#   Storage of only two longitudes needed for variables at grid points
c     Increment (trajectory-background) variables DIMENSION(0:mx, 0:1)
c!#~   u1,v1          wind field increment
c!#~   u1lap,v1lap    laplacian of wind field increment
      REAL, SAVE, ALLOCATABLE, DIMENSION(:, :) :: u1, v1, u1lap, v1lap
c     Trajectory variables DIMENSION(0:mx, 0:1)
c!#~   u5x,v5x        wind field x derivatives (trajectory)
c!#~   u5y,v5y        wind field y derivatives (trajectory)
c!#~   u5lap,v5lap    laplacian of wind field (trajectory)
c!#~   eta5           total vorticity (trajectory)
c!#~   vel5           velocity magnitude (trajectory)
c!#~   vor5u,vor5v    Q terms for vorticity equation (trajectory) 
c!#~   div5u,div5v    Q terms for divergence equation (trajectory) 
      REAL, SAVE, ALLOCATABLE, DIMENSION(:, :) :: u5x, v5x, u5y, v5y,
     &    u5lap, v5lap, eta5, vel5, vor5u, vor5v, div5u, div5v
c     Background variables DIMENSION(0:mx, 0:1)
c!#~   u0x,v0x        wind field x derivatives (background)
c!#~   u0y,v0y        wind field y derivatives (background)
c!#~   u0lap,v0lap    laplacian of wind field (background)
c!#~   eta0           total vorticity (background)
c!#~   vel0           velocity magnitude (background)
c!#~   vor0u,vor0v    Q terms for vorticity equation (background) 
c!#~   div0u,div0v    Q terms for divergence equation (background) 
      REAL, SAVE, ALLOCATABLE, DIMENSION(:, :) :: u0x, v0x, u0y, v0y,
     &    u0lap, v0lap, eta0, vel0, vor0u, vor0v, div0u, div0v
c     Adjoint variables DIMENSION(0:mx, 0:1)
c!#~   du,dv          wind field increments (adjoint)
c!#~   ux,vx          wind field x derivatives (adjoint)
c!#~   uy,vy          wind field y derivatives (adjoint)
c!#~   ulap,vlap      laplacian of wind field (adjoint)
c!#~   eta            total vorticity (adjoint)
c!#~   vel            velocity magnitude (adjoint)
c!#~   voru,vorv      Q terms for vorticity equation (adjoint)
c!#~   divu,divv      Q terms for divergence equation (adjoint)
      REAL, SAVE, ALLOCATABLE, DIMENSION(:, :) :: du, dv, ux, vx,
     &    uy, vy, ulap, vlap, eta, vel, voru, vorv, divu, divv

c!#   r terms (trajectory and adjoint) are stored only for the current band
c     DIMENSION(mx, nlamda)
c!#~   r5           R terms (trajectory)
c!#~   r            R terms (adjoint)
      REAL, SAVE, ALLOCATABLE, DIMENSION (:, :) :: r5, r

      CONTAINS

c     -----------------------------------------------------------------

c!#   CSU IDENTIFICATION : jb

c!##  PURPOSE : Cover routine to calculate background term for 2dVAR.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c     No polar cap in this version.

c     If periodic, assumes grid is expanded.

c!##  LANGUAGE : f90

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      SUBROUTINE jb
     C    (nlon, nlat, iwest, ieast, jsouth, jnorth,
     C    lat0, dlat, dlon,
     C    u0, v0, f, cf, lamda,
     I    u5, v5,
     O    u, v, jbval)

c!#   Grid description:
c!#~   nlon       number of longitude grid points
c!#~   nlat       number of latitude grid points
c!#~   iwest      grid index of western edge of integration domain
c!#~   ieast      grid index of eastern edge of integration domain
c!#~   jsouth     grid index of southern edge of integration domain
c!#~   jnorth     grid index of northern edge of integration domain
c!#~   lat0       latitude of first grid point
c!#~   dlat       latitude increment
c!#~   dlon       longitude increment
      INTEGER, INTENT(IN) :: nlon, nlat, iwest, ieast, jsouth, jnorth
      REAL, INTENT(IN) :: lat0, dlat, dlon

c!#   Input constants:
c!#~   u0,v0     wind field (background)
c!#~   f         Coriolis term
c!#~   cf        friction coefficient
c!#~   lamda     lamda weight for each component
c!#~   lamda.    (within jb_mod, the scaled lamda weight)
      REAL, DIMENSION(nlon,nlat), INTENT(IN) :: u0, v0  !#
      REAL, INTENT(IN) :: f(nlat), cf, lamda(nlamda) !#

c     Here f is assumed to vary only with the y direction.
c     Here cf is assumed to be constant.
c     Both f and cf could easily be full arrays congruent to u0.

c!#   Inputs:
c!#~   u5,v5     wind field (trajectory)
      REAL, DIMENSION(nlon,nlat), INTENT(IN) :: u5, v5  !#

c!#   Outputs:
c!#~   u,v       wind field (adjoint)
c!#~   jbval     Jb value
      REAL, DIMENSION(nlon,nlat), INTENT(INOUT) :: u, v  !#
      REAL(accumulator), INTENT(INOUT) :: jbval !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!#   Variables needed for indexing are calculated and stored as
c!#   module variables.  These are then globally available for all
c!#   other subroutines in the module.

c!##  ALGORITHMS :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     -----------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

      INTEGER ierr

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Initialization

c!#   1.1 Define logicals
c!#   Some control which components are needed
      llamda = lamda .gt. 0
      lback = ANY(llamda((/delu, delv, div, vor, lapu, lapv/)))
      ldyn  = ANY(llamda((/dvor, ddiv/)))

c Catch case of no calculations
      IF (.NOT. ANY(llamda)) RETURN

c!#   1.3 Geometric and grid constants and work arrays.

c     These are a convenience and used only in dimension specs.
      i1=1-iwest
      in=nlon-iwest
      j1=1-jsouth
      jn=nlat-jsouth

      call jbinit (jnorth-jsouth, lat0+(jsouth-1)*dlat,
     &    dlat, ieast-iwest, dlon)

c     ------------------------------------------------------------------

c!#   2. Calculate jb
      CALL jbcalc
     C    (u0, v0, f, cf, lamda,
     I    u5, v5,
     O    u, v, jbval)

c     ------------------------------------------------------------------

c!#   3. Deallocation
c     The module arrays are allocated by jbinit.
c     If the vam runs by itself these deallocations can be skipped.
c     With the deallocations, the save attribute for these arrays is
c     not necessary.

c     Deallocate work arrays (optional)
      ierr = 0
c$$$      DEALLOCATE (u1, v1, u1lap, v1lap,
c$$$     &    u5x, v5x, u5y, v5y,
c$$$     &    u5lap, v5lap, eta5, vel5, vor5u, vor5v, div5u, div5v,
c$$$     &    u0x, v0x, u0y, v0y,
c$$$     &    u0lap, v0lap, eta0, vel0, vor0u, vor0v, div0u, div0v,
c$$$     &    du, dv, ux, vx,
c$$$     &    uy, vy, ulap, vlap, eta, vel, voru, vorv, divu, divv,
c$$$     &    r5, r, STAT=ierr)
c$$$      IF (ierr.NE.0) STOP 'jb: DEALLOCATE (u1, v1...'

c     Deallocate geometric factors (this wastes some calculations)
c$$$      DEALLOCATE (area, adiv, bdiv, cdiv, rdx, gamma, STAT=ierr)
c$$$      IF (ierr.NE.0) STOP 'jb: DEALLOCATE (area, adiv...'

      END SUBROUTINE jb

c     -----------------------------------------------------------------

c!# CSU IDENTIFICATION : jbinit 

c!##  PURPOSE : Calculate constant parameters for the 2dVAR Jb.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c     No polar cap in this version.

c!##  LANGUAGE : f90

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      SUBROUTINE jbinit (my_in, y0_in, dlat, mx_in, dlon)

c!#    Dimensions:
c!#~   my_in     number of latitude grid boxes in integration domain
c!#~   mx_in     number of longitude grid boxes in integration domain
      INTEGER, INTENT(IN) :: my_in, mx_in

c!#   Grid description:
c!#~   y0_in     integration boundary latitude
c!#~   dlat     latitude increment
c!#~   dlon     longitude increment
      REAL, INTENT(IN) :: y0_in, dlat, dlon

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   Calculate various constants needed to calculate Jb.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     -----------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

      REAL :: y(0:my_in), yc(my_in)
      LOGICAL lcalc
      INTEGER i, ierr

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Are calculations needed.

      lcalc = .NOT. (ALLOCATED(area) .AND.
     &    my .EQ. my_in .AND.
     &    y0 .EQ. y0_in .AND.
     &    dy .EQ. dlat .AND.
     &    mx .EQ. mx_in .AND.
     &    dx .EQ. dlon)

c!#   2. We will reallocate arrays if size changes

      ierr = 0
      IF (ALLOCATED(area) .AND. my .NE. my_in)
     &    DEALLOCATE (area, adiv, bdiv, cdiv, rdx, gamma, STAT=ierr)
      IF (ierr.NE.0) STOP 'jbinit: DEALLOCATE (area, adiv...'

      IF (ALLOCATED(u1) .AND. mx .NE. mx_in)
     &    DEALLOCATE (u1, v1, u1lap, v1lap,
     &    u5x, v5x, u5y, v5y,
     &    u5lap, v5lap, eta5, vel5, vor5u, vor5v, div5u, div5v,
     &    u0x, v0x, u0y, v0y,
     &    u0lap, v0lap, eta0, vel0, vor0u, vor0v, div0u, div0v,
     &    du, dv, ux, vx,
     &    uy, vy, ulap, vlap, eta, vel, voru, vorv, divu, divv,
     &    r5, r, STAT=ierr)
      IF (ierr.NE.0) STOP 'jbinit: DEALLOCATE (u1, v1...'

c!#   3. Save parameters describing the grid.

      IF (lcalc) THEN
        my=my_in ; y0=y0_in ; dy=dlat ; mx=mx_in ; dx=dlon
      END IF

c!#   4. Allocate arrays

      IF (.NOT. ALLOCATED(area)) ALLOCATE (area(my), adiv(my),
     &    bdiv(my), cdiv(my), rdx(0:my), gamma(0:my), STAT=ierr)
      IF (ierr.NE.0) STOP 'jbinit: ALLOCATE (area(my), adiv...'

      IF (.NOT. ALLOCATED(u1)) ALLOCATE (u1(0:mx, 0:1), v1(0:mx, 0:1),
     &    u1lap(0:mx, 0:1), v1lap(0:mx, 0:1), u5x(0:mx, 0:1),
     &    v5x(0:mx, 0:1), u5y(0:mx, 0:1), v5y(0:mx, 0:1),
     &    u5lap(0:mx, 0:1), v5lap(0:mx, 0:1), eta5(0:mx, 0:1),
     &    vel5(0:mx, 0:1), vor5u(0:mx, 0:1), vor5v(0:mx, 0:1),
     &    div5u(0:mx, 0:1), div5v(0:mx, 0:1), u0x(0:mx, 0:1),
     &    v0x(0:mx, 0:1), u0y(0:mx, 0:1), v0y(0:mx, 0:1),
     &    u0lap(0:mx, 0:1), v0lap(0:mx, 0:1), eta0(0:mx, 0:1),
     &    vel0(0:mx, 0:1), vor0u(0:mx, 0:1), vor0v(0:mx, 0:1),
     &    div0u(0:mx, 0:1), div0v(0:mx, 0:1),
     &    du(0:mx, 0:1), dv(0:mx, 0:1), ux(0:mx, 0:1), vx(0:mx, 0:1),
     &    uy(0:mx, 0:1), vy(0:mx, 0:1), ulap(0:mx, 0:1),
     &    vlap(0:mx, 0:1), eta(0:mx, 0:1), vel(0:mx, 0:1),
     &    voru(0:mx, 0:1), vorv(0:mx, 0:1), divu(0:mx, 0:1),
     &    divv(0:mx, 0:1), r5(mx, nlamda), r(mx, nlamda), STAT=ierr)
      IF (ierr.NE.0) STOP 'jbinit: ALLOCATE (u1(0:mx, 0:1), v1...'

c!#   5. Calculate latitude at grid points and at grid centers.

      IF (lcalc) THEN
        y(:)  = y0 + (/(i,i=0,my)/)*dy
        yc(:) = y0 + (/(i,i=1,my)/)*dy - dy/2

c!#   5. Calculate constants for calculating divergence at grid centers.
        adiv(:) = 1/(2*a*dx*cos(yc(:)))
        bdiv(:) = 1/(2*a*dy) - tan(yc(:))/(4*a)
        cdiv(:) = 1/(2*a*dy) + tan(yc(:))/(4*a)

c!#   6. Calculate area of grid boxes.
c     Note that
c          Area = a^2 dx del(sin(y)), but
c          del(sin(y))=sin(y_c + dy/2)-sin(y_c - dy/2), so
c          Area = a^2 dx 2 cos(y_c) sin(dy/2)
        area(:) = a*sin(dy/2)/adiv(:)

c!#   7. Calculate reciprocal increments for centered difference
        rdy = 1/(2*a*dy)
        WHERE (cos(y(:)) /= 0)
          rdx(:) = 1/(2*a*cos(y(:))*dx)
          gamma(:) = tan(y(:))/a
        ELSE WHERE
          rdx(:) = 0
          gamma(:) = 0
        END WHERE

      END IF

      END SUBROUTINE jbinit

c     -----------------------------------------------------------------

c!#   CSU IDENTIFICATION : jbcalc

c!##  PURPOSE : Calculate composite background term for 2dVAR.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c     No polar cap in this version.

c     If periodic, assumes grid is expanded.

c!##  LANGUAGE : f90

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      SUBROUTINE jbcalc !#
     C    (u0, v0, f, cf, lamda, !#
     I    u5, v5, !#
     O    u, v, jbval) !#

      REAL, DIMENSION(i1:in,j1:jn), INTENT(IN) :: u0, v0  !#
c!#~   u0,v0     wind field (background)
      REAL, INTENT(IN) :: f(j1:jn), cf, lamda(nlamda) !#
c!#~   f         Coriolis term
c!#~   cf        friction coefficient
c!#~   lamda     lamda weight for each component
c!#~   lamda.    (within jb_mod, the scaled lamda weight)
c     Here f is assumed to vary only with the y direction.
c     Here cf is assumed to be constant.
c     Both f and cf could easily be full arrays congruent to u5.
      REAL, DIMENSION(i1:in,j1:jn), INTENT(IN) :: u5, v5  !#
c!#~   u5,v5     wind field (trajectory)
      REAL, DIMENSION(i1:in,j1:jn), INTENT(INOUT) :: u, v  !#
c!#~   u,v       wind field (adjoint)
      REAL(accumulator), INTENT(INOUT) :: jbval !#
c!#~   jbval     Jb value

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   All components are summed.  We use this fact in 
c!#   the adjoint calculation. Note that
c!#     J = Sum_lamda lamda Sum_lat Sum_lon a_lat R_{lon,lat,lamda}^2
c         = Sum_lat Sum_lamda lamda a_lat Sum_lon R_{lon,lat,lamda}^2
c         = Sum_lat Sum_lamda w_{lamda,lat} J_{lamda,lat}
c!#       = Sum_i w_i J_i
c!#   The LTM of this last expression is the same and the ADJ is
c!#     J_i = w_i J = w_i and J = J
c!#   Thus we know a priori that the adjoint of each entry in the sum
c!#   is the constant weight.

c!#   Also for the adjoint of the forward calculation, note that each of
c!#   the required steps could be done one by one in order over the
c!#   entire grid.  Organizing the calculation by latitude saves
c!#   storage, we need only 2 latitudes for calculated quantities needed
c!#   on the grid and 1 latitude for quantities needed at the centers of
c!#   grid boxes.

c!#   To mesh the adjoint calculation with the forward calculation, the
c!#   adjoint calculation must lag behind the forward calculation in
c!#   terms of latitude.  The requirement is that all terms needed for
c!#   an adjoint calculation must be complete.  This sounds obvious, but
c!#   the code may not look obvious.

c!##  REFERENCES :

c!##  LIMITATIONS :

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     -----------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c     Loop variables
c!#~   ilat     current latitude band
c!#~   ilamda   current Jb component index
      INTEGER :: ilat, ilamda

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Initialize adjoint variables to zero.
c     These are reset to zero as we finish using them.

      du(:,:)=0 ; dv(:,:)=0
      ux(:,:)=0 ; vx(:,:)=0 ; uy(:,:)=0 ; vy(:,:)=0
      ulap(:,:)=0 ; vlap(:,:)=0 ; eta(:,:)=0 ; vel(:,:)=0
      voru(:,:)=0 ; vorv(:,:)=0 ; divu(:,:)=0 ; divv(:,:)=0
      r(:,:)=0

c     ------------------------------------------------------------------

c!#   2. Calculate quantities needed at lat=lmin.

c!#   2.1 Set latitude indices for the band south of the domain.
      CALL setilat (0)

c!#   2.2 Calculate quantities at the northern boundary of this band
c!#                               (southern boundary of the domain)
      CALL calcq (f, cf, u0, v0, u5, v5)

c     ------------------------------------------------------------------

c!#   3. For each latitude band in the integral:
      DO ilat = 1, my

c!#   3.1 Set latitude indices for current band.
        CALL setilat (ilat)

c!#   3.2 Calculate quantities at northern boundary.
        CALL calcq (f, cf, u0, v0, u5, v5)

c!#   3.3 Calculates r terms at grid box centers.
        CALL calcr

c!#   3.4 Increment Jb components for each lamda
c!#   Start adjoint calculation at the same time
c     Applying latitude weight to sum over longitude of squares of r.
c     NOTE: some quantities are already squared

        DO ilamda=1,nlamda
          IF (llamda(ilamda)) THEN
            IF (lsquared(ilamda)) THEN
              jbval = jbval +
     &            area(lat)*lamda(ilamda)*sum(r5(:,ilamda))
              r(:,ilamda) = area(lat)*lamda(ilamda)
            ELSE
              jbval =jbval +
     &            area(lat)*lamda(ilamda)*sum(r5(:,ilamda)**2)
              r(:,ilamda) = 2*area(lat)*lamda(ilamda)*r5(:,ilamda)
            ENDIF
          ENDIF
        END DO

c     ------------------------------------------------------------------

c!#   3. ADJOINT: For each latitude band in the integral:

c!#   3.3 ADJOINT: Calculates r terms at grid box centers.
        CALL calcrad

c!#   3.2 ADJOINT: Calculate quantities at northern boundary.
c     The adjoint is at the southern boundary.
c     Therefore, first set latitude indices for the previous band.
        CALL setilat (ilat-1)
        CALL calcqad (cf, u5, v5, u, v)

      END DO

c     ------------------------------------------------------------------

c!#   2. ADJOINT: Calculate quantities needed at lat=lmin.

c     Because the adjoint calculation always lags behind forward
c     calculation to finish we must now process the northernmost
c     latitude.

c!#   2.1 ADJOINT: Set latitude indices for the band south of the domain.
      CALL setilat (my)

c!#   2.2 ADJOINT: Calculate quantities at northern boundary.
      CALL calcqad (cf, u5, v5, u, v)

c     ------------------------------------------------------------------

c!##  ERROR HANDLING : None

      END SUBROUTINE jbcalc

      SUBROUTINE setilat (ilat)
c!#   Define indices for latitudes.
      INTEGER, INTENT(IN) :: ilat
c!#~   ilat     current latitude band
c!#   Let n and s indicate storage locations for lat and lat-1
c!#   in 2 row arrays.  Possible values are (0,1) or (1,0).
      lat = ilat
      n = mod(lat  ,2) ! Northern
      s = 1 - n        ! Southern = mod(lat-1,2)
c!#   Let t, c, b and l indicate storage for lat+1, lat,
c!#   lat-1 and lat-2 in full arrays.
      t = lat+1 ! Top
      c = lat   ! Center
      b = lat-1 ! Bottom
      END SUBROUTINE setilat

      SUBROUTINE calcq (f, cf, u0, v0, u5, v5)
c!#   Calculates quantities needed at the northern latitude.
      REAL, INTENT(IN) :: f(j1:jn), cf
c!#~   f         Coriolis term
c!#~   cf        friction coefficient
      REAL, INTENT(IN), DIMENSION(i1:in,j1:jn) :: u0, v0, u5, v5
c!#~   u0,v0     wind field (background)
c!#~   u5,v5     wind field (trajectory)
c!#   Derivatives.
      CALL dpqdxy(u0, v0, u0x, v0x, u0y, v0y, u0lap, v0lap)
      CALL dpqdxy(u5, v5, u5x, v5x, u5y, v5y, u5lap, v5lap)
c!#   Dynamics terms.
      IF (ldyn) THEN
        CALL vordiv(f, cf, u0, v0, u0x, v0x, u0y, v0y,
     &      eta0, vel0, vor0u, vor0v, div0u, div0v)
        CALL vordiv(f, cf, u5, v5, u5x, v5x, u5y, v5y,
     &      eta5, vel5, vor5u, vor5v, div5u, div5v)
      ENDIF
c!#   Differences of winds and spatial derivatives.
      IF (lback) THEN
        u1(:,n) = u5(0:mx,c) - u0(0:mx,c)
        v1(:,n) = v5(0:mx,c) - v0(0:mx,c)
        u1lap(:,n) = u5lap(:,n) - u0lap(:,n)
        v1lap(:,n) = v5lap(:,n) - v0lap(:,n)
      ENDIF
      END SUBROUTINE calcq

      SUBROUTINE dpqdxy(p, q, px, qx, py, qy, plap, qlap) !#
c!#   Calculate spatial derivatives for p and q.
      REAL, INTENT(IN), DIMENSION(i1:in,j1:jn) :: p,q
c!#~   p,q         wind field
      REAL, INTENT(OUT), DIMENSION(0:mx, 0:1) :: px,qx,py,qy,plap,qlap
c!#~   px,qx       wind field derivatives wrt x
c!#~   py,qy       wind field derivatives wrt y
c!#~   plap,qlap   laplacian of wind field

c!#   Local variables:
      REAL, DIMENSION(0:mx) :: pxx,qxx,pyy,qyy
c!#~   pxx,qxx     wind field second derivatives wrt x
c!#~   pyy,qyy     wind field second derivatives wrt y
      CALL dwdx(p, px, pxx)
      CALL dwdx(q, qx, qxx)
      CALL dwdy(p, py, pyy)
      CALL dwdy(q, qy, qyy)
      plap(:,n) = pxx(:) - gamma(c)*px(:,n) + pyy(:)
      qlap(:,n) = qxx(:) - gamma(c)*qx(:,n) + qyy(:)
      END SUBROUTINE dpqdxy

      SUBROUTINE dwdx(w, wx, wxx) !#
c!#   Use centered difference for longitude derivative.
      REAL, INTENT(IN) :: w(i1:in,j1:jn)
c!#~   w       wind field component
      REAL, INTENT(OUT) :: wx(0:mx, 0:1), wxx(0:mx)
c!#~   wx      derivative of w wrt x
c!#~   wxx     second derivatives of w wrt x
      wx(:,n) = rdx(c)*(w(1:mx+1,c) - w(-1:mx-1,c))
      wxx(:) = 4*rdx(c)**2*(w(1:mx+1,c) - 2*w(0:mx,c) + w(-1:mx-1,c))
      END SUBROUTINE dwdx

      SUBROUTINE dwdy(w, wy, wyy) !#
c!#   Use centered difference for latitude derivative.
      REAL, INTENT(IN) :: w(i1:in,j1:jn)
c!#~   w       wind field component
      REAL, INTENT(OUT) :: wy(0:mx, 0:1), wyy(0:mx)
c!#~   wy      derivative of w wrt y
c!#~   wyy     second derivatives of w wrt y
      wy(:,n) = rdy*(w(0:mx,t) - w(0:mx,b))
      wyy(:) = 4*rdy**2*(w(0:mx,t) - 2*w(0:mx,c) + w(0:mx,b))
      END SUBROUTINE dwdy

      SUBROUTINE vordiv(f, cf, u, v, ux, vx, uy, vy,  !#
     &    eta, vel, voru, vorv, divu, divv) !#
c!#   Calculate Q terms for vorticity and divergence tendency.
      REAL, INTENT(IN) :: f(j1:jn), cf
c!#~   f           Coriolis term
c!#~   cf          friction coefficient
c!#   NB: Variables defined here as adjoint variables are adjoint in
c!#   vordivad, but trajectory or background in this routine.
      REAL, INTENT(IN), DIMENSION(i1:in,j1:jn) :: u, v
c!#~   u,v         wind field (adjoint)
      REAL, INTENT(IN), DIMENSION(0:mx, 0:1) :: ux, vx, uy, vy
c!#~   ux,vx       wind field x derivatives (adjoint)
c!#~   uy,vy       wind field y derivatives (adjoint)
      REAL, INTENT(OUT), DIMENSION(0:mx, 0:1) ::
     &    eta, vel, voru, vorv, divu, divv
c!#~   eta         total vorticity (adjoint)
c!#~   vel         velocity magnitude (adjoint)
c!#~   voru,vorv   Q terms for vorticity equation (adjoint)
c!#~   divu,divv   Q terms for divergence equation (adjoint)
c!#   Total vorticity.
      eta(:,n) = vx(:,n) - uy(:,n) + gamma(c)*u(0:mx,c) + f(c)
c!#   Velocity magnitude.
      vel(:,n) = SQRT(u(0:mx,c)**2 + v(0:mx,c)**2)
c     Enforce minimum velocity.
      where (vel(:,n)<velmin) vel(:,n)=velmin
c     Q terms for vorticity and divergence equation.
      voru(:,n) = eta(:,n)*u(0:mx,c) - cf*Vel(:,n)*v(0:mx,c)
      vorv(:,n) = eta(:,n)*v(0:mx,c) + cf*Vel(:,n)*u(0:mx,c)
      IF (llamda(ddiv)) THEN
        divu(:,n) = u(0:mx,c)*ux(:,n) + v(0:mx,c)*vx(:,n) - vorv(:,n)
        divv(:,n) = u(0:mx,c)*uy(:,n) + v(0:mx,c)*vy(:,n) + voru(:,n)
      ENDIF
      END SUBROUTINE vordiv

      SUBROUTINE calcr
c!#   Calculates r terms at grid box centers.
c!#   Average wind components (increments)
      IF (llamda(delu)) CALL w2bar(u1, r5(:,delu))
      IF (llamda(delv)) CALL w2bar(v1, r5(:,delv))
c!#   Divergence, vorticity, Laplacian of u, v (increments)
      IF (llamda(div))  CALL q2div(u1, v1, r5(:,div))
      IF (llamda(vor))  CALL q2div(-v1(:,:), u1, r5(:,vor))
      IF (llamda(lapu)) CALL w2bar(u1lap, r5(:,lapu))
      IF (llamda(lapv)) CALL w2bar(v1lap, r5(:,lapv))
c!#   Trajectory - background for dynamic constraints
      IF (llamda(dvor)) CALL q2div(vor5u(:,:)-vor0u(:,:),
     &    vor5v(:,:)-vor0v(:,:), r5(:,dvor))
      IF (llamda(ddiv)) CALL q2div(div5u(:,:)-div0u(:,:),
     &    div5v(:,:)-div0v(:,:), r5(:,ddiv))
      END SUBROUTINE calcr

      SUBROUTINE w2bar(w, rsq) !#
c!#   Squared value at center of grid box.
      REAL, INTENT(IN) :: w(0:mx, 0:1)
c!#~   w       wind field component
      REAL, INTENT(OUT) :: rsq(mx)
c!#~   rsq     R term calculated as a mean square
      rsq(:) = (w(0:mx-1,n)**2+w(1:mx,n)**2+
     &    w(0:mx-1,s)**2+w(1:mx,s)**2)/4
      END SUBROUTINE w2bar

      SUBROUTINE q2div(p, q, rdiv) !#
c!#   Divergence at center of box.
      REAL, INTENT(IN), DIMENSION(0:mx, 0:1) :: p, q
c!#~   p,q         wind field
      REAL, INTENT(OUT) :: rdiv(mx)
c!#~   rdiv        R term calculated as a mean divergence
      rdiv(:) = adiv(lat)
     &        * (p(1:mx,n) + p(1:mx,s) - p(0:mx-1,n) - p(0:mx-1,s))
     &    + bdiv(lat)*(q(1:mx,n) + q(0:mx-1,n))
     &    - cdiv(lat)*(q(1:mx,s) + q(0:mx-1,s))
      END SUBROUTINE q2div

c!#   Algebraically for the LTM and adjoint u=du5=du1=du.  However
c!#   storage is laid out differently and the linear perturbation
c!#   for u5 is u, while that of u1 is du, and similarly for v.
c!#   The adjoints of the above subroutines are simplified by noting
c!#   that terms only involving u0, v0,... have no perturbation.
c!#   When order is immaterial, the adjoint code is not reordered.

c!#   The adjoint of calcr is done for the center of the current grid
c!#   box.  However the adjoint of calcq is done at the southern
c!#   boundary.

c!#   For simplicity no reference is made to the trajectory variables
c!#   in the linear codes.

      SUBROUTINE calcqad (cf, u5, v5, u, v)
c     Calculates quantities needed at the northern latitude.
      REAL, INTENT(IN) :: cf
      REAL, INTENT(IN), DIMENSION(i1:in,j1:jn) :: u5, v5
      REAL, INTENT(INOUT), DIMENSION(i1:in,j1:jn) :: u, v
c     Differences of winds and spatial derivatives.
      IF (lback) THEN
c       u1(:,n) = u5(0:mx,c)-u0(0:mx,c)
        u(0:mx,c) = u(0:mx,c) + du(:,n)
        du(:,n) = 0
c       v1(:,n) = v5(0:mx,c)-v0(0:mx,c)
        v(0:mx,c) = v(0:mx,c) + dv(:,n)
        dv(:,n) = 0
c       u1lap(:,n) = u5lap(:,n)-u0lap(:,n)
c       v1lap(:,n) = v5lap(:,n)-v0lap(:,n)
      ENDIF
c     Dynamics terms.
      IF (ldyn) CALL vordivad(cf, u, v, ux, vx, uy, vy,
     &    u5, v5, u5x, v5x, u5y, v5y, eta5, vel5,
     &    eta, vel, voru, vorv, divu, divv)
c     Derivatives.
      CALL dpqdxyad(u, v, ux, vx, uy, vy, ulap, vlap)
      END SUBROUTINE calcqad

      SUBROUTINE dpqdxyad(p, q, px, qx, py, qy, plap, qlap)
c     Calculate spatial derivatives for p and q.
      REAL, INTENT(INOUT), DIMENSION(i1:in,j1:jn) :: p,q
      REAL, INTENT(INOUT), DIMENSION(0:mx, 0:1) :: px,qx,py,qy,plap,qlap
      REAL, DIMENSION(0:mx) :: pxx,qxx,pyy,qyy
c     initialize
      pxx(:)=0 ; qxx(:)=0 ; pyy(:)=0 ; qyy(:)=0
c     plap(:,n) = pxx(:) - gamma(c)*px(:,n) + pyy(:)
      pxx(:) = pxx(:) + plap(:,n)
      px(:,n) = px(:,n) - gamma(c)*plap(:,n)
      pyy(:) = pyy(:) + plap(:,n)
      plap(:,n) = 0
c     qlap(:,n) = qxx(:) - gamma(c)*qx(:,n) + qyy(:)
      qxx(:) = qxx(:) + qlap(:,n)
      qx(:,n) = qx(:,n) - gamma(c)*qlap(:,n)
      qyy(:) = qyy(:) + qlap(:,n)
      qlap(:,n) = 0
      CALL dwdxad(p, px, pxx)
      CALL dwdxad(q, qx, qxx)
      CALL dwdyad(p, py, pyy)
      CALL dwdyad(q, qy, qyy)
      END SUBROUTINE dpqdxyad

      SUBROUTINE dwdxad(w, wx, wxx)
c     Use centered difference for longitude derivative.
      REAL, INTENT(INOUT) :: w(i1:in,j1:jn)
      REAL, INTENT(INOUT) :: wx(0:mx, 0:1), wxx(0:mx)
c     wx(:,n)=rdx(c)*(w(1:mx+1,c)-w(-1:mx-1,c))
      w(1:mx+1,c) = w(1:mx+1,c) + rdx(c)*wx(:,n)
      w(-1:mx-1,c) = w(-1:mx-1,c) - rdx(c)*wx(:,n)
      wx(:,n) = 0
c     wxx=4*rdx(c)**2*(w(1:mx+1,c)-2*w(0:mx,c)+w(-1:mx-1,c))
      w(1:mx+1,c) = w(1:mx+1,c) + 4*rdx(c)**2*wxx(:)
      w(0:mx,c) = w(0:mx,c) - 8*rdx(c)**2*wxx(:)
      w(-1:mx-1,c) = w(-1:mx-1,c) + 4*rdx(c)**2*wxx(:)
      wxx(:) = 0
      END SUBROUTINE dwdxad

      SUBROUTINE dwdyad(w, wy, wyy)
c     Use centered difference for latitude derivative.
      REAL, INTENT(INOUT) :: w(i1:in,j1:jn)
      REAL, INTENT(INOUT) :: wy(0:mx, 0:1), wyy(0:mx)
c     wy(:,n) = rdy*(w(0:mx,t) - w(0:mx,b))
      w(0:mx,t) = w(0:mx,t) + rdy*wy(:,n)
      w(0:mx,b) = w(0:mx,b) - rdy*wy(:,n)
      wy(:,n) = 0
c     wyy = 4*rdy**2*(w(0:mx,t) - 2*w(0:mx,c) + w(0:mx,b))
      w(0:mx,t) = w(0:mx,t) + 4*rdy**2*wyy(:)
      w(0:mx,c) = w(0:mx,c) - 8*rdy**2*wyy(:)
      w(0:mx,b) = w(0:mx,b) + 4*rdy**2*wyy(:)
      wyy(:) = 0
      END SUBROUTINE dwdyad

      SUBROUTINE vordivad(cf, u, v, ux, vx, uy, vy,
     &    u5, v5, u5x, v5x, u5y, v5y, eta5, vel5,
     &    eta, vel, voru, vorv, divu, divv)
c     Calculate Q terms for vorticity and divergence tendency.
      REAL, INTENT(IN) :: cf
      REAL, INTENT(INOUT), DIMENSION(i1:in,j1:jn) :: u, v
      REAL, INTENT(INOUT), DIMENSION(0:mx, 0:1) :: ux, vx, uy, vy
      REAL, INTENT(IN), DIMENSION(i1:in,j1:jn) :: u5, v5
      REAL, INTENT(IN), DIMENSION(0:mx, 0:1) :: u5x, v5x, u5y, v5y
      REAL, INTENT(IN), DIMENSION(0:mx, 0:1) :: eta5, vel5
      REAL, INTENT(INOUT), DIMENSION(0:mx, 0:1) ::
     &    eta, vel, voru, vorv, divu, divv
c     Q terms for vorticity and divergence equation. [Quadratic]
      IF (llamda(ddiv)) THEN
c     divu(:,n) = u(0:mx,c)*ux(:,n) + v(0:mx,c)*vx(:,n) - vorv(:,n)
        u(0:mx,c) = u(0:mx,c) + divu(:,n)*u5x(:,n)
        ux(:,n) = ux(:,n) + u5(0:mx,c)*divu(:,n)
        v(0:mx,c) = v(0:mx,c) + divu(:,n)*v5x(:,n)
        vx(:,n) = vx(:,n) + v5(0:mx,c)*divu(:,n)
        vorv(:,n) = vorv(:,n) - divu(:,n)
        divu(:,n) = 0
c     divv(:,n) = u(0:mx,c)*uy(:,n) + v(0:mx,c)*vy(:,n) + voru(:,n)
        u(0:mx,c) = u(0:mx,c) + divv(:,n)*u5y(:,n)
        uy(:,n) = uy(:,n) + u5(0:mx,c)*divv(:,n)
        v(0:mx,c) = v(0:mx,c) + divv(:,n)*v5y(:,n)
        vy(:,n) = vy(:,n) + v5(0:mx,c)*divv(:,n)
        voru(:,n) = voru(:,n) + divv(:,n)
        divv(:,n) = 0
      ENDIF
c     voru(:,n) = eta(:,n)*u(0:mx,c) - cf*Vel(:,n)*v(0:mx,c)
      eta(:,n) = eta(:,n) + voru(:,n)*u5(0:mx,c)
      u(0:mx,c) = u(0:mx,c) + eta5(:,n)*voru(:,n)
      Vel(:,n) = Vel(:,n) - cf*voru(:,n)*v5(0:mx,c)
      v(0:mx,c) = v(0:mx,c) - cf*Vel5(:,n)*voru(:,n)
      voru(:,n) = 0
c     vorv(:,n) = eta(:,n)*v(0:mx,c) + cf*Vel(:,n)*u(0:mx,c)
      eta(:,n) = eta(:,n) + vorv(:,n)*v5(0:mx,c)
      v(0:mx,c) = v(0:mx,c) + eta5(:,n)*vorv(:,n)
      Vel(:,n) = Vel(:,n) + cf*vorv(:,n)*u5(0:mx,c)
      u(0:mx,c) = u(0:mx,c) + cf*Vel5(:,n)*vorv(:,n)
      vorv(:,n) = 0
c     Enforce minimum velocity.
      where (vel5(:,n)<velmin) vel(:,n) = 0
c     Velocity magnitude. [Sqrt: vel5*vel=u5*u+v5*v]
c     vel(:,n) = SQRT(u(0:mx,c)**2 + v(0:mx,c)**2)
      u(0:mx,c) = u(0:mx,c) + vel(:,n)*u5(0:mx,c)/vel5(:,n)
      v(0:mx,c) = v(0:mx,c) + vel(:,n)*v5(0:mx,c)/vel5(:,n)
      vel(:,n) = 0
c     Total vorticity. [Linear]
c     eta(:,n) = vx(:,n) - uy(:,n) + gamma(c)*u(0:mx,c) + f(c)
      vx(:,n) = vx(:,n) + eta(:,n)
      uy(:,n) = uy(:,n) - eta(:,n)
      u(0:mx,c) = u(0:mx,c) + gamma(c)*eta(:,n)
      eta(:,n) = 0
      END SUBROUTINE vordivad

      SUBROUTINE calcrad
c     Calculates r terms at grid box centers.
c     Average wind components (increments)
      IF (llamda(delu)) CALL w2barad(du, u1, r(:,delu))
      IF (llamda(delv)) CALL w2barad(dv, v1, r(:,delv))
c     Divergence, vorticity, Laplacian of u, v
      IF (llamda(div))  CALL q2divad(du, dv, r(:,div))
      IF (llamda(vor))  THEN
c     CALL q2div(-v1, u1, r5(:,vor))
        dv(:,:) = -dv(:,:)
        CALL q2divad(dv, du, r(:,vor))
        dv(:,:) = -dv(:,:)
      ENDIF
      IF (llamda(lapu)) CALL w2barad(ulap, u1lap, r(:,lapu))
      IF (llamda(lapv)) CALL w2barad(vlap, v1lap, r(:,lapv))
c     Dynamic constraints
      IF (llamda(dvor)) CALL q2divad(voru, vorv, r(:,dvor))
      IF (llamda(ddiv)) CALL q2divad(divu, divv, r(:,ddiv))
      END SUBROUTINE calcrad

      SUBROUTINE w2barad(w, w5, rsq)
c     Squared value at center of grid box.
      REAL, INTENT(INOUT):: w(0:mx, 0:1)
      REAL, INTENT(IN):: w5(0:mx, 0:1)
      REAL, INTENT(INOUT) :: rsq(mx)
c     rsq=(w(0:mx-1,n)**2+w(1:mx,n)**2+w(0:mx-1,s)**2+w(1:mx,s)**2)/4
      w(0:mx-1,n) = w(0:mx-1,n) + w5(0:mx-1,n)*rsq/2
      w(1:mx,  n) = w(1:mx,  n) + w5(1:mx,  n)*rsq/2
      w(0:mx-1,s) = w(0:mx-1,s) + w5(0:mx-1,s)*rsq/2
      w(1:mx,  s) = w(1:mx,  s) + w5(1:mx,  s)*rsq/2
      rsq(:) = 0
      END SUBROUTINE w2barad

      SUBROUTINE q2divad(p, q, rdiv)
c     Divergence at center of box.
      REAL, INTENT(INOUT), DIMENSION(0:mx, 0:1) :: p, q
      REAL, INTENT(INOUT) :: rdiv(mx)
c     rdiv(:) = adiv(lat) * (p(1:mx,n)+p(1:mx,s)-p(0:mx-1,n)-p(0:mx-1,s))
c    &    + bdiv(lat) * (q(1:mx,n)+q(0:mx-1,n))
c    &    - cdiv(lat) * (q(1:mx,s)+q(0:mx-1,s))
      p(1:mx,  n) = p(1:mx,  n) + adiv(lat)*rdiv(:)
      p(1:mx,  s) = p(1:mx,  s) + adiv(lat)*rdiv(:)
      p(0:mx-1,n) = p(0:mx-1,n) - adiv(lat)*rdiv(:)
      p(0:mx-1,s) = p(0:mx-1,s) - adiv(lat)*rdiv(:)
      q(1:mx,  n) = q(1:mx,  n) + bdiv(lat)*rdiv(:)
      q(0:mx-1,n) = q(0:mx-1,n) + bdiv(lat)*rdiv(:)
      q(1:mx,  s) = q(1:mx,  s) - cdiv(lat)*rdiv(:)
      q(0:mx-1,s) = q(0:mx-1,s) - cdiv(lat)*rdiv(:)
      rdiv(:) = 0
      END SUBROUTINE q2divad
  
      END MODULE jb_mod
