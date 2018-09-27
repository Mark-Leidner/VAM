      MODULE cgr_mod

      USE types, ONLY: accumulator
      USE grid_mod, ONLY: nlon,nlat,lon0,dlon,lat0,dlat
      USE grid_mod, ONLY: fca_mode,nactive,cv_uv,cv_fca

      IMPLICIT NONE
      PRIVATE
      PUBLIC cgr_init,sscgr0,cv2g,g2cv
      PUBLIC period

c!#   Parameters for control vector transformation:
c!#~   imin       grid index of western edge of active domain
c!#~   imax       grid index of eastern edge of active domain
c!#~   jmin       grid index of southern edge of active domain
c!#~   jmax       grid index of northern edge of active domain
c!#~   jsp        grid index of south pole (used only if spole)
c!#~   jnp        grid index of north pole (used only if npole) 
c!#~   npole      is north pole an active grid point
c!#~   spole      is south pole an active grid point
c!#~   gwrap      does grid wrap around in longitude
c!#~   period     period in grid units in longitude direction
c!#~   coslon     cosine of (longitude - lon0) at grid points
c!#~   sinlon     sine of (longitude - lon0) at grid points
c!#~   mt         zonal wavenumber for truncation (for filfft)
      INTEGER, SAVE :: imin, imax, jmin, jmax, jsp, jnp
      LOGICAL, SAVE :: npole, spole, gwrap
      INTEGER, SAVE :: period
      REAL, SAVE, ALLOCATABLE, DIMENSION(:) :: coslon, sinlon
      INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: mt

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE cgr_init (xmin,xmax,ymin,ymax,xs,dx,nx,ys,dy,ny,
     &    boundary)

      USE constants, ONLY: pi
      USE interp_mod, ONLY: igrid, same

c!#~   xmin      minimum value of longitude
c!#~   xmax      maximum value of longitude
c!#~   ymin      minimum value of latitude
c!#~   ymax      maximum value of latitude
c!#~   xs        longitude of first grid point
c!#~   dx        longitude increment
c!#~   nx        number of longitude grid points
c!#~   ys        latitude of first grid point
c!#~   dy        latitude increment
c!#~   ny        number of latitude grid points
c!#~   boundary  should boundary points be allowed to vary?
      REAL, INTENT(INOUT) :: xmin,xmax,ymin,ymax
      REAL, INTENT(IN) :: xs,dx,ys,dy
      INTEGER, INTENT(IN) :: nx,ny
      LOGICAL, INTENT(IN) :: boundary

c!#~   sp        latitude of the South Pole
c!#~   np        latitude of the North Pole
c!#~   ierr      error code
c!#~   i         longitude index
c!#~   j         latitude index
c!#~   lerr      error flag
      REAL, PARAMETER :: sp=-pi/2, np= pi/2
      INTEGER i,j,ierr
      LOGICAL lerr

c Does the grid wrap around in longitude
      gwrap=(nx-1)*dx .gt. 2*pi .or. same((nx-1)*dx, 2*pi)
c Determine the grid periodicity in grid units
c Disable bounds checking by reversing 1 and nx in arg list
      period=igrid(2*pi,0.0,dx,0,nx,1,lerr) - 1
c Must be an ``exact'' integer periodicity if gwrap
      if (gwrap .and. lerr) stop 'cgr_mod: period'

c The active region is defined on input to cgr_init by 
c xmin, xmax, ymin, ymax.  

c If xmax-xmin .ge. 2*pi, there are no east or west boundaries, but we
c have to reset xmax to xmin + 2*pi - dx to avoid duplicate points in
c the control vector.  Otherwise there are boundaries.  If the
c boundaries are not active then we have to increase xmin by dx and
c decrease xmax by dx.
      if (xmax-xmin .gt. 2*pi .or. same(xmax-xmin, 2*pi)) then
        xmax=xmin+2*pi-dx
      elseif (.not.boundary) then
        xmin=xmin+dx; xmax=xmax-dx
      endif

c If ymax .eq. np then there is no northern boundary, but for defining
c the control vector we set npole and decrease ymax by dy.  Otherwise
c there is a northern boundary.  If the boundaries are not active we
c also decrease ymax by dy.
      npole=same(ymax,np)
      if (npole .or. .not.boundary)  ymax=ymax-dy

c If ymin .eq. sp then there is no southern boundary, but for defining
c the control vector we set spole and increase ymin by dy.  Otherwise
c there is a southern boundary.  If the boundaries are not active we
c also increase ymin by dy.
      spole=same(ymin,sp)
      if (spole .or. .not.boundary) ymin=ymin+dy

      imin=igrid(xmin,xs,dx,0,1,nx,lerr)
      if (lerr) stop 'cgr_mod: imin'
      imax=igrid(xmax,xs,dx,0,1,nx,lerr)
      if (lerr) stop 'cgr_mod: imax'
      jmin=igrid(ymin,ys,dy,0,1,ny,lerr)
      if (lerr) stop 'cgr_mod: jmin'
      jmax=igrid(ymax,ys,dy,0,1,ny,lerr)
      if (lerr) stop 'cgr_mod: jmax'

      jsp=jmin+1; jnp=jmax-1

      nactive=(imax-imin+1)*(jmax-jmin+1)
      if (spole) nactive=nactive+1
      if (npole) nactive=nactive+1

c     Also calculate trig constants needed by transforms:
c     Even if the grid hasn't changed recalculate these:

      ierr = 0
      IF (ALLOCATED(mt)) DEALLOCATE (mt, coslon, sinlon, STAT=ierr)
      IF (ierr.NE.0) STOP 'cgr_init: DEALLOCATE (mt, ...'
      ALLOCATE (mt(jmin:jmax), coslon(nx), sinlon(nx), STAT=ierr)
      IF (ierr.NE.0) STOP 'cgr_init: ALLOCATE (mt(jmin:jmax, ...'

c     Calculate first truncated wave number
      WRITE (*,*) 'Fourier filter initialized for ni ', imax-imin+1, '.'
      DO j=jmin,jmax
        mt(j)=(imax-imin+1)*cos(ys+(j-1)*dy)*dx/dy + 1
        IF (2*mt(j).LE.imax-imin+1) 
     &      WRITE (*,*) 'Fourier filter in use at latitude ', j,
     &      ', truncation starts at wave number ', mt(j), '.'
      END DO

      coslon = COS((/(i-1,i=1,nx)/)*dx)
      sinlon = SIN((/(i-1,i=1,nx)/)*dx)

      END SUBROUTINE cgr_init

c     -----------------------------------------------------------------

      SUBROUTINE sscgr0 (ncv,x,ss,g,slamda_back,slamda_cons,
     &     slamda_obs,norm_obs,obs_data)

c     sscgr0 calculates J and its gradient g.

      USE grid_mod, ONLY: u,v,du,dv,ua,va
      USE types, ONLY: obs_data_typ
      USE vam_obs_mod, ONLY: ss_obs
      USE ssback_mod, ONLY: ssback,sscons
      USE wind_grid_stats_mod, ONLY: wind_grid_stats

c!#~   ncv       size of the control vector
c!#~   x         control vector
c!#~   slamda_back  scaled lamda weights (=lamda/lscale) for Jb
c!#~   slamda_cons  scaled lamda weights (=lamda/lscale) for Jc
c!#~   slamda_obs   scaled lamda weights (=lamda/lscale) for ss_obs
c!#~   norm_obs   sum of weights for ss_obs, defined as number of obs
c!#~   norm_obs.  that went into ss (see individual ss_obs for details)
c!#~   g         gradient of sum of squares
c!#~   ss        sum of squares (accumulator)
c!#~   obs_data  linked list of observation data
      INTEGER, INTENT(IN) :: ncv
      REAL, INTENT(IN) :: x(ncv)
      REAL, INTENT(IN) :: slamda_back(:),slamda_cons(:),slamda_obs(:)
      REAL, INTENT(OUT) :: g(ncv)
      REAL(accumulator), INTENT(OUT) :: ss
      REAL, INTENT(OUT) :: norm_obs(:)
      TYPE (obs_data_typ), POINTER :: obs_data

c     Transform from control vector to gridded fields

      CALL cv2g (ncv,x)

c-----Initializes accumulators
      ss=0
      norm_obs(:)=0
      IF (cv_uv .OR. fca_mode.EQ.1) THEN
         u(:,:)=0; v(:,:)=0
      END IF
      IF (cv_fca) THEN
         du(:,:)=0; dv(:,:)=0
      END IF
      IF (fca_mode.EQ.2 .OR. fca_mode.EQ.3) THEN
         ua(:,:)=0; va(:,:)=0
      END IF

c-----Calculates sum of squares and adjoints

      CALL ssback (ss, slamda_back)
      IF (fca_mode.NE.3)
     &    CALL ss_obs (ss, slamda_obs, norm_obs, obs_data)
c Calculate sum of squares for adjustment constraint also
      IF (cv_fca) CALL sscons (ss, slamda_cons)

c-----Transform gridded adjoints to gradient vector
      CALL cv2gad (ncv,g)

      END SUBROUTINE sscgr0

c     -----------------------------------------------------------------

      SUBROUTINE cv2g (ncv, x)

      USE grid_mod, ONLY: u0,v0,u5,v5,du5,dv5,ua5,va5
      USE fca_grid_mod, ONLY: fcagrid

      INTEGER, INTENT(IN) :: ncv
      REAL, INTENT(IN) :: x(ncv)

      INTEGER i0

c     Case depends on fca_mode through cv_uv and cv_fca.

c     This section constructs the gridded analysis
c     from the control vector.
c     This transformation requires map, filter, extend,
c     so that the filtered values are extended.

c     Thus transform adjoint is chain, filter, imap below.

      i0=1
      IF (cv_uv) THEN
        CALL map (x(i0),u5,v5)
	i0 = i0 + 2*nactive

c Note: filfft removes noise in longitude smaller than the latitude
c grid spacing.  We always apply this to (u5,v5)-(u0,v0) and (du5,dv5).

c-----Control small scale noise in deltas
        CALL filfft (u0,u5,.TRUE.)
        CALL filfft (v0,v5,.TRUE.)

        CALL extend (u5,v5)

      END IF

      IF (cv_fca) THEN
        CALL map (x(i0),du5,dv5)

c-----Control small scale noise in field
        CALL filfft (du5,du5,.FALSE.)
        CALL filfft (dv5,dv5,.FALSE.)

        CALL extend (du5,dv5)

      END IF

c-----Define adjusted winds
      IF (fca_mode.EQ.1) THEN
        CALL fcagrid (u0,v0,du5,dv5,u5,v5)
      ELSE IF (fca_mode.EQ.2 .OR. fca_mode.EQ.3) THEN
        CALL fcagrid (u0,v0,du5,dv5,ua5,va5)
      END IF

      END SUBROUTINE cv2g

c     -----------------------------------------------------------------

      SUBROUTINE cv2gad(ncv, g)

      USE grid_mod, ONLY: u0,v0,u5,v5,du5,dv5,ua5,va5
      USE grid_mod, ONLY: u,v,du,dv,ua,va
      USE fca_grid_mod, ONLY: fcagridad

      INTEGER, INTENT(IN) :: ncv
      REAL, INTENT(OUT) :: g(ncv)

      INTEGER i0

c     Case depends on fca_mode through cv_uv and cv_fca.

c     This section constructs the gradient of the control vector
c     from the gridded adjoint variables.
c     The adjoint of cv2g uses chain, filter, imap.

      i0=1
      IF (cv_uv) THEN

        CALL chain (u,v)

c     Note: filfft removes noise in longitude smaller than the latitude
c     grid spacing.  We always apply this to (u5,v5)-(u0,v0) and (du5,dv5).

c-----Control small scale noise in gradient
        CALL filfft (u,u,.FALSE.)
        CALL filfft (v,v,.FALSE.)

        CALL imap (g(i0),u,v)
	i0 = i0 + 2*nactive

      END IF

c-----Adjoint of define adjusted winds
      IF (fca_mode.EQ.1) THEN
        CALL fcagridad (u0,v0,du,dv,du5,dv5,u5,v5,u,v)
      ELSE IF (fca_mode.EQ.2 .OR. fca_mode.EQ.3) THEN
        CALL fcagridad (u0,v0,du,dv,du5,dv5,ua5,va5,ua,va)
      END IF

      IF (cv_fca) THEN

        CALL chain (du,dv)

c-----Control small scale noise in gradient
        CALL filfft (du,du,.FALSE.)
        CALL filfft (dv,dv,.FALSE.)

        CALL imap (g(i0),du,dv)

      END IF

      END SUBROUTINE cv2gad

c     -----------------------------------------------------------------

      SUBROUTINE g2cv(ncv, x)

      USE grid_mod, ONLY: u0,v0,u5,v5,du5,dv5

      INTEGER, INTENT(IN) :: ncv
      REAL, INTENT(OUT) :: x(ncv)

      INTEGER i0

c     Case depends on fca_mode through cv_uv and cv_fca.

c     This section constructs the control vector
c     from the gridded variables.
c     This uses filter, imap.

      i0=1
      IF (cv_uv) THEN

c     Note: filfft removes noise in longitude smaller than the latitude
c     grid spacing.  We always apply this to (u5,v5)-(u0,v0) and (du5,dv5).

c-----Control small scale noise in gradient
        CALL filfft (u0,u5,.TRUE.)
        CALL filfft (v0,v5,.TRUE.)

        CALL imap (x(i0),u5,v5)
	i0 = i0 + 2*nactive

      END IF

      IF (cv_fca) THEN

c-----Control small scale noise in gradient
        CALL filfft (du5,du5,.FALSE.)
        CALL filfft (dv5,dv5,.FALSE.)

        CALL imap (x(i0),du5,dv5)

      END IF

      END SUBROUTINE g2cv

c     -----------------------------------------------------------------

      SUBROUTINE filfft (u0,u5,ldelta)

c     filfft FFT filters around latitude circles near the poles

c     Filters increments of active region only.

c!#~   nlon       number of longitude grid points
c!#~   nlat       number of latitude grid points
c!#~   u0         wind field component (background)
c!#~   u5         wind field component (trajectory)
      REAL, INTENT(INOUT) :: u5(nlon,nlat)
      REAL, INTENT(IN)    :: u0(nlon,nlat)
c!!!----------------------stopped here on comments------------------------!!!
      LOGICAL, INTENT(IN) :: ldelta

      REAL, ALLOCATABLE, SAVE :: wsave(:), uinc(:)
      INTEGER, SAVE :: ni=0
      INTEGER j, ierr

      IF (ni.NE.(imax -imin + 1)) THEN
c-----Initialize work area for FFT
        ni = imax -imin + 1
        ierr = 0 
        IF (ALLOCATED(wsave)) DEALLOCATE(wsave, uinc, STAT=ierr)
        IF (ierr.NE.0) STOP 'filfft: DEALLOCATE(wsave, ...'
        IF (ni.GT.0) THEN
          ALLOCATE(wsave(2*ni+15), uinc(ni), STAT=ierr)
          IF (ierr.NE.0) STOP 'filfft: ALLOCATE(wsave(2*ni+15), ...'
          CALL rffti(ni,wsave)
        END IF
      END IF

      DO j=jmin,jmax
c-----Calculate first truncated wave number
        IF (2*mt(j).LE.ni) THEN
c-----Calculate deltas
          IF (ldelta) THEN
            uinc=u5(imin:imax,j)-u0(imin:imax,j)
          ELSE
            uinc=u5(imin:imax,j)
          ENDIF
c-----FFT forwards
          CALL rfftf(ni,uinc,wsave)
c-----Filter high wave numbers
          uinc(2*mt(j):ni)=0
c-----FFT backwards
          CALL rfftb(ni,uinc,wsave)
c-----Normalize
          uinc(:)=uinc(:)/ni
c-----Construct filtered analysis
          IF (ldelta) THEN
            u5(imin:imax,j)=uinc+u0(imin:imax,j)
          ELSE
            u5(imin:imax,j)=uinc
          ENDIF
        END IF
      END DO

      END SUBROUTINE filfft

c     -----------------------------------------------------------------

      SUBROUTINE map (x,u,v)

c     map maps the vector of active grid values into the (u,v) grid

c     RESTRICTIONS: must be followed by call to extend.

      REAL, INTENT(IN) :: x(2*nactive)
      REAL, INTENT(OUT) :: u(nlon,nlat),v(nlon,nlat)

      INTEGER i,j,n

c-----Initialize
      n=0
c-----Process S.P. if present
      IF (SPOLE) THEN
        n=n+1
        u(1,jsp)=x(n)
        v(1,jsp)=x(n+nactive)
      END IF
c-----Process active non polar region
      DO j=jmin,jmax
        DO i=imin,imax
          n=n+1
          u(i,j)=x(n)
          v(i,j)=x(n+nactive)
        end do
      end do
c-----Process N.P. if present
      IF (npole) THEN
        n=n+1
        u(1,jnp)=x(n)
        v(1,jnp)=x(n+nactive)
      END IF

      END SUBROUTINE map

c     -----------------------------------------------------------------

      SUBROUTINE extend (u,v)

c     extend extends grid array at poles if present and wraps grid
c     around if the active region includes all longitudes.

c     RESTRICTIONS: inactive gridpoints not defined here.

      REAL, INTENT(INOUT) :: u(nlon,nlat),v(nlon,nlat)

      INTEGER i,j

      IF (spole) THEN
        u(:,jsp)= coslon(:)*u(1,jsp)-sinlon(:)*v(1,jsp)
        v(:,jsp)= sinlon(:)*u(1,jsp)+coslon(:)*v(1,jsp)
      END IF
      IF (gwrap) THEN
        DO j=jmin,jmax
          DO i=1,imin-1
            u(i,j)=u(i+period,j)
            v(i,j)=v(i+period,j)
          END DO
          DO i=imax+1,nlon
            u(i,j)=u(i-period,j)
            v(i,j)=v(i-period,j)
          END DO
        END DO
      END IF
      IF (npole) THEN
        u(:,jnp)= coslon(:)*u(1,jnp)+sinlon(:)*v(1,jnp)
        v(:,jnp)=-sinlon(:)*u(1,jnp)+coslon(:)*v(1,jnp)
      END IF

      END SUBROUTINE extend

c     -----------------------------------------------------------------

c     CHAIN applies chain rule to gradient of J with respect to active
c     grid points.  (U,V) are altered but if extended result in a map
c     of X. Note on input (U,V) contains gradient of J with respect
c     to all grid points.
c     See comments for NP below for an explanation of the formula
      SUBROUTINE chain (u,v)

      REAL, INTENT(INOUT) :: u(nlon,nlat),v(nlon,nlat)

      INTEGER i,j

c-----Process S.P. if present
      IF (spole) THEN
        u(1,jsp)=SUM( coslon(:)*u(:,jsp)+sinlon(:)*v(:,jsp))
        v(1,jsp)=SUM(-sinlon(:)*u(:,jsp)+coslon(:)*v(:,jsp))
      END IF
c-----Process active non polar region
      IF (gwrap) THEN
        DO j=jmin,jmax
          DO i=1,imin-1
            u(i+period,j)=u(i+period,j)+u(i,j)
            v(i+period,j)=v(i+period,j)+v(i,j)
          END DO
          DO i=imax+1,nlon
            u(i-period,j)=u(i-period,j)+u(i,j)
            v(i-period,j)=v(i-period,j)+v(i,j)
          END DO
        END DO
      END IF
c-----Process N.P. if present
c..Explanation of formula:
c.. u_i and v_i are dJ/du_i and dJ/dv_i on input.  We want to calculate
c.. dJ/du_* and dJ/dv_*, where * indicates the single value stored in the
c.. control vector.  For example,
c.. 
c.. dJ/du_* = sum_i dJ/du_i du_i/du_* +  dJ/dv_i dv_i/du_*
c..         = sum_i dJ/du_i cos_i     -  dJ/dv_i sin_i
c.. 
c.. since we know from extend at the np that
c.. 
c.. du_i/du = cos_i, BUT dv_i/du_* = -sin_i

      IF (npole) THEN
        u(1,jnp)=SUM( coslon(:)*u(:,jnp)-sinlon(:)*v(:,jnp))
        v(1,jnp)=SUM( sinlon(:)*u(:,jnp)+coslon(:)*v(:,jnp))
      END IF

      END SUBROUTINE chain

c     -----------------------------------------------------------------

      SUBROUTINE imap(x,u,v)

c     IMAP reverses action of MAP, mapping the active grid points in
c     the array onto the vector X.

      REAL, INTENT(OUT) :: x(2*nactive)
      REAL, INTENT(IN) :: u(nlon,nlat),v(nlon,nlat)

      INTEGER i,j,n

c-----Initialize
      n=0
c-----Process S.P. if present
      IF (spole) THEN
        n=n+1
        x(n)=u(1,jsp)
        x(n+nactive)=v(1,jsp)
      END IF
c-----Process active non polar region
      DO j=jmin,jmax
        DO i=imin,imax
          n=n+1
          x(n)=u(i,j)
          x(n+nactive)=v(i,j)
        END DO
      END DO
c-----Process N.P. if present
      IF (npole) THEN
        n=n+1
        x(n)=u(1,jnp)
        x(n+nactive)=v(1,jnp)
      END IF

      END SUBROUTINE imap

      END MODULE cgr_mod
