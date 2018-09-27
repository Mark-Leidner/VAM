
      MODULE ssback_mod

      USE types, ONLY: accumulator

      IMPLICIT NONE
      PRIVATE
      PUBLIC ssback, ssback_init, sscons

c     -----------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :
c!#   Integration domain (for jb):
c!#~   iwest      grid index of western edge of integration domain
c!#~   ieast      grid index of eastern edge of integration domain
c!#~   jsouth     grid index of southern edge of integration domain
c!#~   jnorth     grid index of northern edge of integration domain
      INTEGER, SAVE :: iwest, ieast, jsouth, jnorth

c     -----------------------------------------------------------------

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE ssback (jbval, slamda)

      USE grid_mod, ONLY: nlon,nlat,dlon,lat0,dlat,f,cf
      USE grid_mod, ONLY: u0,v0,u5,v5,du5,dv5,ua5,va5
      USE grid_mod, ONLY: u,v,du,dv,ua,va
      USE grid_mod, ONLY: fca_mode,cv_uv,cv_fca
      USE jb_mod, ONLY: jb

      INTEGER, PARAMETER :: nlamda=8

      REAL(accumulator), INTENT(INOUT) :: jbval
      REAL, INTENT(IN) :: slamda(nlamda)
      REAL(accumulator) :: jbval1

      INTEGER i, j 

c     -----------------------------------------------------------------

c For each case perform appropriate action to increment Jbval, and (u,v)
c and/or (ua,va).

c 0. fca_none: fca_mode=0
c    Jb compares (u5,v5) to (u0,v0).

      IF (fca_mode.EQ.0) THEN

      CALL jb
     C    (nlon, nlat, iwest, ieast, jsouth, jnorth,
     C    lat0, dlat, dlon,
     C    u0, v0, f, cf, slamda,
     I    u5, v5,
     O    u, v, jbval)

c 1. fca_only: fca_mode=1
c    No calculation.  Set Jb = 0.

c 2. fca_too: fca_mode=2
c    Jb compares (u5,v5) to (ua5,va5).

      ELSE IF (fca_mode.EQ.2) THEN

      CALL jb
     C    (nlon, nlat, iwest, ieast, jsouth, jnorth,
     C    lat0, dlat, dlon,
     C    ua5, va5, f, cf, slamda,
     I    u5, v5,
     O    u, v, jbval)

c    But (ua5,va5) depend on (du,dv), so we need to repeat the
c    calculation with the roles of (ua5,va5) and (u5,v5) reversed.  We
c    just want the contributions to (ua,va), so we replace Jbval with a
c    local version that is not further used.

      jbval1 = 0

      CALL jb
     C    (nlon, nlat, iwest, ieast, jsouth, jnorth,
     C    lat0, dlat, dlon,
     C    u5, v5, f, cf, slamda,
     I    ua5, va5,
     O    ua, va, jbval1)

c 3. fca_val: fca_mode=3
c    Jb compares (ua5,va5) to (u5,v5).
c    Since (u5,v5) is fixed and not in the control vector, we only
c    calculate the contributions to (ua,va) here.

      ELSE IF (fca_mode.EQ.3) THEN

      CALL jb
     C    (nlon, nlat, iwest, ieast, jsouth, jnorth,
     C    lat0, dlat, dlon,
     C    u5, v5, f, cf, slamda,
     I    ua5, va5,
     O    ua, va, jbval)

      END IF

      END SUBROUTINE ssback

c     -----------------------------------------------------------------

      SUBROUTINE ssback_init  (xmin,xmax,ymin,ymax,xs,dx,nx,ys,dy,ny)

      USE interp_mod, ONLY: igrid

      REAL, INTENT(IN) :: xmin,xmax,ymin,ymax,xs,dx,ys,dy
      INTEGER, INTENT(IN) :: nx,ny

      LOGICAL lerr

      iwest=igrid(xmin,xs,dx,0,2,nx-1,lerr)
      if (lerr) stop 'ssback_init: iwest'
      ieast=igrid(xmax,xs,dx,0,2,nx-1,lerr)
      if (lerr) stop 'ssback_init: ieast'
      jsouth=igrid(ymin,ys,dy,0,2,ny-1,lerr)
      if (lerr) stop 'ssback_init: jsouth'
      jnorth=igrid(ymax,ys,dy,0,2,ny-1,lerr)
      if (lerr) stop 'ssback_init: jnorth'

      END SUBROUTINE ssback_init

c     -----------------------------------------------------------------

      SUBROUTINE sscons (jbval, slamda)

      USE grid_mod, ONLY: nlon,nlat,dlon,lat0,dlat,du5,dv5,du,dv,f,cf
      USE jb_mod, ONLY: jb

      INTEGER, PARAMETER :: nlamda=8

      REAL(accumulator), INTENT(INOUT) :: jbval
      REAL, INTENT(IN) :: slamda(nlamda)

      INTEGER i, j 

c-----Allocate zeros if needed.

      INTEGER, SAVE :: my=0, mx=0, ierr=0
      REAL, SAVE, ALLOCATABLE, DIMENSION(:, :) :: zeros

      IF (.NOT. (ALLOCATED(zeros) .AND.
     &    my .EQ. nlat .AND. mx .EQ. nlon) ) THEN
        IF (ALLOCATED(zeros)) DEALLOCATE (zeros,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (zeros)'
        ALLOCATE (zeros(nlon,nlat),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (zeros(nlon,nlat))'
        zeros(:,:) = 0
        my = nlat
        mx = nlon
        write (*,*) 'zeros set: mx, my =', mx, my
      END IF

c     -----------------------------------------------------------------

      CALL jb
     C    (nlon, nlat, iwest, ieast, jsouth, jnorth,
     C    lat0, dlat, dlon,
     C    zeros, zeros, f, cf, slamda,
     I    du5, dv5,
     O    du, dv, jbval)

      END SUBROUTINE sscons

      END MODULE ssback_mod
