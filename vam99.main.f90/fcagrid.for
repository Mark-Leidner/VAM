
c     -----------------------------------------------------------------

<NLM> SUBROUTINE fcagrid
<LTM> SUBROUTINE fcagridtl
<ADJ> SUBROUTINE fcagridad

c**** fcagrid determines adjusted winds from adjustment and background

c!#   $Id: fcagrid.for,v 1.1 2005/03/21 20:26:23 rnh Exp $

c     PURPOSE

c     Applies adjustment winds determine source locations in background.
c     Background is then interpolated to these locations to provide the
c     adjusted winds.

c**   INTERFACE

     c     ( u0, v0,
     i       du, dv,
<PER>t       du5, dv5, ua5, va5,
     o       ua, va )

c     u0,v0     background field to be interpolated
c     du,dv     adjustment winds
c     ua,va     adjusted winds
<ALL> REAL, INTENT(IN) :: u0(nlon,nlat), v0(nlon,nlat)
<ALL> REAL, INTENT(INOUT) :: du(nlon,nlat), dv(nlon,nlat)
<PER> REAL, INTENT(IN) :: du5(nlon,nlat), dv5(nlon,nlat)
<PER> REAL, INTENT(IN) :: ua5(nlon,nlat), va5(nlon,nlat)
<ALL> REAL, INTENT(INOUT) :: ua(nlon,nlat), va(nlon,nlat)

c     Local data
<ALL> REAL lonr(nlon), latr(nlat), coslat(nlat)
<ALL> INTEGER i, j
<ALL> LOGICAL lerrx

c     ------------------------------------------------------------------

c     1. Initialize parameters related to the grid
c     If this initialization is saved, then it only needs to be
c     repeated it the grid structure changes.
c     That is if (nlon,nlat,lon0,dlon,lat0,dlat) are unchanged
c     then the initialization does not have to be repeated.

c     1.1 Data region for this purpose is the entire grid.
c     These variables are local and saved in fca_grid_mod.
<ALL> xmin = 1
<ALL> xmax = nlon
<ALL> ymin = 1
<ALL> ymax = nlat
<ALL> xper=igrid(2*pi,0.0,dlon,0,nlon,1,lerrx) - 1
c     If an error condition occurs then 2*pi is not divisable
c     in the sense that xper*dlon = 2*pi.  For true wrap around
c     we also must have 1 + xper .LE. nlon.
c     Otherwise set xper to zero.
<ALL> IF (lerrx .OR. 1 + xper .GT. nlon) xper = 0

c     1.2 Define lat, lon, and cos(lat) for the grid.
<ALL> lonr(:) = lon0 + (/(i,i=0,(nlon-1))/)*dlon
<ALL> latr(:) = lat0 + (/(j,j=0,(nlat-1))/)*dlat
<ALL> coslat(:) = COS(latr(:))

c     ------------------------------------------------------------------

c     2. Determine adjusted winds
c     For each grid point evaluate fcainterp and store in (ua5,va5).
c     Note that (du5,v5) already contains the adjustment winds at the
c     grid points.
      DO j = 1, nlat
        DO i = 1, nlon
<NLM>     CALL fcainterp
<LTM>     CALL fcainterptl
<ADJ>     CALL fcainterpad
     C        ( lonr(i), latr(j), coslat(j),
     C        u0, v0,
     I        du(i,j), dv(i,j),
<PER>T        du5(i,j), dv5(i,j), ua5(i,j), va5(i,j),
     O        ua(i,j), va(i,j) )
        END DO
      END DO

<NLM> END SUBROUTINE fcagrid
<LTM> END SUBROUTINE fcagridtl
<ADJ> END SUBROUTINE fcagridad
