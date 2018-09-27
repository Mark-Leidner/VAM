
c     -----------------------------------------------------------------

      SUBROUTINE fcainterptl

c**** fcainterp interpolates the background to a set of adjusted locations

c!#   $Id: fcainterptl.F,v 1.2 2005/03/21 20:26:23 rnh Exp $

c     PURPOSE

c     Interpolates gridded background to set of observation points,
c     after first adjusting the locations according to the adjustment
c     velocity.

c**   INTERFACE

     c     ( lonr, latr, coslat,
     c       u0, v0,
     i       du, dv,
     t       du5, dv5, ua5, va5,
     o       ua, va )

c     Global data
c     Note xper, xmin, xmax, ymin, ymax are module-scope variables.
c     report_faults, tfactor are module-scope variables.

c     lonr,latr adjustment winds location
c     coslat    COS(latr)
c     u0,v0     background field to be interpolated
c     du,dv     adjustment winds at latr, lonr
c     ua,va     adjusted winds at latr, lonr
      REAL, INTENT(IN) :: lonr, latr, coslat
      REAL, INTENT(IN) :: u0(nlon,nlat), v0(nlon,nlat)
      REAL, INTENT(INOUT) :: du , dv
      REAL, INTENT(IN) :: du5, dv5
      REAL, INTENT(IN) :: ua5, va5
      REAL, INTENT(INOUT) :: ua , va

c     Local data
      REAL, PARAMETER :: m2rad = pi/(111*1000*180)
      REAL xi, yi
      INTEGER i, j
      LOGICAL lerrx, lerry
      REAL lon , lat , x , y
      REAL w11 , w12 , w21 , w22
      REAL lon5, lat5, x5, y5
c     m2rad     convert meters to radians
c     lon ,lat  source location in radians
c     xi, yi    source location in grid units
c     x, y      xi, yi relative to grid corner
c     i, j      grid corner for xi, yi
c     w[12][12] bilinear interpolation weights

c     ------------------------------------------------------------------

c     1. Determine source location

      lon5 = lonr + coslat*du5*tfactor*m2rad
      lon  =        coslat*du *tfactor*m2rad

      lat5 = latr + dv5*tfactor*m2rad
      lat  =        dv *tfactor*m2rad

c     ------------------------------------------------------------------

c     2. Convert to grid units

      xi=xgrid(lon5,lon0,dlon,xper,xmin,xmax,lerrx)
      yi=xgrid(lat5,lat0,dlat, 0.0,ymin,ymax,lerry)

c-----Determine grid cell and location within grid cell.
      i=INT(xi)
      j=INT(yi)

c     NB: xi, lerrx, i and yi, lerry, j are control parameters
c     if point is outside of domain then lerr(xy) is set,
c     constant extrapolation is used, and the derivatives are zero
      x5=MOD(xi,1.0)
      y5=MOD(yi,1.0)

c     Here we note that the linear model of MOD is x = xi and
c     the linear model of xgrid is xi = lon/dlon
      x = 0
      IF (.NOT. lerrx) x = lon/dlon
      y = 0
      IF (.NOT. lerry) y = lat/dlat

c     ------------------------------------------------------------------

c     3. Calculate weights for bilinear interpolation

      w11 = -x*(1-y5) - (1-x5)*y

      w12 = -x*y5 + (1-x5)*y

      w21 = x*(1-y5) - x5*y

      w22 = x*y5 + x5*y

c     ------------------------------------------------------------------

c     5. Get background values interpolated to adjusted point

c     NOTE: Normally in the adjoint code (ua, va) would be set to zero.
c     This is skipped here.  They are set to zero at the start of
c     each call to sscgr0.  This way the values can be written to a file.

      ua = w11*u0(i,j)+w12*u0(i,j+1)+w21*u0(i+1,j)+w22*u0(i+1,j+1)

      va = w11*v0(i,j)+w12*v0(i,j+1)+w21*v0(i+1,j)+w22*v0(i+1,j+1)

      END SUBROUTINE fcainterptl
