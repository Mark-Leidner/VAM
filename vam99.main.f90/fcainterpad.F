
c     -----------------------------------------------------------------

      SUBROUTINE fcainterpad

c**** fcainterp interpolates the background to a set of adjusted locations

c!#   $Id: fcainterpad.F,v 1.2 2005/03/21 20:26:23 rnh Exp $

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
      lat5 = latr + dv5*tfactor*m2rad
      xi=xgrid(lon5,lon0,dlon,xper,xmin,xmax,lerrx)
      yi=xgrid(lat5,lat0,dlat, 0.0,ymin,ymax,lerry)
      i=INT(xi)
      j=INT(yi)
      x5=MOD(xi,1.0)
      y5=MOD(yi,1.0)
      w22 = 0
      w21 = 0
      w12 = 0
      w11 = 0
      y = 0
      x = 0
      lat = 0
      lon = 0
      w22 = w22 + va*v0(i+1,j+1)
      w21 = w21 + va*v0(i+1,j)
      w12 = w12 + va*v0(i,j+1)
      w11 = w11 + va*v0(i,j)
      w22 = w22 + ua*u0(i+1,j+1)
      w21 = w21 + ua*u0(i+1,j)
      w12 = w12 + ua*u0(i,j+1)
      w11 = w11 + ua*u0(i,j)
      y  = y  + x5*w22
      x  = x  + w22*y5
      w22 = 0
      y  = y  - x5*w21
      x  = x  + w21*(1-y5)
      w21 = 0
      y  = y  + (1-x5)*w12
      x  = x  - w12*y5
      w12 = 0
      y  = y  - (1-x5)*w11
      x  = x  - w11*(1-y5)
      w11 = 0
      IF (.NOT. lerry) lat = lat + y/dlat
      y = 0
      IF (.NOT. lerrx) lon = lon + x/dlon
      x = 0
      dv  =        dv  +        lat*tfactor*m2rad
      lat  = 0
      du  =        du  +        coslat*lon*tfactor*m2rad
      lon  = 0
      END SUBROUTINE fcainterpad
