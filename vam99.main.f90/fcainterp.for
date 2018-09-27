
c     -----------------------------------------------------------------

<NLM> SUBROUTINE fcainterp
<LTM> SUBROUTINE fcainterptl
<ADJ> SUBROUTINE fcainterpad

c**** fcainterp interpolates the background to a set of adjusted locations

c!#   $Id: fcainterp.for,v 1.2 2005/03/21 20:26:23 rnh Exp $

c     PURPOSE

c     Interpolates gridded background to set of observation points,
c     after first adjusting the locations according to the adjustment
c     velocity.

c**   INTERFACE

     c     ( lonr, latr, coslat,
     c       u0, v0,
     i       du, dv,
<PER>t       du5, dv5, ua5, va5,
     o       ua, va )

c     Global data
c     Note xper, xmin, xmax, ymin, ymax are module-scope variables.
c     report_faults, tfactor are module-scope variables.

c     lonr,latr adjustment winds location
c     coslat    COS(latr)
c     u0,v0     background field to be interpolated
c     du,dv     adjustment winds at latr, lonr
c     ua,va     adjusted winds at latr, lonr
<ALL> REAL, INTENT(IN) :: lonr, latr, coslat
<ALL> REAL, INTENT(IN) :: u0(nlon,nlat), v0(nlon,nlat)
<ALL> REAL, INTENT(INOUT) :: du , dv
<PER> REAL, INTENT(IN) :: du5, dv5
<PER> REAL, INTENT(IN) :: ua5, va5
<ALL> REAL, INTENT(INOUT) :: ua , va

c     Local data
<ALL> REAL, PARAMETER :: m2rad = pi/(111*1000*180)
<ALL> REAL xi, yi
<ALL> INTEGER i, j
<ALL> LOGICAL lerrx, lerry
<ALL> REAL lon , lat , x , y
<ALL> REAL w11 , w12 , w21 , w22
<PER> REAL lon5, lat5, x5, y5
c     m2rad     convert meters to radians
c     lon ,lat  source location in radians
c     xi, yi    source location in grid units
c     x, y      xi, yi relative to grid corner
c     i, j      grid corner for xi, yi
c     w[12][12] bilinear interpolation weights

c     ------------------------------------------------------------------

c     1. Determine source location

<NLM> lon  = lonr + coslat*du *tfactor*m2rad
<PER> lon5 = lonr + coslat*du5*tfactor*m2rad
<LTM> lon  =        coslat*du *tfactor*m2rad
<ADJ> lon  = 0
<ADJ> du  +=        coslat*lon*tfactor*m2rad

<NLM> lat  = latr + dv *tfactor*m2rad
<PER> lat5 = latr + dv5*tfactor*m2rad
<LTM> lat  =        dv *tfactor*m2rad
<ADJ> lat  = 0
<ADJ> dv  +=        lat*tfactor*m2rad

c     ------------------------------------------------------------------

c     2. Convert to grid units

<NLM> xi=xgrid(lon ,lon0,dlon,xper,xmin,xmax,lerrx)
<NLM> yi=xgrid(lat ,lat0,dlat, 0.0,ymin,ymax,lerry)
<PER> xi=xgrid(lon5,lon0,dlon,xper,xmin,xmax,lerrx)
<PER> yi=xgrid(lat5,lat0,dlat, 0.0,ymin,ymax,lerry)
<NLM> IF (report_faults) THEN
<NLM>   IF (lerrx) WRITE (*,*) 'xgrid lerrx: ', xi, lon
<NLM>   IF (lerry) WRITE (*,*) 'xgrid lerry: ', yi, lat
<NLM> END IF 

c-----Determine grid cell and location within grid cell.
<ALL> i=INT(xi)
<ALL> j=INT(yi)

c     NB: xi, lerrx, i and yi, lerry, j are control parameters
c     if point is outside of domain then lerr(xy) is set,
c     constant extrapolation is used, and the derivatives are zero
<NLM> x =MOD(xi,1.0)
<NLM> y =MOD(yi,1.0)
<PER> x5=MOD(xi,1.0)
<PER> y5=MOD(yi,1.0)

cPER> Here we note that the linear model of MOD is x = xi and
cPER> the linear model of xgrid is xi = lon/dlon
<LTM> x = 0
<LTM> IF (.NOT. lerrx) x = lon/dlon
<ADJ> x = 0
<ADJ> IF (.NOT. lerrx) lon = lon + x/dlon
<LTM> y = 0
<LTM> IF (.NOT. lerry) y = lat/dlat
<ADJ> y = 0
<ADJ> IF (.NOT. lerry) lat = lat + y/dlat

c     ------------------------------------------------------------------

c     3. Calculate weights for bilinear interpolation

<NLM> w11 = (1-x)*(1-y)
<LTM> w11 = -x*(1-y5) - (1-x5)*y
<ADJ> w11 = 0
<ADJ> x  += -w11*(1-y5)
<ADJ> y  += -(1-x5)*w11

<NLM> w12 = (1-x)*y
<LTM> w12 = -x*y5 + (1-x5)*y
<ADJ> w12 = 0
<ADJ> x  += -w12*y5
<ADJ> y  += (1-x5)*w12

<NLM> w21 = x*(1-y)
<LTM> w21 = x*(1-y5) - x5*y
<ADJ> w21 = 0
<ADJ> x  += w21*(1-y5)
<ADJ> y  += -x5*w21

<NLM> w22 = x*y
<LTM> w22 = x*y5 + x5*y
<ADJ> w22 = 0
<ADJ> x  += w22*y5
<ADJ> y  += x5*w22

c     ------------------------------------------------------------------

c     5. Get background values interpolated to adjusted point

c     NOTE: Normally in the adjoint code (ua, va) would be set to zero.
c     This is skipped here.  They are set to zero at the start of
c     each call to sscgr0.  This way the values can be written to a file.

<NLM> ua = w11*u0(i,j)+w12*u0(i,j+1)+w21*u0(i+1,j)+w22*u0(i+1,j+1)
<LTM> ua = w11*u0(i,j)+w12*u0(i,j+1)+w21*u0(i+1,j)+w22*u0(i+1,j+1)
cADJ> ua = 0
<ADJ> w11 += ua*u0(i,j)
<ADJ> w12 += ua*u0(i,j+1)
<ADJ> w21 += ua*u0(i+1,j)
<ADJ> w22 += ua*u0(i+1,j+1)

<NLM> va = w11*v0(i,j)+w12*v0(i,j+1)+w21*v0(i+1,j)+w22*v0(i+1,j+1)
<LTM> va = w11*v0(i,j)+w12*v0(i,j+1)+w21*v0(i+1,j)+w22*v0(i+1,j+1)
cADJ> va = 0
<ADJ> w11 += va*v0(i,j)
<ADJ> w12 += va*v0(i,j+1)
<ADJ> w21 += va*v0(i+1,j)
<ADJ> w22 += va*v0(i+1,j+1)
cADJ>
cADJ> ------------------------------------------------------------------
cADJ>
cADJ> 6. Local variables must be intialized for adjoint
cADJ>
<ADJ> lon = 0
<ADJ> lat = 0
<ADJ> x = 0
<ADJ> y = 0
<ADJ> w11 = 0
<ADJ> w12 = 0
<ADJ> w21 = 0
<ADJ> w22 = 0

<NLM> END SUBROUTINE fcainterp
<LTM> END SUBROUTINE fcainterptl
<ADJ> END SUBROUTINE fcainterpad
