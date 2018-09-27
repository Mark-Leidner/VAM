c     ******************************************************************

<NLM> SUBROUTINE uvinterp
<LTM> SUBROUTINE uvinterptl
<ADJ> SUBROUTINE uvinterpad

c**** uvinterp interpolates the analysis to a set of observation locations

c!#   $Id: uvinterp.for,v 1.3 2000/01/24 15:44:10 trn Exp $

c     PURPOSE

c     Interpolates gridded analysis to set of observation points.
     
c**   INTERFACE

     c     ( nx, ny,
     c       xi, yi,
     i       u, v,
     o       ui, vi )

cPER> LINEAR ROUTINE - NO TRAJECTORY VALUES !!!

c!#~   nx        number of longitude grid points
c!#~   ny        number of latitude grid points
c     xi,yi     location  of obs in grid coordinates
c     u,v       gridpoint values
c     ui,vi     interpolated values
      INTEGER, INTENT(IN) :: nx, ny
      REAL, INTENT(IN) :: xi, yi
<NLM> REAL, INTENT(IN) :: u(nx,ny), v(nx,ny)
<NLM> REAL, INTENT(OUT) :: ui, vi
<LTM> REAL, INTENT(IN) :: u(nx,ny), v(nx,ny)
<LTM> REAL, INTENT(OUT) :: ui, vi
<ADJ> REAL, INTENT(INOUT) :: u(nx,ny), v(nx,ny)
<ADJ> REAL, INTENT(INOUT) :: ui, vi

c     Local variables:
c     The grid cell is identified by its lower left corner (LLC).
c     i,j        indices of lower left-hand corner of grid box
c     xobs       x-coordinate of observation within the grid cell
c     yobs       y-coordinate of observation within the grid cell
      INTEGER i,j
      REAL xobs, yobs

c     ------------------------------------------------------------------

c     1. Interpolate analysis to location of observation.

c-----Determine grid cell and location within grid cell.
<ALL> IF (interp.EQ.0) STOP 'uvinterp(tl,ad): interp undefined'
<ALL> i=INT(xi)
<ALL> xobs=MOD(xi,1.0)
<ALL> j=INT(yi)
<ALL> yobs=MOD(yi,1.0)

c-----Calculate weights for interpolation

<ALL> CALL bidimw ( xobs, yobs )

c-----Get analyzed values interpolated to point

<NLM> CALL bidim
<LTM> CALL bidimtl
<ADJ> CALL bidimad
     c    ( nx, ny,
     i    u, i, j,
     o    ui )
      
<NLM> CALL bidim
<LTM> CALL bidimtl
<ADJ> CALL bidimad
     c    ( nx, ny,
     i    v, i, j,
     o    vi )
      
<NLM> END SUBROUTINE uvinterp
<LTM> END SUBROUTINE uvinterptl
<ADJ> END SUBROUTINE uvinterpad
