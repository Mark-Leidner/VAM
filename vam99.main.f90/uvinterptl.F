c     ******************************************************************

      SUBROUTINE uvinterptl

c**** uvinterp interpolates the analysis to a set of observation locations

c!#   $Id: uvinterptl.F,v 1.1 2000/11/06 19:53:26 mcc Exp $

c     PURPOSE

c     Interpolates gridded analysis to set of observation points.
     
c**   INTERFACE

     c     ( nx, ny,
     c       xi, yi,
     i       u, v,
     o       ui, vi )

c     LINEAR ROUTINE - NO TRAJECTORY VALUES !!!

c!#~   nx        number of longitude grid points
c!#~   ny        number of latitude grid points
c     xi,yi     location  of obs in grid coordinates
c     u,v       gridpoint values
c     ui,vi     interpolated values
      INTEGER, INTENT(IN) :: nx, ny
      REAL, INTENT(IN) :: xi, yi
      REAL, INTENT(IN) :: u(nx,ny), v(nx,ny)
      REAL, INTENT(OUT) :: ui, vi

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
      IF (interp.EQ.0) STOP 'uvinterp(tl,ad): interp undefined'
      i=INT(xi)
      xobs=MOD(xi,1.0)
      j=INT(yi)
      yobs=MOD(yi,1.0)

c-----Calculate weights for interpolation

      CALL bidimw ( xobs, yobs )

c-----Get analyzed values interpolated to point

      CALL bidimtl
     c    ( nx, ny,
     i    u, i, j,
     o    ui )
      
      CALL bidimtl
     c    ( nx, ny,
     i    v, i, j,
     o    vi )
      
      END SUBROUTINE uvinterptl
