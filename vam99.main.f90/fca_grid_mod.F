
      MODULE fca_grid_mod

      USE constants, ONLY: pi
      USE grid_mod, ONLY: nlon,nlat,lon0,dlon,lat0,dlat
      USE interp_mod, ONLY: igrid, xgrid
      USE wind_grid_stats_mod, ONLY: wind_grid_stats

      IMPLICIT NONE
      PRIVATE

      PUBLIC test
      PUBLIC fcagrid, fcagridtl, fcagridad

      LOGICAL, SAVE :: report_faults
      REAL, SAVE :: tfactor=60*60
      REAL, SAVE :: xmin, xmax, ymin, ymax, xper

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE test (iuvam, data_begin)

c     English Name: Test

c     Purpose: This version is used to initialize FCA runs

c**********************************************************************

      USE types, ONLY: obs_data_typ
      USE grid_mod, ONLY: u0,v0,du5,dv5,ua5,va5

      INTEGER, INTENT(IN) :: iuvam
      TYPE(obs_data_typ), POINTER :: data_begin

      LOGICAL start, gridstats
      NAMELIST /fca/ start, gridstats, report_faults, tfactor

c     start      if true set things up, else finish
c     gridstats  calculate and display gridstats
c     report_faults report out of bounds
c     tfactor    time interval to apply adjustment winds

      INTEGER ierr
      LOGICAL lerr

c     1. Initialize and read namelist

      start = .FALSE.
      gridstats = .FALSE.
      report_faults = .FALSE.
      READ (*, fca, iostat=ierr)
      WRITE (*, fca)
      IF (ierr .NE. 0) STOP 'Error reading namelist fca'

      IF (gridstats) THEN
        IF (ALLOCATED(u0))
     &      CALL wind_grid_stats(u0,v0,nlon,nlat,'background')
        IF (ALLOCATED(du5))
     &      CALL wind_grid_stats(du5,dv5,nlon,nlat,'adjustment')
        IF (ALLOCATED(ua5))
     &      CALL wind_grid_stats(ua5,va5,nlon,nlat,'adjusted')
      ENDIF

      END SUBROUTINE test

#include "fcagrid.F"
#include "fcagridtl.F"
#include "fcagridad.F"

#include "fcainterp.F"
#include "fcainterptl.F"
#include "fcainterpad.F"

      END MODULE fca_grid_mod
