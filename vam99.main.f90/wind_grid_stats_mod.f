      MODULE wind_grid_stats_mod

      IMPLICIT NONE
      PRIVATE
      PUBLIC wind_grid_stats

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE wind_grid_stats (u,v,nx,ny,gname)

      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(IN), DIMENSION(nx,ny) :: u,v
      CHARACTER*(*), INTENT(IN) :: gname

      WRITE (*,*) 'Statistics for the ', trim(gname),
     &     ' wind grid [DIMENSION(',nx,',',ny,')]:'

      CALL grid_stats(u,nx,ny,'u',.TRUE.)
      CALL grid_stats(v,nx,ny,'v',.FALSE.)
      CALL grid_stats(SQRT(u(:,:)**2+v(:,:)**2),nx,ny,'speed',.FALSE.)

      END SUBROUTINE wind_grid_stats

c     -----------------------------------------------------------------

      SUBROUTINE grid_stats (u,nx,ny,gname,lhead)

      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(IN), DIMENSION(nx,ny) :: u
      CHARACTER*(*), INTENT(IN) :: gname
      LOGICAL, INTENT(IN) :: lhead

      IF (lhead) WRITE (*,'(a10,4a15)') 'Name',
     &    'Mean', 'RMS', 'Min', 'Max'

      WRITE (*,'(a10,4f15.3)') gname,
     &    sum(u(:,:))/(nx*ny), sqrt(sum(u(:,:)**2)/(nx*ny)),
     &    minval(u(:,:)), maxval(u(:,:))

      END SUBROUTINE grid_stats
c     -----------------------------------------------------------------
      END MODULE wind_grid_stats_mod
