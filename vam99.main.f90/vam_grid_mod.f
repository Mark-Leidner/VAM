
      MODULE vam_grid_mod

      USE constants, ONLY: pi

      IMPLICIT NONE
      PRIVATE
      PUBLIC vam_grid, reset_grid

      CONTAINS

c     -----------------------------------------------------------------

*     Grid operations

      SUBROUTINE vam_grid (iuvam, obs_data)

c     Purpose: Sets up initial grids, regrids as needed,
c     saves analysis grid.

      USE constants, ONLY: omega
      USE grid_mod, ONLY: nlon,nlat,lon0,dlon,lat0,dlat,name0,name5,
     &    idate, itime
      USE grid_mod, ONLY: u0,v0,u5,v5,u,v,f,cf
      USE grid_mod, ONLY: du5,dv5,du,dv,ua5,va5,ua,va
      USE grid_mod, ONLY: fca_mode,cv_uv,cv_fca
      USE cgr_mod, ONLY: period
      USE fca_grid_mod, ONLY: fcagrid
      USE grid_ops, ONLY: readhead,readgrid,dumphead,dumpgrid
      USE interp_mod, ONLY: interpgrid
      USE types, ONLY: obs_data_typ, len_fname
      USE vam_obs_mod, ONLY: regrid_obs, count_obs
      USE wind_grid_stats_mod, ONLY: wind_grid_stats

c!#~   iuvam     reserved i/o unit for vam file operations.
c!#~   obs_data  linked list of observation data
      INTEGER, INTENT(IN) :: iuvam
      TYPE (obs_data_typ), POINTER :: obs_data

c     Warning if date/times do not match

**    Declarations:

c!# gridops namelist:
      CHARACTER (len=len_fname) :: analysis_fname, background_fname,
     &    save_fname, analysis_name, background_name,
     &    adjustment_fname, save_adjustment_fname, save_adjusted_fname
      REAL, SAVE :: cdrag=1E-3
      LOGICAL copyback, copygradients, gridstats, vprint, save_all
      NAMELIST /gridops/ analysis_fname, background_fname,
     &    save_fname, analysis_name, background_name,
     &    adjustment_fname, save_adjustment_fname, save_adjusted_fname,
     &    cdrag, fca_mode, idate, itime,
     &    copyback, copygradients, gridstats, vprint, save_all
c!#~   analysis_fname    name of file to read analysis wind field
c!#~   background_fname  name of file to read background wind field
c!#~   save_fname        name of file to write analysis wind field
c!#~   analysis_name     used to set name in grid data structure
c!#~   analysis_name.    defaults to save_fname (if defined)
c!#~   analysis_name..   or to analysis_fname
c!#~   background_name   like analysis_name but for background
c!#~   background_name.  defaults to background_fname
c!#~   adjustment_fname  name of file to read adjustment wind field
c!#~   save_adjustment_fname name of file to write adjustment wind field
c!#~   save_adjusted_fname name of file to write adjusted wind field
c!#~   cdrag             drag coefficient
c!#~   fca_mode          mode of fca operation (see grid_mod)
c!#~   idate             valid date (yyyymmdd) of the wind field
c!#~   itime             valid time (hhmmss) of the wind field
c!#~   copyback          copy background to analysis?
c!#~   copygradients     copy gradients to trajectory values?
c!#~   copygradients.    use to write out gradients to save fnames
c!#~   gridstats         calculate and print grid statistics?
c!#~   vprint            print verification information?
c!#~   save_all          if periodic grid, save wrap around region?

c     Local variables:
c!#~   ncounts    counts of data used
c!#~   i0         longititude index
c!#~   i          longititude index
c!#~   j          latitude index
c!#~   ierr       error code
      INTEGER, ALLOCATABLE, DIMENSION(:,:):: ncounts
      INTEGER i0,i,j,ierr
c     Copy of current_GD: 
c!#~   xs0       longitude of first grid point (in grid_mod)
c!#~   dx0       longitude increment (in grid_mod)
c!#~   nx0       number of longitude grid points (in grid_mod)
c!#~   ys0       latitude of first grid point (in grid_mod)
c!#~   dy0       latitude increment (in grid_mod)
c!#~   ny0       number of latitude grid points (in grid_mod)
      REAL xs0, dx0, ys0, dy0
      INTEGER nx0, ny0
c     file_GD: 
c!#~   xs1       longitude of first grid point (in file header)
c!#~   dx1       longitude increment (in file header)
c!#~   nx1       number of longitude grid points (in file header)
c!#~   ys1       latitude of first grid point (in file header)
c!#~   dy1       latitude increment (in file header)
c!#~   ny1       number of latitude grid points (in file header)
c!#~   idate1     date of gridded wind field (in file header)
c!#~   itime1     time of gridded wind field (in file header)
      REAL xs1, dx1, ys1, dy1
      INTEGER nx1, ny1, idate1, itime1

**    Set defaults and read namelist gridops

      analysis_fname = ' '
      background_fname = ' '
      save_fname = ' '
      background_name=' '
      analysis_name=' '
      adjustment_fname = ' '
      save_adjustment_fname = ' '
      save_adjusted_fname = ' '
      copyback = .FALSE.
      copygradients = .FALSE.
      gridstats = .FALSE.
      vprint = .FALSE.
      save_all = .FALSE.

      READ (*, gridops, iostat=ierr)
      IF (background_name .eq. ' ') background_name = background_fname
      IF (analysis_name .eq. ' ') THEN
        IF (analysis_fname .ne. ' ') analysis_name = analysis_fname
        IF (save_fname .ne. ' ') analysis_name = save_fname
      END IF
      WRITE (*, gridops)
      IF (ierr .NE. 0) STOP 'Error reading namelist gridops'

      IF (background_name .ne. ' ') name0 = background_name
      IF (analysis_name .ne. ' ') name5 = analysis_name

c     fca_mode is used to set cv_uv and cv_fca
      cv_uv = fca_mode.EQ.0 .OR. fca_mode.EQ.2
      cv_fca = fca_mode.EQ.1 .OR. fca_mode.EQ.2 .OR. fca_mode.EQ.3

**    Copy gradient variables to trajectory variables
c WARNING: destroys trajectory values.
c This should be used only to write out gradient variables.

      IF (copygradients) THEN
        IF (ALLOCATED(u5)) THEN
          u5(:,:)=u(:,:); v5(:,:)=v(:,:)
        ENDIF
        IF (ALLOCATED(du5)) THEN
          du5(:,:)=du(:,:); dv5(:,:)=dv(:,:)
        ENDIF
        IF (ALLOCATED(ua5)) THEN
          ua5(:,:)=ua(:,:); va5(:,:)=va(:,:)
        ENDIF
      ENDIF

**    Save the current grid description for later interpolation

      xs0=lon0; dx0=dlon; nx0=nlon
      ys0=lat0; dy0=dlat; ny0=nlat

**    Redefine the grid description.
      CALL grid_description (analysis_fname, background_fname,
     &    iuvam, lon0, dlon, nlon, lat0, dlat, nlat)

**    Update the rest of grid_mod

***   Compute friction coefficient from drag coefficient:
      cf=-cdrag/1000
      WRITE (*,*) 'cf = - drag coefficient / delta Z = - ',
     &    cdrag,' / 1000 = ',cf

***   Coriollis parameter
      IF (ALLOCATED(f)) DEALLOCATE (f,STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (f,...'
      ALLOCATE (f(nlat),STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (f(nlat),...'
      f(:)=2*omega*SIN((lat0+(/(j-1,j=1,nlat)/)*dlat))

**    Allocate gradient winds (u,v), and ncounts
**    which will be used as a workspace
      IF (ALLOCATED(u)) DEALLOCATE (u,v,STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (u,v)'
      IF (ALLOCATED(ncounts)) DEALLOCATE (ncounts,STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (ncounts)'
      ALLOCATE (u(nlon,nlat),v(nlon,nlat),ncounts(nlon,nlat),STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (u(nlon,nlat),...'

**    Define background winds (u0,v0) using (u,v) as workspace

      idate1 = -1 ; itime1 = -1	!#=missing code unless reading from file

      IF (background_fname .NE. ' ') THEN

c     Read and interpolate (u,v) from background_fname

        CALL readhead(iuvam,background_fname,
     &      xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint)
        IF (ALLOCATED(u0)) DEALLOCATE (u0,v0,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (u0,v0,...'
        ALLOCATE (u0(nx1,ny1),v0(nx1,ny1),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (u0(nx1,ny1),...'
        CALL readgrid(iuvam,background_fname,nx1,ny1,u0,v0)
        IF (gridstats)
     &      CALL wind_grid_stats(u0,v0,nx1,ny1,'background')
        CALL interpgrid (xs1,dx1,nx1,ys1,dy1,ny1,u0,v0,
     &      lon0,dlon,nlon,lat0,dlat,nlat,u,v)     

      ELSE IF (nx0.ne.0 .and. ny0.ne.0) THEN

c     If u0, v0 are not empty interpolate to new grid.

        IF (gridstats)
     &      CALL wind_grid_stats(u0,v0,nx0,ny0,'background')
        CALL interpgrid (xs0,dx0,nx0,ys0,dy0,ny0,u0,v0,
     &      lon0,dlon,nlon,lat0,dlat,nlat,u,v)     

      ELSE

c     Set (u,v) to zero

        u(:,:) = 0; v(:,:) = 0

      END IF

***   Copy (u,v) to (u0,v0)

      IF (ALLOCATED(u0)) DEALLOCATE (u0,v0,STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (u0,v0,...'
      ALLOCATE (u0(nlon,nlat),v0(nlon,nlat),STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (u0(nlon,nlat),...'
      u0(:,:) = u(:,:); v0(:,:) = v(:,:) 

      IF (idate1 .gt. 0) THEN	!#
        IF (idate .le. 0) THEN	!#initialize valid date/time
          idate=idate1 ; itime=itime1
        ELSE
          IF (idate .ne. idate1 .or. itime .ne. itime1) print *,
     &        'Warning: Mismatch between background date/time=',
     &        idate1,'/',itime1,' and valid date/time =',
     &        idate,'/',itime
        END IF
      END IF

**    Define analysis winds (u5,v5) using (u,v) as workspace.

      idate1 = -1 ; itime1 = -1	!#=missing code unless reading from file

      IF (copyback) THEN

c     Set (u,v) equal to (u0,v0)
        u(:,:) = u0(:,:); v(:,:) = v0(:,:) 

      ELSE IF (analysis_fname.NE.' ' ) THEN

c     Read and interpolate (u,v) from analysis_fname

        CALL readhead(iuvam,analysis_fname,
     &      xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint)
        IF (ALLOCATED(u5)) DEALLOCATE (u5,v5,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (u5,v5,...'
        ALLOCATE (u5(nx1,ny1),v5(nx1,ny1),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (u5(nx1,ny1),...'
        CALL readgrid(iuvam,analysis_fname,
     &      nx1,ny1,u5,v5)
        IF (gridstats)
     &      CALL wind_grid_stats(u5,v5,nx1,ny1,'analysis')
        CALL interpgrid (xs1,dx1,nx1,ys1,dy1,ny1,u5,v5,
     &      lon0,dlon,nlon,lat0,dlat,nlat,u,v)     

      ELSE IF (nx0.ne.0 .and. ny0.ne.0) THEN

c     If u5, v5 are not empty interpolate to new grid.

        IF (gridstats)
     &      CALL wind_grid_stats(u5,v5,nx0,ny0,'analysis')
        CALL interpgrid (xs0,dx0,nx0,ys0,dy0,ny0,u5,v5,
     &      lon0,dlon,nlon,lat0,dlat,nlat,u,v)     

      ELSE

c     Set (u,v) to zero

        u(:,:) = 0; v(:,:) = 0

      END IF

***   Copy (u,v) to (u5,v5)

      IF (ALLOCATED(u5)) DEALLOCATE (u5,v5,STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (u5,v5,...'
      ALLOCATE (u5(nlon,nlat),v5(nlon,nlat),STAT=ierr)
      IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (u5(nlon,nlat),...'
      u5(:,:) = u(:,:); v5(:,:) = v(:,:) 

      IF (idate1 .gt. 0) THEN	!#
        IF (idate .le. 0) THEN	!#initialize valid date/time
          idate=idate1 ; itime=itime1
        ELSE
          IF (idate .ne. idate1 .or. itime .ne. itime1) print *,
     &        'Warning: Mismatch between analysis date/time=',
     &        idate1,'/',itime1,' and valid date/time =',
     &        idate,'/',itime
        END IF
      END IF

** Define (du5,dv5) using (u,v) as workspace.

      IF (cv_fca) THEN

        IF (adjustment_fname .NE. ' ' ) THEN

c     Read and interpolate (u,v) from adjustment_fname

          CALL readhead(iuvam,adjustment_fname,
     &        xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint)
          IF (ALLOCATED(du5)) DEALLOCATE (du5,dv5,STAT=ierr)
          IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (du5,dv5,...'
          ALLOCATE (du5(nx1,ny1),dv5(nx1,ny1),STAT=ierr)
          IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (du5(nx1,ny1),...'
          CALL readgrid(iuvam,adjustment_fname,
     &        nx1,ny1,du5,dv5)
          IF (gridstats)
     &        CALL wind_grid_stats(du5,dv5,nx1,ny1,'adjustment')
          CALL interpgrid (xs1,dx1,nx1,ys1,dy1,ny1,du5,dv5,
     &        lon0,dlon,nlon,lat0,dlat,nlat,u,v)     

        ELSE IF (nx0.ne.0 .and. ny0.ne.0) THEN

c     If du5, dv5 are not empty interpolate to new grid.

          IF (gridstats)
     &        CALL wind_grid_stats(du5,dv5,nx0,ny0,'adjustment')
          CALL interpgrid (xs0,dx0,nx0,ys0,dy0,ny0,du5,dv5,
     &        lon0,dlon,nlon,lat0,dlat,nlat,u,v)     

        ELSE

c     Set (u,v) to zero

          u(:,:) = 0; v(:,:) = 0

        END IF

***   Copy (u,v) to (du5,dv5)

        IF (ALLOCATED(du5)) DEALLOCATE (du5,dv5,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (du5,dv5,...'
        ALLOCATE (du5(nlon,nlat),dv5(nlon,nlat),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (du5(nlon,nlat),...'
        du5(:,:) = u(:,:); dv5(:,:) = v(:,:) 

***   Allocate adjoint variables (du,dv) and set to zero

        IF (ALLOCATED(du)) DEALLOCATE (du,dv,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (du,dv,...'
        ALLOCATE (du(nlon,nlat),dv(nlon,nlat),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (du(nlon,nlat),...'
        du(:,:) = 0; dv(:,:) = 0 

      ENDIF

**   Define adjusted winds
      IF (.NOT. copygradients) THEN
      IF (fca_mode.EQ.1) THEN
        CALL fcagrid (u0,v0,du5,dv5,u5,v5)
      ELSE IF (fca_mode.EQ.2 .OR. fca_mode.EQ.3) THEN

***   Allocate (ua5,va5) and define using fcainterp.

        IF (ALLOCATED(ua5)) DEALLOCATE (ua5,va5,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (ua5,va5,...'
        ALLOCATE (ua5(nlon,nlat),va5(nlon,nlat),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (ua5(nlon,nlat),...'
        CALL fcagrid (u0,v0,du5,dv5,ua5,va5)

***   Allocate adjoint variables (ua,va) and set to zero

        IF (ALLOCATED(ua)) DEALLOCATE (ua,va,STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (ua,va,...'
        ALLOCATE (ua(nlon,nlat),va(nlon,nlat),STAT=ierr)
        IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (ua(nlon,nlat),...'
        ua(:,:) = 0; va(:,:) = 0 

      END IF
      END IF

**    Grid statistics

      IF (gridstats) THEN

        CALL wind_grid_stats(u0,v0,nlon,nlat,'background')
        CALL wind_grid_stats(u5,v5,nlon,nlat,'analysis')
        u(:,:) = u5(:,:)-u0(:,:); v(:,:) = v5(:,:)-v0(:,:) 
        CALL wind_grid_stats(u ,v ,nlon,nlat,'differenced')
        IF (ALLOCATED(du5))
     &      CALL wind_grid_stats(du5,dv5,nlon,nlat,'adjustment')
        IF (ALLOCATED(ua5)) THEN
          CALL wind_grid_stats(ua5,va5,nlon,nlat,'adjusted')
          u(:,:) = u5(:,:)-ua5(:,:); v(:,:) = v5(:,:)-va5(:,:) 
          CALL wind_grid_stats(u ,v ,nlon,nlat,'u5-ua5;v5-va5')
          u(:,:) = ua5(:,:)-u0(:,:); v(:,:) = va5(:,:)-v0(:,:) 
          CALL wind_grid_stats(u ,v ,nlon,nlat,'ua5-u0;va5-v0')
        END IF

      END IF

**    Regrid data if data present:

      IF (ASSOCIATED(obs_data)) CALL regrid_obs(obs_data)

**    Count data present in grid cells using ncounts as workspace:

      CALL count_obs(obs_data,nlon,nlat,period,ncounts)

**    Determine longitude range to write out
***   If periodic, unless save_all is specified, write minimum longitude range
      IF ( (nlon .GT. period) .AND. .NOT. save_all) THEN
        i0 = 2; i = period + 1; xs0 = lon0 + dlon; nx0 = period
      ELSE
        i0 = 1; i = nlon; xs0 = lon0; nx0 = nlon
      END IF

**    Save analysis winds (u5,v5) and ncounts to file save_fname

      IF (save_fname .NE. ' ') THEN
        CALL dumphead(iuvam,save_fname,
     &      xs0,dlon,nx0,lat0,dlat,nlat,idate,itime,vprint)
        CALL dumpgrid (iuvam,save_fname,
     &      nx0,nlat,u5(i0:i,:),v5(i0:i,:),ncounts(i0:i,:))
      END IF

**    Save adjustment winds (du5,dv5) and ncounts to file save_adjustment_fname

      IF (cv_fca .AND. save_adjustment_fname .NE. ' ') THEN
        CALL dumphead(iuvam,save_adjustment_fname,
     &      xs0,dlon,nx0,lat0,dlat,nlat,idate,itime,vprint)
        CALL dumpgrid (iuvam,save_adjustment_fname,
     &      nx0,nlat,du5(i0:i,:),dv5(i0:i,:),ncounts(i0:i,:))
      END IF

**    Save adjusted winds (ua5,va5) and ncounts to file save_adjusted_fname

      IF (ALLOCATED(ua5) .AND. save_adjusted_fname .NE. ' ') THEN
        CALL dumphead(iuvam,save_adjusted_fname,
     &      xs0,dlon,nx0,lat0,dlat,nlat,idate,itime,vprint)
        CALL dumpgrid (iuvam,save_adjusted_fname,
     &      nx0,nlat,ua5(i0:i,:),va5(i0:i,:),ncounts(i0:i,:))
      END IF

      END SUBROUTINE vam_grid

c     -----------------------------------------------------------------

*     Reset grid

      SUBROUTINE reset_grid

c     Purpose: Resets the entire grid structure

      USE grid_mod, ONLY: nlon,nlat,name0,name5,idate,itime
      USE grid_mod, ONLY: u0,v0,u5,v5,u,v,f,cf
      USE grid_mod, ONLY: dlon, dlat, lon0, lat0
      USE grid_mod, ONLY: du5,dv5,du,dv,ua5,va5,ua,va
      USE grid_mod, ONLY: fca_mode,nactive,cv_uv,cv_fca

      INTEGER ierr

**    Reset constants
      nlon = 0; nlat = 0
      dlon = 1; dlat = 1; lon0 = 0; lat0 = 0
      name0 = ' '; name5 = ' '
      idate = 0; itime = 0
      cf=-1E-3/1000
      fca_mode=0; nactive=0
      cv_uv=.TRUE.; cv_fca=.FALSE.

**    Deallocate arrays
***   Coriollis parameter
      IF (ALLOCATED(f)) DEALLOCATE (f,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (f,...'
***   Background winds
      IF (ALLOCATED(u0)) DEALLOCATE (u0,v0,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (u0,v0,...'
***   Trajectory winds
      IF (ALLOCATED(u5)) DEALLOCATE (u5,v5,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (u5,v5,...'
**    Gradient winds (u,v)
      IF (ALLOCATED(u)) DEALLOCATE (u,v,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (u,v,...'
***   Adjustment winds
      IF (ALLOCATED(du5)) DEALLOCATE (du5,dv5,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (du5,dv5,...'
***   Gradient wrt adjustment winds
      IF (ALLOCATED(du)) DEALLOCATE (du,dv,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (du,dv,...'
***   Adjusted winds
      IF (ALLOCATED(ua5)) DEALLOCATE (ua5,va5,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (ua5,va5,...'
***   Gradient wrt adjusted winds
      IF (ALLOCATED(ua)) DEALLOCATE (ua,va,STAT=ierr)
      IF (ierr.NE.0) STOP 'reset_grid: DEALLOCATE (ua,va,...'

      END SUBROUTINE reset_grid

c     -----------------------------------------------------------------

      SUBROUTINE grid_description (analysis_fname, background_fname,
     &    iuvam, xs, dx, nx, ys, dy, ny)

c     Purpose: Redefine grid description from namelists,
c     using file header information as defaults if the current grid
c     description is empty.

      USE interp_mod, ONLY: igrid, gridcell_init, same
      USE grid_ops, ONLY: readhead
      USE cgr_mod, ONLY: cgr_init
      USE ssback_mod, ONLY: ssback_init
      USE solve_mod, ONLY: solve_init

c!#~   analysis_fname    name of file to read analysis wind field
c!#~   background_fname  name of file to read background wind field
c!#~   iuvam     reserved i/o unit for vam file operations.
c     Description of the grid
c     On input these variables describe the contents of grid_mod
c     On output these variables describe the new grid
c!#~   xs        longitude of first grid point
c!#~   dx        longitude increment
c!#~   nx        number of longitude grid points
c!#~   ys        latitude of first grid point
c!#~   dy        latitude increment
c!#~   ny        number of latitude grid points
      CHARACTER*(*), INTENT(IN) :: analysis_fname, background_fname
      INTEGER, INTENT(IN) :: iuvam
      REAL, INTENT(INOUT) :: xs, dx, ys, dy
      INTEGER, INTENT(INOUT) :: nx, ny

c     Local variables:

c     file_GD: 
c!#~   date1     date of gridded wind field (in file header)
c!#~   time1     time of gridded wind field (in file header)
      INTEGER idate1, itime1

c!# Grid namelists:
      REAL xmin, xmax, ymin, ymax, delx, dely
      INTEGER, SAVE :: interp_method=1
      LOGICAL details, boundary
      NAMELIST /grid/ xmin, xmax, ymin, ymax,
     &    delx, dely, interp_method, details, boundary
      NAMELIST /data/ xmin, xmax, ymin, ymax
      NAMELIST /active/ xmin, xmax, ymin, ymax
      NAMELIST /integration/ xmin, xmax, ymin, ymax
c!#~   xmin      minimum value of longitude
c!#~   xmax      maximum value of longitude
c!#~   ymin      minimum value of latitude
c!#~   ymax      maximum value of latitude
c!#~   details   should the detailed namelists be read?
c!#~   boundary  should boundary points be allowed to vary?
c!#~   interp_method  interpolation method
c!#~   interp_method. (1=>bilinear;2=>quasi-bicubic)

c Namelist angle variables are input and written in degrees, but stored
c in radians.  When converting from radians to degrees values within
c 0.0005 of a whole number of degrees are rounded to that whole number.

c     Local variables:
c!#~   sp        latitude of the South Pole
c!#~   np        latitude of the North Pole
c!#~   ierr      error code
c!#~   xf        longitude of last (final) grid point
c!#~   yf        latitude of last (final) grid point
c!#~   lerr      error flag
      REAL, PARAMETER :: sp=-pi/2, np= pi/2
      INTEGER ierr
      REAL xf, yf
      LOGICAL lerr

**    Define the new_GD

c     The default for the new_GD is equal to the current_GD, which is
c     input unless the current_GD is empty.

***   If the (default) new_GD is empty

      IF (nx.EQ.0 .OR. ny.EQ.0) THEN

****  If there is an analysis file, set the new_GD = analysis_GD

        IF (analysis_fname.NE.' ' ) THEN
           CALL readhead(iuvam,analysis_fname,
     &             xs,dx,nx,ys,dy,ny,idate1,itime1,.FALSE.)

****  Or, if there is a background file, set the new_GD = background_GD

        ELSE IF (background_fname .NE. ' ' ) THEN
           CALL readhead(iuvam,background_fname,
     &             xs,dx,nx,ys,dy,ny,idate1,itime1,.FALSE.)

****  Otherwise leave the new_GD to empty.

        ELSE
          WRITE (*,*) 'Grid descriptor is starting empty.'

        END IF
      END IF

***   Alter the new_GD using namelist grid

****  Set the defaults for the namelist variables from the new_GD.

      details=.FALSE.
      boundary=.FALSE.

c     Define xf and yf as the last values:

      xf=xs+dx*(nx-1)
      yf=ys+dy*(ny-1)

c     Remove one row of grid points from each edge, except if an edge
c     corresponds to a pole.  

c     NB: The grid usually extends past the default domain one grid
c     point to allow calculations of derivatives at default domain edge.

      delx=dx; xmin=xs+dx; xmax=xf-dx
      dely=dy; ymin=ys+dy; ymax=yf-dy
      IF (same(ys,sp)) ymin=sp; IF (same(yf,np)) ymax=np

****  Read the namelist

      CALL rad2deg
      READ (*, grid, iostat=ierr)
      WRITE (*, grid)
      IF (xmax.LE.xmin .OR. ymax.LE.ymin) ierr=1
      IF (delx.LT.0 .OR. dely.LT.0) ierr=2
      IF (ierr.NE.0) WRITE(*,*) 'Error: ', ierr
      IF (ierr.NE.0) STOP 'Error reading namelist /grid/'
      CALL deg2rad

****  Set the grid parameters from the namelist variables

c     Add one row of grid points on each edge, except for edges
c     corresponding to pole points:
      dx=delx; xs=xmin-delx; xf=xmax+delx
      dy=dely; ys=ymin-dely; yf=ymax+dely
      IF (same(ymin,sp)) ys=sp; IF (same(ymax,np)) yf=np

c     Now calculate nx and ny:
      nx=igrid(xf,xs,dx,0,1,0,lerr)
      IF (lerr) STOP 'vam_grid_mod: nx'
      ny=igrid(yf,ys,dy,0,1,0,lerr)
      IF (lerr) STOP 'vam_grid_mod: ny'

**    Define the auxillary grid variables

***   For the data window, using namelist data

****  Use the new_GD to set defaults for the namelist variables

      xmin=xs+dx; xmax=xf-dx; ymin=ys+dy; ymax=yf-dy

c!!!  For now polar caps are excluded.

c     NB: data in polar caps must be skipped if the interpolation method
c     is not bilinear.

****  Read the namelist

      CALL rad2deg
      IF (details) READ (*, data, IOSTAT=ierr)
      WRITE (*, data)
      IF (xmax.LE.xmin .OR. ymax.LE.ymin) ierr=1
      IF (ierr.NE.0) WRITE(*,*) 'Error: ', ierr
      IF (ierr.NE.0) STOP 'Error reading namelist /data/'
      CALL deg2rad

****  Set the grid parameters from the namelist variables

      CALL gridcell_init (xmin,xmax,ymin,ymax,xs,dx,nx,ys,dy,ny,
     &    interp_method)

***   For the active region, using namelist active

c     Points within the active region change during the minimization.
c     Points on and external to the active region boundary will be held
c     fixed during the minimization, except that pole points are
c     interior, as are points on the east and western boundaries for a
c     0-360 longitude extent.  In these special cases the points are not
c     all independent, and only the independent points are included in
c     the control vector and hence the active region.

****  Use the new_GD to set defaults for the namelist variables

      xmin=xs+dx; xmax=xf-dx; ymin=ys+dy; ymax=yf-dy
      IF (same(ys,sp)) ymin=sp; IF (same(yf,np)) ymax=np

****  Read the namelist

      CALL rad2deg
      IF (details) READ (*, active, IOSTAT=ierr)
      WRITE (*, active)
      IF (xmax.LE.xmin .OR. ymax.LE.ymin) ierr=1
      IF (ierr.NE.0) WRITE(*,*) 'Error: ', ierr
      IF (ierr.NE.0) STOP 'Error reading namelist /active/'
      CALL deg2rad

****  Set the grid parameters in cgr_mod from the namelist variables

      CALL cgr_init (xmin,xmax,ymin,ymax,xs,dx,nx,ys,dy,ny,
     &    boundary)

***   For the integration domain, using namelist integration

****  Use the new_GD to set defaults for the namelist variables

      xmin=xs+dx; xmax=xf-dx; ymin=ys+dy; ymax=yf-dy

****  Read the namelist

      CALL rad2deg
      IF (details) READ (*, integration, IOSTAT=ierr)
      WRITE (*, integration)
      IF (xmax.LE.xmin .OR. ymax.LE.ymin) ierr=1
      IF (ierr.NE.0) WRITE(*,*) 'Error: ', ierr
      IF (ierr.NE.0) STOP 'Error reading namelist /integration/'
      CALL deg2rad

****  Set the grid parameters in ssback_mod from the namelist variables

      CALL ssback_init (xmin,xmax,ymin,ymax,xs,dx,nx,ys,dy,ny)

****  Set the grid parameters in solve_mod from the namelist variables

      CALL solve_init (xmin,xmax,ymin,ymax)

c     -----------------------------------------------------------------

      CONTAINS

c     Values are radians internally, but degrees on
c     input/output to the namelists.  

      SUBROUTINE deg2rad

c     Purpose: Convert namelist variables to radians.

      delx=(pi/180)*delx; xmin=(pi/180)*xmin; xmax=(pi/180)*xmax
      dely=(pi/180)*dely; ymin=(pi/180)*ymin; ymax=(pi/180)*ymax

      END SUBROUTINE deg2rad

      SUBROUTINE rad2deg

c     Purpose: Convert namelist variables to degrees.
c     If the results are close enough degrees are rounded.

      delx=r2d(delx); xmin=r2d(xmin); xmax=r2d(xmax)
      dely=r2d(dely); ymin=r2d(ymin); ymax=r2d(ymax)

      END SUBROUTINE rad2deg

      END SUBROUTINE grid_description

c     -----------------------------------------------------------------

      REAL FUNCTION r2d(angle)

c     Purpose: Convert to degrees, rounding to nearest 1/scale degrees.

      USE interp_mod, ONLY: same

c!#~   angle      variable
      REAL, INTENT(IN) :: angle

c!#~   scale  inverse scale for rounding degrees
      INTEGER, PARAMETER :: scale=1000

      r2d=REAL(INT(scale*(180/pi)*angle + 0.5))/scale

      IF (.NOT.same(angle,r2d*(pi/180))) r2d=(180/pi)*angle

      END FUNCTION r2d

      END MODULE vam_grid_mod

