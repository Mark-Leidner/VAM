      program dunbar_to_native
c!#   $Id: dunbar_to_native.f,v 1.3 1999/12/15 21:27:53 trn Exp $
c!#   $Log: dunbar_to_native.f,v $
c!#   Revision 1.3  1999/12/15 21:27:53  trn
c!#   Changes needed for ASCII format gridded data header file
c!#
c!#   Revision 1.2  1999/10/08 14:30:07  trn
c!#   Updated vam_obs data file structure, some
c!#   reorganization of modules, other stylistic changes.
c!#
c!#   Revision 1.1  1999/09/14 15:20:40  trn
c!#   Initial revision
c!#

c     Purpose: Converts gridded data sets between Dunbar and
c              native formats

      USE constants, ONLY: pi
      USE wind_grid_stats_mod, ONLY: wind_grid_stats
      USE grid_ops, ONLY: dumpgrid
      USE grid_ops_native_m, ONLY: dumphead_native
      USE put_grads_header_m

      implicit none

c!#~   iuvam     reserved i/o unit for vam file operations.
      INTEGER :: iuvam=10
c!#~   maxpath   maximum characters allowed in file name
c!#~   in_fname    name of file to read analysis wind field
c!#~   out_fname  name of file to read background wind field
c!#~   gridstats calculate and print grid statistics?
c!#~   vprint    print verification information?
      INTEGER, PARAMETER :: maxpath=255
      CHARACTER (len=maxpath) ::
     &     in_fname, out_fname
      LOGICAL gridstats, vprint, more

      NAMELIST /files/ in_fname, out_fname,
     &     gridstats, vprint, more

c     Local variables:
c!#~   ierr       error code
      INTEGER ierr

      integer, parameter :: gribcode_u=33, gribcode_v=34,
     &     winds_zeta=105, winds_level=10, itype_latlon=0

c     Copy of current_GD: (lat/lon in radians)
c     file_GD: 
c!#~   xs        longitude of first grid point (in file header)
c!#~   dx        longitude increment (in file header)
c!#~   nx        number of longitude grid points (in file header)
c!#~   ys        latitude of first grid point (in file header)
c!#~   dy        latitude increment (in file header)
c!#~   ny        number of latitude grid points (in file header)
c!#~   date      date of gridded wind field (in file header)
c!#~   time      time of gridded wind field (in file header)

c      ilat = 1 corresponds to lat = -90 deg (rows of U and V arrays
c        stored in order from south to north)
      REAL, parameter :: xs=0, ys=-pi/2
      real :: dx, dy
      INTEGER, parameter :: nx=144, ny=73
      integer :: date, time

      real, dimension(nx,ny) :: u, v

      dx = 2*pi / nx
      dy = pi / (ny-1)

**    Set defaults and read namelist files

      in_fname = ' '
      out_fname = ' '
      gridstats = .TRUE.
      vprint = .TRUE.
      more = .TRUE.

      do while (more)
         READ (*, files, iostat=ierr)
         WRITE (*, files)
         IF (ierr.NE.0) STOP 'Error reading namelist files'

c     Read (u,v) from input

         call read_dunbar(iuvam, in_fname, u, v, nx, ny, ierr,
     &        date, time)
         if (ierr .ne. 0) then
            write (*,*) 'Error code returned from read_dunbar=',ierr
            stop 'read_dunbar'
         endif

         if (gridstats) then
            CALL wind_grid_stats(u,v,nx,ny,'input')
         END IF

**    Save winds to output file

         call dumphead_native(iuvam,out_fname,
     &        xs,dx,nx,ys,dy,ny,date,time,vprint,
     &              gribcode_u, gribcode_v,
     &              winds_zeta, winds_level, itype_latlon)
         CALL dumpgrid (iuvam,out_fname,nx,ny,u,v)
      enddo !#endwhile

      end

      subroutine read_dunbar(lun, uvmap, u, v, n1, n2, ierr, date, time)

c
c     Read Scott Dunbar's NMC analysis files of u and v from file 
c     uvmap (unit lun), return result in u,v
c     date and time coded as yyyymmdd, hhmmss
c     ierr = record number for read errors, -1/-2 for open/close errors
c          =-10 for wrong n1, n2

      implicit none
      
      character (len=*), intent(in) :: uvmap
      integer, intent(in) :: lun, n1, n2
      real, intent(out) :: U(n1,n2),V(n1,n2)
      integer, intent(out) :: ierr, date, time

      integer, parameter :: NLON=144,NLAT=73
      character (len=2) :: cbufr(NLON), c2null
      character (len=4) :: c4bufr
      integer :: ibufr(nlon)

      integer :: i, j, irec, yy, mm, dd, hh100

c      ilat = 1 corresponds to lat = -90 deg (rows of U and V arrays
c        stored in order from south to north)

      ierr = 0
      if (nlat .ne. n2 .or. nlon .ne. n1) ierr=-10
      if (ierr .ne. 0) goto 900
      ierr=-1
      open(lun,file=uvmap,access='direct',recl=NLON*2,err=900)
      call read_record(lun,1,ibufr,nlon,ierr)
      if (ierr .ne. 0) goto 900
c      idate(1:4) = YY,MM,DD,HH*100 (analysis time is 0,600,1200,1800)
c      idate(5:NLON) = zero (null fill)
      yy = ibufr(1)
      if (yy .lt. 100) then
         if (yy .le. 50) then
            yy = yy + 2000
         else
            yy = yy + 1900
         endif
      endif
      mm = ibufr(2)
      dd = ibufr(3)
      date = dd + 100*(mm+100*yy) !# (yyyymmdd)
      hh100 = ibufr(4)
      time = 100*hh100	!#hhmmss
      do j=1,NLAT
              irec=j+1
              call read_record(lun,irec,ibufr,nlon,ierr)
              if (ierr .ne. 0) goto 900
              do i=1,nlon
                 U(i,j) = ibufr(i)*0.01
              enddo
      enddo

      do j=1,NLAT
              irec=j+1 + NLAT
              call read_record(lun,irec,ibufr,nlon,ierr)
              if (ierr .ne. 0) goto 900
              do i=1,nlon
                 V(i,j) = ibufr(i)*0.01
              enddo
      enddo

      ierr=0
  900 if (ierr .ge. 0) close(lun,iostat=ierr)
      if (ierr .ne. 0) ierr=-2
      return
      end
      subroutine read_record(lun,rec,ibufr,n,ierr)      

c
c     Reads n values stored as (signed) integer*2, returns them in integer*4
c     Needed because Sun f90 only supports integer*4
c
      implicit none

      integer, intent(in) :: lun, rec, n
      integer, intent(out) :: ibufr(n), ierr

      character (len=2) :: cbufr(N), c2null
      character (len=4) :: c4bufr
      integer :: i, itemp
c$$$      integer :: imin, imax
      integer, parameter :: byteval=256, signbit=byteval*(byteval/2)

      i=0
      c2null = transfer(i,c2null)
      read(lun,rec=rec,iostat=ierr) cbufr
      if (ierr .ne. 0) ierr=rec
c$$$              imin=1000
c$$$              imax=-1000
      do i=1,n
         c4bufr = c2null // cbufr(i)	!pad leading characters with zero
         ibufr(i) = transfer(c4bufr,i)  !convert to integer
c$$$         print *,rec,i,ibufr(i)
c$$$         write (*,'(10x,z8.8,i10,1x,b32.32)') ibufr(i), ibufr(i)
c$$$     &        ,ibufr(i)
c!#   Negative numbers: 2's complement in trailing 2 bytes
         if (ibufr(i) .gt. signbit) then
            itemp = 0
            call mvbits(not(ibufr(i)),0,16,itemp,0)
c$$$            write (*,'(10x,z8.8,i10,1x,b32.32)') itemp, itemp
c$$$     &           ,itemp
            ibufr(i) = - (itemp+1)
c$$$            write (*,'(10x,z8.8,i10,1x,b32.32)') ibufr(i), ibufr(i)
c$$$     &           ,ibufr(i)
         endif
c$$$                 imin=min(ibufr(i),imin)
c$$$                 imax=max(ibufr(i),imax)
      enddo
c$$$              print '(a,3i10)', 'Max/min=',rec,imax,imin
      return
      end
