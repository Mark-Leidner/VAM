
      MODULE grid_ops

      use types, only : len_fname, len_name
      use config_mod, only : read_config, write_config
      IMPLICIT NONE
      PRIVATE
      PUBLIC readhead,readgrid,dumphead,dumpgrid,native_fname

c!# Local data needed in readhead and dumphead:
c!# N.B.: Any changes here need corresponding changes in mapping
c!#       of dummy args to these local arrays:
      integer, parameter :: nvar=8, ni=4, nr=4, nd=0, nc=0, nl=0
      character (len=len_name) :: varnames(nvar) = (/
     &     'xs   ','dx   ','nx   ','ys   ','dy   ','ny   ',
     &     'idate','itime' /)
      character (len=1) :: vartypes(nvar) = (/
     &     'r','r','i','r','r','i','i','i' /)
      integer :: varlengths(nvar) = (/
     &     1  , 1 , 1 , 1 , 1 , 1 , 1 , 1  /)
      integer :: ivals(ni)
      real :: rvals(nr)
      double precision :: dvals(1)
      complex :: cvals(1)
      logical :: lvals(1)

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE readhead (iu,fname,xs,dx,nx,ys,dy,ny,
     &     idate,itime,vprint)

      USE constants, ONLY: pi

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(OUT) :: nx,ny,idate,itime
      REAL, INTENT(OUT) :: xs,dx,ys,dy
      logical, INTENT(IN) :: vprint

c
      character (len=len_fname) :: line
      integer :: varmatch(nvar)
      
      character (len=len_fname) :: local_fname
      INTEGER ierr

      call native_fname(fname, len_fname, '.txt', local_fname)

      ivals(:) = 0; rvals(:) = 0
      call read_config(iu, local_fname, line,
     &     nvar, len_name, varnames, vartypes, varlengths,
     &        ni, ivals, nr, rvals, nd, dvals, nc, cvals,
     &        nl, lvals, varmatch, ierr, vprint)

      if (ierr .ne. 0 .or. any(varmatch .ne. varlengths) )
     &     stop 'readhead'

c!#   Copy local arrays to dummy args:
c!#   N.B.: This must agree with the module-scope arrays (varnames, 
c!#         vartypes, varlengths, etc)
      xs = rvals(1)
      dx = rvals(2)
      nx = ivals(1)
      ys = rvals(3)
      dy = rvals(4)
      ny = ivals(2)
      idate = ivals(3)
      itime = ivals(4)

c!#   ASCII format lat-lon grids have grid definition in degrees, convert:
      xs=xs*(pi/180)
      dx=dx*(pi/180)
      ys=ys*(pi/180)
      dy=dy*(pi/180)

      WRITE (*,*) 'VAM: Grid header has been read'

      END SUBROUTINE readhead

c     -----------------------------------------------------------------

      SUBROUTINE readgrid (iu,fname,nx,ny,u,v,lcounts)

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(OUT), DIMENSION(nx,ny) :: u,v
c!#~   lcounts    are obs data counts present in the file just read?
      LOGICAL, INTENT(OUT), OPTIONAL :: lcounts

      character (len=len_fname) :: local_fname
      INTEGER ierr, recl
      REAL, DIMENSION(nx,ny) :: dum

c!#   Open data file, read u and v
      call native_fname(fname, len_fname, '.dat', local_fname)
      WRITE (*,*) 'VAM: Readgrid: File open: ',TRIM(local_fname)
      inquire(iolength=recl) u
      open(unit=iu, file=local_fname, access='direct',
     &     form='unformatted', recl=recl, iostat=ierr)
      if (ierr .ne. 0) stop 'readgrid: open error'

c!#   This requires specific ordering of u and v
      read (iu, rec=1, iostat=ierr) u
      IF (ierr .eq. 0) read (iu, rec=2, iostat=ierr) v
      if (ierr .ne. 0) stop 'readgrid: u,v read'

      if (PRESENT (lcounts) ) then
c!#   Check for existence of ncounts record after u,v
        lcounts = .FALSE.  !# initialize
        read (iu, rec=3, iostat=ierr) dum
        if (ierr .eq. 0) lcounts = .TRUE.
      endif

      close (iu,iostat=ierr)	!#close data file
      if (ierr .ne. 0) stop 'readgrid: file close error'
      WRITE (*,*) 'VAM: Grid values read'

      END SUBROUTINE readgrid

c     -----------------------------------------------------------------

      SUBROUTINE dumphead (iu,fname,xs,dx,nx,ys,dy,ny,
     &     idate,itime,vprint)

      USE constants, ONLY: pi

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny,idate,itime
      REAL, INTENT(IN) :: xs,dx,ys,dy
      logical, INTENT(IN) :: vprint

c
      character (len=len_fname) :: local_fname
      INTEGER ierr
      integer, parameter :: nhead=2
      character(len=len_fname) :: header_lines(nhead)
      character(len=6) :: fmti='i10   ', fmtr='g30.20', fmtd=' ',
     &     fmtc=' ', fmtl=' '

      real, parameter :: degree_precision=1.e-4	!#lat/lon to nearest 10^-4 degrees
c
c!#   Copy dummy args to local arrays:
      header_lines(1) = 'VAM gridded data format header for fname:'
      header_lines(2) = fname
c!#   N.B.: This must agree with the module-scope arrays (varnames, 
c!#         vartypes, varlengths, etc)
c!#   ASCII format lat-lon grids have grid definition in degrees, convert:
      rvals(1) = degree_precision*nint( xs *(180/pi)/degree_precision)
      rvals(2) = degree_precision*nint( dx *(180/pi)/degree_precision)
      ivals(1) = nx
      rvals(3) = degree_precision*nint( ys *(180/pi)/degree_precision)
      rvals(4) = degree_precision*nint( dy *(180/pi)/degree_precision)
      ivals(2) = ny
      ivals(3) = idate
      ivals(4) = itime

      call native_fname(fname, len_fname, '.txt', local_fname)

      WRITE (*,*) 'VAM: Dumphead to File: ', TRIM(local_fname)

      call write_config(iu, local_fname, nhead, header_lines,
     &     nvar, len_name, varnames, vartypes, varlengths,
     &        ni, ivals, nr, rvals, nd, dvals, nc, cvals,
     &        nl, lvals, fmti, fmtr, fmtd, fmtc, fmtl, ierr, vprint)

      if (ierr .ne. 0)
     &     stop 'dumphead'

      WRITE (*,*) 'VAM: Grid header has been dumped'

      END SUBROUTINE dumphead

c     -----------------------------------------------------------------

      SUBROUTINE dumpgrid (iu,fname,nx,ny,u,v,n)

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(IN), DIMENSION(nx,ny) :: u,v
      INTEGER, INTENT(IN), DIMENSION(nx,ny), OPTIONAL :: n

      character (len=len_fname) :: local_fname
      INTEGER ierr, recl
      REAL, DIMENSION(nx,ny) :: nr



      call native_fname(fname, len_fname, '.dat', local_fname)

      WRITE (*,*) 'VAM: Dumpgrid: File open: ',TRIM(local_fname)
      inquire(iolength=recl) u
      open(unit=iu, file=local_fname, access='direct',
     &     form='unformatted', recl=recl, iostat=ierr)
      if (ierr .ne. 0) stop 'dumpgrid: open error'

      write (iu, rec=1, iostat=ierr) u
      if (ierr .eq. 0) write (iu, rec=2, iostat=ierr) v
      if (PRESENT(n) . and. ierr .eq. 0) then
        nr = n    !# convert integer obs counts to real values
        write (iu, rec=3, iostat=ierr) nr
      endif
      if (ierr .ne. 0) stop 'dumpgrid: u,v,n dump'

      close (iu,iostat=ierr)
      if (ierr .ne. 0) stop 'dumpgrid: file close error'
      WRITE (*,*) 'VAM: Grid values dumped'

      END SUBROUTINE dumpgrid

c     -----------------------------------------------------------------

      subroutine native_fname(in_fname, maxpath, out_ext, out_fname)
      character (len=*), intent(in) :: in_fname, out_ext
      integer, intent(in) :: maxpath
      character (len=*), intent(out) :: out_fname

      integer :: len_root

      len_root = max(index(in_fname,'.hdr',.TRUE.),
     &     index(in_fname,'.dat',.TRUE.),
     &     index(in_fname,'.ctl',.TRUE.),
     &     index(in_fname,'.txt',.TRUE.)) - 1
      if (len_root .eq. -1) len_root = len_trim(in_fname)
      if (len_root + len(out_ext) .gt. maxpath)
     &     stop 'native_fname: maxpath too small'
      out_fname = in_fname(1:len_root) // out_ext

      return
      end subroutine native_fname
c     -----------------------------------------------------------------

      END MODULE grid_ops
