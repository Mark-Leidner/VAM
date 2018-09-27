c!# CSU IDENTIFICATION : grid_ops_native
c!#     $Id: grid_ops_native.f,v 1.3 2004/12/16 21:11:08 leidner Exp $

c!# PURPOSE : Read/Write native format header for VAM gridded data files

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!# REQUIREMENTS : 
c!# Read/Write native format binary header for VAM gridded data files
c!# On output, also write GrADS style header (control) file

c!# CONSTRAINTS : 

c!# LANGUAGE : Fortran

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :

      MODULE grid_ops_native_m   !#
c!# GLOBAL AND SHARED DATA : 
      use types, only: len_fname
      USE constants, ONLY: pi
      use grid_hdr_typ_m
      use grid_hdr_io_m
      use grid_ops, only: native_fname

      implicit none

      private
      public readhead_native, dumphead_native

c      INTERFACE grid_ops_native  !only needed for overloading
c         MODULE PROCEDURE grid_ops_native        !
c      END INTERFACE   !

c!# CHANGE LOG : 
c!#	$Log: grid_ops_native.f,v $
c!#	Revision 1.3  2004/12/16 21:11:08  leidner
c!#	Added ability to write out native/GrADS header info for obs counts, if
c!#	present.
c!#	
c!#	Revision 1.2  2003/01/31 15:33:30  trn
c!#	Changeover to txthdr version of native
c!#	
c!#	Revision 1.1  1999/12/15 21:32:43  trn
c!#	Initial revision
c!#	

      CONTAINS        !#

c     -----------------------------------------------------------------

      SUBROUTINE readhead_native (iu,fname,xs,dx,nx,ys,dy,ny,
     &     idate,itime,vprint,
     &              gribcode_u, gribcode_v,
     &              winds_zeta, winds_level, itype_latlon)

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(OUT) :: nx,ny,idate,itime
      REAL, INTENT(OUT) :: xs,dx,ys,dy
      logical, INTENT(IN) :: vprint
      integer, intent(in) :: 
     &              gribcode_u, gribcode_v,
     &              winds_zeta, winds_level, itype_latlon
c
      type (grid_hdr_typ) :: in_hdr
      character (len=len_fname) :: local_fname
      INTEGER ierr

      call native_fname(fname, len_fname, '.hdr', local_fname)

      WRITE (*,*) 'VAM: Readhead_Native from File: ', TRIM(local_fname)
      call get_grid_hdr(trim(local_fname),in_hdr,ierr)
      if (vprint) call print_grid_hdr(in_hdr)
      if (ierr .ne. 0) stop 'get_grid_hdr error'

c!#   extract date/time, grid parameters and check grid type/levs/vars      
      idate = in_hdr%ncicod
      itime = in_hdr%ncicot
c!#   TBD: allow for more general levels, variables, extract desired ones:
      if (size(in_hdr%levs) .ne. 1 .or. size(in_hdr%vars) .gt. 3 .or.
     &     in_hdr%vars(1) .ne. gribcode_u .or.
     &     in_hdr%vars(2) .ne. gribcode_v)
     &     stop 'Invalid levs and/or vars'
      nx = in_hdr%dims(1)
      ny = in_hdr%dims(2)
      if (in_hdr%map%itype .ne. itype_latlon) stop 'invalid itype'
      dx = in_hdr%map%dx
      dy = in_hdr%map%dy
      if (in_hdr%map%mapfun .eq. map_mm5) stop 'Cannot use mm5 mapfun'
      xs = in_hdr%map%reflon - (1 - in_hdr%map%refi) * dx
      ys = in_hdr%map%reflat - (1 - in_hdr%map%refj) * dy

c!#   Native format lat-lon grids have grid definition in degrees, convert:
      xs=xs*(pi/180)
      dx=dx*(pi/180)
      ys=ys*(pi/180)
      dy=dy*(pi/180)

      WRITE (*,*) 'VAM: Grid header has been read'

c!#   deallocate memory allocated in get_grid_hdr:
      deallocate(in_hdr%levs,in_hdr%vars,in_hdr%ipds,
     &     in_hdr%map%igds, in_hdr%map%inesting, stat=ierr)
      if (ierr .ne. 0) stop 'deallocate(in_hdr%levs...'
      END SUBROUTINE readhead_native

c     -----------------------------------------------------------------

      SUBROUTINE dumphead_native (iu,fname,xs,dx,nx,ys,dy,ny,
     &     idate,itime,vprint,
     &              gribcode_u, gribcode_v, gribcode_counts,
     &              winds_zeta, winds_level, itype_latlon,
     &              counts_present)

      use put_grads_header_m

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny,idate,itime
      REAL, INTENT(IN) :: xs,dx,ys,dy
      logical, INTENT(IN) :: vprint, counts_present
      integer, intent(in) :: 
     &              gribcode_u, gribcode_v, gribcode_counts,
     &              winds_zeta, winds_level, itype_latlon

c
      type (grid_hdr_typ) :: out_hdr
      character (len=len_fname) :: local_fname
      INTEGER ierr

      character (len=len_fname) :: grads_file, dat_file
      character (len=20) :: first_dattim
      integer :: d_time=1, lead_units=1, n_times=1, n_ydef=1
      logical :: l_yrev, l_pdef=.T.
      character (len=80) :: pdef_line, xdef_line, ydef_line(1)

      call native_fname(fname, len_fname, '.hdr', local_fname)

      WRITE (*,*) 'VAM: Dumphead_Native to File: ', TRIM(local_fname)

c!#   Generate out_hdr from header info
      out_hdr%ncicod = idate 
      if (out_hdr%ncicod / 10000 .lt. 50) then
c!# ..Fix 2-digit year: use windowing technique, 1950 - 2049
         out_hdr%ncicod = out_hdr%ncicod + 2000*10000
      elseif (out_hdr%ncicod / 10000 .lt. 100) then
         out_hdr%ncicod = out_hdr%ncicod + 1900*10000
      endif
      out_hdr%ncicot = itime 
      out_hdr%dims(1) = nx
      out_hdr%dims(2) = ny 
      out_hdr%lonlim = (/xs , xs + (nx-1)*dx /) * (180/pi)
      out_hdr%latlim = (/ys , ys + (ny-1)*dy /) * (180/pi)
      out_hdr%map%dx = dx * (180/pi)
      out_hdr%map%dy = dy * (180/pi)
      out_hdr%map%reflon = xs * (180/pi)
      out_hdr%map%reflat = ys * (180/pi)
      out_hdr%map%refi = 1
      out_hdr%map%refj = 1

c!#...and from hardwired defaults
      out_hdr%zeta = winds_zeta
      allocate(out_hdr%levs(1),out_hdr%ipds(len_pds),
     &     out_hdr%map%igds(len_gds), out_hdr%map%inesting(1),
     &     stat=ierr)
      if (ierr .ne. 0) stop 'allocate(out_hdr%levs(1)...'
      if (counts_present) then
         allocate(out_hdr%vars(3), stat=ierr)
      else
         allocate(out_hdr%vars(2), stat=ierr)
      endif
      if (ierr .ne. 0) stop 'allocate(out_hdr%vars)'
      out_hdr%levs = winds_level
      if (counts_present) then
        out_hdr%vars = (/gribcode_u, gribcode_v, gribcode_counts/)
      else
        out_hdr%vars = (/gribcode_u, gribcode_v/)
      endif
      out_hdr%ipds(:) = 0
      out_hdr%dxy = -1
      out_hdr%scale = -1
      out_hdr%map%mapfun = map_grib
      out_hdr%map%igrid = 0
      out_hdr%map%itype = itype_latlon
      out_hdr%map%nproj = itype_latlon
      out_hdr%map%stdlat1 = 0
      out_hdr%map%stdlat2 = 0
      out_hdr%map%stdlon = 0
      out_hdr%map%igds(:) = 0
      out_hdr%map%inesting(:) = 0

c!#  Define igds from mapping parameters:
      call gribpa(ierr, 0, out_hdr%map%igds, 
     &     out_hdr%map%itype,
     &     out_hdr%map%reflat, out_hdr%map%reflon,
     &     out_hdr%map%refi, out_hdr%map%refj,
     &     out_hdr%map%dx, out_hdr%map%dy, 
     &     out_hdr%map%stdlat1, out_hdr%map%stdlat2,
     &     out_hdr%map%stdlon) 
      if (vprint .or. ierr .ne. 0) call print_grid_hdr(out_hdr)
      if (ierr .ne. 0) stop 'gribpa error'
      call put_grid_hdr(trim(local_fname),out_hdr,ierr)
      if (ierr .ne. 0) stop 'put_grid_hdr error'
      WRITE (*,*) 'VAM: Grid header has been dumped'

      call native_fname(fname, len_fname, '.ctl', grads_file)
      call native_fname(fname, len_fname, '.dat', dat_file)
      write (first_dattim, '(i8.8,i6.6)',iostat=ierr)
     &     out_hdr%ncicod, out_hdr%ncicot
      if (ierr .ne. 0) then
         ierr=0
         write (*,*)
     &        'Error encoding time in grads file, using dummy value'
         first_dattim = '19991231000000'
      endif
      pdef_line = '* No pdef needed for latlon'
      write (xdef_line,'(a,i5,a,2f12.4)',iostat=ierr) 'XDEF',
     &     out_hdr%dims(1),' LINEAR', out_hdr%map%reflon, out_hdr%map%dx
      if (ierr .ne. 0) stop 'dumphead_native - xdef_line'
      l_yrev = out_hdr%map%dy .lt. 0
      if (l_yrev) then
         write (ydef_line(1),'(a,i5,a,2f12.4)',iostat=ierr) 'YDEF',
     &        out_hdr%dims(2),' LINEAR',
     &        out_hdr%map%reflat + (out_hdr%dims(2)-1) * out_hdr%map%dy,
     &        -out_hdr%map%dy
      else
         write (ydef_line(1),'(a,i5,a,2f12.4)',iostat=ierr) 'YDEF',
     &        out_hdr%dims(2),' LINEAR',
     &        out_hdr%map%reflat, out_hdr%map%dy
      endif
      if (ierr .ne. 0) stop 'dumphead_native - ydef_line'

      call put_grads_header(
     &     iu, trim(grads_file), trim(dat_file),
     &     first_dattim, n_times, d_time, lead_units,
     &     out_hdr%dims, size(out_hdr%levs), out_hdr%levs,
     &     size(out_hdr%vars), out_hdr%vars,
     &     pdef_line, xdef_line, n_ydef, ydef_line, l_yrev, l_pdef)
      WRITE (*,*) 'VAM: GrADS Grid header has been dumped to ',
     &     trim(grads_file)

c!#   deallocate memory allocated in get_grid_hdr:
      deallocate(out_hdr%levs,out_hdr%vars,out_hdr%ipds,
     &     out_hdr%map%igds, out_hdr%map%inesting, stat=ierr)
      if (ierr .ne. 0) stop 'deallocate(out_hdr%levs...'


      END SUBROUTINE dumphead_native

      end module grid_ops_native_m
