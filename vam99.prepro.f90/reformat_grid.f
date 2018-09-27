c!#   $Id: reformat_grid.f,v 1.7 2004/12/16 21:11:08 leidner Exp $
c!#   $Log: reformat_grid.f,v $
c!#   Revision 1.7  2004/12/16 21:11:08  leidner
c!#   Added ability to write out native/GrADS header info for obs counts, if
c!#   present.
c!#
c!#   Revision 1.6  2000/01/21 16:16:11  trn
c!#   Add support for NSCAT scatterometer data
c!#
c!#   Revision 1.5  1999/12/15 21:27:53  trn
c!#   Changes needed for ASCII format gridded data header file
c!#
c!#   Revision 1.4  1999/10/08 14:30:07  trn
c!#   Updated vam_obs data file structure, some
c!#   reorganization of modules, other stylistic changes.
c!#
c!#   Revision 1.3  1999/08/30 15:06:40  trn
c!#   Added code for outputting grads header from reformat_grid
c!#
c!#   Revision 1.2  1999/07/21 19:17:51  trn
c!#   added RCS keywords, diagnostic printout
c!#
      program reformat_grid

c     Purpose: Converts gridded data sets between native and
c              old VAM restart formats

      USE wind_grid_stats_mod, ONLY: wind_grid_stats
      USE grid_ops, ONLY: readhead,readgrid,dumphead,dumpgrid,
     &     native_fname
      USE grid_ops_native_m, ONLY: readhead_native,dumphead_native
      USE put_grads_header_m

      IMPLICIT NONE

      integer, parameter :: gribcode_u=33, gribcode_v=34,
     &     gribcode_counts=254, winds_zeta=105, winds_level=10,
     &     itype_latlon=0

c!#~   iuvam     reserved i/o unit for vam file operations.
      INTEGER :: iuvam=10
c!#~   maxpath   maximum characters allowed in file name
c!#~   in_fname    name of file to read analysis wind field
c!#~   out_fname  name of file to read background wind field
c!#~   in_old    name of file to read analysis wind field
c!#~   out_old  name of file to read background wind field
c!#~   gridstats calculate and print grid statistics?
c!#~   in_degrees does the input old format have xs,dx,... in degrees?
c!#~   vprint    print verification information?
c!#~   date      date of gridded wind field
c!#~   time      time of gridded wind field
      INTEGER, PARAMETER :: maxpath=255
      CHARACTER (len=maxpath) ::
     &     in_native, out_native, in_old, out_old
c!#~   counts_present    are obs data counts expected in the input file?
      LOGICAL gridstats, vprint, in_degrees, more, hdr_in, txt_in,
     &     hdr_out, txt_out, dat_out, counts_present

      NAMELIST /files/ in_native, out_native,
     &     in_old, out_old,
     &     gridstats, vprint, in_degrees,
     &     hdr_in, txt_in, 
     &     hdr_out, txt_out, dat_out, more,
     &     counts_present

c     Local variables:
c!#~   ierr       error code
      INTEGER j,ierr
c     Copy of current_GD: 
c     file_GD: 
c!#~   xs1       longitude of first grid point (in file header)
c!#~   dx1       longitude increment (in file header)
c!#~   nx1       number of longitude grid points (in file header)
c!#~   ys1       latitude of first grid point (in file header)
c!#~   dy1       latitude increment (in file header)
c!#~   ny1       number of latitude grid points (in file header)
c!#~   date1     date of gridded wind field (in file header)
c!#~   time1     time of gridded wind field (in file header)
c!#~   name1     name of gridded wind field (in file header)
      REAL xs1, dx1, ys1, dy1
      INTEGER nx1, ny1, idate1, itime1
      CHARACTER*8 name1

      real, dimension(:,:), allocatable :: u0,v0

c!#~   lcounts    are obs data counts present in the file just read?
      LOGICAL :: need_uv, lcounts
      CHARACTER (len=maxpath) :: test_in, test_out

**    Set defaults and read namelist files

      need_uv = .FALSE.
      name1 = ' '
      more = .TRUE.

      do while (more)
         in_degrees = .TRUE.
         in_native = ' '
         in_old = ' '
         out_native = ' '
         out_old = ' '
         hdr_in = .F.
         txt_in = .F.
         hdr_out = .F.
         txt_out = .F.
         dat_out = .F.
         gridstats = .TRUE.
         vprint = .TRUE.
         counts_present = .FALSE.
         READ (*, files, iostat=ierr)
         IF (ierr.NE.0) STOP 'Error reading namelist files'
c!#      if in_native is specified, and no output file is specified,
c!#      then assume that out_native = in_native
         if (out_native .eq. ' ' .and. out_old .eq. ' ' .and.
     &        in_native .ne. ' ') out_native = in_native
c!#      if reformatting native with the same input and output
c!#      names, only output additional header
         if (in_native .ne. ' ' .and. out_native .ne. ' ') then
            call native_fname(in_native, maxpath, 'tst', test_in)
            call native_fname(out_native, maxpath, 'tst', test_out)
            if (test_in .eq. test_out) then
               dat_out = .F.
               hdr_out = .not. hdr_in
               txt_out = .not. txt_in
            endif
         endif
         WRITE (*, files)

         need_uv = out_old .ne. ' ' .or. dat_out .or.
     &             ( out_native .ne. ' ' .and. counts_present )

**    Define background winds (u0,v0)

         IF (in_native .NE. ' ' .or.
     &        in_old .NE. ' ' ) THEN

c     Read header info from input, either from _native
c     (one of .txt ot .hdr) or from _old

            if (in_native .ne. ' ') then
               if (in_old .ne. ' ') write (*,*)
     &              'Warning: both _native and _old specified, ',
     &              'only _native being used for input'
               if ((txt_in .and. hdr_in) .or.
     &              .not. (txt_in .or. hdr_in))
     &              stop 'Exactly one of txt_in, hdr_in must be .T.'
               if (txt_in)
     &              call readhead(iuvam,in_native,
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint)
               if (hdr_in)
     &              call readhead_native(iuvam,in_native,
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint,
     &              gribcode_u, gribcode_v,
     &              winds_zeta, winds_level, itype_latlon)
            else
               CALL readhead_old (iuvam,in_old,in_degrees,
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,name1)
               if (vprint) print *,
     &              'xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,name1=',
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,name1
            endif
            
            if (need_uv) then
c     Read (u,v) from input
               IF (ALLOCATED(u0)) DEALLOCATE (u0,v0,STAT=ierr)
               IF (ierr.NE.0) STOP 'vam_grid: DEALLOCATE (u0,v0,...'
               ALLOCATE (u0(nx1,ny1),v0(nx1,ny1),STAT=ierr)
               IF (ierr.NE.0) STOP 'vam_grid: ALLOCATE (u0(nx1,ny1),...'
               if (in_native .ne. ' ') then
                  call readgrid(iuvam,in_native,
     &                 nx1,ny1,u0,v0,lcounts)
                  if (.not. lcounts) stop
     &              'expected to find obs_counts record in input file'
               else
                  CALL readgrid_old (iuvam,in_old,nx1,ny1,u0,v0)
               endif
            endif
         ELSE
            
            stop 'no input file specified'
            
         END IF

         if (gridstats .and. need_uv) then

            CALL wind_grid_stats(u0,v0,nx1,ny1,'input')

         END IF

**    Save winds (u0,v0) to output file

         IF (out_native .NE. ' '  .or.
     &        out_old .NE. ' ' ) THEN
            if (out_native .ne. ' ') then
               if (out_old .ne. ' ') write (*,*)
     &              'Warning: both _native and _old specified, ',
     &              'only _native being used for save'

c!#     Output header info to _native:
               if (.not. (txt_out .or. hdr_out))
     &              stop 'At least one of txt_out, hdr_out must be .T.'
               if (txt_out)
     &              call dumphead(iuvam,out_native,
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint)
               if (hdr_out)
     &              call dumphead_native(iuvam,out_native,
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,vprint,
     &              gribcode_u, gribcode_v, gribcode_counts,
     &              winds_zeta, winds_level, itype_latlon,
     &              counts_present)

c!#     Output data to native if so specified
               if (dat_out)
     &              CALL dumpgrid (iuvam,out_native,nx1,ny1,u0,v0)
            else
c!#     Output header and data to old VAM format file:
               CALL dumphead_old (iuvam,out_old,
     &              xs1,dx1,nx1,ys1,dy1,ny1,idate1,itime1,name1)
               CALL dumpgrid_old (iuvam,out_old,nx1,ny1,u0,v0)
            endif
         else
            
            stop 'no output file specified'
            
         END IF
      enddo !#endwhile

      END program

c     -----------------------------------------------------------------

      SUBROUTINE io_ops (iu,fname,action)

      implicit none

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname, action

      INTEGER ierr

      IF (action.EQ.'OPEN/READ') THEN
        OPEN (iu,FILE=fname,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     &      IOSTAT=ierr,STATUS='OLD',ACTION='READ')
      ELSE IF (action.EQ.'OPEN/WRITE') THEN
        OPEN (iu,FILE=fname,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     &      IOSTAT=ierr,STATUS='REPLACE',ACTION='WRITE')
      ELSE IF (action.EQ.'OPEN/READWRITE') THEN
        OPEN (iu,FILE=fname,ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     &      IOSTAT=ierr,STATUS='OLD',ACTION='READWRITE')
      ELSE IF (action.EQ.'CLOSE') THEN
        CLOSE(iu,IOSTAT=ierr)
      ELSE
        ierr=-1
      END IF

      IF (ierr.NE.0) THEN
        WRITE (*,*) 'VAM: ', action,' error: ', ierr
        WRITE (*,*) '     While processing file: ', TRIM(fname)
        WRITE (*,*) '     On unit:               ', iu 
        STOP 'File operations error'
      END IF

      END SUBROUTINE io_ops

c     -----------------------------------------------------------------

      SUBROUTINE readhead_old
     &     (iu,fname,in_degrees,xs,dx,nx,ys,dy,ny,idate,itime,name)

      USE constants, ONLY: pi

      implicit none
      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      logical, intent(in) :: in_degrees
      INTEGER, INTENT(OUT) :: nx,ny,idate,itime
      REAL, INTENT(OUT) :: xs,dx,ys,dy
      CHARACTER*8, INTENT(OUT) :: name

      INTEGER ierr

      WRITE (*,*) 'VAM: Readhead: File open: ',TRIM(fname)
      CALL io_ops (iu,fname,'OPEN/READ')

      READ(iu,IOSTAT=ierr) idate,itime,nx,xs,dx,ny,ys,dy
      IF (ierr.NE.0) CALL io_ops (iu,fname,'Read grid header')

      name='.........'
c!!!  Currently these are in degrees on the test data set:
      if (in_degrees) then
         xs=xs*(pi/180)
         dx=dx*(pi/180)
         ys=ys*(pi/180)
         dy=dy*(pi/180)
      endif

      CALL io_ops (iu,fname,'CLOSE')
      WRITE (*,*) 'VAM: Grid header has been read'

      END SUBROUTINE readhead_old

c     -----------------------------------------------------------------

      SUBROUTINE readgrid_old (iu,fname,nx,ny,u,v)

      implicit none
      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(OUT), DIMENSION(nx,ny) :: u,v

      INTEGER ierr,idate1,itime1,nx1,ny1
      REAL xs1,dx1,ys1,dy1

      WRITE (*,*) 'VAM: Readgrid: File open: ',TRIM(fname)
      CALL io_ops (iu,fname,'OPEN/READ')

      READ(iu,IOSTAT=ierr) idate1,itime1,nx1,xs1,dx1,ny1,ys1,dy1
      IF (ierr.NE.0) CALL io_ops (iu,fname,'Read grid header')

      IF (nx1.NE.nx .OR. ny1.NE.ny) THEN
        WRITE (*,*) 'VAM: nx,ny,nx1,ny1: ', nx,ny,nx1,ny1
        CALL io_ops (iu,fname,'Grid descriptor mismatch')
      END IF

      READ(iu,IOSTAT=ierr) u
      IF (ierr.EQ.0) READ(iu,IOSTAT=ierr) v
      IF (ierr.NE.0) CALL io_ops (iu,fname,'Read grid values')

      CALL io_ops (iu,fname,'CLOSE')
      WRITE (*,*) 'VAM: Grid values read'

      END SUBROUTINE readgrid_old

c     -----------------------------------------------------------------

      SUBROUTINE dumphead_old
     &     (iu,fname,xs,dx,nx,ys,dy,ny,idate,itime,name)

      implicit none
      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny,idate,itime
      REAL, INTENT(IN) :: xs,dx,ys,dy
      CHARACTER*8, INTENT(IN) :: name

      INTEGER ierr

      WRITE (*,*) 'VAM: Dumphead: File open: ',TRIM(fname)
      CALL io_ops (iu,fname,'OPEN/WRITE')

      WRITE(iu,IOSTAT=ierr) idate,itime,nx,xs,dx,ny,ys,dy
      IF (ierr.NE.0) CALL io_ops (iu,fname,'Dump grid header')

      CALL io_ops (iu,fname,'CLOSE')
      WRITE (*,*) 'VAM: Grid header dumped'

      END SUBROUTINE dumphead_old

c     -----------------------------------------------------------------

      SUBROUTINE dumpgrid_old (iu,fname,nx,ny,u,v)

      implicit none
      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: nx,ny
      REAL, INTENT(IN), DIMENSION(nx,ny) :: u,v

      INTEGER ierr,idate1,itime1,nx1,ny1
      REAL xs1,dx1,ys1,dy1

      WRITE (*,*) 'VAM: Dumpgrid: File open: ',TRIM(fname)
      CALL io_ops (iu,fname,'OPEN/READWRITE')

      READ(iu,IOSTAT=ierr) idate1,itime1,nx1,xs1,dx1,ny1,ys1,dy1
      IF (ierr.NE.0) CALL io_ops (iu,fname,'Read grid header')

      IF (nx1.NE.nx .OR. ny1.NE.ny) THEN
        WRITE (*,*) 'VAM: nx,ny,nx1,ny1: ', nx,ny,nx1,ny1
        CALL io_ops (iu,fname,'Grid descriptor mismatch')
      END IF

      WRITE(iu,IOSTAT=ierr) u
      IF (ierr.EQ.0) WRITE(iu,IOSTAT=ierr) v
      IF (ierr.NE.0) CALL io_ops (iu,fname,'Dump grid values')

      CALL io_ops (iu,fname,'CLOSE')
      WRITE (*,*) 'VAM: Grid values dumped'

      END SUBROUTINE dumpgrid_old

