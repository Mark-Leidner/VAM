c!#   $Id: restart_dp_sp.f,v 1.1 1999/08/06 17:46:58 trn Exp $
c!#   $Log: restart_dp_sp.f,v $
c!#   Revision 1.1  1999/08/06 17:46:58  trn
c!#   Initial revision
c!#
      program restart_dp_sp

c     Purpose: Converts old VAM restart formats gridded data sets 
c              from double precision to single precision

      IMPLICIT NONE

c!#~   iuin     input i/o unit
c!#~   iuout     output i/o unit
      INTEGER :: iuin=10, iuout=11
c!#~   maxpath   maximum characters allowed in file name
c!#~   in_fname    input file name
c!#~   out_fname    input file name
      INTEGER, PARAMETER :: maxpath=255
      CHARACTER (len=maxpath) ::
     &     in_fname, out_fname
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
      DOUBLE PRECISION xs1, dx1, ys1, dy1
      REAL xs2, dx2, ys2, dy2
      INTEGER nx1, ny1, idate1, itime1, nx1a, ny1a, idate1a, itime1a
      INTEGER nx2, ny2, idate2, itime2
      CHARACTER*8 name1

      double precision, dimension(:,:), allocatable :: uv1
      real, dimension(:,:), allocatable :: uv2
      integer iuv
      logical l_done

      l_done = .FALSE.
      do while (.not. l_done)
         write (*,*) 'Enter input filename (in single quotes):'
         read (*,*) in_fname
         write (*,'(1x,a)') 'Opening input file:',trim(in_fname)
         OPEN (iuin,FILE=in_fname,ACCESS='SEQUENTIAL',
     &        FORM='UNFORMATTED',
     &        IOSTAT=ierr,STATUS='OLD',ACTION='READ')
         if (ierr .ne. 0)
     &        write (*,*) 'Error opening input file, please try again:'
         l_done = ierr .eq. 0
      enddo
      l_done = .FALSE.
      do while (.not. l_done)
         write (*,*) 'Enter output filename (in single quotes):'
         read (*,*) out_fname
         write (*,'(1x,a)') 'Opening output file:',trim(out_fname)
         OPEN (iuout,FILE=out_fname,ACCESS='SEQUENTIAL',
     &        FORM='UNFORMATTED',
     &        IOSTAT=ierr,STATUS='REPLACE',ACTION='WRITE')
         if (ierr .ne. 0)
     &        write (*,*) 'Error opening output file, please try again:'
         l_done = ierr .eq. 0
      enddo

      READ(iuin,IOSTAT=ierr) idate1,idate1a,itime1,itime1a,nx1,nx1a,
     &     xs1,dx1,ny1,ny1a,ys1,dy1
      IF (ierr.NE.0) stop 'Read grid header'

      write (*,*)
     &     'idate1,itime1,xs1,dx1,nx1,ys1,dy1,ny1',
     &     idate1,itime1,xs1,dx1,nx1,ys1,dy1,ny1
      write (*,*)
     &     'idate1a,itime1a,nx1a,ny1a',
     &     idate1a,itime1a,nx1a,ny1a

      idate2=idate1
      itime2=itime1
      nx2=nx1
      xs2=xs1
      dx2=dx1
      ny2=ny1
      ys2=ys1
      dy2=dy1

      WRITE(iuout,IOSTAT=ierr) idate2,itime2,nx2,xs2,dx2,ny2,ys2,dy2
      IF (ierr.NE.0) stop 'Write grid header'

      ALLOCATE(uv1(nx2,ny2))
      ALLOCATE(uv2(nx2,ny2))

      do iuv=1,2
         READ(iuin,IOSTAT=ierr) uv1
         IF (ierr.NE.0) stop 'Read uv'

         uv2=uv1

         WRITE(iuout,IOSTAT=ierr) uv2
         IF (ierr.NE.0) stop 'Write uv'
      enddo

      write (*,*) 'Normal Program stop'
      end

