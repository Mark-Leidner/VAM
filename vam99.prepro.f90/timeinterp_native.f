c!#
c!# PURPOSE : Interpolate two native wind fields to a central time.

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!# REQUIREMENTS : 
c!# Read in central date/time and two native-format filenames.
c!# Perform time interpolation of native met fields to central 
c!# time and write them out.

c!# CONSTRAINTS : 
c!# The input native format data must not contain any missing data.

c!# LANGUAGE : Fortran

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :
c!# Code requires five inputs at the command line. Creates new 
c!# native format files 'temp_native.dat' and 'temp_native.hdr'.
c!# An optional sixth command line input will override the
c!# 'temp_native.*' naming convention for output native file.
c!# See subroutine get_input_args_qs.
c!#
c!# If command line inputs 'hdrin_file1' and 'hdrin_file2' are 
c!# identical, then timeinterp_native replicates the input native 
c!# file header and data to output.

c*****************************************************************
      program timeinterp_native
c*****************************************************************

c!# GLOBAL AND SHARED DATA : 
      USE grid_hdr_typ_m   !#
      USE grid_hdr_io_m   !#
      USE delta_time_m

c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# REFERENCES : Adopted from native_rotate_winds.f

c!# LIMITATIONS : 
c!# Output header contains date and time of central quikscat rev time.
c!# Note: idps will contain timing information that does not reflect
c!# this central time.

c!# CHANGE LOG : 
c
c $Id: timeinterp_native.f,v 1.4 2003/01/31 15:33:30 trn Exp $
c 
c $Log: timeinterp_native.f,v $
c Revision 1.4  2003/01/31 15:33:30  trn
c Changeover to txthdr version of native
c
c Revision 1.3  1999/10/25 16:41:03  mcc
c updated to use routine delta_time.
c
c Revision 1.2  1999/10/05 13:58:43  mcc
c Made code compatible with any year.
c
c Revision 1.1  1999/10/04 14:04:00  mcc
c Initial revision
c
c  programmer:
c  Mark C. Cerniglia - AER Inc.
c  created: Sept 1999.
c 

c!# LOCAL DATA ELEMENTS : 

      implicit none

      integer :: i, ierr, ilev, ivar, recl, j,
     &     nlevs_in1, nvars_in1, nlevs_in2, nvars_in2

c!#   Date/time variables:
c!#   
      integer :: idate, itime, avetim
      integer :: time1, time2, timdif
      real :: tfact
      character (len=14) :: native_tim1, native_tim2
      character (len=14) :: janone_tim, central_tim

      integer, parameter :: maxpath = 255, maxline=512
      integer, parameter :: outunit=20
      integer, parameter :: inunit1=10
      integer, parameter :: inunit2=11

      character (len=maxpath) :: hdrout_file, datout_file,
     &     hdrin_file1, datin_file1, hdrin_file2, datin_file2

c!# LOCAL DATA STRUCTURES :
      type (grid_hdr_typ) :: in_hdr, out_hdr

      real, dimension(:,:), allocatable :: dummet1, dummet2
      real, dimension(:,:), allocatable :: dummetout
      logical chktim

c!# DATA FILES : 
c!# Input: 4d hdr and data files for winds (and others)
c!# Output:  4d hdr and data files for rotated winds (and optionally others)

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c!#   get inputs 

      call get_input_args_qs(maxpath, hdrin_file1, datin_file1,
     &     hdrin_file2, datin_file2,
     &     hdrout_file, datout_file, idate, itime)

c!#   read input headers

      print *,'Reading input from: ',trim(hdrin_file1)
      call get_grid_hdr(trim(hdrin_file1),in_hdr,ierr)
      if (ierr .ne. 0) then
         print *,'timeinterp_native: Aborting with ierr ',
     &        'from get_grid_hdr= ',ierr
         stop 'timeinterp_native: Aborting'
      endif

c!#   initialize and output header,
c!#   load number of levels and variables.

      out_hdr = in_hdr
      nlevs_in1 = size(in_hdr%levs)
      nvars_in1 = size(in_hdr%vars)

c!#   extract yyyy from header of first native file to set time in 
c!#   seconds from start of january 1, yyyy. 

      write (janone_tim,'(i8.8)') in_hdr%ncicod
      janone_tim(1:14) = janone_tim(1:4)//'0101000000'

c!#   find aveage obs time in seconds from start of january 1, yyyy
c!#   using idate and itime. 

      write (central_tim,'(i8.8,i6.6)') idate, itime
      call delta_time(janone_tim,central_tim,-1,timdif,ierr)
      avetim = timdif 

c!#   for first and second native files, load header date and time
c!#   information into character strings

      write (native_tim1,'(i8.8,i6.6)') in_hdr%ncicod,
     & in_hdr%ncicot
      
      print *,'Reading input from: ',trim(hdrin_file2)
      call get_grid_hdr(trim(hdrin_file2),in_hdr,ierr)
      if (ierr .ne. 0) then
         print *,'timeinterp_native: Aborting with ierr ',
     &        'from get_grid_hdr= ',ierr
         stop 'timeinterp_native: Aborting'
      endif

      nlevs_in2 = size(in_hdr%levs)
      nvars_in2 = size(in_hdr%vars)

c!#   check if nlevs_in and nvars_in are the same for both
c!#   native input files

      if( nlevs_in1 .ne. nlevs_in2 ) then
         print *,'timeinterp_native: Aborting  ',
     &        ' nlevs_in1 ne nlevs_in2 ', nlevs_in1, nlevs_in2 
         stop 'timeinterp_native: Aborting'
      endif

      if( nvars_in1 .ne. nvars_in2 ) then
         print *,'timeinterp_native: Aborting  ',
     &        ' nvars_in1 ne nvars_in2 ', nvars_in1, nvars_in2 
         stop 'timeinterp_native: Aborting'
      endif

      write (native_tim2,'(i8.8,i6.6)') in_hdr%ncicod,
     & in_hdr%ncicot

c!#   calculate time in seconds from start of yyyy and time of native files.
c!#   this is consistant with quikscat central time. calculate weight for
c!#   time interpolation, tfact.

      call delta_time(janone_tim,native_tim1,0,timdif,ierr)
      time1 = timdif * 60
      call delta_time(janone_tim,native_tim2,0,timdif,ierr)
      time2 = timdif * 60

      print*,' '
      print*,' Time of first native file:  ',time1
      print*,' Cental observation time:    ',avetim
      print*,' Time of second native file: ',time2
      print*,' '

c!#   check if central time falls between times of bounding
c!#   native files. if both input native analysis files are 
c!#   the same, interpolated native file will be equivalent
c!#   (override chktim logic for this case).
c!#   note: idps will contain timing information that does not 
c!#   reflect this central time.

      chktim=.false.
      if(time1 <= avetim .and. avetim <= time2) then
         chktim = .true.
      else
         print*,' '
         print*,'NOTE: Central time not properly bounded'
         print*,'by 1st and 2nd native file times.'
         print*,' '
      endif

c!#   determine weight for time interpolation.

      if( time1 .eq. time2 ) then
         print*,' '
         print*,'Times of 1st and 2nd native files are equal.'
         print*,'Interpolated native file will be equivalent.'
         print*,' '
         chktim = .true.
         tfact = 0.
         out_hdr%ncicod = in_hdr%ncicod
         out_hdr%ncicot = in_hdr%ncicot
      else      
         tfact = float(avetim-time1)/float(time2 - time1)
         out_hdr%ncicod = idate
         out_hdr%ncicot = itime
      endif

      if(.not. chktim) then
         print *,'timeinterp_native: Aborting  ',
     &        'obs central time outside of native file times'
         print *,time1, avetim, time2
         stop 'timeinterp_native: Aborting'
      endif

      print *,'writing header to file: ', trim(hdrout_file)

      call put_grid_hdr(trim(hdrout_file), out_hdr,ierr)
      if (ierr .ne. 0) then
         print *,'timeinterp_native: Aborting with ierr ',
     &        'from put_grid_header= ',ierr
         stop 'timeinterp_native: Aborting'
      endif

      allocate(dummet1(in_hdr%dims(1),in_hdr%dims(2)),
     &     dummet2(in_hdr%dims(1),in_hdr%dims(2)),
     &     dummetout(in_hdr%dims(1),in_hdr%dims(2)),
     &     stat=ierr)

      if (ierr .ne. 0) stop
     &  'timeinterp_native: Aborting at allocation of working arrays'

c!#   read in met fields, perform time interpolation and store
      inquire(iolength=recl) dummet1

      print *,'Reading input from: ',trim(datin_file1)
      open(unit=inunit1,file=datin_file1,
     &      form='unformatted',access='direct',recl=recl,iostat=ierr)
      if (ierr .ne. 0) stop
     &     'timeinterp_native: Aborting at datin_file1 open'

      print *,'Reading input from: ',trim(datin_file2)
      open(unit=inunit2,file=datin_file2,
     &      form='unformatted',access='direct',recl=recl,iostat=ierr)
      if (ierr .ne. 0) stop
     &     'timeinterp_native: Aborting at datin_file2 open'

      print *,'Writing data to: ',trim(datout_file)
      open(unit=outunit,file=datout_file,
     &     form='unformatted',access='direct',recl=recl,iostat=ierr)
      if (ierr .ne. 0) stop
     &     'timeinterp_native: Aborting datout_file open'

c!#   read in field, interpolate wrt time, write out field
c!#   loop over all fields and vertical levels.
   
      do ivar = 1, size(out_hdr%vars)
         do ilev=1,nlevs_in1

c     read met field from first native file: dummet1
            call read_native_field(inunit1,in_hdr,
     &           in_hdr%levs(ilev),in_hdr%vars(ivar),dummet1,ierr)
            if (ierr .ne. 0) stop
     &           'timeinterp_native: Aborting at dummet1 read'

c     read met field from second native file: dummet2
            call read_native_field(inunit2,in_hdr,
     &           in_hdr%levs(ilev),in_hdr%vars(ivar),dummet2,ierr)
            if (ierr .ne. 0) stop
     &           'timeinterp_native: Aborting at dummet2 read'

c     interpolate in time to create new analysis
            dummetout(:,:)=(1.-tfact)*dummet1(:,:)+tfact*dummet2(:,:)

            write (outunit,rec=(ivar-1)*nlevs_in1 + ilev,iostat=ierr)
     &           dummetout
            if (ierr .ne. 0) stop
     &           'timeinterp_native: Aborting at dummet1 write'
         enddo
      enddo

      close(inunit1)
      close(inunit2)
      close(outunit)

      deallocate(dummet1,dummet2,dummetout)
      print *,'normal program termination'

      end program timeinterp_native
c
c*****************************************************************
      subroutine get_input_args_qs(maxpath, hdrin_file1,
     &     datin_file1, hdrin_file2, datin_file2,
     &     hdrout_file, datout_file, idate, itime)
c*****************************************************************
c
      implicit none

      integer, intent(in) :: maxpath
      character (len=maxpath), intent(out) :: hdrout_file, datout_file,
     &     hdrin_file1, datin_file1, hdrin_file2, datin_file2

      integer, intent(out) :: idate, itime

      integer :: ierr, nargs, iarg	!#
      integer :: inlens, len_tim
      character*(MAXPATH+2) :: argv	!#
c!# external functions for handling command lines
      integer iargc	!#
      external getarg	!#
      character*24 :: idatec, itimec

c!#   set up defaults if switches don't override
      hdrin_file1 = ' ' 
      hdrin_file2 = ' ' 
      hdrout_file = 'temp_native.hdr' 

c!#   retrieve command line arguments; output help msg if error
      nargs = iargc()
      do iarg = 1, nargs
         call getarg (iarg, argv)
         if (argv(1:2) == '-D') then	
            idatec = argv(3:)
            inlens = index(idatec,' ') - 1
            if (inlens < 0) goto 100
            read (idatec, '(i8)') idate
         else if (argv(1:2) == '-T') then	
            itimec = argv(3:)
            inlens = index(itimec,' ') - 1
            len_tim = len_trim(itimec)
            if (inlens < 0) goto 100
            read (itimec, '(i6)') itime
         else if (argv(1:2) == '-F') then	
            hdrin_file1 = argv(3:)	
            print *,'hdrin_file1 = -F',trim(hdrin_file1)
         else if (argv(1:2) == '-S') then	
            hdrin_file2 = argv(3:)	
            print *,'hdrin_file2 = -S',trim(hdrin_file2)
         else if (argv(1:2) == '-O') then	
            hdrout_file = argv(3:)	
            print *,'hdrout_file = -O',trim(hdrout_file)
         else	
            print *, 'Unknown argument: ', trim(argv)	
            goto 100	
         endif	
      enddo

c!#   exit if required command-line inputs missing
      if (hdrin_file1(1:1) == ' ') then	!#
         print *,'Missing required arguments'	!#
         goto 100	!#
      endif	!#
      if (hdrin_file2(1:1) == ' ') then	!#
         print *,'Missing required arguments'	!#
         goto 100	!#
      endif	!#

      datin_file1 =
     &     hdrin_file1(1:index(hdrin_file1,'.',BACK=.TRUE.)-1) // '.dat'
      datin_file2 =
     &     hdrin_file2(1:index(hdrin_file2,'.',BACK=.TRUE.)-1) // '.dat'

      datout_file =
     &     hdrout_file(1:index(hdrout_file,'.',BACK=.TRUE.)-1) // '.dat'

      print *,' '
      print *,'Using hdrin_file1=',trim(hdrin_file1)
      print *,'Using datin_file1=',trim(datin_file1)
      print *,'Using hdrin_file2=',trim(hdrin_file2)
      print *,'Using datin_file2=',trim(datin_file2)
      print *,'Writing hdrout_file=',trim(hdrout_file)
      print *,'Writing datout_file=',trim(datout_file)
      print *,' '
      return

c!# ERROR HANDLING : program stop
  100 continue	!#
      print *, 'legal arguments: '	!#
      print *,' Required: -Dobs central date in yyyymmdd format'!#
      print *,' Required: -Tobs central time in hhmmss format'	!#
      print *,' Required: -Cobs central time in seconds '	!#
      print *,' Required: -Fhdrin_file1 (input 1st native file 4d hdr)'	!#
      print *,' Required: -Shdrin_file2 (input 2nd native file 4d hdr)'	!#
      print *,' Optional: -Ohdrout_file (output native format 4d hdr) '	!#
      stop 'Abort: Bad input args'	!#

      end subroutine get_input_args_qs
c
c
