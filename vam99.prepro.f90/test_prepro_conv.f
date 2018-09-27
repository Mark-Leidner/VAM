c!#   $Id: test_prepro_conv.f,v 1.5 2004/03/19 20:47:33 rnh Exp $
c!#   $Log: test_prepro_conv.f,v $
c!#   Revision 1.5  2004/03/19 20:47:33  rnh
c!#   FGAT: u_fgat=vfgat=0, alpha=1.  Still TBD: are times useful
c!#
c!#   Revision 1.4  1999/10/08 14:30:07  trn
c!#   Updated vam_obs data file structure, some
c!#   reorganization of modules, other stylistic changes.
c!#
c!#   Revision 1.3  1999/08/06 13:53:23  trn
c!#   Updated to version 1.2 of obs_data_typ_mod.f
c!#
c!#   Revision 1.2  1999/07/21 19:17:51  trn
c!#   added RCS keywords, comments
c!#
      program test_prepro_conv

c     Purpose: Creates binary obs data sets for conventional data
c              from ASCII format input

      USE types, ONLY: obs_data_typ
c
      implicit none

c!# obsfiles namelist
      logical :: lmore, vprint
      integer, parameter :: maxpath=255
      character (len=maxpath) :: in_filename, out_filename
      namelist /obsfiles/ in_filename, out_filename, lmore, vprint

c!# Local Data :
      character (len=maxpath) :: hdr_fname, bin_fname
      integer :: ierr, iuvam=10, num_files, num_obs_total, recl

c!# LOCAL DATA STRUCTURES : 
      type (obs_data_typ) :: conv_data

      lmore = .TRUE.
      vprint = .FALSE.
      num_files = 0
      num_obs_total = 0
      in_filename = ' '
      out_filename = ' '
      do while (lmore)

         read (*,obsfiles,iostat=ierr)
         if (ierr .ne. 0) then
            write (*,obsfiles)
            write (*,*) 'Error ', ierr,' reading namelist ',
     &           '/obsfiles/ from file standard input.'
            stop 777
         endif
         num_files = num_files + 1
c!#   ...Fill up conv_data data structure from file (test ASCII format):
         call fill_conv_obs(iuvam, in_filename,
     &        conv_data, vprint, ierr)
         if (ierr .ne. 0) then  !#
c!#      ...Error handling: discard current input file
            print *,'Warning: file= ',in_filename,
     &           ' not used because ierr= ',ierr
         else                   !#
            num_obs_total = num_obs_total + conv_data%n_loc
c!#      ...write output to file
            hdr_fname = trim(out_filename) // '.txt'
            bin_fname = trim(out_filename) // '.dat'
            if (vprint) print *,'Writing data to ',
     &           trim(hdr_fname),' and ',
     &           trim(bin_fname)
            inquire(iolength=recl) conv_data%lat_deg
            call wrtobs(iuvam, hdr_fname, bin_fname,
     &           conv_data%c_obs_id,
     &           conv_data%n_const, conv_data%n_loc,
     &           conv_data%n_occ, conv_data%n_var,
     &           conv_data%names_const, conv_data%const,
     &           conv_data%names_var, recl,
     &           conv_data%num_occ, conv_data%record_id, conv_data%time,
     &           conv_data%lat_deg, conv_data%lon_deg,
     &           conv_data%u_int, conv_data%v_int, 
     &           conv_data%u_fgat, conv_data%v_fgat, conv_data%alpha, 
     &           conv_data%qc_flag, conv_data%data, ierr)
            if (ierr .ne. 0) stop 'Error writing output'
         endif                  !#
      enddo

      print *,'Processed ',num_files,' files for a total of ',
     &     num_obs_total,' observations'
      end

      SUBROUTINE fill_conv_obs (       !#
     &     inunit, infile,	!#
     &     conv_data, vprint, ierr)	!#
c!# NOTE: This is a test version of the routine.  Needs to agree with the
c!#       corresponding interface definition included in the test version
c!#       of create_conv_obs.

      USE types, ONLY: obs_data_typ
      implicit none

      integer, intent(in) :: inunit
      character*(*), intent(in) :: infile
      type (obs_data_typ) :: conv_data
      integer, intent(out) :: ierr
      logical, intent(in) :: vprint

c!# LOCAL DATA ELEMENTS : 
      integer, parameter :: MAXLEN=255, nocc=1, nvar=2, nconst=2
     &     
      character*(MAXLEN) :: inline
      integer :: i, j, nloc, ierr_local
      real, parameter :: uoesd=1, voesd=1

      ierr = 0

c!#   1. Open file, read header and nloc:

      open(unit=inunit, file=infile, iostat=ierr)
c!# ERROR HANDLING : return nonzero error codes
      if (ierr .ne. 0) then
         if (vprint) print *,'Error opening file ',
     &        trim(infile)
         return
      endif
      
      if (vprint) print *,'Reading header from file ',
     &     trim(infile)
      inline = ' '
      do while (inline(1:10) .ne. 'END HEADER')
         read (inunit,'(a)',iostat=ierr) inline
         if (ierr .ne. 0) then
c!# ERROR HANDLING : see below
            if (vprint) print *,'Error reading header '
            goto 900
         endif
         if (vprint) write (*,'(1x,a)') trim(inline)
      enddo !endwhile

      if (vprint) print *,'Reading nloc from file ',
     &     trim(infile)
      read (inunit,*,iostat=ierr) nloc
      if (ierr .ne. 0) then
c!# ERROR HANDLING : see below
         if (vprint) print *,'Error reading nloc '
         goto 900
      endif
      if (vprint) print *,'nloc= ',nloc

      if (nloc .le. 0) then
c!# ERROR HANDLING : see below
         ierr = nloc - 1
         goto 900
      endif
      
c!#   2. Allocate space for data to be read in

c!# Initialize conv_data structure:
      conv_data%c_obs_id = 'convention'
      conv_data%n_const = nconst
      conv_data%n_loc = nloc
      conv_data%n_occ = nocc
      conv_data%n_var = nvar
      allocate (conv_data%names_const(nconst),
     &     conv_data%const(nconst),stat=ierr_local)
      if (ierr_local .ne. 0)
     &     stop 'Abort: fill_conv_data allocate failure'
      allocate (conv_data%names_var(nvar),
     &     conv_data%num_occ(nloc), conv_data%record_id(nloc),
     &     conv_data%time(nloc), conv_data%lat_deg(nloc),
     &     conv_data%lon_deg(nloc), conv_data%gridi(nloc),
     &     conv_data%gridj(nloc), conv_data%u_int(nloc),
     &     conv_data%v_int(nloc), conv_data%u_fgat(nloc),
     &     conv_data%v_fgat(nloc), conv_data%alpha(nloc),
     &     conv_data%qc_flag(nocc,nloc),
     &     conv_data%data(nvar,nocc,nloc), stat=ierr_local)
      if (ierr_local .ne. 0)
     &     stop 'Abort: fill_conv_data allocate failure'

      conv_data%names_var(1) = 'u_wind'
      conv_data%names_var(2) = 'v_wind'
      conv_data%names_const(1) = 'sd(u)'
      conv_data%names_const(2) = 'sd(v)'
      conv_data%const(1) = uoesd
      conv_data%const(2) = voesd

c!#   3. Fill up the structure with the data from file, 
c!#      or with missing value indicators:

      do i = 1 , nloc
         read (inunit,*,iostat=ierr)
     &      conv_data%time(i),
     &      conv_data%lat_deg(i),
     &      conv_data%lon_deg(i),
     &      (conv_data%data(j,1,i),j=1,nvar)
         if (vprint) print *,'Read obs no ',i
         conv_data%num_occ(i) = 1
         conv_data%record_id(i) = i
         conv_data%u_int(i) = 0
         conv_data%v_int(i) = 0
         conv_data%u_fgat(i) = 0
         conv_data%v_fgat(i) = 0
         conv_data%alpha(i) = 1
         conv_data%qc_flag(1,i) = .FALSE.
         if (ierr .ne. 0) then
            if (vprint) print *,'Error reading obs no: ', i
c!# ERROR HANDLING : Deallocate if needed
            deallocate (conv_data%names_const,
     &           conv_data%const,
     &           conv_data%names_var,
     &           conv_data%num_occ, conv_data%record_id,
     &           conv_data%time, conv_data%lat_deg,
     &           conv_data%lon_deg, conv_data%gridi,
     &           conv_data%gridj, conv_data%u_int,
     &           conv_data%v_int, conv_data%u_fgat,
     &           conv_data%v_fgat, conv_data%alpha,
     &           conv_data%qc_flag, conv_data%data, stat=ierr_local)
            if (ierr_local .ne. 0)
     &           stop 'Abort: read_conv_obs deallocate failure'
            goto 900
         endif
         
      enddo

c!# Exit from routine: close file and return
  900 close (unit=inunit)
      return
      end
      
