c!#   $Id: obs_to_ascii.f,v 1.15 2005/07/11 17:19:04 leidner Exp $
c!#   $Log: obs_to_ascii.f,v $
c!#   Revision 1.15  2005/07/11 17:19:04  leidner
c!#   changed wrt_anal_nscat to set missing values where ambiguities are
c!#   missing, NOT where quality flags are set.  Also, the f90 intrinsic
c!#   'merge' does not work correctly when compiled with pgf90 5.2-4, so
c!#   implemented work around code.
c!#
c!#   Revision 1.14  2005/03/18 18:18:09  leidner
c!#   writing out of ssmi interpolated winds now skips all obs-related info;
c!#   only Vel_int data written out.
c!#
c!#   Revision 1.13  2004/12/16 21:09:59  leidner
c!#   Added writing out of qc_bit_map.
c!#
c!#   Revision 1.12  2004/09/30 19:58:42  leidner
c!#   added qc_bit_map arg to rddobs subroutine call.
c!#
c!#   Revision 1.11  2004/09/16 16:56:19  leidner
c!#   added output option for conventional data.
c!#
c!#   Revision 1.10  2004/09/01 17:34:46  leidner
c!#   added code to set a missing flag value for missing wind speeds derived from
c!#   passive microwave instruments (SSMI, TMI, AMSR)
c!#
c!#   Revision 1.9  2004/07/27 15:45:39  leidner
c!#   Add support for microwave-sensed wind speed data (use
c!#   vam2d.fortran/wsl_wrlos.F format)
c!#
c!#   Revision 1.8  2004/04/09 20:06:27  leidner
c!#   New VAM FGAT parameters now written out.
c!#
c!#   Revision 1.7  2004/03/19 20:47:33  rnh
c!#   FGAT: u_fgat=vfgat=0, alpha=1.  Still TBD: are times useful
c!#
c!#   Revision 1.6  2000/03/28 16:50:08  trn
c!#   Fixed format for backscatter output lat/lon
c!#
c!#   Revision 1.5  2000/01/24 15:41:45  trn
c!#   Add support for sigma0 data (use wna_wrback.f format)
c!#
c!#   Revision 1.4  1999/10/22 18:50:57  trn
c!#   Added code to ensure longitudes are within specified range (-180 to +180
c!#   by default, changable via namelist); added code to check for u, v, mle (had
c!#   to add a hardwired default as a compiler bug workaround; this version also
c!#   includes a number of debugging print statements.
c!#
c!#   Revision 1.3  1999/10/08 14:30:07  trn
c!#   Updated vam_obs data file structure, some
c!#   reorganization of modules, other stylistic changes.
c!#
c!#   Revision 1.2  1999/09/07 18:33:38  trn
c!#   Write out proper wgt for scatterometer winds; add comments
c!#
c!#   Revision 1.1  1999/08/30 15:28:38  trn
c!#   Initial revision
c!#
      program obs_to_ascii

c!#   Purpose: Creates ASCII format obs data sets for Splus code ingest
c!#            from VAM binary obs data set format

      USE types, ONLY: obs_data_typ, len_name, len_fname
      USE string_mod, ONLY: where_in

      implicit none

c!#   Program control via namelist:
      real :: lonmin=-180.
      logical :: more=.T., vprint=.T., interp=.F., qcflag=.T.	!#
      character (len=len_fname) :: in_file=' ', out_obs_file=' ',	!#
     &     out_int_file=' '	!#
      namelist /files/ in_file, out_obs_file, out_int_file,	!#
     &     more, vprint, qcflag, lonmin !#

      integer :: ierr, iuvam=10
      type(obs_data_typ) :: obs_data
      character (len=len_fname) :: hdr_fname, bin_fname
      character(len=len_name) :: in_obsid, temp_cname(1), temp_vname(1)
      real :: temp_const(1)
      integer :: i, num_const, num_loc, num_occ, num_var, recl, obsid
      integer idate, itime	!#obs_data_typ scalars

c!#   Number and names of supported observation types
c!#   this must agree with the id stored in the input files
      integer, parameter :: n_obsids=6
      character (len_name) :: names_obsid(n_obsids) = 
     &     (/'convention','ambiguous ','sigma0    ',
     &       'ssmi      ','tmi       ','amsr      '/)
c!#   Storage locations of names in array:
      integer, parameter :: conv_id=1,  amb_id=2, s0_id=3,
     &     ssmi_id=4, tmi_id=5, amsr_id=6

      do while (more)
         out_obs_file = ' '
         out_int_file = ' '
         read (*,files,iostat=ierr)
         write (*,files)
         if (ierr .ne. 0) then
            write (*,*) 'Error ', ierr,' reading namelist ',
     &           '/files/ from file standard input.'
            stop 777
         endif
         
c!#  Open input file and read header
         hdr_fname = trim(in_file) // '.txt'
         bin_fname = trim(in_file) // '.dat'
         
         if (vprint) print *,'Reading header from file ',
     &        trim(hdr_fname)
         call rdhobs(iuvam, hdr_fname, 1, 1,
     &        in_obsid, idate, itime, num_const, num_loc, num_occ,
     &        num_var,
     &        temp_cname, temp_const, temp_vname, ierr)
cdeb:
         write (*,*) in_obsid, idate, itime, num_const, num_loc,
     &        num_occ, num_var, temp_cname, temp_const, temp_vname,
     &        ierr
         if (ierr .ne. 0) stop 'Abort:  rdhobs failure'
      
         if (vprint) print *,
     &        'in_obsid, idate, itime, num_const, num_loc, num_occ,',
     &        ' num_var= ',
     &        in_obsid, idate, itime, num_const, num_loc, num_occ,
     &        num_var

c!# Initialize obs_data structure:
         obs_data%c_obs_id = in_obsid
         obs_data%n_const = num_const
         obs_data%n_loc = num_loc
         obs_data%n_occ = num_occ
         obs_data%n_var = num_var
         if (num_const .gt. 0) then
            allocate (obs_data%names_const(num_const),
     &           obs_data%const(num_const),stat=ierr)
            if (ierr .ne. 0)
     &           stop 'Abort:  allocate failure'
         endif
         allocate (obs_data%names_var(num_var),
     &        obs_data%num_occ(num_loc), obs_data%record_id(num_loc),
     &        obs_data%time(num_loc), obs_data%lat_deg(num_loc),
     &        obs_data%lon_deg(num_loc), obs_data%gridi(num_loc),
     &        obs_data%gridj(num_loc), obs_data%u_int(num_loc),
     &        obs_data%v_int(num_loc), obs_data%u_fgat(num_loc),
     &        obs_data%v_fgat(num_loc), obs_data%alpha(num_loc),
     &        obs_data%qc_flag(num_occ,num_loc),
     &        obs_data%qc_bit_map(num_occ,num_loc),
     &        obs_data%data(num_var,num_occ,num_loc)
     &        ,stat=ierr)
         if (ierr .ne. 0) stop 'Abort:  allocate failure'
         call rdhobs(iuvam, hdr_fname, obs_data%n_const, obs_data%n_var,
     &        in_obsid, obs_data%idate, obs_data%itime, num_const,
     &        num_loc, num_occ, num_var,
     &        obs_data%names_const, obs_data%const,
     &        obs_data%names_var, ierr)
         if (ierr .ne. 0) then
            write (*,*) 'Abort:  rdhobs failure, ierr= ',
     &           ierr
            stop 'Abort:  rdhobs failure'
         endif
cdeb:
         write (*,*) 'len_name= ',len_name
         write (*,*) 'n_var= ',obs_data%n_var
         write (*,*) 'names_var= ',obs_data%names_var
         write (*,*) 'n_const= ',obs_data%n_const
         write (*,*) 'names_const= ',obs_data%names_const
         write (*,*) 'const= ',obs_data%const

c!#  Read data and close input file
         inquire(iolength=recl) obs_data%lat_deg
cdeb:
         write (*,*) 'recl= ',recl
         call rddobs(iuvam, bin_fname, num_loc, num_occ, num_var, recl,
     &        obs_data%num_occ, obs_data%record_id,
     &        obs_data%time, obs_data%lat_deg,
     &        obs_data%lon_deg, obs_data%u_int, obs_data%v_int,
     &        obs_data%u_fgat, obs_data%v_fgat, obs_data%alpha,
     &        obs_data%qc_flag, obs_data%qc_bit_map,
     &        obs_data%data, ierr)
         if (ierr .ne. 0) then
            write (*,*) 'Abort:  rddobs failure, ierr= ',
     &           ierr
            stop 'Abort:  rddobs failure'
         endif

         obsid = where_in (obs_data%c_obs_id, names_obsid,
     &        len_name, len_name, n_obsids)

c!# Enforce lonmin <= lon_deg <= lonmin+360
         obs_data%lon_deg = merge(obs_data%lon_deg + 360.,
     &        obs_data%lon_deg,obs_data%lon_deg < lonmin)
         obs_data%lon_deg = merge(obs_data%lon_deg - 360.,
     &        obs_data%lon_deg,obs_data%lon_deg > lonmin+360)

c!#   Supported obs types so far:
         if (obsid .eq. amb_id) then	!#Ambiguous winds
            if (out_obs_file .ne. ' ') call wrt_anal_nscat(
     &           iuvam, out_obs_file, obs_data, 
     &           .FALSE., qcflag, vprint, ierr)
            if (out_int_file .ne. ' ') call wrt_anal_nscat(
     &           iuvam, out_int_file, obs_data, 
     &           .TRUE., qcflag, vprint, ierr)
         elseif (obsid .eq. s0_id) then	!#backscatter
            if (out_obs_file .ne. ' ') call wrt_anal_s0(
     &           iuvam, out_obs_file, obs_data, 
     &           .FALSE., qcflag, vprint, ierr)
            if (out_int_file .ne. ' ') call wrt_anal_s0(
     &           iuvam, out_int_file, obs_data, 
     &           .TRUE., qcflag, vprint, ierr)
         elseif (obsid .eq. ssmi_id .or.
     &           obsid .eq. tmi_id  .or.
     &           obsid .eq. amsr_id) then  !# windspeeds derived from microwave instruments
            if (out_obs_file .ne. ' ') call wrt_anal_ssmi(
     &           iuvam, out_obs_file, obs_data, 
     &           .FALSE., qcflag, vprint, ierr)
            if (out_int_file .ne. ' ') call wrt_anal_ssmi(
     &           iuvam, out_int_file, obs_data, 
     &           .TRUE., qcflag, vprint, ierr)

         elseif (obsid .eq. conv_id) then	!#conventional obs
            if (out_obs_file .ne. ' ') call wrt_anal_conventional(
     &           iuvam, out_obs_file, obs_data, 
     &           .FALSE., qcflag, vprint, ierr)
            if (out_int_file .ne. ' ') call wrt_anal_conventional(
     &           iuvam, out_int_file, obs_data, 
     &           .TRUE., qcflag, vprint, ierr)
         else
            stop 'unsupported obs_id'
         endif
         if (ierr .ne. 0) stop 'error writing data to file'

      enddo !# endwhile

      end
      
      subroutine wrt_anal_nscat(iuvam, out_file, obs_data, 
     &     interp, qcflag, vprint, ierr)

c!#   Routine for writing out interpolated analysis or obs values at
c!#   scatterometer winds locations, in ASCII format compatible with 
c!#   Splus codes in read.vam.datasets

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      implicit none

      integer, intent(in) :: iuvam
      integer, intent(out) :: ierr
      character (len=*) :: out_file
      logical, intent(in) :: interp, qcflag, vprint

      type(obs_data_typ) :: obs_data

      integer, parameter :: nrevs=15
c!#   Use dummy values for:
      integer :: revs(nrevs), nbyrevs(nrevs),	!#
     &     ngood, i, k, iocc
      integer, dimension(:), allocatable :: igood, row, cell
      real, dimension(:), allocatable :: wgt, uvtemp
      real :: lognum=1, vmiss=-999.	!#

c!#   Names of variables:
      character (len=len_name), parameter :: u_name='u_wind',
     &     v_name='v_wind', mle_name='mle'
      integer :: i_u, i_v, i_mle

c-----Check data structure - find needed variables:
      write (*,*) 'u_name= ',u_name
      write (*,*) 'v_name= ',v_name
      write (*,*) 'n_var= ',obs_data%n_var
      write (*,*) 'names_var= ',obs_data%names_var
      write (*,*) 'len_name= ',len_name
      i_u = where_in(u_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_v = where_in(v_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_mle = where_in(mle_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)

      ierr = 1
      if (vprint) write (*,*) 'Writing nscat winds file to ',
     &     trim(out_file)
      open (unit=iuvam, file=out_file, err=900)	!#

      ierr = 2
      write (iuvam, '(i10)', err=900) obs_data%idate,
     &     obs_data%itime
      write (iuvam, '(f10.2)', err=900) lognum		!#dummy values
      revs(:) = 0
      revs(1) = 1
      write (iuvam, '(15i6)', err=900) revs		!#dummy values
      nbyrevs(:) = 0
      if (qcflag) then	!# Skip obs without any valid data
         ngood = 0
         do i=1,obs_data%n_loc
            if (.not. ALL(obs_data%qc_flag(:,i))) ngood = ngood + 1
         enddo
      else
         ngood = obs_data%n_loc !#
      endif
      nbyrevs(1) = ngood				!#
      write (iuvam, '(15i6)', err=900) nbyrevs		!#
      write (iuvam, '(15i6)', err=900) ngood		!#

      ierr=3
      allocate (igood(ngood), wgt(ngood), uvtemp(ngood),
     &     row(ngood), cell(ngood))
      k=0
      do i=1,obs_data%n_loc
         if (.not. (qcflag .and.  ALL(obs_data%qc_flag(:,i)))) then
            k=k+1
            igood(k)=i
            wgt(k) = 1
            if (ALL(obs_data%qc_flag(:,i))) wgt(k) = -1
         endif
      enddo
      
      if (k .ne. ngood) stop 'Internal logic error'

      write (iuvam, '(10f10.3)', err=900) obs_data%lat_deg(igood)	!#
      write (iuvam, '(10f10.3)', err=900) obs_data%lon_deg(igood)	!#

      ierr=4
      row = obs_data%record_id(igood) / 100
      cell = mod(obs_data%record_id(igood) , 100)
      if (interp) then	!# gridded field interpolated to obs:
         write (*,*) 'Writing u_int and v_int'
         write (iuvam, '(10f10.3)', err=900) obs_data%u_int(igood)	!#u
         write (iuvam, '(10f10.3)', err=900) obs_data%v_int(igood)	!#v
         write (iuvam, '(10i10)', err=900) row	!#row - dummy values
         write (iuvam, '(10i10)', err=900) cell	!#column - dummy values
      else	!# obs values
         if (i_u .le. 0 .or. i_v .le. 0) then
            write (*,*) 'wrt_anal_nscat: Cannot find u and/or v',
     &           ' Matching codes= ',i_u, i_v
c$$$cdeb: reset to defaults as a temporary solution:
c$$$            i_u=1 ; i_v=2
c$$$            write (*,*) 'WORKAROUND FOR COMPILER BUG: ',
c$$$     &           'use hardwired values= ',i_u, i_v
            stop 'wrt_anal_nscat:  Cannot find u and/or v'
         endif
         write (*,*) 'Writing ambiguous winds'
         write (iuvam, '(10i10)', err=900) obs_data%num_occ(igood)	!#namb
         do iocc=1,4	!# 4 ambiguities - missing fill for missing ambiguities
c           uvtemp = merge(spread(vmiss,1,ngood),
c    &           obs_data%data(i_u,iocc,igood),
c    &           iocc .gt. obs_data%num_occ(igood))
            uvtemp = obs_data%data(i_u,iocc,igood)
            do i=1,ngood
               if (iocc .gt. obs_data%num_occ(igood(i)))  
     &               uvtemp(i) = vmiss
            enddo
            write (iuvam, '(10f10.3)', err=900) uvtemp !#u,u2,u3,u4

c           uvtemp = merge(spread(vmiss,1,ngood),
c    &            obs_data%data(i_v,iocc,igood),
c    &            iocc .gt. obs_data%num_occ(igood))
            uvtemp = obs_data%data(i_v,iocc,igood)
            do i=1,ngood
               if (iocc .gt. obs_data%num_occ(igood(i)))
     &               uvtemp(i) = vmiss
            enddo
            write (iuvam, '(10f10.3)', err=900) uvtemp !#v,v2,v3,v4

            if (i_mle .eq. 0) then
               if (iocc .eq. 1) write (*,*) 'Writing dummy MLE'
               write (iuvam, '(10f10.3)', err=900) !#           
     &              obs_data%data(i_u,iocc,igood) !# mle - dummy values
            else
               if (iocc .eq. 1) write (*,*) 'Writing actual MLE'
c              uvtemp = merge(spread(vmiss,1,ngood),
c    &                  obs_data%data(i_mle,iocc,igood),
c    &                  iocc .gt. obs_data%num_occ(igood))
            uvtemp = obs_data%data(i_mle,iocc,igood)
            do i=1,ngood
               if (iocc .gt. obs_data%num_occ(igood(i)))
     &               uvtemp(i) = vmiss
            enddo
               write (iuvam, '(10f10.3)', err=900) uvtemp !#mle,mle2,mle3,mle4
            endif
         enddo
         write (iuvam, '(10i10)', err=900) row	!# row 
         write (iuvam, '(10i10)', err=900) cell	!# column 
         write (iuvam,  '(10f10.3)', err=900) wgt	!#
         write (iuvam,  '(10i10)', err=900) (1,i=1,ngood)	!# selected - dummy values
         write (iuvam, '(10f10.1)', err=900) obs_data%time(igood)	!#
         write (iuvam, '(10f10.3)', err=900) obs_data%u_fgat(igood)	!#
         write (iuvam, '(10f10.3)', err=900) obs_data%v_fgat(igood)	!#
         write (iuvam, '(10f10.3)', err=900) obs_data%alpha(igood)	!#

         do iocc=1,4
            write (iuvam, '(10i10)', err=900)
     &          obs_data%qc_bit_map(iocc,igood)
         enddo

      endif

      ierr = 0
      write (*,*) 'Wrote out ',ngood,' obs '
  900 if (ierr .ne. 1) close (iuvam)	!#
      if (allocated(igood)) deallocate(igood,wgt,uvtemp,row,cell)	!#
      return
      end subroutine wrt_anal_nscat

      subroutine wrt_anal_s0(iuvam, out_file, obs_data, 
     &     interp, qcflag, vprint, ierr)

c!#   Routine for writing out interpolated analysis or obs values at
c!#   scatterometer winds locations, in ASCII format compatible with 
c!#   wna_wrback.F

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      implicit none

      integer, intent(in) :: iuvam
      integer, intent(out) :: ierr
      character (len=*) :: out_file
      logical, intent(in) :: interp, qcflag, vprint

      type(obs_data_typ) :: obs_data

      integer, parameter :: nrevs=15
c!#   Use dummy values for:
      integer :: date=961118, time=120000, revs(nrevs), nbyrevs(nrevs),	!#
     &     ngood, i, k, iocc
      integer, dimension(:), allocatable :: igood, row, cell
      real, dimension(:), allocatable :: wgt, uvtemp
      real :: lognum=1, vmiss=-999.	!#

c!#   Names of variables:
      character (len=len_name), parameter ::
c!# Constants:
     &     satid_name='satid',	!#
     &     modfn_name='modfn',	!#
     &     sdcalc_name='sdcalc',	!#
     &     ndcalc_name='ndcalc',	!#
     &     kpm2_name='kpm2',	!this may instead be computed on the fly!#
c$$$     &     s0c_name='sigma0-c',	!#
c!# Variables:
     &     polar_name='hv-polar',	!#hor/vert polarization
     &     incid_name='incidence',	!#incidence angle
     &     azim_name='azimuth',		!#azimuth angle
     &     s0_name='sigma0',	!#
     &     kpa_name='kpa',	!#
     &     kpb_name='kpb',	!#
     &     kpc_name='kpc'	!#
      integer ::
     &     i_modfn, i_sdcalc, i_ndcalc, i_kpm2,
     &     i_polar, i_incid, i_azim, i_s0, i_kpa, i_kpb, i_kpc, i_s0c

c-----Check data structure - find needed constants and variables:
      i_modfn = where_in(modfn_name, obs_data%names_const,
     &     len_name, len_name, obs_data%n_const)
      i_sdcalc = where_in(sdcalc_name, obs_data%names_const,
     &     len_name, len_name, obs_data%n_const)
      i_ndcalc = where_in(ndcalc_name, obs_data%names_const,
     &     len_name, len_name, obs_data%n_const)
c$$$      i_s0c = where_in(s0c_name, obs_data%names_const,
c$$$     &     len_name, len_name, obs_data%n_const)
      i_kpm2 = where_in(kpm2_name, obs_data%names_const,
     &     len_name, len_name, obs_data%n_const)

      i_polar = where_in(polar_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_incid = where_in(incid_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_azim = where_in(azim_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_s0 = where_in(s0_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_kpa = where_in(kpa_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_kpb = where_in(kpb_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_kpc = where_in(kpc_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)

c$$$      if (i_modfn .le. 0 .or. i_polar .le. 0 .or.
      if (.not. interp) then
         if (obs_data%n_occ .ne. 1) stop
     &        'wrt_anal_s0:  Only supporting n_occ=1'
         if (i_polar .le. 0 .or.
     &        i_incid .le. 0 .or. i_azim .le. 0 .or.
     &        i_s0 .le. 0 .or. i_kpa .le. 0
     &        .or. i_kpb .le. 0 .or. i_kpc .le. 0) then
            write (*,*)
     &           'wrt_anal_s0: Cannot find needed constants/variables.'
            write (*,*) ' Matching codes (required)= ',
     &           i_polar, i_incid, i_azim, i_s0, i_kpa, i_kpb, i_kpc
            write (*,*) ' Matching codes (optional)= ',
     &           i_modfn, i_sdcalc, i_ndcalc, i_s0c
            stop 'wrt_anal_s0:  Cannot find needed constants/variables'
         endif
      endif

      ierr = 1
      if (vprint) write (*,*) 'Writing nscat backsc file to ',
     &     trim(out_file)
      open (unit=iuvam, file=out_file, err=900)	!#

      ierr = 2
      write (iuvam, '(i10)', err=900) date, time	!#dummy values
      write (iuvam, '(f10.2)', err=900) lognum		!#dummy values
      revs(:) = 0
      revs(1) = 1
      write (iuvam, '(15i6)', err=900) revs		!#dummy values
      nbyrevs(:) = 0
      if (qcflag) then	!# Skip obs without any valid data
         ngood = 0
         do i=1,obs_data%n_loc
            if (.not. ALL(obs_data%qc_flag(:,i))) ngood = ngood + 1
         enddo
      else
         ngood = obs_data%n_loc !#
      endif
      nbyrevs(1) = ngood				!#
      write (iuvam, '(15i6)', err=900) nbyrevs		!#
      write (iuvam, '(15i6)', err=900) ngood		!#

      ierr=3
      allocate (igood(ngood),
     &     row(ngood), cell(ngood))
      k=0
      do i=1,obs_data%n_loc
         if (.not. (qcflag .and.  ALL(obs_data%qc_flag(:,i)))) then
            k=k+1
            igood(k)=i
         endif
      enddo
      
      if (k .ne. ngood) stop 'Internal logic error'

      ierr=4
      row = obs_data%record_id(igood) / 100
      cell = mod(obs_data%record_id(igood) , 100)
      if (interp) then	!# gridded field interpolated to obs:
         write (*,*) 'Writing u_int and v_int'
         write (iuvam, '(10f10.3)', err=900) obs_data%lat_deg(igood) !#
         write (iuvam, '(10f10.3)', err=900) obs_data%lon_deg(igood) !#
         write (iuvam, '(10f10.3)', err=900) obs_data%u_int(igood)	!#u
         write (iuvam, '(10f10.3)', err=900) obs_data%v_int(igood)	!#v
         write (iuvam, '(10i10)', err=900) row	!#row - dummy values
         write (iuvam, '(10i10)', err=900) cell	!#column - dummy values
      else	!# obs values
         write (*,*) 'Writing backscatter'
         write (iuvam, '(f40.10)', err=900) obs_data%lat_deg(igood) !#
         write (iuvam, '(f40.10)', err=900) obs_data%lon_deg(igood) !#
         write (iuvam, '(10i5)') nint(obs_data%data(i_polar,1,igood))
         write (iuvam, '(10i5)') igood	!dummy values for antenna
         write (iuvam,'(f40.10)') obs_data%data(i_incid,1,igood)
         write (iuvam,'(f40.10)') obs_data%data(i_azim,1,igood)
         write (iuvam,'(f40.10)') obs_data%data(i_s0,1,igood)
         write (iuvam,'(f40.10)') obs_data%data(i_s0,1,igood)	!dummy for s05
         write (iuvam,'(f40.10)') obs_data%data(i_s0,1,igood)   !dummy for s0sd
         write (iuvam,'(f40.10)') obs_data%data(i_kpa,1,igood)
         write (iuvam,'(f40.10)') obs_data%data(i_kpb,1,igood)
         write (iuvam,'(f40.10)') obs_data%data(i_kpc,1,igood)
         write (iuvam, '(15i6)', err=900) row	!# row 
         write (iuvam, '(15i6)', err=900) cell	!# column 
      endif

      ierr = 0
      write (*,*) 'Wrote out ',ngood,' obs '
  900 if (ierr .ne. 1) close (iuvam)	!#
      if (allocated(igood)) deallocate(igood,row,cell)	!#
      return
      end subroutine wrt_anal_s0

      subroutine wrt_anal_ssmi(iuvam, out_file, obs_data, 
     &     interp, qcflag, vprint, ierr)

c!#   Routine for writing out interpolated analysis or obs values at
c!#   SSMI winds locations, in ASCII format compatible with 
c!#   Splus codes in read.vam.datasets.  In particular, the output
c!#   format was taken from vam2d.fortran/wsl_wrlos.F.  While
c!#   developed particularly for SSMI Line-of-Sight (LoS) winds,
c!#   the data structure contains all of the elements needed for
c!#   vanilla SSMI wind speeds.

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      implicit none

      integer, intent(in) :: iuvam
      integer, intent(out) :: ierr
      character (len=*) :: out_file
      logical, intent(in) :: interp, qcflag, vprint

      type(obs_data_typ) :: obs_data

c!#   Use dummy values for:
      integer :: ngood, i, k
      integer, dimension(:), allocatable :: igood, idum
      real, dimension(:), allocatable :: lons, Vel, Vel_int, dum
      real :: lognum=1, vmiss=-999.	!#

c!#   Names of variables:
      character (len=len_name), parameter :: Vel_name='speed'
      integer :: i_Vel

c-----Check data structure - find needed variables:
      write (*,*) 'Vel_name= ',Vel_name
      write (*,*) 'n_var= ',obs_data%n_var
      write (*,*) 'names_var= ',obs_data%names_var
      write (*,*) 'len_name= ',len_name
      i_Vel = where_in(Vel_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)

      ierr = 1
      if (vprint) write (*,*) 'Writing ssmi winds file to ',
     &     trim(out_file)
      open (unit=iuvam, file=out_file, err=900)	!#

      ierr = 2
      write (iuvam, '(i10)', err=900) obs_data%idate,
     & obs_data%itime
      write (iuvam, '(f10.2)', err=900) lognum		!#dummy values
      if (qcflag) then	!# Skip obs without any valid data
         ngood = 0
         do i=1,obs_data%n_loc
            if (.not. ALL(obs_data%qc_flag(:,i))) ngood = ngood + 1
         enddo
      else
         ngood = obs_data%n_loc !#
      endif
      write (iuvam, '(15i6)', err=900) ngood		!#

      ierr=3
      allocate (igood(ngood), Vel(ngood), Vel_int(ngood), dum(ngood),
     &          idum(ngood), lons(ngood) )
      dum(:) = vmiss
      idum(:) = nint(vmiss)

      k=0
      do i=1,obs_data%n_loc
         if (.not. (qcflag .and. ALL(obs_data%qc_flag(:,i)))) then
            k=k+1
            igood(k)=i
            Vel_int(k)=sqrt(obs_data%u_int(i)**2 +
     &                      obs_data%v_int(i)**2)
         endif
      enddo

      
      if (k .ne. ngood) stop 'Internal logic error'

c!#   convert vam longitudes (0,360) to Splus longitudes (-180,180)
      lons(:)  = obs_data%lon_deg(igood)
      do i=1,ngood
        if (lons(i) .gt. 180 ) lons(i)=lons(i)-360
      enddo              

      write (iuvam, '(10f10.3)', err=900) obs_data%lat_deg(igood)	!#
      write (iuvam, '(10f10.3)', err=900) lons	!#

      ierr=4
      if (interp) then	!# gridded field interpolated to obs:
         write (*,*) 'Writing analysis/background winds interplolated',
     &               ' to microwave instrument obs locations'
         write (iuvam, '(10f10.3)', err=900) Vel_int	!#Vel
      else	!# obs values
         if (i_Vel .le. 0 ) then
            write (*,*) 'wrt_anal_ssmi: Cannot find wind Velocity',
     &           ' Matching codes= ',i_Vel
            stop 'wrt_anal_ssmi:  Cannot find wind Velocity'
         endif
         write (*,*) 'Writing wind speeds from microwave instrument'

c!# Set missing data values to -999.
         do i=1,obs_data%n_loc
           if (obs_data%data(i_Vel,1,i) .gt. 1000. )
     &       obs_data%data(i_Vel,1,i) = -999.
         enddo              

         write (iuvam,'(10f10.3)',err=900) obs_data%data(i_Vel,1,igood)

         write (iuvam, '(10f10.3)', err=900) dum	!#losm - dummy values
         write (iuvam, '(10f10.3)', err=900) dum	!#ipx - dummy values
         write (iuvam, '(10f10.3)', err=900) dum   !#ipy
         write (iuvam, '(10i10)', err=900) idum    !#isrow
         write (iuvam, '(10i10)', err=900) idum    !#iscol
         write (iuvam, '(10f10.3)', err=900) dum   !#isrev
         write (iuvam, '(10f10.3)', err=900) dum   !#isst
         write (iuvam, '(10f10.3)', err=900) dum   !#itice
         write (iuvam, '(10f10.3)', err=900) dum   !#iwv
         write (iuvam, '(10f10.3)', err=900) dum   !#icw
         write (iuvam, '(10f10.3)', err=900) dum   !#irain
         write (iuvam, '(10f10.3)', err=900) dum   !#ivelm5
         write (iuvam, '(10f10.3)', err=900) dum   !#ilosm5
         write (iuvam, '(10f10.1)', err=900) obs_data%time(igood)	!#
         write (iuvam, '(10f10.3)', err=900) obs_data%u_fgat(igood)	!#
         write (iuvam, '(10f10.3)', err=900) obs_data%v_fgat(igood)	!#
         write (iuvam, '(10f10.3)', err=900) obs_data%alpha(igood)	!#
         write (iuvam, '(10i10)', err=900) obs_data%qc_bit_map(1,igood)

      endif

      ierr = 0
      write (*,*) 'Wrote out ',ngood,' obs '
  900 if (ierr .ne. 1) close (iuvam)	!#
      if (allocated(igood)) deallocate(igood,Vel,Vel_int,dum,idum,lons)
      return
      end subroutine wrt_anal_ssmi

      subroutine wrt_anal_conventional(iuvam, out_file, obs_data, 
     &     interp, qcflag, vprint, ierr)

c!#   Routine for writing out interpolated analysis or obs values at
c!#   conventional winds locations, in ASCII format compatible with 
c!#   Splus codes in read.vam.datasets.  In particular, the output
c!#   format was taken from vam2d.fortran/wca_wrwind.F.

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      implicit none

      integer, intent(in) :: iuvam
      integer, intent(out) :: ierr
      character (len=*) :: out_file
      logical, intent(in) :: interp, qcflag, vprint

      type(obs_data_typ) :: obs_data

c!#   Use dummy values for:
      integer :: ngood, i, k
      integer, dimension(:), allocatable :: igood, idum
      real, dimension(:), allocatable :: lons
      real :: lognum=1, vmiss=-999.	!#

c!#   Names of variables:
      character (len=len_name), parameter :: u_name='u_wind',
     &     v_name='v_wind'
      integer :: i_u, i_v


c-----Check data structure - find needed variables:
      write (*,*) 'u_name= ',u_name
      write (*,*) 'v_name= ',v_name
      write (*,*) 'n_var= ',obs_data%n_var
      write (*,*) 'names_var= ',obs_data%names_var
      write (*,*) 'len_name= ',len_name
      i_u = where_in(u_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)
      i_v = where_in(v_name, obs_data%names_var,
     &     len_name, len_name, obs_data%n_var)

      ierr = 1
      if (vprint) write (*,*) 'Writing conventional winds data to ',
     &     trim(out_file)
      open (unit=iuvam, file=out_file, err=900)	!#

      ierr = 2
      write (iuvam, '(i10)', err=900) obs_data%idate,
     & obs_data%itime
      if (qcflag) then	!# Skip obs without any valid data
         ngood = 0
         do i=1,obs_data%n_loc
            if (.not. ALL(obs_data%qc_flag(:,i))) ngood = ngood + 1
         enddo
      else
         ngood = obs_data%n_loc !#
      endif

      allocate (igood(ngood), lons(ngood))
      k=0
      do i=1,obs_data%n_loc
         if (.not. (qcflag .and.  ALL(obs_data%qc_flag(:,i)))) then
            k=k+1
            igood(k)=i
         endif
      enddo

      if (k .ne. ngood) stop 'Internal logic error'

      write (iuvam, '(15i6)', err=900) ngood		!#

c!#   convert vam longitudes (0,360) to Splus longitudes (-180,180)
      lons(:)  = obs_data%lon_deg(igood)
      do i=1,ngood
        if (lons(i) .gt. 180 ) lons(i)=lons(i)-360
      enddo              

      write (iuvam, '(10f10.3)', err=900) obs_data%lat_deg(igood)	!#
      write (iuvam, '(10f10.3)', err=900) lons	!#

      ierr=4
      if (interp) then	!# gridded field interpolated to obs:
         write (*,*) 'Writing u_int and v_int for conventional'
         write (iuvam, '(10f10.3)', err=900) obs_data%u_int(igood)	!#u
         write (iuvam, '(10f10.3)', err=900) obs_data%v_int(igood)	!#v
      else	!# obs values
         if (i_u .le. 0 .or. i_v .le. 0) then
            write (*,*) 'wrt_anal_nscat: Cannot find u and/or v',
     &           ' Matching codes= ',i_u, i_v
c$$$cdeb: reset to defaults as a temporary solution:
c$$$            i_u=1 ; i_v=2
c$$$            write (*,*) 'WORKAROUND FOR COMPILER BUG: ',
c$$$     &           'use hardwired values= ',i_u, i_v
            stop 'wrt_anal_nscat:  Cannot find u and/or v'
         endif
         write (*,*) 'Writing conventional winds'
         write (iuvam, '(10f10.3)', err=900)
     &     obs_data%data(i_u,1,igood)
         write (iuvam, '(10f10.3)', err=900)
     &     obs_data%data(i_v,1,igood)
      endif

      write (iuvam, '(10f10.1)', err=900) obs_data%time(igood)	!#
      write (iuvam, '(10f10.3)', err=900) obs_data%u_fgat(igood)	!#
      write (iuvam, '(10f10.3)', err=900) obs_data%v_fgat(igood)	!#
      write (iuvam, '(10f10.3)', err=900) obs_data%alpha(igood)	!#

      write (iuvam, '(10i10)', err=900) obs_data%qc_bit_map(1,igood)

      ierr = 0
      write (*,*) 'Wrote out ',ngood,' conventional obs '
  900 if (ierr .ne. 1) close (iuvam)	!#
      if (allocated(igood)) deallocate(igood)
      return
      end subroutine wrt_anal_conventional

