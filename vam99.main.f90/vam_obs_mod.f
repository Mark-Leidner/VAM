
      MODULE vam_obs_mod

      USE types, ONLY: obs_data_typ, len_name, len_fname
      USE string_mod, ONLY: where_in, lcomp !# case insensitive comparisons

      USE obs_conv_mod, ONLY: ss_conv !#methods for conv data
      USE obs_amb_mod, ONLY: ss_amb, qc_amb !#methods for ambiguous winds data
      USE obs_s0_mod, ONLY: ss_s0, qc_s0 !#methods for sigma0 data
      USE obs_ssmi_mod, ONLY: ss_ssmi !#methods for ssmi data
c!# ***************************************************************************
c!#   NOTE: TMI and AMSR use the same methods as SSMI presently.
c!# ***************************************************************************

      IMPLICIT NONE
      PRIVATE
c!#   This module provides generic methods for observational data
      PUBLIC vam_obs	!# reading in obs data, QC obs, write obs
c!#                        (controlled by namelist)
      PUBLIC ss_obs     !# compute the obs error sum of squares
      PUBLIC regrid_obs	!# compute grid coordinates and interpolated gridded data
      PUBLIC count_obs	!# counts number of observations used
      PUBLIC date_time_check !# checks status of date-time matchups

c!#   Number and names of supported observation types
c!#   this must agree with the id stored in the input files
      INTEGER, PARAMETER :: n_obsids=7
      CHARACTER (len_name) :: names_obsid(n_obsids) =
     &     (/'convention','ambiguous ','sigma0    ',
     &       'ssmi      ','tmi       ','amsr      ',
     &       'scatconv  '/)
c!#   Storage locations of names in array:
      INTEGER, PARAMETER :: conv_id=1,  amb_id=2, s0_id=3,
     &     ssmi_id=4, tmi_id=5, amsr_id=6, scatconv_id=7
c!# ***************************************************************************
c!#   NOTE: These must agree with the iobs array in solve_mod for proper
c!#         indexing of the slamdas and norm in ss_obs
c!# ***************************************************************************

c     QC bit definitions

      INTEGER, PARAMETER :: max_bit_flag = 13, ib_generic=0,
     &    ib_prepro=1, ib_land=2, ib_near_land=3, ib_rain=4,
     &    ib_near_rain=5, ib_ice=6, ib_near_ice=7, ib_ambig=8,
     &    ib_vector=9, ib_angle=10, ib_speed=11, ib_s0=12,
     &    ib_bounds=13

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE vam_obs(iuvam, obs_data)

      INTEGER, INTENT(IN) :: iuvam	!# VAM input unit number
      TYPE(obs_data_typ), POINTER :: obs_data	!#in/out: obs data structure

c!# LOCAL DATA ELEMENTS :
c!# vam_obs namelist
      CHARACTER (LEN=len_name) :: obsid	!#
      CHARACTER (LEN=len_fname) :: filename, indir, outdir !#
      LOGICAL :: read_obs, qc_obs, write_obs, more, vprint !#
      NAMELIST /vamobs/ obsid, filename, indir, outdir,	!#
     &     read_obs, qc_obs, write_obs, more, vprint !#

c!#~ obsid     observation type matching value
c!#~ filename  file name matching value
c!#~ indir     directory for reading
c!#~ outdir    directory for writting
c!#~ read_obs  read a data set?
c!#~ qc_obs    qc matching or newly read data
c!#~ qc_obs.   (a qc namelist will be read immediately)
c!#~ write_obs write matching or newly read data
c!#~ more      read another namelist after processing this one?
c!#~ vprint    print verification information?
c!#~
      CHARACTER (LEN=len_fname) :: testname
      INTEGER :: ierr
      INTEGER :: num_files, num_obs_total, num_replaced, num_added,
     &     num_qc, num_write
      LOGICAL :: read_namelist, file_exists

c!#   LOCAL DATA STRUCTURES :
      TYPE (obs_data_typ), POINTER :: current, input, previous

c     initialize counters
c     (defined implicitly by print statements below)
      num_files=0
      num_obs_total=0
      num_replaced=0
      num_added=0
      num_qc=0
      num_write=0

c!#   Keep processing until namelist more=.FALSE.
      read_namelist = .TRUE.
      more_loop: DO WHILE (read_namelist)

c!#   Reset namelist variables to their defaults, then read namelist:
         read_obs = .FALSE.
         qc_obs = .FALSE.
         write_obs = .FALSE.
         more = .FALSE.
         vprint = .FALSE.
         filename =' '
         obsid =' '
         indir =' '
         outdir =' '
         READ (*,vamobs,IOSTAT=ierr)
         read_namelist = more
         IF (vprint) WRITE (*,vamobs)
         IF (ierr .NE. 0) THEN
            IF (.NOT. vprint) WRITE (*,vamobs)
            WRITE (*,*) 'Error ', ierr,' reading namelist ',
     &        '/vamobs/ from file standard input.'
            STOP 'Error reading namelist vamobs'
         ENDIF

c     Initialize qc: read qc namelists
         IF (qc_obs) call qc_obs_data(obs_data,.TRUE.,obsid,vprint)

c!#   Reading a data set is a special case
         IF (read_obs) THEN

c     A file name must be specified (else ABORT)
            IF (filename .EQ. ' ') STOP
     &        'vamobs: Need to specify a filename for reading'

c     The file must exist (else CYCLE)
c     Determine header file name
c!# strip filename of .txt or .dat extensions
            CALL obs_fname('',filename,len_fname,'',filename)
            CALL obs_fname(indir,filename,len_fname,'.txt',testname)
            INQUIRE (FILE=TRIM(testname),IOSTAT=ierr,
     &          EXIST=file_exists)
            IF (ierr .NE. 0) THEN
               WRITE (*,*) 'Error ', ierr,' during inquire of file',
     &           TRIM(testname)
               STOP 'Error during inquire in vam_obs'
            ENDIF
            IF (.NOT. file_exists) CYCLE more_loop

c     Allocate input structure and fill from input file
c!#   ...Allocate entry to be filled:
            IF (ASSOCIATED(input)) NULLIFY(input)
            ALLOCATE(input, STAT=ierr)
            IF (ierr .NE. 0) STOP 'vam_obs: allocate failure'
c!#   ...Fill up data structure from file:
            CALL fill_obs_data(iuvam, indir, filename,
     &        input, vprint, ierr)
            IF (ierr .NE. 0) STOP 'vam_obs: read error'
            num_files = num_files + 1
            num_obs_total = num_obs_total + input%n_loc

c     Set current pointer
c     In case of read we will only process the new data in input.
c     Otherwise we will traverse the entire obs_data structure.
            current => input
         ELSE
            current => obs_data
         ENDIF

c     MAIN LOOP:
c     Search for matches in current, perform QC, and write as requested.
         IF (qc_obs .OR. write_obs) THEN
c     Traverse current
         DO WHILE (ASSOCIATED(current))
c!#   Process all data sets
            IF ((obsid .EQ. ' ' .AND. filename .EQ. ' ') .OR.
c!#   or match all datasets with specified obsid:
     &        (lcomp(obsid, current%c_obs_id, len_trim(obsid)) .AND.
     &        filename .EQ. ' ') .OR.
c!#   or match all dataset with specified filename:
     &         (obsid .EQ. ' ' .AND.
     &         lcomp(filename, current%fname, len_trim(filename))) .OR.
c!#   or match all datasets with specified obsid and filename:
     &        (lcomp(obsid, current%c_obs_id, len_trim(obsid)) .AND.
     &         lcomp(filename, current%fname, len_trim(filename)))) THEN

               IF (qc_obs) THEN
                  CALL qc_obs_data(current, .FALSE., obsid, vprint)
                  num_qc = num_qc + 1
               END IF

               IF (write_obs) THEN
                  CALL write_obs_data(iuvam, current, outdir, vprint)
                  num_write = num_write + 1
               END IF
            END IF
            current => current%next_set_of_obs
         END DO
         END IF

c     Newly read data must be spliced into obs_data
         IF (read_obs) THEN
c     Case of first data set
            IF (.NOT. ASSOCIATED(obs_data)) THEN
               obs_data => input
               num_added = num_added + 1
            ELSE
               current => obs_data
               IF (ASSOCIATED(previous)) NULLIFY(previous)
c     Traverse the list looking for a match to replace
               DO WHILE (ASSOCIATED(current))
                  IF (lcomp(filename, current%fname,
     &              len_trim(filename))) THEN
                     input%next_set_of_obs => current%next_set_of_obs
                     IF (ASSOCIATED(previous)) THEN	!#connect to previous
                        previous%next_set_of_obs => input
                     ELSE	!#replacing first one in list
                        obs_data => input
                     END IF
                     CALL destruct_obs_data(current)
                     DEALLOCATE(current)
                     num_replaced = num_replaced + 1
                     CYCLE more_loop
                  END IF
                  previous => current
                  current => current%next_set_of_obs
               END DO
c      If none found, add new entry to end
               previous%next_set_of_obs => input
               num_added = num_added + 1
            END IF
         END IF
      END DO more_loop

c!#...nullify local pointers (but do not deallocate/destruct)
      IF (ASSOCIATED(current)) NULLIFY(current)
      IF (ASSOCIATED(input)) NULLIFY(input)
      IF (ASSOCIATED(previous)) NULLIFY(previous)

      PRINT *,'Read data from ',num_files,' files'
      IF (num_files .GT. 0) then
         PRINT *,' for a total of ', num_obs_total,
     &        ' observation locations.'
         PRINT *,'Replaced ',num_replaced,' added ',num_added,
     &        ' obs data sets'
      END IF
      PRINT *,'Did qc on ',num_qc,' obs data sets'
      PRINT *,'Wrote out ',num_write,' obs data sets'

      RETURN
      END SUBROUTINE vam_obs

c     -----------------------------------------------------------------

      subroutine fill_obs_data(inunit, indir, infile,
     &    obs_data, vprint, ierr)

      integer, intent(in) :: inunit
      character*(*), intent(in) :: indir, infile
      type (obs_data_typ), pointer :: obs_data
      integer, intent(out) :: ierr
      logical, intent(in) :: vprint

c     !# LOCAL DATA ELEMENTS :
      character(len=len_fname) :: io_fname
      character(len=len_name) :: in_obsid, temp_cname(1), temp_vname(1)
      real :: temp_const(1)
      integer :: i, in_date, in_time, num_const, num_loc, num_occ,
     &    num_var, recl
c***  WARNING num_occ matches n_occ NOT num_occ in obs_data!

      ierr = 0

c     !#   1. Open file, read scalars:

      call obs_fname(indir, infile, len_fname, '.txt', io_fname)
      if (vprint) print *,'Reading header from file ',
     &    trim(io_fname)
      call rdhobs(inunit, io_fname, 1, 1, in_obsid,
     &    in_date, in_time, num_const, num_loc, num_occ, num_var,
     &    temp_cname, temp_const, temp_vname, ierr)
      if (ierr .ne. 0) stop 'fill_obs_data: rdhobs failure'

      if (vprint) print *, 'in_obsid, in_date, in_time, num_const, ',
     &    'num_loc, num_occ, num_var= ', in_obsid, ' ',
     &    in_date, in_time, num_const, num_loc, num_occ, num_var

c     !# Initialize obs_data structure:
      obs_data%fname = infile
      obs_data%c_obs_id = in_obsid
      obs_data%n_const = num_const
      obs_data%n_loc = num_loc
      obs_data%n_occ = num_occ
      obs_data%n_var = num_var
      nullify(obs_data%next_set_of_obs)
      if (num_const .gt. 0) then
        allocate (obs_data%names_const(num_const),
     &      obs_data%const(num_const),stat=ierr)
        if (ierr .ne. 0) stop 'fill_obs_data: allocate failure'
      endif
      allocate (obs_data%names_var(num_var),
     &    obs_data%num_occ(num_loc), obs_data%record_id(num_loc),
     &    obs_data%time(num_loc), obs_data%lat_deg(num_loc),
     &    obs_data%lon_deg(num_loc), obs_data%gridi(num_loc),
     &    obs_data%gridj(num_loc), obs_data%u_int(num_loc),
     &    obs_data%v_int(num_loc), obs_data%u_fgat(num_loc),
     &    obs_data%v_fgat(num_loc), obs_data%alpha(num_loc),
     &    obs_data%qc_flag(num_occ,num_loc),
     &    obs_data%qc_bit_map(num_occ,num_loc),
     &    obs_data%data(num_var,num_occ,num_loc), stat=ierr)
      if (ierr .ne. 0) stop 'fill_obs_data: allocate failure'
      call rdhobs(inunit, io_fname, obs_data%n_const, obs_data%n_var,
     &    in_obsid, obs_data%idate, obs_data%itime,
     &    num_const, num_loc, num_occ, num_var,
     &    obs_data%names_const, obs_data%const,
     &    obs_data%names_var, ierr)
      if (ierr .ne. 0) stop 'fill_obs_data: rdhobs failure'

c     !#   3. Read in data
      call obs_fname(indir, infile, len_fname, '.dat', io_fname)
      inquire(iolength=recl) obs_data%lat_deg
      if (vprint) print *,'Reading data from file ',
     &    trim(io_fname)
      call rddobs(inunit, io_fname, num_loc, num_occ, num_var, recl,
     &    obs_data%num_occ, obs_data%record_id,
     &    obs_data%time, obs_data%lat_deg,
     &    obs_data%lon_deg, obs_data%u_int, obs_data%v_int,
     &    obs_data%u_fgat, obs_data%v_fgat, obs_data%alpha,
     &    obs_data%qc_flag, obs_data%qc_bit_map,
     &    obs_data%data, ierr)

      if (ierr .ne. 0) stop 'fill_obs_data: rddobs failure'

c     !#...Regrid obs just read in:
      CALL regrid_obs(obs_data)

c     !# Exit from routine
      RETURN
      END SUBROUTINE fill_obs_data

c     -----------------------------------------------------------------
      subroutine destruct_obs_data(obs_data)

c     !#   Undo the allocations done in fill_obs_data

      type (obs_data_typ) :: obs_data

c     !# LOCAL DATA ELEMENTS :
      integer :: ierr

      if (associated(obs_data%names_const)) then
        deallocate (obs_data%names_const,
     &      obs_data%const,stat=ierr)
        if (ierr .ne. 0)
     &      stop 'vam_obs_mod: destruct_obs_data deallocate failure'
      endif
      if (associated(obs_data%names_var)) deallocate (
     &    obs_data%names_var,
     &    obs_data%num_occ, obs_data%record_id,
     &    obs_data%time, obs_data%lat_deg,
     &    obs_data%lon_deg, obs_data%gridi,
     &    obs_data%gridj, obs_data%u_int,
     &    obs_data%v_int, obs_data%u_fgat,
     &    obs_data%v_fgat, obs_data%alpha,
     &    obs_data%qc_flag, obs_data%qc_bit_map,
     &    obs_data%data, stat=ierr)
      if (ierr .ne. 0)
     &    stop 'vam_obs_mod: destruct_obs_data deallocate failure'
      return

      end subroutine destruct_obs_data

c     -----------------------------------------------------------------

      SUBROUTINE ss_obs(ss, slamda, norm, obs_data)

      USE types, ONLY: accumulator

      REAL(accumulator), INTENT(INOUT) :: ss
      REAL, INTENT(IN) :: slamda(:)
      REAL, INTENT(INOUT) :: norm(:)
      TYPE (obs_data_typ), POINTER :: obs_data

c!# LOCAL DATA ELEMENTS :
      type (obs_data_typ), pointer :: current
      integer :: obsid

c!#...Initialize local pointer:
      current => obs_data

c!# ERROR HANDLING : hard abort
      if (.not. associated(current)) stop 'ss_obs: no obs_data'

      do while (associated(current))
         obsid = where_in (current%c_obs_id, names_obsid,
     &        len_name, len_name, n_obsids)
c!#      branch here depending on obs_id:
         if (obsid .eq. conv_id) then
            if (slamda(conv_id) .ne. 0)
     &           call ss_conv(ss, slamda(conv_id), norm(conv_id),
     &           current)
         elseif (obsid .eq. amb_id) then
            if (slamda(amb_id) .ne. 0)
     &           call ss_amb(ss, slamda(amb_id), norm(amb_id),
     &           current)
         elseif (obsid .eq. s0_id) then
            if (slamda(s0_id) .ne. 0)
     &           call ss_s0(ss, slamda(s0_id), norm(s0_id),
     &           current)
         elseif (obsid .eq. ssmi_id) then
            if (slamda(ssmi_id) .ne. 0)
     &           call ss_ssmi(ss, slamda(ssmi_id), norm(ssmi_id),
     &           current)
          elseif (obsid .eq. tmi_id) then
            if (slamda(tmi_id) .ne. 0)
     &           call ss_ssmi(ss, slamda(tmi_id), norm(tmi_id),
     &           current)
          elseif (obsid .eq. amsr_id) then
            if (slamda(amsr_id) .ne. 0)
     &           call ss_ssmi(ss, slamda(amsr_id), norm(amsr_id),
     &           current)
          elseif (obsid .eq. scatconv_id) then
            if (slamda(scatconv_id) .ne. 0)
     &           call ss_conv(ss, slamda(scatconv_id),
     &           norm(scatconv_id), current)
         else
            print *,'Unsupported or ambigous obs id=',current%c_obs_id,
     &           ' Matching code=',obsid,' Valid names: ',names_obsid
            stop 'Unsupported or ambiguous obs id'
         endif

         current => current%next_set_of_obs

      enddo                     !endwhile
      return
      end subroutine ss_obs

c     -----------------------------------------------------------------

      SUBROUTINE regrid_obs(obs_data)

      USE grid_mod, ONLY: xs=>lon0, delx=>dlon, ys=>lat0, dely=>dlat,
     &    nlon, nlat, u5, v5
      USE interp_mod, ONLY: xgrid,lonmin,lonmax,latmin,latmax,lonper,
     &    uvinterp
      USE constants, ONLY: pi

      type(obs_data_typ), pointer :: obs_data

c     !# LOCAL DATA ELEMENTS :
      type (obs_data_typ), pointer :: current
      real :: ui5, vi5
      integer :: i, nin, nset_total, nloc_total, nin_total
      logical :: lerrx, lerry

c     !#...Initialize local pointer:
      current => obs_data

c     !# ERROR HANDLING : return if no obs_data or gridded data
      if (.not. associated(current)) return
      if (nlon .eq. 0 .or. nlat .eq. 0) return

      nset_total = 0
      nloc_total = 0
      nin_total = 0
      write (*,'(1x/1x,a,t30,a12,2a10)') 'Regrid_obs Obs locations:',
     &    'Obs_id','All','active'
c     !#...Loop over all obs_data structures in linked list
c     !#...(starting with current one passed in):
      do while (associated(current))
c     nin counts the number of in bounds data
        nin = 0
c     Clear out of bounds bit
        current%qc_bit_map(:,:) =
     &      IBCLR(current%qc_bit_map(:,:),ib_bounds)
c     Recompute grid coordinates for all obs (even those that are flagged):
        do i= 1, current%n_loc
          current%gridi(i)=xgrid(current%lon_deg(i)*(pi/180),
     &        xs,delx,lonper,lonmin,lonmax,lerrx)
          current%gridj(i)=xgrid(current%lat_deg(i)*(pi/180),
     &        ys,dely,   0.0,latmin,latmax,lerry)
c     Flag obs outside grid boundaries (even those that are flagged):
          if (lerrx .or. lerry) then
            current%qc_flag(:,i) = .TRUE.
c     and set out of bounds bit
            current%qc_bit_map(:,i) =
     &          ibset(current%qc_bit_map(:,i),ib_bounds)
c     and define u_int = v_int = 0
            current%u_int(i)=0
            current%v_int(i)=0
          else
            nin = nin + 1
c     !#      ...Compute interpolated values, store in data structure
            CALL uvinterp
     &          ( nlon, nlat,
     &          current%gridi(i), current%gridj(i),
     &          u5, v5,
     &          ui5, vi5 )
c
c-----Store interpolated values
            current%u_int(i) = current%alpha(i)*ui5 +
     &          current%u_fgat(i)
            current%v_int(i) = current%alpha(i)*vi5 +
     &          current%v_fgat(i)

          endif
        enddo

        nset_total = nset_total + 1
        nloc_total = nloc_total + current%n_loc
        nin_total = nin_total + nin
        write (*,'(t30,a12,2i10)') current%c_obs_id,current%n_loc,
     &      nin
        current => current%next_set_of_obs

      enddo                     !endwhile
      write (*,'(1x,a,i8,a,t42,2i10)')
     &    'All sets processed (',nset_total,'):',
     &    nloc_total,nin_total
      return

      end subroutine regrid_obs

c     -----------------------------------------------------------------

      SUBROUTINE count_obs(obs_data,nlon,nlat,period,ncounts)

c     For each observation with some data that passed QC
c     increment the count for the grid location.

      TYPE(obs_data_typ), POINTER :: obs_data
      INTEGER :: nlon,nlat,period,ncounts(nlon,nlat)
c!#~   obs_data  linked list of observation data
c!#~   nlon       number of longitude grid points
c!#~   nlat       number of latitude grid points
c!#~   period     period in grid units in longitude direction
c!#~   ncounts    counts of data used

c     !# LOCAL DATA ELEMENTS :
      TYPE (obs_data_typ), POINTER :: current
      INTEGER :: n,i,j

c     initialize
      ncounts(:,:) = 0

c     traverse data structure
      current => obs_data
      do while (associated(current))
c     increment count at grid coordinates for all obs not flagged
        do n= 1, current%n_loc
          if (.NOT.ALL(current%qc_flag(1:current%num_occ(n),n))) THEN
            i = current%gridi(n)+0.5
            j = current%gridj(n)+0.5
            ncounts(i,j) = ncounts(i,j) + 1
          END IF
        END DO
        current => current%next_set_of_obs
      END DO

c     deal with wrap around case
      IF (nlon .GT. period) THEN
c     add eastern counts to western for overlap
        ncounts(1:nlon-period,:) =
     &      ncounts(1:nlon-period,:) + ncounts(1+period:nlon,:)
c     copy the sum (in western overlap region) to eastern overlap
        ncounts(1+period:nlon,:) = ncounts(1:nlon-period,:)
      END IF

      RETURN

      END SUBROUTINE count_obs

c     -----------------------------------------------------------------

      SUBROUTINE qc_obs_data(obs_data, read_namelist_in, obsid_default,
     &    vprint)

      USE grid_mod, ONLY: nlon, nlat

      TYPE (obs_data_typ), POINTER :: obs_data
      LOGICAL, INTENT(IN) :: read_namelist_in, vprint
      CHARACTER (LEN=len_name), INTENT(IN) :: obsid_default

c The logic to handle qc depends on a variable qc_bit_map  that stores
c the current qc decision details and qc history for each datum.
c The qc decision whether or not to use a datum is stored in a variable
c qc_flag.  qc_flag is set on if any of the bits set in qc_bit_map match
c a specified mask.  this allows some flexibility.

c Usually for ambiguous data a special mask is used so that the current
c analysis checks are ignored.  This is because the cost function
c essentially includes interactive robust qc.  However if closest is
c .TRUE. then only ambiguities close enough to the background pass qc.

c Another mask is used to reset the qc_flag prior to qc.  This resets
c qc_flag according to the preprocessing and out-of-bounds bits in
c qc_bit_map.  Note that if ignore_rain is .TRUE. any preprocessing rain
c flags will be ignored in setting qc_flag in this and every other case.
c Since qc_flag is always reset every pass of qc is independent of
c previous passes.  This means that if you do dualqc while reading an
c ambiguity data set, these qc flags will be lost if you do a gross qc
c check later.  In other words it is necessary to redo all vam qc tests
c that you are interested in every time a data set is qc'd.

c Wind analysis checking is based on three criteria:
c
c 1. relative vector difference (for all wind vector obs & analysis >
c vcalm).  The test is (delv2 .GT. gamma1**2*velavg**2) where delv2 =
c (ui5-uobs)**2 + (vi5-vobs)**2 and velavg = (veli5 + velobs)/2
c
c 2. relative speed difference (for all wind obs or analysis > vcalm)
c The test is (ABS(velobs - veli5) .GT. gamma2*velavg)
c
c 3. angle difference (for all wind vector obs & analysis > vcalm)
c This is based on dot product definition,
c (ui5*uobs + vi5*vobs .LT. cos_gamma3*velobs*veli5)
c Note that gamma3 is in degrees.
c
c sigma0 analysis checking is based on a fourth criterion:
c
c 4. dB difference (for sigma0).  The test is (ABS(10 * log10(s05(i)) -
c 10 * log10(s0obs(i))) .gt. gamma4)

c Using two vamobs namelists and associated qcobs namelists makes
c it possible to do a sort of generic qc followed by a qc of
c data from a single input file.  Since the second qc resets the
c qc_flag and redoes the qc decisions for this data set only, it is
c possible to retain all of a special data type.  If you do this
c you might want to specify ipass on the second qcobs namelist if you
c want to consider the sequence of generic qc followed by qc of a
c specific data set to be a single qc pass.

c Ambiguity editting includes:

c a. Using only first nambig ambiguities.  eg, if nambig=2
c ambiguities 3 and 4 (if they exist) will be flagged.  if
c nambig=1 only the most likely amibuity will be used or only the
c selected ambiguity will be used depending on how the
c preprocessor orders things.

c b.  Dual ambiguity quality control.  The first two ambiguities often
c unambiguously define the streamine.  For dualqc = .TRUE., the entire
c WVC will be flagged if ambiguity 2 is not within 45 degrees of
c opposition to ambiguity 1.  We have always used nambig=2 when
c dualqc=.TRUE.

c c. For closest = .TRUE. only ambiguities close enough to pass analysis
c checking will be used.  Could be used in combination with nambig=2.

c     !# LOCAL DATA ELEMENTS :
c     !# qc_obs namelist
      CHARACTER (LEN=len_name) :: obsid
      REAL, SAVE :: gamma(4)=(/1,1,60,9/), vcalm=1
      INTEGER, SAVE :: nambig=4, ipass=0
      LOGICAL :: dualqc, closest, ignore_rain, more
      NAMELIST /qcobs/ obsid, gamma, vcalm, nambig, ipass,
     &    dualqc, closest, ignore_rain, more

c     !#~ obsid    apply gamma parameters to this obsid
c     !#~ obsid.   (blank => overwrite all; none => do nothing with gamma)
c     !#~ gamma    critical values for tests
c     !#~ gammas   storage for all gammas
c     !#~ vcalm    wind speeds <= vcalm are considered calm
c     !#~ nambig   keep ambiguities for 1,...,nambig (ignore if < 0)
c     !#~ ipass    qc pass index for storing qc history /0/
c     !#~ dualqc   dual qc processing (implies nambig = 2)
c     !#~ closest  edit ambiguities that are not close
c     !#~ ignore_rain ignore pre-processing rain flag in qc decisions?
c     !#~ more     read another qc namelist?

      CHARACTER (LEN=len_name), PARAMETER :: obsid_none='none'
      REAL, SAVE :: gammas(4,n_obsids)
      INTEGER, SAVE :: imask=0, jmask, kmask
      INTEGER :: i, m, n, ierr, iobsid !# error code, obsid number
      INTEGER :: num, numqc(-1:max_bit_flag)
      LOGICAL :: read_namelist

c     Read the namelist
      IF (read_namelist_in) THEN

c     Set default gammas the first time through
        IF (imask .EQ. 0) THEN
          DO i = 1, n_obsids
            gammas(:,i) = gamma
          END DO
        END IF

c     Keep reading qcobs namelist more=.FALSE.
c     This allows setting gamma values for different obsid types
c     NB: only the last namelist values are used for the other variables
c     Default obsid is from vamobs namelist.
        obsid = obsid_default
        ipass = ipass + 1
        dualqc = .FALSE.
        closest = .FALSE.
        ignore_rain = .FALSE.
        more = .FALSE.
        read_namelist = .TRUE.
        DO WHILE (read_namelist)
          READ (*,qcobs,IOSTAT=ierr)
          read_namelist = more
          IF (vprint) WRITE (*,qcobs)
          IF (ierr .NE. 0) THEN
            IF (.NOT. vprint) WRITE (*,qcobs)
            WRITE (*,*) 'Error ', ierr,' reading namelist ',
     &          '/qcobs/ from file standard input.'
            STOP 'Error reading namelist qcobs'
          ENDIF

c     Store gammas
          IF (obsid .eq. ' ') THEN
            DO i = 1, n_obsids
              gammas(:,i) = gamma
            END DO
          ELSEIF (.NOT. lcomp(obsid,obsid_none,len_name)) THEN
            iobsid = where_in (obsid, names_obsid,
     &          len_name, len_name, n_obsids)
            IF (iobsid .LE. 0) STOP 'qc_obs_data: unsupported obsid'
            gammas(:,iobsid) = gamma
          END IF

          obsid = obsid_none

        END DO

c     Setup test masks for setting qc_flag
        IF (imask .EQ. 0) THEN
          DO i = 0, max_bit_flag
            imask = IBSET(imask,i)
          END DO
c     Should the rain flag be ignored?
          IF (ignore_rain) imask = IBCLR(imask, ib_rain)
c     For ambiguous data ignore wind qc bits (unless closest)
          jmask = imask
          jmask = IBCLR(jmask,ib_vector)
          jmask = IBCLR(jmask,ib_angle)
          jmask = IBCLR(jmask,ib_speed)
c     Mask for clearing bits to be tested
          kmask = 0
C$RNH$ will this work?  Diff than INOT.
          kmask = NOT(kmask)
          kmask = IBCLR(kmask,ib_vector)
          kmask = IBCLR(kmask,ib_angle)
          kmask = IBCLR(kmask,ib_speed)
          kmask = IBCLR(kmask,ib_s0)
          kmask = IBCLR(kmask,ib_ambig)
        END IF

c     If not reading namelists
      ELSE IF (.NOT. read_namelist_in) THEN

        iobsid = where_in (obs_data%c_obs_id, names_obsid,
     &      len_name, len_name, n_obsids)
        IF (iobsid .LE. 0) STOP 'qc_obs_data: unsupported obsid'

c     Reset all qc bits to be tested and then all qc flags.  Then during
c     a test we only need to set a bit.  Note that tests apply only to
c     data not flagged in general because some calculations are only
c     safe to do on unflagged data.  That is all calculations, as well
c     as qc calculations are skipped if qc_flag is set.  Note that
c     flagrain is used to construct imask and so control whether rain
c     contaminated points are included or not in these qc calculations.
        obs_data%qc_bit_map = IAND(obs_data%qc_bit_map(:,:), kmask)
        obs_data%qc_flag(:,:) =
     &       IAND(obs_data%qc_bit_map(:,:), imask) .NE. 0

c     Grid data required for QC, except for ambiguity editting
        IF (nlon .GT. 0 .AND. nlat .GT. 0) THEN
          IF (iobsid .NE. s0_id) THEN
            CALL qc_wind(obs_data,
     &          gammas(1,iobsid), gammas(2,iobsid), gammas(3,iobsid),
     &          ib_vector, ib_angle, ib_speed, vcalm, vprint)
          ELSE IF (iobsid .EQ. s0_id) THEN
            CALL qc_s0(obs_data, gammas(4, s0_id), ib_s0, vprint)
          END IF
        END IF

        IF (iobsid .EQ. amb_id)
     &      CALL qc_amb(obs_data, nambig, dualqc, ib_ambig, vprint)

c     To allow some sloppiness in the qc routines called above clean
c     bits that should not be set.  Then we can assume this is true for
c     setting flags, counting bits that are set, and storing the
c     history.
        WHERE (obs_data%qc_flag(:,:))
          obs_data%qc_bit_map(:,:) =
     &       IAND(obs_data%qc_bit_map(:,:), kmask)
        END WHERE
        DO n=1,obs_data%n_loc
          DO m = obs_data%num_occ(n) + 1, obs_data%n_occ
            obs_data%qc_bit_map(m,n) = 0
          END DO
        END DO

c     Set qc_flag
c     Special case for ambiguities (and not closest) uses jmask
c     to skip checking wind qc flags.
        IF (.NOT. closest .AND. iobsid .EQ. amb_id) THEN
          obs_data%qc_flag(:,:) =
     &       IAND(obs_data%qc_bit_map(:,:), jmask) .NE. 0
        ELSE
          obs_data%qc_flag(:,:) =
     &       IAND(obs_data%qc_bit_map(:,:), imask) .NE. 0
        ENDIF

c     Store history

        IF (ipass .GT. 0 .AND.
     &      ipass + max_bit_flag .LT. BIT_SIZE(i)) THEN
          WHERE (obs_data%qc_flag(:,:))
            obs_data%qc_bit_map(:,:) = IBSET(obs_data%qc_bit_map(:,:),
     &          ipass+max_bit_flag)
          END WHERE
        END IF

      END IF

c     Print summary results for this obs data set
c     NB: Formats allow for max_bit_flag = 15 or less.
      IF (vprint .AND. read_namelist_in) THEN
        WRITE (*, '(a3,a3,1x,a5,1x,a4,1x,a8,1x,a12,2a7,1x,a6,16i7)')
     &      '@@@','ALL','N_OCC','PASS','OBSID','FNAME',
     &      'TOTAL','QCed','bit#:0',(i, i=1,max_bit_flag)
        WRITE (*, '(a3,a3,47x,a,a)')
     &      '@@@','ALL','generic prepro   land ner.ln   rain ner.rn ',
     &      '   ice ner.ic  ambig vector  angle  speed     s0 bounds'
      ELSEIF (vprint .AND. .NOT. read_namelist_in) THEN
        num = SUM(obs_data%num_occ(:))
        numqc(-1) = COUNT(obs_data%qc_flag(:,:))
        DO i = 0, max_bit_flag
          numqc(i) = COUNT(BTEST(obs_data%qc_bit_map(:,:),i))
        END DO
        WRITE (*, '(a3,a3,3x,i1,4x,i2,2x,a8,1x,a12,i7,17i7)')
     &      '@@+','ALL',obs_data%n_occ,ipass,obs_data%c_obs_id,
     &        obs_data%fname,num,numqc
        WRITE (*, '(a3,a3,3x,i1,4x,i2,2x,a8,1x,a12,i7,
     &               1x,17(f6.2,"%"))')
     &      '@@%','ALL',obs_data%n_occ,ipass,obs_data%c_obs_id,
     &      obs_data%fname,num,(100.0*numqc)/num
c     For ambiguous data repeat for each ambiguity.
        IF (iobsid .EQ. amb_id) THEN
          do m = 1, obs_data%n_occ
            num = COUNT(obs_data%num_occ(:) .GE. m)
            numqc(-1) = COUNT(obs_data%qc_flag(m,:))
            DO i = 0, max_bit_flag
              numqc(i) = COUNT(BTEST(obs_data%qc_bit_map(m,:),i))
            END DO
            WRITE (*, '(a3,a3,3x,i1,4x,i2,2x,a8,1x,a12,i7,17i7)')
     &          '@@+','AMB',m,ipass,obs_data%c_obs_id,obs_data%fname,
     &          num,numqc
            WRITE (*, '(a3,a3,3x,i1,4x,i2,2x,a8,1x,a12,i7,
     &                   1x,17(f6.2,"%"))')
     &          '@@%','AMB',m,ipass,obs_data%c_obs_id,obs_data%fname,
     &          num,(100.0*numqc)/num
          END DO
        END IF
      END IF

      RETURN
      END SUBROUTINE qc_obs_data

c     -----------------------------------------------------------------

      SUBROUTINE qc_wind(data, gamma1, gamma2, gamma3,
     &    ib_vector, ib_angle, ib_speed, vcalm, vprint)

      USE constants, ONLY : pi

      CHARACTER (LEN=len_name), PARAMETER :: Vel_name='speed'
      CHARACTER (LEN=len_name), PARAMETER :: u_name='u_wind',
     &    v_name='v_wind'

c     qc_wind performs generic qc on wind data

      INTEGER, INTENT(IN) :: ib_vector, ib_angle, ib_speed
      LOGICAL, INTENT(IN) :: vprint
      REAL, INTENT(IN) :: gamma1, gamma2, gamma3, vcalm
      TYPE (obs_data_typ), INTENT(INOUT) :: data

      INTEGER :: m, n, iu, iv, is, id=-777
      REAL :: cos_gamma3, ui5, vi5, uobs, vobs,
     &    delv2, veli5, velobs, velavg
      LOGICAL :: vector

cDBG  INTEGER, DIMENSION(3) :: nfail

c-----Check data structure - find needed variables:
      iu = where_in(u_name, data%names_var,
     &    len_name, len_name, data%n_var)
      iv = where_in(v_name, data%names_var,
     &    len_name, len_name, data%n_var)
      is = where_in(Vel_name, data%names_var,
     &    len_name, len_name, data%n_var)

      vector = (iu .GT. 0 .AND. iv .GT. 0) .OR.
     &    (is .GT. 0 .AND. id .GT. 0)

c-----Error condition?
      IF (.NOT. vector .AND. is .LE. 0) THEN
        WRITE (*,*) 'qc_wind: Cannot find wind data.',
     &      ' Matching codes= ',iu, iv, is, id
        STOP 'qc_wind:  Cannot find wind data'
      END IF

c-----Initialize
      cos_gamma3 = COS(gamma3*pi/180)

      DO n=1,data%n_loc
        ui5 = data%u_int(n)
        vi5 = data%v_int(n)
        veli5 = SQRT(ui5**2 + vi5**2)
c
c-----Calculate residuals for this location
        DO m = 1, data%num_occ(n)
          IF (.NOT. data%qc_flag(m,n)) THEN
c-----For vector winds
            IF (iu. GT. 0 .AND. iv .GT. 0) THEN
              uobs = data%data(iu,m,n)
              vobs = data%data(iv,m,n)
              delv2 = (ui5-uobs)**2 + (vi5-vobs)**2
              velobs = SQRT(uobs**2 + vobs**2)
c-----For wind speed and direction
c-----NOT IMPLEMENTED YET.  SHOULD DEFINE uobs, vobs, delv2, velobs
            ELSE IF (is .GT. 0 .AND. id .GT. 0)  THEN
              STOP 'qc_wind: unexplored territory!'
c-----For wind speeds
            ELSE IF (is .gt. 0) THEN
              velobs = data%data(is,m,n)
            END IF

            velavg = (veli5 + velobs)/2

c-----Speeds must exceed vcalm for vector and angle test
            IF (veli5 .GT. vcalm .AND. velobs .GT. vcalm
     &          .AND. vector) THEN

c-----Vector test (test 1)
              IF (delv2 .GT. gamma1**2*velavg**2) THEN
                data%qc_bit_map(m,n) =
     &              ibset(data%qc_bit_map(m,n),ib_vector)

cDBG  write(*,*) "vct tst fail: delv2 gamma1**2 velavg**2=",
cDBG &                       delv2, gamma1**2, velavg**2
cDBG  nfail(1) = nfail(1) + 1

              END IF


c-----Angle test (test 3) uses dot product definition
              IF (ui5*uobs + vi5*vobs
     &            .LT. cos_gamma3*velobs*veli5) THEN
                data%qc_bit_map(m,n) =
     &              ibset(data%qc_bit_map(m,n),ib_angle)

cDBG  WRITE(*,*) "ang tst fail: delv2 gamma1**2 velavg**2=",
cDBG &                       delv2, gamma1**2, velavg**2
cDBG  nfail(2) = nfail(2) + 1

              END IF

            END IF
c-----One speed must exceed vcalm for speed test
            IF (veli5 .GT. vcalm .OR. velobs .GT. vcalm) THEN

c-----Speed test (test 2)
              IF (ABS(velobs - veli5) .GT. gamma2*velavg) THEN
                data%qc_bit_map(m,n) =
     &              ibset(data%qc_bit_map(m,n),ib_speed)

cDBG  write(*,*) "spd tst fail: ui5 vi5 velobs veli5 gamma2 velavg=",
cDBG &                       ui5, vi5, velobs, veli5, gamma2, velavg
cDBG  nfail(3) = nfail(3) + 1

              END IF

            END IF
          END IF
        END DO
      END DO

cDBG  write(*,*) "VCT test failed", nfail(1), "ambiguous winds."
cDBG  write(*,*) "ANG test failed", nfail(2), "ambiguous winds."
cDBG  write(*,*) "SPD test failed", nfail(3), "ambiguous winds."

      RETURN

      END SUBROUTINE qc_wind

c     -----------------------------------------------------------------

      SUBROUTINE date_time_check(obs_data, status)

      USE grid_mod, ONLY: idate, itime, nlon, nlat

      TYPE(obs_data_typ), POINTER :: obs_data
      INTEGER, INTENT(OUT) :: status

c!# The final value of status indicates the number of obs data date-times
c!# that do not match the grid date-time.
c!# A value of 0 indicates all date-times match.
c!# A negative values indicate something is missing:
c!# -1 if the grid is undefined, -2 if there are not obs data, and
c!# -3 if both are missing.

c!# LOCAL DATA ELEMENTS :
      TYPE (obs_data_typ), POINTER :: current

      status = 0

      IF (nlon .EQ. 0 .OR. nlat .EQ. 0) status = status - 1

      IF (.NOT. ASSOCIATED(obs_data)) status = status - 2

      IF (status .LT. 0) RETURN

c!#...Initialize local pointer:
      current => obs_data

c!#...Loop over all obs_data structures in linked list
c!#...(starting with current one passed in):
      DO WHILE (ASSOCIATED(current))

         IF (idate .NE. current%idate .OR.
     &       itime .NE. current%itime) THEN
           status = status + 1
           write (*,*) 'ERROR: ', current%c_obs_id, ' obs data',
     &      ' has date ', current%idate, ', and time ', current%itime,
     &      '.  From file ', current%fname
         END IF

         current => current%next_set_of_obs

      END DO

      END SUBROUTINE date_time_check

c     -----------------------------------------------------------------

      SUBROUTINE write_obs_data(iuvam, obs_data, outdir, vprint)

c     #!#  Write out obs_data structure(s) to file(s).
c     #!#  Optionally write out interpolated analysis values instead of obs
c***  This last is not implemented:  To do so QC could replace obs
c***  variables with trajectory variables before writting out.

      integer, intent(in) :: iuvam
      TYPE (obs_data_typ), pointer :: obs_data
      character*(*), intent(in) :: outdir
      logical, intent(in) :: vprint

c     !# LOCAL DATA ELEMENTS :
      integer :: recl		!# record length for binary data file
      integer :: ierr		!# error code

      character (len=len_fname) :: hdr_fname, bin_fname
c     !# ERROR HANDLING : hard abort
      if (.not. associated(obs_data)) stop 'write_obs: no obs_data'

c     !#  Update interpolated values u_int, v_int
c$RNH$ no longer needed here: call regrid_obs(obs_data)
c     !#  write out obs values:
      call obs_fname(outdir, obs_data%fname, len_fname,
     &    '.txt', hdr_fname)
      call obs_fname(outdir, obs_data%fname, len_fname,
     &    '.dat', bin_fname)
      inquire(iolength=recl) obs_data%lat_deg
      if (vprint) write (*,*) 'Writing obs to files ',
     &    trim(hdr_fname),' and ',trim(bin_fname)
      call wrtobs(iuvam, hdr_fname, bin_fname,
     &    obs_data%c_obs_id, obs_data%idate,
     &    obs_data%itime, obs_data%n_const,
     &    obs_data%n_loc, obs_data%n_occ,
     &    obs_data%n_var, obs_data%names_const,
     &    obs_data%const, obs_data%names_var, recl,
     &    obs_data%num_occ, obs_data%record_id,
     &    obs_data%time, obs_data%lat_deg,
     &    obs_data%lon_deg, obs_data%u_int, obs_data%v_int,
     &    obs_data%u_fgat, obs_data%v_fgat, obs_data%alpha,
     &    obs_data%qc_flag, obs_data%qc_bit_map,
     &    obs_data%data, ierr)

      if (ierr .ne. 0) STOP 'write_obs_data: error writing obs'

      if (vprint) write (*,*) 'Wrote out ',obs_data%n_loc,
     &    ' obs locations'

      return
      end subroutine write_obs_data

c     -----------------------------------------------------------------

      SUBROUTINE obs_fname(in_dir, in_fname, maxpath,
     &    out_ext, out_fname)
      CHARACTER (LEN=*), INTENT(IN) :: in_dir, in_fname, out_ext
      INTEGER, INTENT(IN) :: maxpath
      CHARACTER (LEN=*), INTENT(OUT) :: out_fname

      INTEGER :: len_root

      len_root = MAX(INDEX(in_fname,'.txt',.TRUE.),
     &     INDEX(in_fname,'.dat',.TRUE.)) - 1
      IF (len_root .EQ. -1) len_root = LEN_TRIM(in_fname)

      IF (LEN_TRIM(in_dir)+len_root+LEN(out_ext) .GT. maxpath) THEN
         WRITE (*,*) 'obs_fname: maxpath too small: ', maxpath,
     &     TRIM(in_dir) // in_fname(1:len_root) // out_ext
         STOP 'obs_fname: maxpath too small'
      END IF

      out_fname = TRIM(in_dir) // in_fname(1:len_root) // out_ext

      RETURN
      END SUBROUTINE obs_fname
c     -----------------------------------------------------------------

      END MODULE vam_obs_mod
