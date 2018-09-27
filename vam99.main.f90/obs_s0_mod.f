c!# CSU IDENTIFICATION : obs_s0_mod
c!#     $Id: obs_s0_mod.f,v 1.12 2006/09/11 20:38:33 rnh Exp $

c!# PURPOSE : Constants and code specific to sigma0 scatterometer obs

      MODULE obs_s0_mod   !#

      USE types, ONLY: accumulator, obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      IMPLICIT NONE

      PRIVATE

      CHARACTER (LEN=len_name), PRIVATE, PARAMETER ::
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

      LOGICAL, PRIVATE :: do_qc=.FALSE. !# flag for whether call to ss_s0 is for QC
      REAL, PRIVATE :: gamma_mod !# module-scope copy of gamma from qc_s0
      INTEGER, PRIVATE :: ibs0_mod !# module-scope copy of ibs0 from qc_s0

      PUBLIC ss_s0, qc_s0

      CONTAINS

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: obs_s0_mod.f,v $
c!#	Revision 1.12  2006/09/11 20:38:33  rnh
c!#	Replaced .T. and .F. with .TRUE. and .FALSE.
c!#	
c!#	Revision 1.11  2005/03/01 19:11:33  rnh
c!#	Style changes
c!#	
c!#	Revision 1.10  2004/09/24 18:52:25  leidner
c!#	Bug fixes, so recent implementation of qc_bit_map now compiles.  Testing remains.
c!#	
c!#	Revision 1.9  2004/09/08 20:42:54  rnh
c!#	Clarified handling of qc_bit_map.
c!#	
c!#	Revision 1.8  2004/09/07 19:59:29  rnh
c!#	adding qc features
c!#	
c!#	Revision 1.7  2004/09/02 15:40:05  rnh
c!#	Added wind qc; ability to skip non-existing files
c!#	
c!#	Revision 1.6  2004/03/19 21:36:01  rnh
c!#	FGAT: first version, fgat_add good only for one data set
c!#	
c!#	Revision 1.5  2001/02/23 16:46:15  rnh
c!#	The following error fixed by changing the order of the subroutines
c!#	within the module.
c!#	
c!#	f90 -O3	 -c obs_s0_mod.f
c!#	Unexpected object class (5) in FWlinear_type
c!#	Assertion failed: 0, file ../srcfw/FWcvrt.c, line 3370
c!#	f90: Fatal error in f90comp: Abort
c!#	
c!#	Revision 1.4  2001/01/03 20:39:36  trn
c!#	Corrected printout of sum of weights
c!#	
c!#	Revision 1.3  2000/01/28 19:42:48  trn
c!#	Initialize jscat, for the case of no unflagged obs
c!#	
c!#	Revision 1.2  2000/01/24 15:44:10  trn
c!#	Added QC
c!#
c!#	Revision 1.1  2000/01/20 16:35:17  trn
c!#	Initial revision
c!#

c!# ------------------------------------------------------------------------
      SUBROUTINE qc_s0(data, gamma, ibs0, vprint)

      USE s0_init_mod, ONLY: ladcalc, jdebug

c     qc_s0 performs a gross-error check of sigma0 obs against trajectory values

      REAL, INTENT(IN) :: gamma
      LOGICAL, INTENT(IN) :: vprint 
      INTEGER, INTENT(IN) :: ibs0
      TYPE (obs_data_typ), INTENT(INOUT) :: data
     

      REAL (accumulator) :: ss_dummy
      REAL :: slamda_dummy=1., norm_dummy=0.
      INTEGER :: ierr, jdebug_save
      LOGICAL :: ladcalc_save

c!#   Temporarily reset module_scope and s0_init_mod variables for ss_s0:
      ladcalc_save = ladcalc
      ladcalc = .FALSE.
      do_qc = .TRUE.
      jdebug_save = jdebug
      if (vprint) jdebug = data%n_loc/10
      gamma_mod = gamma
      ibs0_mod = ibs0
      ss_dummy = 0

      call ss_s0(ss_dummy, slamda_dummy, norm_dummy, data)

c!#   Reset module_scope and s0_init_mod variables to saved values:
      ladcalc = ladcalc_save
      do_qc = .FALSE.
      jdebug = jdebug_save

      return
      end subroutine qc_s0

c!# ------------------------------------------------------------------------
      SUBROUTINE ss_s0(ss, slamda, norm, data)

      USE types, ONLY: s0_table_typ
      use s0_init_mod, only: s0_tables

      USE grid_mod, ONLY: nlon,nlat,u5,v5,u,v
      USE interp_mod, ONLY: uvinterp,uvinterpad

      USE jscenv_m, ONLY: jscenv

c     ss_s0 calculates the sums of squares and gradient due to
c     sigma0 scatterometer data

      REAL(accumulator), INTENT(INOUT) :: ss
      REAL, INTENT(IN) :: slamda
      REAL, INTENT(INOUT) :: norm
      TYPE (obs_data_typ), INTENT(INOUT) :: data

c     Local variables
      INTEGER i, ierr, n,
     &     i_modfn, i_sdcalc, i_ndcalc, i_kpm2,
     &     i_polar, i_incid, i_azim, i_s0, i_kpa, i_kpb, i_kpc, i_s0c

      integer :: isdcalc, indcalc, ikpm2

      REAL ui5, vi5, ui, vi, uobs, vobs
      
      type(s0_table_typ), pointer :: current

      logical :: found

c!#    Arrays needed for jscenv call
c!#~   Pol       Polarization (0=Hpol, 1=Vpol)
c!#~   Theta     Beam incidence angle (rad)
c!#~   Azm       Radar pointing direction (rad)
c!#~   Azm.      From direction, clockwise from north
c!#~   S0obs     Sigma0 observed (normalized)
c!#~   S0KpA     Sigma0 KpA term
c!#~   S0KpB     Sigma0 KpB term
c!#~   S0KpC     Sigma0 KpC term
c!#~   lDataOK   Good data flag
c!#~   ui5, vi5  u- and v-wind components
c!#~   ui5, vi5. Interpolated to NSCAT data locations
c!#~   U5        Neutral wind speed (m/s)
c!#~   U5.       At reference level
c!#~   D5        Wind direction (rad)
c!#~   D5.       From direction, clockwise from north
c!#~   S05       Sigma0 (radar backscatter) (normalized)
c!#~   S0sd5     Sigma0 standard deviation
c!#~   E5        Normalized departures (1)

      real :: s0kpm2
      integer, dimension(:), allocatable :: pol
      real, dimension(:), allocatable :: theta, azm, s0obs,
     &     s0kpa, s0kpb, s0kpc,
     &     ui5_a, vi5_a, umag5, dir5, s05, s0sd5, e5
      logical, dimension(:), allocatable :: ldataok

c!#   Adjoint variables corresponding to the above:
      real, dimension(:), allocatable :: ui_a, vi_a,
     &     umag, dir, s0, s0sd, e

c!#   jscenv output variables
      integer :: nused
      real(accumulator) :: jscat, jdepart, jvar

      real :: j1                !used for QC

c-----Check data structure - find needed constants and variables:
      i_modfn = where_in(modfn_name, data%names_const,
     &     len_name, len_name, data%n_const)
      i_sdcalc = where_in(sdcalc_name, data%names_const,
     &     len_name, len_name, data%n_const)
      i_ndcalc = where_in(ndcalc_name, data%names_const,
     &     len_name, len_name, data%n_const)
c$$$      i_s0c = where_in(s0c_name, data%names_const,
c$$$     &     len_name, len_name, data%n_const)
      i_kpm2 = where_in(kpm2_name, data%names_const,
     &     len_name, len_name, data%n_const)

      i_polar = where_in(polar_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_incid = where_in(incid_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_azim = where_in(azim_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_s0 = where_in(s0_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_kpa = where_in(kpa_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_kpb = where_in(kpb_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_kpc = where_in(kpc_name, data%names_var,
     &     len_name, len_name, data%n_var)

      if (i_modfn .le. 0 .or. i_polar .le. 0 .or.
     &     i_incid .le. 0 .or. i_azim .le. 0 .or.
     &     i_s0 .le. 0 .or. i_kpa .le. 0) then
         write (*,*) 'ss_s0: Cannot find needed constants/variables.'
         write (*,*) ' Matching codes (required)= ',
     &     i_modfn, i_polar, i_incid, i_azim, i_s0, i_kpa
         write (*,*) ' Matching codes (optional)= ',
     &     i_sdcalc, i_ndcalc, i_kpb, i_kpc, i_s0c
         stop 'ss_s0:  Cannot find needed constants/variables'
      endif

c!#   Set local variables from obs data set values
      isdcalc = 2	!default: compute sd from trajectory values	!#
      if (i_sdcalc .gt. 0) isdcalc = nint(data%const(i_sdcalc))
      indcalc = 1	!#default: compute in linear space	!#
      if (i_ndcalc .gt. 0) indcalc = nint(data%const(i_ndcalc))
      s0kpm2 = 0
      ikpm2 = 0
      if (i_kpm2 .gt. 0) then
         ikpm2 = 1
         s0kpm2 = data%const(i_kpm2)
      endif

c!#   sigma0 lookup table: find one with matching model function
      current => s0_tables
      found = .FALSE.
      do while (associated(current) .and. .not. found)
         do i=1,current%nmodfns
            found = found .or.
     &           data%const(i_modfn) .eq. current%modfns(i)
         enddo
         if (.not. found) current => current%next_tbl
      enddo
      if (.not. found) then
         write (*,*) 'Cannot find s0_table for modfn=',
     &        data%const(i_modfn)
         stop 'ss_s0: Cannot find s0_table'
      endif

c!#   Set ikpm2=2 if current s0_table also contains values for s0kpm2 

c!#   Allocate local arrays, preset default values
      ierr = 0
      if (allocated(pol)) deallocate(pol, theta, azm, s0obs,
     &     s0kpa, s0kpb, s0kpc,
     &     ui5_a, vi5_a, umag5, dir5, s05, s0sd5, e5,
     &     ldataok, ui_a, vi_a,
     &     umag, dir, s0, s0sd, e, stat=ierr)
      if (ierr .ne. 0) stop 'ss_s0: deallocate pol ...'
      allocate(pol(data%n_occ), theta(data%n_occ), azm(data%n_occ),
     &     s0obs(data%n_occ),
     &     s0kpa(data%n_occ), s0kpb(data%n_occ), s0kpc(data%n_occ),
     &     ui5_a(data%n_occ), vi5_a(data%n_occ), umag5(data%n_occ),
     &     dir5(data%n_occ), s05(data%n_occ), s0sd5(data%n_occ),
     &     e5(data%n_occ),
     &     ldataok(data%n_occ), ui_a(data%n_occ), vi_a(data%n_occ),
     &     umag(data%n_occ), dir(data%n_occ), s0(data%n_occ),
     &     s0sd(data%n_occ), e(data%n_occ), stat=ierr)
      if (ierr .ne. 0) stop 'ss_s0: allocate pol ...'

      s0kpb(:) = 0
      s0kpc(:) = 0

c!#   Initialize accumulators (note: jscat is also initialized inside jscenv)
      nused = 0
      jdepart = 0
      jvar = 0
      jscat = jdepart + jvar

c-----Loop through points
      DO n=1,data%n_loc
        IF (data%num_occ(n) .gt. 0 .and.
     &        .not. all(data%qc_flag(:,n)) ) THEN
c
          CALL uvinterp
     &        ( nlon, nlat,
     &        data%gridi(n), data%gridj(n),
     &        u5, v5,
     &        ui5, vi5 )
c
c-----FGAT correction
          ui5 = data%alpha(n)*ui5 + data%u_fgat(n)      
          vi5 = data%alpha(n)*vi5 + data%v_fgat(n)      
c
c Fill arrays for jscenv
          ui5_a(:) = ui5
          vi5_a(:) = vi5

          pol(:) = nint(data%data(i_polar,:,n))
          theta(:) = data%data(i_incid,:,n)
          azm(:) = data%data(i_azim,:,n)
          s0obs(:) = data%data(i_s0,:,n)
          s0kpa(:) = data%data(i_kpa,:,n)
          if (i_kpb .gt. 0) s0kpb(:) = data%data(i_kpb,:,n)
          if (i_kpc .gt. 0) s0kpc(:) = data%data(i_kpc,:,n)
          ldataok(:) = .not. data%qc_flag(:,n)

          ui_a(:) = 0
          vi_a(:) = 0
          umag(:) = 0
          dir(:) = 0
          s0(:) = 0
          s0sd(:) = 0
          e(:) = 0

c-----Increment sums and gradients
          call jscenv(
     C    data%num_occ(n), isdcalc, indcalc, ikpm2, current, s0kpm2,  !#
     I    Pol, Theta, Azm, !#
     I    S0obs, S0KpA, S0KpB, S0KpC, lDataOK, !#
     I    ui5_a, vi5_a, !#
     O    Umag5, Dir5, S05, S0sd5, E5, !#
     O    ui_a, vi_a, !#
     O    Umag, Dir, S0, S0sd, E, !#
     O    Nused, Jscat, Jdepart, Jvar ) !#

          if (do_qc) then
             do i=1,data%num_occ(n)
                if (ldataok(i)) then
                   j1 = 10 * log10(s05(i)) -
     &                  10 * log10(s0obs(i))
                   if (abs(j1) .gt. gamma_mod) then
                      data%qc_bit_map(i,n) =
     &                    ibset(data%qc_bit_map(i,n),ibs0_mod)
                   endif
                endif
             enddo
          else
             ui = slamda * sum(ui_a)
             vi = slamda * sum(vi_a)

c-----FGAT correction
             ui = data%alpha(n)*ui
             vi = data%alpha(n)*vi

             CALL uvinterpad
     &            ( nlon, nlat,
     &            data%gridi(n), data%gridj(n),
     &            u, v,
     &            ui, vi )

          END IF
        END IF
      END DO

      ss = ss + slamda * Jscat
c!#~   norm(ss_s0)	sum of weights = number of sigma0 obs used
      norm = norm + Nused	!#

      ierr = 0
      if (allocated(pol)) deallocate(pol, theta, azm, s0obs,
     &     s0kpa, s0kpb, s0kpc,
     &     ui5_a, vi5_a, umag5, dir5, s05, s0sd5, e5,
     &     ldataok, ui_a, vi_a,
     &     umag, dir, s0, s0sd, e, stat=ierr)
      if (ierr .ne. 0) stop 'ss_s0: deallocate pol ...'

      RETURN

      END SUBROUTINE ss_s0

      END MODULE obs_s0_mod

