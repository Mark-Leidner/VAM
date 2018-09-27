c!# CSU IDENTIFICATION : s0_init_mod
c!#     $Id: s0_init_mod.f,v 1.5 2006/09/11 20:38:33 rnh Exp $

c!# PURPOSE : Hold and initialize constant data for jscenv

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!# REQUIREMENTS : 
c!# Hold and initialize constant data for jscenv

c!# CONSTRAINTS : 

c!# LANGUAGE : Fortran

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :

      MODULE s0_init_mod   !#
c!# GLOBAL AND SHARED DATA : 
      USE types, only : s0_table_typ, len_fname, npol   !#

      implicit none

      private

c!#~  s0_tables		Linked list of sigma0 lookup tables
      type (s0_table_typ), public, pointer :: s0_tables

      public s0_init

c!#   2. Values to guard against division by zero.
c!#   If a calculation results in a smaller value it is reset.
c!#~   Vmin      Minimum wind speed (m/s)
c!#~   S0min     Minimum sigma0 (normalized)
c!#~   S0sdmin   Minimum sigma0 standard deviation

      real, public, parameter :: Vmin = 1e-8,	!#
     &     S0min=1e-10,		! -100 dB	!#
     &     S0sdmin=1e-12 	! -120 dB       !#

c!#~  s00min	 Minimum table value of sigma0 at zero wind.
      real, private, parameter :: S00min = 1d-6 ! -60 dB	!#

c!#   Control variables:

c!#~   lCalc     Calculate forward model
c!#~   lAdCalc   Calculate adjoint
c!#~   lVDCalc   Calculate speed and direction U and D
c!#~   lS0Calc   Calculate backscatter S0
      logical, public :: lcalc=.TRUE., ladcalc=.TRUE.,
     &     lvdcalc=.TRUE., ls0calc=.TRUE.

c!#~   iJoCalc   Method to calculate loss functional
c!#~   iJoCalc.  (1=>Sum of squares,2=>MLE,>0=>Calculate)
c!#~   jDebug    Debug output increment (0=>none)
c!#~   jNiter    Iteration number
c!#~   jTask     Task number (batch index)
      integer, public :: iJocalc=2, jdebug=0, jniter=1, jtask=1

c      INTERFACE s0_init  !only needed for overloading
c         MODULE PROCEDURE s0_init        !
c      END INTERFACE   !
      CONTAINS        !#

c     ------------------------------------------------------------------

      SUBROUTINE s0_init (iuvam)       !#

      use s0_tblio_mod, only: read_s0table_head, read_s0table_data,
     &     s0table_fname

      integer, intent(in) :: iuvam

c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: s0_init_mod.f,v $
c!#	Revision 1.5  2006/09/11 20:38:33  rnh
c!#	Replaced .T. and .F. with .TRUE. and .FALSE.
c!#	
c!#	Revision 1.4  2004/09/28 14:52:06  leidner
c!#	cleaned up last bugs before tagging pre-fgat04
c!#	
c!#	Revision 1.3  2004/09/24 19:43:58  rnh
c!#	sorting out namelist defaults
c!#	
c!#	Revision 1.2  2000/01/24 15:44:10  trn
c!#	Cosmetic changes
c!#	
c!#	Revision 1.1  2000/01/20 16:35:17  trn
c!#	Initial revision
c!#

c!# LOCAL DATA ELEMENTS : 
      character (len=len_fname) :: filename, indir
      integer, parameter :: max_modfns=20
      integer :: modfns(max_modfns), interp_order
      logical :: more, vprint
      namelist /s0init/ filename, indir, modfns, interp_order,
     &     iJocalc, jdebug, more, vprint
      
      character (len=len_fname) :: filename_hdr, filename_data
      integer :: i, ierr, num_files, number_sets,
     &    n_modfns, nmodfns, tmp_modfns(max_modfns)
      LOGICAL :: read_namelist

c!# LOCAL DATA STRUCTURES : 
      type(s0_table_typ), pointer :: last, current

c!# DATA FILES : None

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c!#   Initialize last, previous, current for loop over linked list
      nullify(last)
      nullify(current)
      number_sets = 0
c!#...Add to linked list if not empty (initialize below otherwise)
      if (associated(s0_tables)) then
         current => s0_tables
         do while (associated(current)) 
            number_sets = number_sets + 1
            last => current	!#last points to end of list
            current => current%next_tbl
         enddo
         nullify(current)	!#current not associated initially
      endif
      print *,'Starting with ',number_sets,
     &     ' s0_tables'

c!#   Keep processing until namelist more=.FALSE.
      read_namelist = .TRUE.
      more_loop: DO WHILE (read_namelist)

c!#   Reset namelist variables to their defaults, then read namelist:
         indir =' '
         filename =' '
         interp_order = -1	!#use file defaults
         modfns(:) = -1		!#use file defaults
         more = .FALSE.
         vprint = .FALSE.
         read (*,s0init,iostat=ierr)
         read_namelist = more
         if (vprint) write (*,s0init)
         if (ierr .ne. 0) then
            if (.not. vprint) write (*,s0init)
            write (*,*) 'Error ', ierr,' reading namelist ',
     &           '/s0init/ from file standard input.'
            stop 'Error reading namelist s0init'
         endif
c!# strip filename of .txt or .dat extensions:
         call s0table_fname(filename,len_fname,'.txt',filename_hdr)
         call s0table_fname(filename,len_fname,'.dat',filename_data)
         call s0table_fname(filename_hdr,len_fname,'',filename)

c!#     1. Allocate and fill new s0_table from file:
         if (filename .eq. ' ')
     &        stop 's0_init: Need to specify a filename for reading'
c!#   ...Allocate entry to be filled:
         allocate(current, stat=ierr)
         if (ierr .ne. 0)
     &        stop 's0_init: allocate failure'
         if (vprint) write (*,*) 'Reading s0_table from ',
     &        trim(filename)
c!#   ...Fill up data structure from file:
         call read_s0table_head(iuvam, filename_hdr,
     &        current%lcubic, nmodfns, current%ntheta, current%nu,
     &        current%nphi, current%zs0tbl, current%theta0,
     &        current%dtheta, current%u0, current%du, current%phi0,
     &        current%dphi,
     &        max_modfns, tmp_modfns, ierr, vprint)
         if (ierr .ne. 0) then
            if (vprint) write (*,*) 'read_s0table_head ierr= ',ierr
            stop 's0_init: error reading s0_table_head'
         endif
c!#   ...override file values with namelist values
         if (interp_order .ne. -1) current%lcubic = interp_order .eq. 3
         n_modfns = count(modfns .ne. -1)
         if (n_modfns .gt. 0) then
            nmodfns = 0
            do i=1, max_modfns
               if (modfns(i) .ne. -1) then
                  nmodfns = nmodfns + 1
                  tmp_modfns(nmodfns) = modfns(i)
               endif
            enddo
         endif
c!#   Allocate and fill modfns array:
         ierr = 0
         if (associated(current%modfns))
     &        deallocate(current%modfns,stat=ierr)
         if (ierr .ne. 0) stop 's0_init: deallocate modfns'
         allocate(current%modfns(nmodfns),stat=ierr)
         if (ierr .ne. 0) stop 's0_init: allocate modfns'
         current%modfns(:) = tmp_modfns(1:nmodfns)
         current%nmodfns = nmodfns

c!#      Allocate table arrays
         ierr = 0
         if (associated(current%s0tbl))
     &        deallocate(current%s0tbl,current%s00tbl,stat=ierr)
         if (ierr .ne. 0) stop 's0_init: deallocate s0tbl...'
         allocate(current%s0tbl(current%ntheta,current%nu,current%nphi,
     &        npol),
     &        current%s00tbl(current%ntheta,npol),stat=ierr)
         if (ierr .ne. 0) stop 's0_init: allocate s0tbl'

c!#     2. Read table data from file
         call read_s0table_data(iuvam, filename_data,
     &        current%ntheta, current%nu,
     &        current%nphi, npol,
     &        current%s0tbl, ierr)
         if (ierr .ne. 0) then
            if (vprint) write (*,*) 'read_s0table_data ierr= ',ierr
            stop 's0_init: error reading s0_table_data'
         endif
         num_files = num_files + 1

c!#     3. Compute zero-wind table values
         call jsccs00(
     C        current%ntheta, current%nu, current%nphi,
     C        current%ntheta, current%u0, current%du, s00min,
     I        current%s0tbl,
     O        current%s00tbl)

c!#      ...Update counters and pointers:
         if (.not. associated(s0_tables)) then !#
c!#      ...Initialize the output pointer:
            if (vprint) print *,
     &           'Initializing s0_tables for file= ',
     &           trim(filename)
            s0_tables => current
         else                   !#
            if (vprint) print *,
     &           'Adding to end s0_tables for file= ',
     &           trim(filename)
c!#         ...Link this set to end of linked list
            last%next_tbl => current
         endif                  !# associated(s0_tables)
c!#      ...Update element at end of list:
         last => current
         nullify(current)

      enddo more_loop	!#endwhile more

c!# ERROR HANDLING : return nonzero error codes
      return
      end subroutine s0_init

c     ------------------------------------------------------------------

      subroutine JscCS00   !#
     C    (MTheta, MU, NPhi, NTheta, U0, DU, S00min, !#
     I    S0Tbl, !#
     O    S00Tbl) !#

c!##  PURPOSE : JscCS00 calculates values of sigma0 for zero wind.

c!#   JscCS00 calculates values of sigma0 for zero wind at each
c!#   incidence angle in the sigma look up table.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!#   The sigma0 look up table must be real*4.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

c!#   Constants:
c!#~   MTheta     Maximum Theta points
c!#~   MU         Maximum U points
c!#~   NPhi       Number of relative azimuth angles
c!#~   NTheta     Number of incidence angles
c!#~   U0         First wind speed (m/s)
c!#~   DU         Increment in wind speed (m/s)
c!#~   S00min     Sigma0 minimum value (normalized)
      integer MTheta, MU, NPhi, NTheta !#
      real U0, DU, S00min !#

c!#   Input:
c!#~   S0Tbl      Sigma0 table (normalized)
      real*4 S0Tbl( MTheta, MU, NPhi ) !#

c!#   Ouput:
c!#~   S00Tbl     Sigma0 table for zero wind (normalized)
c!#~   S00Tbl.    as function of Theta
      real S00Tbl(MTheta) !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   For each incidence angle, the value of sigma0 at zero wind is
c!#   calculated by fitting a parabola symmetric about zero to the data
c!#   at U0, U0+DU for each value of Phi and averaging over Phi.  A
c!#   minimum value of S00min is enforced.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: s0_init_mod.f,v $
c!#   Revision 1.5  2006/09/11 20:38:33  rnh
c!#   Replaced .T. and .F. with .TRUE. and .FALSE.
c!#
c!#   Revision 1.4  2004/09/28 14:52:06  leidner
c!#   cleaned up last bugs before tagging pre-fgat04
c!#
c!#   Revision 1.3  2004/09/24 19:43:58  rnh
c!#   sorting out namelist defaults
c!#
c!#   Revision 1.2  2000/01/24 15:44:10  trn
c!#   Cosmetic changes
c!#
c!#   Revision 1.1  2000/01/20 16:35:17  trn
c!#   Initial revision
c!#
c!#   Revision 1.3  1998/01/13 19:32:20  rnh
c!#   Major diet complete.
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

      real c0, c1, c2, S0u0, S0u1
c     FTNCHEK: c1 is set but not used
      integer kTheta, kPhi

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. For each incidence angle:

c!#   1.1 Initialize sum.

      do 200 kTheta = 1, NTheta
        S00Tbl(kTheta) = 0

c!#   1.2 For each relative azimuth angle:

c!#   1.2.1 Extract table values.

        do 150 kPhi = 1, NPhi
          S0u0 = S0Tbl(kTheta, 1, kPhi)
          S0u1 = S0Tbl(kTheta, 2, kPhi)

c!#   1.2.2 Fit symmetric parabola (y'(0) = 0) to first two data points.
c     Parabola is y(x) = c0 + c1 x + c2 x^2.
          c1 = 0
          c2 = (S0u1 - S0u0)/((2*U0 + DU)*DU)
          c0 = S0u0 - c2*U0**2

c!#   1.2.3 Accumulate values with 1:2:2....:2:1 weighting.

          if (kPhi .ne. 1 .and. kPhi .ne. NPhi) then
            S00Tbl(kTheta) = S00Tbl(kTheta) + 2*c0
          else
            S00Tbl(kTheta) = S00Tbl(kTheta) + c0
          endif

  150   continue

c!#   1.3 Compute average.

        S00Tbl(kTheta) = S00Tbl(kTheta)/(2*NPhi - 2)

c!#   1.4 Enforce minimum value.

        if (S00Tbl(kTheta) .le. S00min) S00Tbl(kTheta) = S00min

  200 continue

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end subroutine JscCS00

      end module s0_init_mod
