
      MODULE types

      IMPLICIT NONE
      PRIVATE
      PUBLIC accumulator
      PUBLIC obs_data_typ, len_name, len_fname
      PUBLIC s0_table_typ, npol
c     -----------------------------------------------------------------
c!##  GLOBAL AND SHARED DATA :

c!#   Define type for accumulators
c!#~   accumulator    kind specification for sums of squares
      INTEGER, PARAMETER :: accumulator=KIND(0.0d0)
c     -----------------------------------------------------------------
c!#   Maximum string lengths:
      INTEGER, PARAMETER :: len_name=10, !#for names in obs_data_typ
     &     len_fname=255        !#for file names (throughout the VAM)

      TYPE :: obs_data_typ	!#
c!#   Derived type for observation data:

c!# Header scalars:
c!# The name of the obs_id must match one of those supported in vam_obs
         CHARACTER (LEN=len_name) :: c_obs_id	!#ID of obs type
         CHARACTER (LEN=len_fname) :: fname !#filename ID
         INTEGER :: idate, itime !# synoptic date, time
         INTEGER :: n_loc	!#Number of obs locations
         INTEGER :: n_occ	!#Max Number of observation occurences per location
         INTEGER :: n_var	!#Number of observed variables per occurence
         INTEGER :: n_const	!#Number of constants for this dataset
c!# Header vectors:
c!# The names of the variables must match one of those supported in vam_obs
         CHARACTER (LEN=len_name), DIMENSION(:), POINTER ::
     &        names_var	!# names of variables (n_var)
c!# The names of the constants must match those supported for the specific type
         CHARACTER (LEN=len_name), DIMENSION(:), POINTER ::
     &        names_const	!# names of constants (n_const)
         REAL, DIMENSION(:), POINTER :: const	!# values of constants (n_const)
c!# Data Vectors (n_loc):

c     Note: time(:) is actively used by FGAT, and should be set by an
c     obs preprocessor to epoch time in seconds
c     Nominally this is time relative to the idate-itime.

         INTEGER, DIMENSION(:), POINTER :: num_occ  !# 0 <= num_occ <= n_occ
         INTEGER, DIMENSION(:), POINTER :: record_id  !# link to input dataset
         REAL, DIMENSION(:), POINTER :: time, lat_deg, lon_deg	 !#
         REAL, DIMENSION(:), POINTER :: gridi, gridj,
     &     u_int, v_int, u_fgat, v_fgat, alpha
c!# QC arrays: (n_occ, n_loc)
         LOGICAL, DIMENSION(:,:), POINTER :: qc_flag	 !#
         INTEGER, DIMENSION(:,:), POINTER :: qc_bit_map	 !#
c#! Array of (n_var, n_occ, n_loc) containing obs data:
         REAL, DIMENSION(:,:,:), POINTER ::	data !#
         TYPE (obs_data_typ), POINTER :: next_set_of_obs	!#for linked list
      END TYPE obs_data_typ	!#
c     -----------------------------------------------------------------

c!#~   NPol       Number of polarizations
      INTEGER, PARAMETER :: npol=2 ! 0=hor polar; 1=vert polar !#

      TYPE :: s0_table_typ	!#
c!#   Derived type for sigma0 look up table:

c!# scalars:
         CHARACTER (LEN=len_fname) :: fname !#filename ID

c!#~   lCubic     Use cubic interpolation if true, linear otherwise
         LOGICAL :: lCubic

c!#~   ZS0Tbl     Reference heights for S0 table (m)
c!#~   Theta0     First incidence angle (rad)
c!#~   DTheta     Increment in incidence angle (rad)
c!#~   NTheta     Number of incidence angles
c!#~   U0         First wind speed (m/s)
c!#~   DU         Increment in wind speed (m/s)
c!#~   NU         Number of wind speeds
c!#~   Phi0       First relative azimuth angle (rad)
c!#~   DPhi       Increment in relative azimuth angle (rad)
c!#~   NPhi       Number of relative azimuth angles
c!#~   nmodfns	  Number of model functions IDs for this table 
         integer :: nmodfns, ntheta, nu, nphi
         real :: zs0tbl, theta0, dtheta, u0, du, phi0, dphi

c!# Header vectors:
c!#~   modfns	  model functions IDs for this table
         INTEGER, dimension(:), pointer :: modfns !#

c!# Table data:
c!#   TBD: also include table for KpM2 values
c!#~   S0Tbl(ntheta,nu,nphi,npol)      Sigma0 table (normalized)
         REAL, DIMENSION(:,:,:,:), POINTER :: s0tbl
c!#~   S00Tbl(ntheta,npol)      Sigma0 table for zero wind (normalized)
         REAL, DIMENSION(:,:), POINTER :: s00tbl
         TYPE (s0_table_typ), POINTER :: next_tbl !#for linked list
      END TYPE s0_table_typ	!#
c     -----------------------------------------------------------------

      END MODULE types
