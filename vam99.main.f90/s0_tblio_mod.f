
      MODULE s0_tblio_mod

      use types, only : len_fname, len_name
      use config_mod, only : read_config, write_config
      IMPLICIT NONE
      PRIVATE
      PUBLIC read_s0table_head, read_s0table_data,
     &     s0table_fname

c!# Local data needed in read_s0table_head:
c!# N.B.: Any changes here need corresponding changes in mapping
c!#       of dummy args to these local arrays:
      integer, parameter :: nvar=12, ni0=3, nr=7, nd=0, nc=0, nl=1
      character (len=len_name) :: varnames(nvar) = (/
     &     'lcubic','modfns','ntheta','nu    ','nphi  ',
     &     'zs0tbl','theta0','dtheta','u0    ','du    ',
     &     'phi0  ','dphi  '/)
      character (len=1) :: vartypes(nvar) = (/
     &     'l','i','i','i','i',
     &     'r','r','r','r','r',
     &     'r','r' /)
      integer :: varlengths(nvar) = (/
     &     1  , 0 , 1 , 1 , 1 ,
     &     1 , 1 , 1, 1, 1,
     &     1, 1 /)
      real :: rvals(nr)
      double precision :: dvals(1)
      complex :: cvals(1)
      logical :: lvals(nl)

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE read_s0table_head (iu,fname,
     &     lcubic, nmodfns, ntheta, nu, nphi, zs0tbl, theta0,
     &     dtheta, u0, du, phi0, dphi, max_modfns, modfns, ierr,
     &     vprint)

      Use constants, only: pi

      INTEGER, INTENT(IN) :: iu, max_modfns
      CHARACTER*(*), INTENT(IN) :: fname
      logical, intent(out) :: lcubic
      INTEGER, INTENT(OUT) :: nmodfns, ntheta, nu, nphi,
     &     modfns(max_modfns), ierr
      REAL, INTENT(OUT) :: zs0tbl, theta0,
     &     dtheta, u0, du, phi0, dphi
      logical, INTENT(IN) :: vprint

c
      character (len=len_fname) :: line
      integer :: varmatch(nvar), ivals(ni0+max_modfns)
      
      ivals(:) = 0; rvals(:) = 0; lvals(:) = .FALSE.

c!#   N.B.: This must agree with the module-scope arrays (varnames, 
c!#         vartypes, varlengths, etc)
      varlengths(2) = max_modfns
      call read_config(iu, fname, line,
     &     nvar, len_name, varnames, vartypes, varlengths,
     &        ni0+max_modfns, ivals, nr, rvals, nd, dvals, nc, cvals,
     &        nl, lvals, varmatch, ierr, vprint)

      if (ierr .ne. 0) return

c!#   Copy local arrays to dummy args:
c!#   N.B.: This must agree with the module-scope arrays (varnames, 
c!#         vartypes, varlengths, etc)
c!# Require at least one value for modfns, and exactly one value for all
c!# others:
      nmodfns = varmatch(2)
      if (nmodfns .lt. 1 .or. nmodfns .gt. max_modfns .or.
     &     sum(varmatch)-nmodfns .ne. sum(varlengths)-max_modfns)
     &     ierr = -10	!wrong number of values for required fields	!#
      if (ierr .ne. 0) return
      lcubic = lvals(1)
      modfns(1:nmodfns) = ivals(1:nmodfns)
      ntheta = ivals(max_modfns + 1)
      nu = ivals(max_modfns + 2)
      nphi = ivals(max_modfns + 3)
      zs0tbl = rvals(1)
c!#   All angles stored as degrees in .txt files, as radian internally
      theta0 = rvals(2) * (pi/180)
      dtheta = rvals(3) * (pi/180)
      u0 = rvals(4)
      du = rvals(5)
      phi0 = rvals(6) * (pi/180)
      dphi = rvals(7) * (pi/180)

      END SUBROUTINE read_s0table_head

c     -----------------------------------------------------------------

      SUBROUTINE read_s0table_data (iu,fname,
     &        ntheta, nu, nphi, npol, s0tbl, ierr)

      INTEGER, INTENT(IN) :: iu
      CHARACTER*(*), INTENT(IN) :: fname
      INTEGER, INTENT(IN) :: ntheta, nu, nphi, npol
      REAL, INTENT(OUT) :: s0tbl(ntheta, nu, nphi, npol)
      integer, intent(out) :: ierr

c!#   Open data file, read table
      open(unit=iu, file=fname, access='sequential',
     &     form='unformatted', action='read', iostat=ierr)
      if (ierr .ne. 0) then
         ierr = 1	!error opening file	!#
         return
      endif

c!#   This requires specific ordering of u and v
      read (iu, iostat=ierr) s0tbl
      if (ierr .ne. 0) then
         ierr = 2	!error reading file	!#
         return
      endif

      close (iu,iostat=ierr)	!#close data file
      if (ierr .ne. 0) ierr = 3	!error closing file	!#

      END SUBROUTINE read_s0table_data

c     -----------------------------------------------------------------

      subroutine s0table_fname(in_fname, maxpath, out_ext, out_fname)
      character (len=*), intent(in) :: in_fname, out_ext
      integer, intent(in) :: maxpath
      character (len=*), intent(out) :: out_fname

      integer :: len_root

      len_root = max(index(in_fname,'.dat',.TRUE.),
     &     index(in_fname,'.txt',.TRUE.)) - 1
      if (len_root .eq. -1) len_root = len_trim(in_fname)
      if (len_root + len(out_ext) .gt. maxpath)
     &     stop 's0table_fname: maxpath too small'
      out_fname = in_fname(1:len_root) // out_ext

      return
      end subroutine s0table_fname
c     -----------------------------------------------------------------

      END MODULE s0_tblio_mod
