
      MODULE test_mod

      USE types, ONLY: obs_data_typ, len_name, len_fname

      IMPLICIT NONE
      PRIVATE
      PUBLIC test

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE test (iuvam, obs_data)

c English Name: Test

c Purpose: Provides an entry into an arbitrary event subroutine.

c**********************************************************************

      INTEGER, INTENT(IN) :: iuvam
      TYPE(obs_data_typ), POINTER :: obs_data

      real, parameter :: PI=3.1415926535897932D0

c     Namelist bogusobs

c     lat,lon  Location of bogus
c     spd,dir  Bogus wind
c     lmore    Is there another bogus record
      real lat, lon, spd, dir
      logical lmore

      namelist /bogusobs/ lat, lon, spd, dir, lmore

c     Local variables
      integer ierr

      write (*,*) 'Do-nothing version of test has been loaded:',
     &     ' will read and discard namelist /bogusobs/'

c     Bogus conventional data
      lmore = .TRUE.
  10  read (*,bogusobs,iostat=ierr)
      if (ierr .ne. 0) then
        write (*,bogusobs)
        write (*,*) 'Error ', ierr,
     &      ' reading namelist /bogusobs/ from file standard input.'
        stop 'test_mod: Error reading namelist bogusobs'
      endif
      if (lmore) go to 10

      END SUBROUTINE test

      END MODULE test_mod
