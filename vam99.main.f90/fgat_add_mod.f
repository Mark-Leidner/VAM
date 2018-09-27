
      MODULE fgat_add_mod

      USE types, ONLY: obs_data_typ, len_name, len_fname
      USE grid_mod, ONLY: idate, itime
      USE vam_grid_mod, ONLY: vam_grid, reset_grid
      USE vam_obs_mod, ONLY: regrid_obs

      IMPLICIT NONE
      PRIVATE
c     !#   This module calculates the FGAT constants

      PUBLIC fgat_add

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE fgat_add(iuvam, obs_data_begin)

c     !#   Purpose: Calculate the FGAT constants in the data structure

      INTEGER, INTENT(IN) :: iuvam
      TYPE(obs_data_typ), POINTER :: obs_data_begin

c     !#~  iuvam            VAM input unit number
c     !#~  obs_data_begin   obs data structure (IN/OUT)

c     !# LOCAL DATA ELEMENTS : 
c     !# fgat namelist
      REAL, SAVE :: t2=0, dt=6*60*60
      INTEGER, SAVE, DIMENSION(3) :: dates=0, times=0
      LOGICAL :: quadratic, analysis, vprint, initialize
      NAMELIST /interp/ t2, dt, dates, times,
     &    quadratic, analysis, initialize, vprint
c     !#~  t2         time of central analysis
c     !#~  dt         time increment between analyses
c     !#~  dates      analyses dates
c     !#~  times      analyses times
c     !#~  quadratic  quadratic interpolation in time?
c     !#~  analysis   time interpolation thru analysis?
c     !#~  initialize only reset fgat variables?
c     !#~  vprint     verification print out?

c     t2 should be consistent with dates2/times2, t2-dt with dates1/times1
c     and t2+dt with dates3/times3.  Code to generate dates1, times1,
c     dates2, ... from t2 and dt could be added.  But this depends on being
c     able to convert the epoch time (units for dt, t2, time) to an
c     dates/times pair, and this would add complexity.

c!!   This subroutine uses a linked list data structure parallel
c!!   to obs_data_begin for temporary fgat work space.
c!!   Comments like this describe how this is done.

c!!   Defined type for fgat work space suitable for forming a linked list 
      TYPE :: fgat_typ          !#
c     Derived type to hold temporary data for fgat variables calculation
c     Data Vectors (n_loc):
      REAL, DIMENSION(:), POINTER :: u, v, c1, c2, c3
      TYPE (fgat_typ), POINTER :: next_set_of_obs
      END TYPE fgat_typ

c!!   Define pointers for two instances
c!!   fgat_begin will point to the start of the fgat list
c!!   fgat will point to the current element of the fgat list
      TYPE (fgat_typ), POINTER :: fgat_begin, fgat

c!!   obs_data will point to the current element of data list
      TYPE (obs_data_typ), POINTER :: obs_data

      INTEGER :: ierr, n, i, ii, j
      LOGICAL :: jj(3)
c     jj(j) is .TRUE. if jth time has been processed
      jj(:) = .FALSE.

c     !#   Read namelist
      quadratic = .FALSE.
      analysis = .FALSE.
      initialize = .FALSE.
      vprint = .FALSE.
      READ (*, interp, IOSTAT=ierr)
      IF (vprint) WRITE (*, interp)
      IF (ierr .NE. 0) THEN
        IF (.NOT. vprint) WRITE (*, interp)
        WRITE (*,*) 'Error ', ierr,' reading namelist ',
     &      '&interp from file standard input.'
        STOP 'Error reading namelist &interp.'
      ENDIF

c     !# ERROR HANDLING : return if no obs_data
      IF (.NOT. ASSOCIATED(obs_data_begin)) RETURN

c     !# Initialize only and RETURN
      IF (initialize) THEN
c     Traverse data structure
        obs_data => obs_data_begin
        DO WHILE (ASSOCIATED(obs_data))
          obs_data%u_fgat(:) = 0
          obs_data%v_fgat(:) = 0 
          obs_data%alpha(:) = 1 
c     Set pointers to next element in list
          obs_data => obs_data%next_set_of_obs
        END DO 
c     It is now possible to define u_int, v_int
        CALL regrid_obs(obs_data_begin)
        RETURN
      END IF

c     !#   Initialization
c!!   Pointers are initially in an undefined state.
c!!   Nullify'ing a pointer sets it to NULL or unassociated.
c!!   Pointer should be NULL before allocation.
      NULLIFY(fgat_begin)
c!!   Allocation sets the pointer and allocates the data structure,
c!!   but the arrays and pointers within the data structure are undefined
      ALLOCATE(fgat_begin, STAT=ierr)
      IF (ierr .NE. 0) STOP 'fgat_add: allocate failure'

c     !#   Traverse data structure initializing (and allocating) fgat data
c!!   In this traverse we are building the parallel data structure.
      obs_data => obs_data_begin
      fgat => fgat_begin
      DO WHILE (ASSOCIATED(obs_data))

c     !#   Ensure that regrid will do simple interpolation (ie, no FGAT)
        obs_data%u_fgat(:) = 0
        obs_data%v_fgat(:) = 0 
        obs_data%alpha(:) = 1 

c     !#   Allocate fgat temporary variables
        n = obs_data%n_loc
c!!   Arrays within fgat are made the same size as in obs_data
        ALLOCATE (fgat%c1(n), fgat%c2(n), fgat%c3(n),
     &      fgat%u(n), fgat%v(n), STAT=ierr)
        IF (ierr .NE. 0) STOP 'fgat_add: allocate failure'
c!!   Pointer in fgat is set to NULL
        NULLIFY(fgat%next_set_of_obs)

c     !#   Initialize local versions for accumulation
        fgat%u(:) = 0
        fgat%v(:) = 0

c     !#   Using namelist variables and obs_data%time determine c1,c2,c3

        CALL tinterp (n, t2, dt, obs_data%time,
     &      fgat%c1, fgat%c2, fgat%c3, quadratic)

c!!   Set pointers to next element in list
        obs_data => obs_data%next_set_of_obs
c!!   This checks if there is a next element (otherwise done)
        IF (ASSOCIATED(obs_data)) THEN
c!!   If there is a next element allocate the work space
c!!   and set fgat to point to this new element.
c!!   If there is no next element the pointer next_set_of_obs remains NULL.
          ALLOCATE(fgat%next_set_of_obs, STAT=ierr)
          IF (ierr .NE. 0) STOP 'fgat_add: allocate failure'
          fgat => fgat%next_set_of_obs
        END IF
      END DO                    ! WHILE

c     !#   For each of the three analysis times

      DO i = 1, 3

c     !#   Read in a grid
c     !#   This triggers regrid_obs thus defining u_int, v_int
c     !#   NB: u_int, v_int are based on analysis grid !!
c     !#   Can read backgrounds so long as copyback=T.
        CALL vam_grid(iuvam, obs_data_begin)

c     !#   Check from idate, itime and namelist parameters whether this grid is
c     !#   j = 1, 2 or 3.
        j = 0
        DO ii = 1, 3
          IF (idate .eq. dates(ii) .and.
     &        itime .eq. times(ii) .and.
     &        .not. jj(ii)) j = ii
        END DO
        IF (j .EQ. 0) THEN
          WRITE (*,*) 'Error matching grid idate-itime ', idate, itime
          STOP 'Error with grid date/times  in fgat_add.'
        END IF

c     !#   Traverse data structure accumulating fgat data
c!!   Typical situation begins here.
        obs_data => obs_data_begin
        fgat => fgat_begin
        DO WHILE (ASSOCIATED(obs_data))

          IF (j .EQ. 1) THEN 
            fgat%u(:) = fgat%u(:) + fgat%c1(:)*obs_data%u_int(:)
            fgat%v(:) = fgat%v(:) + fgat%c1(:)*obs_data%v_int(:)
          END IF

          IF (j .EQ. 2 .AND. .NOT. analysis) THEN
            fgat%u(:) = fgat%u(:) + (fgat%c2(:)-1)*obs_data%u_int(:)
            fgat%v(:) = fgat%v(:) + (fgat%c2(:)-1)*obs_data%v_int(:)
          END IF

          IF (j .EQ. 3) THEN 
            fgat%u(:) = fgat%u(:) + fgat%c3(:)*obs_data%u_int(:)
            fgat%v(:) = fgat%v(:) + fgat%c3(:)*obs_data%v_int(:)
          END IF

c!!   Typical situation ends here.
          obs_data => obs_data%next_set_of_obs
          fgat => fgat%next_set_of_obs
        END DO                  ! WHILE

c     Note that time level j is done
        jj(j) = .TRUE.

      END DO                    ! i = 1 , 3

c     !#   Traverse data structure storing fgat data
c     !#   and deallocating work space
c!!   In this traverse we are destroying the parallel data structure.
      obs_data => obs_data_begin
      fgat => fgat_begin
      DO WHILE (ASSOCIATED(obs_data))

c     !#   Store results
        obs_data%u_fgat(:) = fgat%u(:)
        obs_data%v_fgat(:) = fgat%v(:)

c     !#   NOTE alpha already initialized to one as needed if fgat, otherwise
c     !#   for time interpolation using the analysis
        IF (analysis) obs_data%alpha(:) = fgat%c2(:)

c!!   Set pointers to next element in list
        obs_data => obs_data%next_set_of_obs
        fgat => fgat%next_set_of_obs
c     !#   Deallocate local variables
c!!   Before deallocating the element that has just been processed
c!!   we must deallocate the work arrays.
c!!   Note below that as we destroy the list from the beginning we 
c!!   continue to redefine fgat_begin.
c!!   In other words fgat_begin follows fgat through the list.
        DEALLOCATE(fgat_begin%c1, fgat_begin%c2, fgat_begin%c3,
     &      fgat_begin%u, fgat_begin%v, STAT=ierr)
        IF (ierr .NE. 0) STOP 'fgat_add: deallocate failure'
c!!   Next deallocate the list element
        DEALLOCATE(fgat_begin, STAT=ierr)
        IF (ierr .NE. 0) STOP 'fgat_add: deallocate failure'
c!!   Reset fgat_begin to point to the beginning of the list.
c!!   At the end of the process fgat and fgat_begin will be NULL
c!!   because when we created the last element the pointer
c!!   next_set_of_obs was NULL.
        fgat_begin => fgat
      END DO                    ! WHILE

c     !#   If j is not 2, then the central time must be read again
c     !#   Whatever is now present is deleted
      IF (j .ne. 2) CALL reset_grid

c     !#   Otherwise it is now possible to define u_int, v_int
      IF (j .eq. 2) CALL regrid_obs(obs_data_begin)

c     !#   End
      RETURN
      END SUBROUTINE fgat_add

c     -----------------------------------------------------------------

      SUBROUTINE tinterp (n, t2, dt, t, c1, c2, c3, quadratic)

c!#   Purpose: Calculate the three coefficients for time interpolation

      INTEGER, INTENT(IN) :: n
      REAL, INTENT(IN) :: t2, dt, t(n)
      REAL, INTENT(OUT) :: c1(n), c2(n), c3(n)
      LOGICAL, INTENT(IN) :: quadratic

c!#~  n         length of data vectors
c!#~  t2        central time
c!#~  dt        time increment (dt = t3-t2 = t2-t1)
c!#~  t         observation times
c!#~  c1,c2,c3  time interpolation coefficients

c!# LOCAL DATA ELEMENTS : 

      REAL :: delta(n)

c!#~  delta     normalized observation time

      delta(:) = (t(:)-t2)/dt

      IF (.NOT. quadratic) THEN

c!#   Linear case
          WHERE (delta(:) < 0)
              c1(:) = -delta(:)
              c3(:) = 0
          ELSE WHERE
              c1(:) = 0
              c3(:) = delta(:)
          END WHERE

      ELSE

c!#   Quadratic case
          c1(:) = delta(:) * (delta(:) - 1) / 2
          c3(:) = (delta(:) + 1) * delta(:) / 2
  
      END IF

c!#   Values add to one, so
      c2(:) = 1 - c1(:) - c3(:)

c!#   End
      RETURN
      END SUBROUTINE tinterp

c     -----------------------------------------------------------------

      END MODULE fgat_add_mod
