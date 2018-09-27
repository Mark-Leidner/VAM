c!#   $Id: vam_main.f,v 1.12 2005/03/21 20:26:24 rnh Exp $
c**********************************************************************
c English Name: VAM (Variational Analysis Method)
c -------------
c
c Purpose: A gridded surface wind analysis is obtained by minimizing an
c -------- objective function, the magnitude of which measures the
c          error made by the gridded analysis in fitting satellite
c          wind speed data, SASS ambiguous winds, ERS1 backscatter,
c          conventional surface wind observations and the forecast
c          surface wind field.
c
c Notes: 1) Execution of this program is completely guided the namelist
c ------    file.
c
c Usage: vam [namelist]
c ------
c
c   namelist - name of file containing control parameters for executing
c              the program (see "vam.namelist.doc" for information on 
c              building namelists).
c        
c Copyright: 1999
c ----------
c
c Principal Programmer: Dr. Ross N. Hoffman
c --------------------- (Atmospheric and Environmental Research)
c                       (840 Memorial Dr.)
c                       (Cambridge, MA 02139)
c**********************************************************************

      PROGRAM vam_main

c     Purpose: Execute the VAM procedures in the order specified by 
c     the namelist file.

      USE s0_init_mod, ONLY: s0_init
      USE vam_grid_mod, ONLY: vam_grid
      USE types, ONLY: obs_data_typ
      USE vam_obs_mod, ONLY: vam_obs, date_time_check
      USE grid_mod, ONLY: fca_mode
      USE solve_mod, ONLY: solve
      USE fgat_add_mod, ONLY: fgat_add
      USE fca_grid_mod, ONLY: test
      USE string_mod, ONLY: lcomp

      IMPLICIT NONE

c     Global Variables
c     ----------------
c!#~   iuvam     reserved i/o unit for vam file operations.
      INTEGER, PARAMETER :: iuvam=8
c!#~   obs_data  linked list of observation data
      TYPE(obs_data_typ), POINTER :: obs_data

c     Local Variables
c     ---------------
c
c!#~   more_to_do is there more for the VAM to do?
c!#~   ierr       error code
c!#~   procedure  name of procedure for VAM to execute
c!#~  procedure.  (GRID, OBS, CALC, TEST)   
      LOGICAL more_to_do
      INTEGER ierr, status
      CHARACTER*80 procedure

  100 FORMAT(/1x,79('*')//1x, 'VAM: ', A, ': "', A, '"')

      WRITE(*,100) '$Revision: 1.12 $', '$Date: 2005/03/21 20:26:24 $'

c
c     Begin Variational Analysis
c     ==========================
c
c
c        Initialize
c        ==========
      nullify (obs_data)
      ierr = 0
      more_to_do = ierr.eq.0
c
c        Execute VAM control procedures as directed by namelist.
c        =======================================================
c
      DO WHILE (more_to_do)

        READ (*,FMT='(A80)',IOSTAT=ierr) procedure
        IF (ierr.EQ.0) WRITE (*,100) 'Procedure', TRIM(procedure)

c       Check status
        CALL date_time_check(obs_data, status)
        WRITE (*,*) 'Current status: ', status
        IF (status .GT. 0) STOP 'Date-time mismatch'
c
c           No more procedures.
c           -------------------
c
        IF (ierr.NE.0 .OR. lcomp(procedure,'END',3)) THEN 

          more_to_do = .FALSE.
c
c     Grid operations
c     ---------------
c
        ELSE IF (lcomp(procedure,'GRID',4)) THEN 

          CALL vam_grid(iuvam, obs_data)
c
c     Initialization for scatterometer sigma0 calculations
c     ---------------
c
        ELSE IF (lcomp(procedure,'INIT',4)) THEN 

          CALL s0_init(iuvam)
c
c
c     Observations ingest, qc, and/or output
c     ------------
c
        ELSE IF (lcomp(procedure,'OBS',3)) THEN 

          CALL vam_obs(iuvam, obs_data)
c
c     Minimize/Sum of Squares
c     -----------------------
c
        ELSE IF (lcomp(procedure,'CALC',4)) THEN 

c       Check status
          IF (status .LT. 0 .AND. .NOT.
     &        (status .EQ. -2 .AND. fca_mode .EQ. 3))
     &        STOP 'Incomplete data'

          CALL solve(iuvam, obs_data)
c
c     Add FGAT information
c     --------------------
c
        ELSE IF (lcomp(procedure,'FGAT',4)) THEN 
          
          CALL fgat_add(iuvam, obs_data)
c
c     Test procedure
c     --------------
c
        ELSE IF (lcomp(procedure,'TEST',4)) THEN 
          
          CALL test(iuvam, obs_data)
c
c     Unknown Procedure
c     -----------------
c
        ELSE

          WRITE (*,100) 'Unkown procedure', TRIM(procedure)
          more_to_do = .FALSE.

        ENDIF

      END DO

      END PROGRAM vam_main
