
      MODULE test_ssmi_mod

      USE types, ONLY: obs_data_typ, len_name, len_fname
      USE string_mod, ONLY: where_in

      IMPLICIT NONE
      PRIVATE

      CHARACTER (LEN=len_name), PRIVATE, PARAMETER :: u_name='u_wind',
     &     v_name='v_wind', Vel_name='speed', 
     &     ambig_name='ambiguous', ssmi_name='ssmi'
      CHARACTER (LEN=len_fname), PRIVATE, PARAMETER :: 
     &     file_name = 'Simulated-SSMI-file'

      PUBLIC test

      CONTAINS

c     -----------------------------------------------------------------

      SUBROUTINE test (iuvam, data_begin)

c English Name: Test

c Purpose: This version converts ambiguity data sets into a
c     pseudo-ssmi data sets, using the mean ambiguity wind speed.

c Note: Could be used for conventional data by setting
c      ambig_name = 'convention'

c**********************************************************************

      INTEGER, INTENT(IN) :: iuvam
      TYPE(obs_data_typ), POINTER :: data_begin

c     Local variables
      INTEGER n, iamb, i_u, i_v, namb, namb_good, ierr
      REAL vbar(data_begin%n_loc)
      LOGICAL qc_flag(data_begin%n_occ), good(data_begin%n_loc)
      INTEGER num_occ, num_var, num_loc

c     data will point to the current element of data list
      TYPE (obs_data_typ), POINTER :: data

c!# ERROR HANDLING : return if no data
      IF (.NOT. ASSOCIATED(data_begin)) RETURN

c!#   Traverse data structure
      data => data_begin
      DO WHILE (ASSOCIATED(data))
c!#   Processing ambiguous data sets
      IF (data%c_obs_id .EQ. ambig_name) THEN

c-----Check data structure - find needed variables:
        i_u = where_in(u_name, data%names_var,
     &    len_name, len_name, data%n_var)
        i_v = where_in(v_name, data%names_var,
     &    len_name, len_name, data%n_var)
        IF (i_u .LE. 0 .OR. i_v .LE. 0) THEN
          WRITE (*,*) 'test: Cannot find u, and/or v.',
     &      ' Matching codes= ',i_u, i_v
          STOP 'test:  Cannot find u, and/or v'
        END IF

c-----Loop through points
        DO n = 1, data%n_loc

          namb = data%num_occ(n)
          qc_flag(:) = data%qc_flag(:,n)

c-----Initialize for average. (Note that values are correct in case
c-----that there is no good data.)
          vbar(n) = 0
          namb_good = 0

          IF (namb .GT. 0 .AND.
     &      .NOT. ALL(qc_flag(1:namb))) THEN
c
c-----Calculate mean wind speed
            DO iamb = 1, namb
              IF (.NOT. qc_flag(iamb)) THEN
                namb_good = namb_good + 1
                vbar(n)=vbar(n) + SQRT(data%data(i_u,iamb,n)**2 +
     &            data%data(i_v,iamb,n)**2)
              END IF
            END DO
          vbar(n) = vbar(n)/namb_good
          END IF
        good(n) = namb_good .GT. 0
        END DO

c-----Now edit the data structure

        num_occ = 1
        num_var = 1
        num_loc = data%n_loc
      
        data%fname = file_name
        data%c_obs_id = ssmi_name
        data%n_const = 0
        data%n_occ = num_occ
        data%n_var = num_var

        DEALLOCATE (data%names_const, data%const, STAT=ierr)
        IF (ierr .NE. 0) STOP 'test: deallocate failure'
        DEALLOCATE (data%names_var, data%qc_flag, data%data, STAT=ierr)
        IF (ierr .NE. 0) STOP 'test: deallocate failure'

        ALLOCATE (data%names_var(num_var),data%qc_flag(num_occ,num_loc),
     &    data%data(num_var,num_occ,num_loc), STAT=ierr)
        IF (ierr .NE. 0) STOP 'test: allocate failure'

        data%names_var(1) = Vel_name
        data%qc_flag(1,:) = good
        data%data(1,1,:) = vbar

      END IF
      data => data%next_set_of_obs
      END DO ! WHILE

      END SUBROUTINE test

      END MODULE test_ssmi_mod
