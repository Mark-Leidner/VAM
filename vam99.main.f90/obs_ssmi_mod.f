c!# CSU IDENTIFICATION : obs_ssmi_mod
c!#     $Id: obs_ssmi_mod.f,v 1.4 2006/11/22 18:35:06 rnh Exp $

c!# PURPOSE : Constants and code specific to SSMI obs

      MODULE obs_ssmi_mod   !#

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in
      USE s0_init_mod, ONLY: vmin

      IMPLICIT NONE

      PRIVATE

      CHARACTER (LEN=len_name), PRIVATE, PARAMETER :: Vel_name='speed'

      PUBLIC ss_ssmi

      CONTAINS

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: obs_ssmi_mod.f,v $
c!#	Revision 1.4  2006/11/22 18:35:06  rnh
c!#	Adjoint variable initialization fixed.
c!#	
c!#	Revision 1.3  2004/09/24 20:09:22  leidner
c!#	removed a gross check for "buggy" SSMI data.  This will now be handled
c!#	by the new qc_bit_map flags.
c!#	
c!#	Revision 1.2  2004/09/24 18:52:25  leidner
c!#	Bug fixes, so recent implementation of qc_bit_map now compiles.  Testing remains.
c!#	
c!#	Revision 1.1  2004/06/11 21:10:18  rnh
c!#	Adding ssmi operator, test routine, and associated changes.
c!#	

c!# ------------------------------------------------------------------------
      SUBROUTINE ss_ssmi(ss, slamda, norm, data)

      USE types, ONLY: accumulator

      USE grid_mod, ONLY: nlon,nlat,u5,v5,u,v
      USE interp_mod, ONLY: uvinterp,uvinterpad

c     ss_ssmi calculates the sums of squares and gradient due to
c     SSMI wind speed data

      REAL(accumulator), INTENT(INOUT) :: ss
      REAL, INTENT(INOUT) :: norm
      REAL, INTENT(IN) :: slamda
      TYPE (obs_data_typ), INTENT(INOUT) :: data

c     Local variables
      INTEGER n, i_Vel
      REAL ui, vi, Vel, Dir, ui5, vi5, Vel5, Dir5, Vobs
      
c-----Check data structure - find needed variables:
      i_Vel = where_in(Vel_name, data%names_var,
     &     len_name, len_name, data%n_var)
      IF (i_Vel .LE. 0) THEN
         WRITE (*,*) 'ss_ssmi: Cannot find speed.',
     &        ' Matching code= ',i_Vel
         STOP 'ss_ssmi:  Cannot find speed'
      END IF

c-----Loop through points
      DO n = 1, data%n_loc
        IF (data%num_occ(n) .gt. 0 .and.
     &        .not. data%qc_flag(1,n)) THEN
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
c-----Convert to wind speed
          CALL jsccvd
     &        ( Vmin,
     &        ui5, vi5,
     &        Vel5, Dir5 )
c
c-----Increment sums and gradients
          Vobs = data%data(i_Vel,1,n)
          ss=ss + slamda*(Vel5-Vobs)**2
c!#~   norm(ss_ssmi)	sum of weights = number of ssmi wind obs used
          norm = norm + 1
c
c-----Initialize adjoint calculation
          Vel = 2*slamda*(Vel5-Vobs)
          Dir = 0
          ui = 0
          vi = 0
c
c-----Convert to wind speed
          CALL jsccvdad
     &        ( Vmin,
     &        ui, vi,
     &        ui5, vi5, Vel5, Dir5,
     &        Vel, Dir )
c
c-----FGAT correction
          ui = data%alpha(n)*ui
          vi = data%alpha(n)*vi

          CALL uvinterpad
     &        ( nlon, nlat,
     &        data%gridi(n), data%gridj(n),
     &        u, v,
     &        ui, vi )

        END IF
      END DO

      RETURN

      END SUBROUTINE ss_ssmi

      END MODULE obs_ssmi_mod

