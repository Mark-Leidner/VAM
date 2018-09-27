c!# CSU IDENTIFICATION : obs_conv_mod
c!#     $Id: obs_conv_mod.f,v 1.10 2005/03/01 19:11:33 rnh Exp $

c!# PURPOSE : Constants and code specific to conventional obs

      MODULE obs_conv_mod   !#

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      IMPLICIT NONE

      PRIVATE

      CHARACTER (LEN=len_name), PRIVATE, PARAMETER :: u_name='u_wind',
     &     v_name='v_wind'

      PUBLIC ss_conv

      CONTAINS

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: obs_conv_mod.f,v $
c!#	Revision 1.10  2005/03/01 19:11:33  rnh
c!#	Style changes
c!#	
c!#	Revision 1.9  2004/09/02 15:40:05  rnh
c!#	Added wind qc; ability to skip non-existing files
c!#	
c!#	Revision 1.8  2004/03/19 21:36:01  rnh
c!#	FGAT: first version, fgat_add good only for one data set
c!#	
c!#	Revision 1.7  2001/01/03 20:39:35  trn
c!#	Corrected printout of sum of weights
c!#	
c!#	Revision 1.6  1999/10/08 13:45:13  trn
c!#	Updated vam_obs data file structure, revised gridops and vamobs namelists, some
c!#	reorganization of modules, other stylistic changes.
c!#	
c!#	Revision 1.5  1999/07/27 18:52:56  trn
c!#	Change in obs_data_typ: use integer summ_qcpass instead of logical summ_qcf
c!#
c!#	Revision 1.4  1999/03/02 14:45:59  trn
c!#	Use binary format for obs data
c!#
c!#	Revision 1.3  1999/02/26 20:37:02  trn
c!#	Minor changes (mostly stylistic)
c!#
c!#	Revision 1.2  1999/02/15 15:33:12  trn
c!#	Updates for built-in regridding for vam_grid
c!#
c!#	Revision 1.1  1999/02/15 13:10:49  trn
c!#	Initial revision
c!#

c!# ------------------------------------------------------------------------
      SUBROUTINE ss_conv(ss, slamda, norm, data)

      USE types, ONLY: accumulator

      USE grid_mod, ONLY: nlon,nlat,u5,v5,u,v
      USE interp_mod, ONLY: uvinterp,uvinterpad

c     ss_conv calculates the sums of squares and gradient due to
c     conventional wind data

      REAL(accumulator), INTENT(INOUT) :: ss
      REAL, INTENT(INOUT) :: norm
      REAL, INTENT(IN) :: slamda
      TYPE (obs_data_typ), INTENT(INOUT) :: data

c     Local variables
      INTEGER n, i_u, i_v
      REAL ui, vi, ui5, vi5, uobs, vobs
      
c-----Check data structure - find needed constants and variables:
      i_u = where_in(u_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_v = where_in(v_name, data%names_var,
     &     len_name, len_name, data%n_var)
      if (i_u .le. 0 .or. i_v .le. 0) then
         write (*,*) 'ss_conv: Cannot find u and/or v.',
     &        ' Matching codes= ',i_u, i_v
         stop 'ss_conv:  Cannot find u and/or v'
      endif

c-----Loop through points
      DO n=1,data%n_loc
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
c-----Increment sums and gradients
c!#   NB: uses first ambiguity only when applied to ambiguous data.
          uobs = data%data(i_u,1,n)
          vobs = data%data(i_v,1,n)
          ss=ss + slamda*((ui5-uobs)**2 + (vi5-vobs)**2)
c!#~   norm(ss_conv)	sum of weights = number of conv wind obs used
          norm = norm + 1

          ui = 2*slamda*(ui5-uobs)
          vi = 2*slamda*(vi5-vobs)
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

      END SUBROUTINE ss_conv

      END MODULE obs_conv_mod

