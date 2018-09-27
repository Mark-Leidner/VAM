c     !# CSU IDENTIFICATION : obs_amb_mod
c     !#     $Id: obs_amb_mod.f,v 1.9 2004/09/24 18:52:25 leidner Exp $

c     !# PURPOSE : Constants and code specific to ambiguous winds obs

      MODULE obs_amb_mod        !#

      USE types, ONLY: obs_data_typ, len_name
      USE string_mod, ONLY: where_in

      implicit none

      private

      character (len=len_name), private, parameter :: u_name='u_wind',
     &    v_name='v_wind', dfac_name='dfac'

      public ss_amb, qc_amb

      CONTAINS

c     !# REFERENCES : None

c     !# LIMITATIONS : 

c     !# CHANGE LOG : 
c     !#	$Log: obs_amb_mod.f,v $
c     !#	Revision 1.9  2004/09/24 18:52:25  leidner
c     !#	Bug fixes, so recent implementation of qc_bit_map now compiles.  Testing remains.
c     !#	
c     !#	Revision 1.8  2004/09/08 20:42:54  rnh
c     !#	Clarified handling of qc_bit_map.
c     !#	
c     !#	Revision 1.7  2004/09/07 19:59:29  rnh
c     !#	adding qc features
c     !#	
c     !#	Revision 1.6  2004/09/02 15:40:05  rnh
c     !#	Added wind qc; ability to skip non-existing files
c     !#	
c     !#	Revision 1.5  2004/03/19 21:36:01  rnh
c     !#	FGAT: first version, fgat_add good only for one data set
c     !#	
c     !#	Revision 1.4  2001/02/23 16:29:10  rnh
c     !#	The following error fixed by changing the order of the subroutines
c     !#	within the module.
c     !#	
c     !#	f90 -O3	 -c obs_amb_mod.f
c     !#	Unexpected object class (5) in FWlinear_type
c     !#	Assertion failed: 0, file ../srcfw/FWcvrt.c, line 3370
c     !#	f90: Fatal error in f90comp: Abort
c     !#	
c     !#	Revision 1.3  2001/01/03 20:39:35  trn
c     !#	Corrected printout of sum of weights
c     !#	
c     !#	Revision 1.2  1999/10/08 13:45:13  trn
c     !#	Updated vam_obs data file structure, revised gridops and vamobs namelists, some
c     !#	reorganization of modules, other stylistic changes.
c     !#	
c     !#	Revision 1.1  1999/08/30 15:38:01  trn
c     !#	Initial revision
c     !#

c     !# ------------------------------------------------------------------------
      SUBROUTINE qc_amb(data, nalias, dualqc, ibambig, vprint)

      USE constants, ONLY : pi

c     qc_amb performs dualqc on ambiguous winds data

      INTEGER, INTENT(IN) :: nalias, ibambig
      LOGICAL, INTENT(IN) :: dualqc, vprint 
      TYPE (obs_data_typ), INTENT(INOUT) :: data

      INTEGER :: ia, ia_count, ia0_count, m, n, ndual, ndualqc
      INTEGER :: i_u, i_v, loop
      REAL :: del, speed1, dir1, speed2, dir2

c-----Check data structure - find needed variables:
      i_u = where_in(u_name, data%names_var,
     &    len_name, len_name, data%n_var)
      i_v = where_in(v_name, data%names_var,
     &    len_name, len_name, data%n_var)
      IF (i_u .le. 0 .or. i_v .le. 0) THEN
        WRITE (*,*) 'qc_amb: Cannot find u and/or v.',
     &      ' Matching codes= ',i_u, i_v
        STOP 'qc_amb:  Cannot find u and/or v'
      END IF

      IF (nalias .GE. 0 .AND. nalias .LT. data%n_occ) THEN
c     !#   Only use up to nalias ambiguities:
c     !#   For this, the ambiguities should be in descending order of MLE
        data%qc_bit_map(nalias+1:data%n_occ,:) =
     &      IBSET(data%qc_bit_map(nalias+1:data%n_occ,:),ibambig)
        IF (vprint) WRITE (*,'(/a,i4)')
     &      ' Limiting ambiguities to no more than ',nalias
      END IF

      IF (dualqc) THEN
        ndual = 0
        ndualqc = 0
c-----Loop through points
        DO n=1,data%n_loc

          IF (data%num_occ(n) .gt. 1 .and.
     &        .not. ANY(data%qc_flag(:,n))) THEN
            ndual = ndual + 1
            CALL winds(data%data(i_u,1,n),
     &          data%data(i_v,1,n), speed1, dir1, .FALSE. )
            CALL winds(data%data(i_u,2,n),
     &          data%data(i_v,2,n), speed2, dir2, .FALSE. )
            del = pi - ABS( pi - ABS( dir1 - dir2 ) )
            IF ( del .LT. (3.*pi/4.) ) then
              data%qc_bit_map(:,n) =
     &            IBSET(data%qc_bit_map(:,n),ibambig)
              ndualqc = ndualqc + 1
            END IF
          ELSE
c     Eliminate single ambiguities and questionable points
            data%qc_bit_map(:,n) =
     &          IBSET(data%qc_bit_map(:,n),ibambig)
          END IF
        END DO

        if (vprint) write (*,'(/a,i6,a,i6,a,f6.2,a/)')
     &    ' Of ', ndual, ' dual ambiguity WVCs, ', ndualqc,
     &    ' (', 100.*float(ndualqc)/float(max(1,ndual)),
     &    ' %) failed dual QC.'

      END IF

      RETURN
      END SUBROUTINE qc_amb

c!# ------------------------------------------------------------------------
      SUBROUTINE ss_amb(ss, slamda, norm, data)

      USE types, ONLY: accumulator

      USE grid_mod, ONLY: nlon,nlat,u5,v5,u,v
      USE interp_mod, ONLY: uvinterp,uvinterpad

c     ss_amb calculates the sums of squares and gradient due to
c     ambiguous winds data

      REAL(accumulator), INTENT(INOUT) :: ss
      REAL, INTENT(IN) :: slamda
      REAL, INTENT(INOUT) :: norm
      TYPE (obs_data_typ), INTENT(INOUT) :: data

c     Local variables
      INTEGER n, iamb, i_u, i_v, i_dfac, namb, namb_good
      REAL dfac, ui, vi, ui5, vi5, uobs, vobs
      REAL vbar2, d2max, s, dsdui, dsdvi, d2, t
      logical qc_flag(data%n_occ)

c-----Check data structure - find needed constants and variables:
      i_u = where_in(u_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_v = where_in(v_name, data%names_var,
     &     len_name, len_name, data%n_var)
      i_dfac = where_in(dfac_name, data%names_const,
     &     len_name, len_name, data%n_const)
      if (i_u .le. 0 .or. i_v .le. 0 .or. i_dfac .le. 0) then
         write (*,*) 'ss_amb: Cannot find u, v, and/or dfac.',
     &        ' Matching codes= ',i_u, i_v, i_dfac
         stop 'ss_amb:  Cannot find u, v, and/or dfac'
      endif
      dfac = data%const(i_dfac)

c-----Loop through points
      DO n=1,data%n_loc

        namb = data%num_occ(n)
        qc_flag(:) = data%qc_flag(:,n)

        IF (namb .gt. 0 .and.
     &        .not. ALL(qc_flag(1:namb))) THEN
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
C-----CALCULATE D2MAX = $d_0^2$
          vbar2=0
          namb_good=0
          do iamb=1,namb
             if (.not. qc_flag(iamb)) then
c$$$ Commented out lines define vbar2 as mean square windspeed
c$$$             vbar2=vbar2 + data%data(i_u,iamb,n)**2 +
c$$$     &                     data%data(i_v,iamb,n)**2
c This computes vbar2 as the square of the mean wind speed:
                namb_good = namb_good + 1
                vbar2=vbar2 + sqrt(data%data(i_u,iamb,n)**2 +
     &               data%data(i_v,iamb,n)**2)
             endif
          enddo
c$$$          vbar2 = vbar2/namb_good
          vbar2 = ( vbar2/namb_good ) ** 2
          D2MAX=DFAC*VBAR2
C-----CALCULATE LOSS AND PARTIAL WRT UI, VI
          S=1
          DSDUI=0
          DSDVI=0
          DO iamb=1,namb
             if (.not. qc_flag(iamb)) then
                uobs = data%data(i_u,iamb,n)
                vobs = data%data(i_v,iamb,n)
                D2=(ui5-uobs)**2 + (vi5-vobs)**2
C-----AVOID UNDERFLOWS WHICH MAY OCCUR IF PERSS IF WAY OFF AND FIRST
C-----STEP TAKEN BY ZXCGR GOES WAY OFF
                IF (D2 .le. 170*D2MAX) then
                   T=1-EXP(-D2/D2MAX)
                   S=S*T
C-----AVOID DIVIDE CHECK - WHEN T = 0, S = 0 AND DSDUI, DSDVI ARE ZERO
                   IF (T .gt. 0) then
                      DSDUI=DSDUI + (1/T - 1)*(ui5-uobs)
                      DSDVI=DSDVI + (1/T - 1)*(vi5-vobs)
                   endif
                endif
             endif
          enddo
          DSDUI=2.*S*DSDUI
          DSDVI=2.*S*DSDVI
          S=D2MAX*S
C-----INCREMENT SUMS AND GRADIENTS
          SS=SS + slamda * S
c!#~   norm(ss_amb)	sum of weights = number of wind vector cells used
          norm = norm + 1
c
          ui = slamda * dsdui
          vi = slamda * dsdvi

c-----FGAT correction
          ui = data%alpha(n)*ui
          vi = data%alpha(n)*vi

          CALL uvinterpad
     &        ( nlon, nlat,
     &        data%gridi(n), data%gridj(n),
     &        u, v,
     &        ui, vi )

       END IF !#valid winds at this locations
      END DO !#loop over obs locations

      return
      END SUBROUTINE ss_amb

      end module obs_amb_mod

