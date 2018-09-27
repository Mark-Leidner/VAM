
c!#   CSU IDENTIFICATION : JscES0
c!#   $Id: jsces0ad.F,v 1.1 2000/11/06 19:52:53 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Feb 93

c!##  PURPOSE : JscES0 calculates sigma0 values using a model function.

c!#   JscES0 calculates the scatterometer sigma0 values from the neutral
c!#   stability wind using the scatterometer model function.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!#   Avoid division by zero by enforcing a minimum value of sigma0.

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscES0ad   !#
     C    ( N, s0table, !#
     I    Pol, Theta, Azm, !#
     I    U, D, !#
     T    U5, D5, S05, !#
     O    S0) !#

c!##  GLOBAL AND SHARED DATA :

      use types, only: s0_table_typ, npol
      use constants, only: pi
      use s0_init_mod, only: s0min
      implicit none
      integer N !#
      type (s0_table_typ), intent(in) :: s0table
      integer Pol(N) !#
      real Theta(N), Azm(N) !#
      real U(N), D(N) !#
      real S0(N) !#
      real U5(N), D5(N), S05(N) !#
      integer j, iAngle, jPol
      real Delta, Angle
      do 240 j = N, 1, -1
         jPol = 2 - Pol(j)
         if (jpol .gt. npol .or. jpol .lt. 1) then
            write (*,*) 'jsces0: invalid value for polarization: ',
     &      pol(j)
            stop 'jsceso(td,ad): invalid value for polarization'
         endif
         if (S05(j) .gt. S0min) then
            call JscCS0Tbad
     C        (s0table%nTheta, s0table%nU, Pi, s0table%lCubic,
     C        s0table%Theta0, s0table%DTheta, s0table%NTheta,
     C        s0table%U0, s0table%DU, s0table%NU,
     C        s0table%Phi0, s0table%DPhi, s0table%NPhi,
     C        s0table%S0Tbl(1, 1, 1, jPol), s0table%S00Tbl(1, jPol),
     I        Theta(j), Azm(j), U(j), D(j),
     T        U5(j), D5(j), S05(j),
     O        S0(j))
         else
            S0(j) = 0
         endif
  240 enddo

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
