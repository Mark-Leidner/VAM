
c!#   CSU IDENTIFICATION : JscCalc
c!#   $Id: jsccalcad.F,v 1.1 2000/11/06 19:51:47 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Feb 93

c!##  PURPOSE : JscCalc organizes the calculations for Jscat.

c!#   JscCalc calculates speed, and direction, sigma0, standard
c!#   deviation, and normalized departure for each observation in a
c!#   batch of scatterometer data, and accumulates the components of
c!#   Jscat, the scatterometer loss function.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!#   The winds are assumed corrected for stability effects and valid at
c!#   scatterometer reference height.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscCalcad   !#
     C    ( N, lVDCalc, lS0Calc, lSdCalc, lNDCalc, !#
     C    lSd5Calc, lNDdBCalc, lMLECalc, lJoCalc, ikpm2, s0table, !#
     C    s0kpm2,	!#
     P    Pol, Theta, Azm, S0obs, S0KpA, S0KpB, S0KpC, lDataOK, !#
     I    uc, vc, !#
     T    uc5, vc5, !#
     T    U5, D5, S05, S0sd5, E5, Jdepart5, Jvar5, !#
     O    U, D, S0, S0sd, E, Jdepart, Jvar ) !#

      use types, only: accumulator, s0_table_typ
      implicit none
      integer N !#
      type (s0_table_typ), intent(in) :: s0table
      logical lVDCalc, lS0Calc, lSdCalc, lNDCalc, lSd5Calc, lNDdBCalc !#
      logical lMLECalc, lJoCalc !#
      integer :: ikpm2
      real :: s0kpm2
      integer Pol(N) !#
      real Theta(N), Azm(N), S0obs(N), S0KpA(N), S0KpB(N), S0KpC(N) !#
      logical lDataOK(N) !#
      real uc(N), vc(N) !#
      real U(N), D(N), S0(N), S0sd(N), E(N) !#
      real (accumulator) :: Jdepart, Jvar	!#
      real uc5(N), vc5(N) !#
      real U5(N), D5(N), S05(N), S0sd5(N), E5(N) !#
      real (accumulator) :: Jdepart5, Jvar5 !#
      real :: s0kpa_nscat(n)
      if (lsdcalc) then
         if (ikpm2 .eq. 1) then
            s0KpA_nscat(:) = s0kpa(:) * (1 + s0kpm2) - 1
         elseif (ikpm2 .eq. 0) then
            s0KpA_nscat(:) = s0kpa(:)
         else
            stop 'jsccalc(td,ad): unsupported value of ikpm2'
         endif
      endif

c!#   1. Evaluate wind speed and direction.

      if (lJoCalc) call JscCJoad
     C    ( N, lMLECalc, lDataOK,
     I    S0sd, E,
     T    S0sd5, E5, Jdepart5, Jvar5,
     O    Jdepart, Jvar )
      if (lNDCalc) call JscEEad
     C   ( N, lNDdBCalc,
     P   S0obs, lDataOK,
     I   S0, S0sd,
     T   S05, S0sd5, E5,
     O   E )
      if (lSdCalc) call JscESdad
     C    ( N, lSd5Calc,
     I    S0obs, S0KpA_nscat, S0KpB, S0KpC, lDataOK,
     I    S0,
     T    S05, S0sd5,
     O    S0sd )
      if (lS0Calc) call JscES0ad
     C   ( N, s0table,
     P   Pol, Theta, Azm,
     I   U, D,
     T   U5, D5, S05,
     O   S0 )
      if (lVDCalc) call JscEVDad
     C    ( N,
     I    uc, vc,
     T    uc5, vc5, U5, D5,
     O    U, D )
      return
      end
