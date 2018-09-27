
c!#   CSU IDENTIFICATION : JscEE
c!#   $Id: jsceead.F,v 1.1 2000/11/06 19:52:43 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Jun 93

c!##  PURPOSE : JscEE calculates the sigma0 normalized departures.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscEEad   !#
     C   ( N, lNDdBCalc, !#
     P   S0obs, lDataOK, !#
     I   S0, S0sd, !#
     T   S05, S0sd5, E5, !#
     O   E ) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

      use s0_init_mod, only: s0min
      implicit none
      integer N !#
      logical lNDdBCalc !#
      real S0obs(N) !#
      logical lDataOK(N) !#
      real S0(N), S0sd(N) !#
      real E(N) !#
      real S05(N), S0sd5(N), E5(N) !#
      integer j
      do 140 j = N, 1, -1
        if (lDataOK(j)) then
          call JscCEad
     C      ( lNDdBCalc, S0min,
     P      S0obs(j),
     I      S0(j), S0sd(j),
     T      S05(j), S0sd5(j), E5(j),
     O      E(j) )
        else
          E(j) = 0
        endif
  140 continue
      return
      end
