

c!#   CSU IDENTIFICATION : JscESd
c!#   $Id: jscesdad.F,v 1.1 2000/11/06 19:53:03 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Jun 93

c!##  PURPOSE : JscESd calculates the sigma0 standard deviations.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscESdad   !#
     C      ( N, lSd5Calc, !#
     I      S0obs, S0KpA, S0KpB, S0KpC, lDataOK, !#
     I      S0, !#
     T      S05, S0sd5, !#
     O      S0sd ) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

      use s0_init_mod, only: s0sdmin
      implicit none
      integer N !#
      logical lSd5Calc !#
      real S0obs(N), S0KpA(N), S0KpB(N), S0KpC(N) !#
      logical lDataOK(N) !#
      real S0(N) !#
      real S0sd(N) !#
      real S05(N), S0sd5(N) !#
      integer j
      if (lSd5Calc) then
  240   continue
        do 140 j = N, 1, -1
          call JscCSdad
     C        ( S0sdmin,
     I        S0KpA(j), S0KpB(j), S0KpC(j),
     I        S0(j),
     T        S05(j), S0sd5(j),
     O        S0sd(j) )
  140   continue
      endif
      return
      end
