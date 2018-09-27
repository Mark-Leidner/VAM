
c!#   CSU IDENTIFICATION : JscCJo
c!#   $Id: jsccjoad.F,v 1.1 2000/11/06 19:52:07 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Feb 93

c!##  PURPOSE : JscCJo accumulates Jscat components.

c!#   JscCJo accumulates the components---Jdepart and Jvar---of the
c!#   scatterometer loss function---Jscat---for a single batch of data.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!#   This routine may be inlined.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscCJoad   !#
     C    ( N, lMLECalc, lDataOK, !#
     I    S0sd, E, !#
     T    S0sd5, E5, Jdepart5, Jvar5, !#
     O    Jdepart, Jvar ) !#

      use types, only: accumulator
      implicit none
      integer N !#
      logical lMLECalc, lDataOK(N) !#
      real S0sd(N), E(N) !#
      real(accumulator) :: Jdepart, Jvar !#
      real S0sd5(N), E5(N) !#
      real (accumulator) :: Jdepart5, Jvar5 !#
      integer j
      do 140 j = N, 1, -1
        if (lDataOK(j)) then
          if (lMLECalc) then
            S0sd(j) = S0sd(j) + 2*Jvar/S0sd5(j)
          endif
          E(j) = E(j) + 2*E5(j)*Jdepart
        endif
  140 continue
      return
      end
