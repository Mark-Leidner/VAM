
c!#   CSU IDENTIFICATION : JscCJo
c!#   $Id: jsccjo.F,v 1.1 2000/11/06 19:52:04 mcc Exp $

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

      subroutine JscCJo   !#
     C    ( N, lMLECalc, lDataOK, !#
     I    S0sd, E, !#
     O    Nscat, !#
     O    Jdepart, Jvar ) !#

      use types, only: accumulator

      implicit none

c!#   Input constants:
c!#~   N         Length of data vectors
c!#~   lMLECalc  Calculate variance penalty term
c!#~   lDataOK   Good data flag
      integer N !#
      logical lMLECalc, lDataOK(N) !#

c!#   Inputs:
c!#~   S0sd      Sigma0 standard deviation
c!#~   E         Normalized departures (1)
      real S0sd(N), E(N) !#

c!#   Output values
c!#~   Jdepart   Cost function due to departures
c!#~   Jvar      Cost function due to variance penalty term
c!#~   Nscat     Number of sigma0s used
      real(accumulator) :: Jdepart, Jvar !#
      integer Nscat !#


c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   The components of the obs. function---Jdepart and Jvar---and the
c!#   number of observations used---Nscat---are accumulated:
c!#   
c!#             Jdepart = Jdepart + E(j)**2
c!#             Jvar = Jvar + 2*LOG(S0sd(j))
c!#             Nscat = Nscat + 1
c!#
c!#   NOTE: When the standard deviations are independent of the
c!#   trajectory values, then the variance penalty term is a constant
c!#   and may be included or excluded from the calculation.  In this
c!#   case it will affect the value of Jscat, but only by a constant and
c!#   should not affect the minimization.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!#   Initialization must be done outside of this routine.

c!##  CHANGE LOG :
c!#   $Log: jsccjo.F,v $
c!#   Revision 1.1  2000/11/06 19:52:04  mcc
c!#   Added for non in-house build
c!#
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.1  1998/01/13 19:32:20  rnh
c!#   Major diet complete.
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!#~   j         Loop variable
      integer j

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Accumulate Nscat, Jdepart, Jvar.

      do 140 j = 1, N
        if (lDataOK(j)) then
          Nscat = Nscat + 1

          Jdepart = Jdepart + E(j)**2

          if (lMLECalc) then
            Jvar = Jvar + 2*LOG(S0sd(j))
          endif

        endif
  140 continue

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
