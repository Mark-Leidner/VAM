
c!#   CSU IDENTIFICATION : JscCJo
c!#   $Id: jsccjo.for,v 1.1 2000/01/20 16:31:39 trn Exp $

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

<NLM> subroutine JscCJo   !#
<LTM> subroutine JscCJotl   !#
<ADJ> subroutine JscCJoad   !#
     C    ( N, lMLECalc, lDataOK, !#
     I    S0sd, E, !#
<PER>T    S0sd5, E5, Jdepart5, Jvar5, !#
<NLM>O    Nscat, !#
     O    Jdepart, Jvar ) !#

<ALL> use types, only: accumulator

<ALL> implicit none

c!#   Input constants:
c!#~   N         Length of data vectors
c!#~   lMLECalc  Calculate variance penalty term
c!#~   lDataOK   Good data flag
<ALL> integer N !#
<ALL> logical lMLECalc, lDataOK(N) !#

c!#   Inputs:
c!#~   S0sd      Sigma0 standard deviation
c!#~   E         Normalized departures (1)
<ALL> real S0sd(N), E(N) !#

c!#   Output values
c!#~   Jdepart   Cost function due to departures
c!#~   Jvar      Cost function due to variance penalty term
c!#~   Nscat     Number of sigma0s used
<ALL> real(accumulator) :: Jdepart, Jvar !#
<NLM> integer Nscat !#

cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
<PER> real S0sd5(N), E5(N) !#
<PER> real (accumulator) :: Jdepart5, Jvar5 !#
cPER> FTNCHEK: Jdepart5 and Jvar5 are not referenced.

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
c!#   $Log: jsccjo.for,v $
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
<ALL> integer j

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Accumulate Nscat, Jdepart, Jvar.

      do 140 j = 1, N
        if (lDataOK(j)) then
<NLM>     Nscat = Nscat + 1

<NLM>     Jdepart = Jdepart + E(j)**2
<LTM>     Jdepart = Jdepart + 2*E5(j)*E(j)
<ADJ>     E(j) += 2*E5(j)*Jdepart

          if (lMLECalc) then
<NLM>       Jvar = Jvar + 2*LOG(S0sd(j))
<LTM>       Jvar = Jvar + 2*S0sd(j)/S0sd5(j)
<ADJ>       S0sd(j) += 2*Jvar/S0sd5(j)
          endif

        endif
  140 continue

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
