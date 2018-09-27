
c!#   CSU IDENTIFICATION : JscEE
c!#   $Id: jscee.for,v 1.1 2000/01/20 16:31:39 trn Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Jun 93

c!##  PURPOSE : JscEE calculates the sigma0 normalized departures.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

<NLM> subroutine JscEE   !#
<LTM> subroutine JscEEtl   !#
<ADJ> subroutine JscEEad   !#
     C   ( N, lNDdBCalc, !#
     P   S0obs, lDataOK, !#
     I   S0, S0sd, !#
<PER>T   S05, S0sd5, E5, !#
     O   E ) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

<ALL> use s0_init_mod, only: s0min

<ALL> implicit none

c!#   Input constants:
c!#~   N         Length of data vectors
c!#~   lNDdBCalc Use dB space to calculate normalized departure
<ALL> integer N !#
<ALL> logical lNDdBCalc !#

c!#   Observational data and constants:
c!#~   S0obs     Sigma0 observed (normalized)
c!#~   lDataOK   Good data flag
<ALL> real S0obs(N) !#
<ALL> logical lDataOK(N) !#

c!#   Inputs:
c!#~   S0        Sigma0 (radar backscatter) (normalized)
c!#~   S0sd      Sigma0 standard deviation
<ALL> real S0(N), S0sd(N) !#

c!#   Outputs:
c!#~   E         Normalized departures (1)
<ALL> real E(N) !#

cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
<PER> real S05(N), S0sd5(N), E5(N) !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   Set E=0 for bad or excluded data.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: jscee.for,v $
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.4  1998/01/13 19:32:20  rnh
c!#   Major diet complete.
c!#

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!#~   j        Loop variable
<ALL> integer j

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Calculate departures.
c     For missing data set departure to zero.

      do 140 j = 1, N
        if (lDataOK(j)) then
<NLM>     call JscCE
<LTM>     call JscCEtl
<ADJ>     call JscCEad
     C      ( lNDdBCalc, S0min,
     P      S0obs(j),
     I      S0(j), S0sd(j),
<PER>T      S05(j), S0sd5(j), E5(j),
     O      E(j) )
        else
<ALL>     E(j) = 0
        endif
  140 continue

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
