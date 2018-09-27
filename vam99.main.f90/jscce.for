
c!#   CSU IDENTIFICATION : JscCE
c!#   $Id: jscce.for,v 1.1 2000/01/20 16:31:39 trn Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Jun 93

c!##  PURPOSE : JscCE calculates the sigma0 departures.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!#   This routine may be inlined.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

<NLM> subroutine JscCE   !#
<LTM> subroutine JscCEtl   !#
<ADJ> subroutine JscCEad   !#
     C   ( lNDdBCalc, S0min, !#
     P   S0obs, !#
     I   S0, S0sd, !#
<PER>T   S05, S0sd5, E5, !#
     O   E ) !#

      implicit none

c!#   Input constants:
c!#~   lNDdBCalc Use dB space to calculate normalized departure
c!#~   S0min     Minimum sigma0 (normalized)
      logical lNDdBCalc !#
      real S0min !#
cPER> FTNCHEK: S0min not referenced.

c!#   Observational data and constants:
c!#~   S0obs     Sigma0 observed (normalized)
cPER> FTNCHEK: S0obs is not referenced
      real S0obs !#

c!#   Inputs:
c!#~   S0        Sigma0 (radar backscatter) (normalized)
c!#~   S0sd      Sigma0 standard deviation
      real S0, S0sd !#

c!#   Outputs:
c!#~   E         Normalized departures (1)
      real E !#

cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
<PER> real S05, S0sd5, E5 !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!#   Since all inputs are in linear space, for dB space we use
c!#   (10/LOG(10.0))*LOG(x) for x=S0, S0obs and S0sd.
c!#   Note that constant factor cancels in calculation of E.

c!##  ALGORITHMS :

c!#   E = (S0 - S0obs)/S0sd

c!##  REFERENCES :

c!##  LIMITATIONS :

c!#   In dB space, use MAX(S0min,S0obs) to guard against S0obs .LE. 0.

c!##  CHANGE LOG :
c!#   $Log: jscce.for,v $
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.2  1998/01/13 19:32:20  rnh
c!#   Major diet complete.
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Calculate departures in linear space.

      if (.not. lNDdBCalc) then

<NLM>   E = (S0 - S0obs)/S0sd
cPER> Consider S0sd*E = S0 - S0obs
cPER> Then S0sd*dE + dS0sd*E = dS0
<LTM>   E = (S0 - E5*S0sd)/S0sd5
<ADJ>   E = 0
<ADJ>   S0 += E/S0sd5
<ADJ>   S0sd += -E5*E/S0sd5

      else

c!#   2. Or calculate departures in dB space.
<NLM>   E = LOG( S0 / MAX(S0min, S0obs) )/LOG(S0sd)
cPER> Consider LOG(S0sd)*E = LOG(S0) - constant
cPER> Then LOG(S0sd)*dE + (dS0sd/S0sd)*E = dS0/S0
<LTM>   E = (S0/S05 - (S0sd/S0sd5)*E5)/LOG(S0sd5)
<ADJ>   E = 0
<ADJ>   S0 += E/(S05*LOG(S0sd5))
<ADJ>   S0sd += -E*E5/(S0sd5*LOG(S0sd5))

      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
