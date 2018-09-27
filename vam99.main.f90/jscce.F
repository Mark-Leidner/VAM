
c!#   CSU IDENTIFICATION : JscCE
c!#   $Id: jscce.F,v 1.1 2000/11/06 19:51:54 mcc Exp $

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

      subroutine JscCE   !#
     C   ( lNDdBCalc, S0min, !#
     P   S0obs, !#
     I   S0, S0sd, !#
     O   E ) !#

      implicit none

c!#   Input constants:
c!#~   lNDdBCalc Use dB space to calculate normalized departure
c!#~   S0min     Minimum sigma0 (normalized)
      logical lNDdBCalc !#
      real S0min !#

c!#   Observational data and constants:
c!#~   S0obs     Sigma0 observed (normalized)
      real S0obs !#

c!#   Inputs:
c!#~   S0        Sigma0 (radar backscatter) (normalized)
c!#~   S0sd      Sigma0 standard deviation
      real S0, S0sd !#

c!#   Outputs:
c!#~   E         Normalized departures (1)
      real E !#


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
c!#   $Log: jscce.F,v $
c!#   Revision 1.1  2000/11/06 19:51:54  mcc
c!#   Added for non in-house build
c!#
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

        E = (S0 - S0obs)/S0sd

      else

c!#   2. Or calculate departures in dB space.
        E = LOG( S0 / MAX(S0min, S0obs) )/LOG(S0sd)

      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
