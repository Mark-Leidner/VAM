
c!#   CSU IDENTIFICATION : JscCalc
c!#   $Id: jsccalc.for,v 1.2 2000/01/24 15:44:10 trn Exp $

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

<NLM> subroutine JscCalc   !#
<LTM> subroutine JscCalctl   !#
<ADJ> subroutine JscCalcad   !#
     C    ( N, lVDCalc, lS0Calc, lSdCalc, lNDCalc, !#
     C    lSd5Calc, lNDdBCalc, lMLECalc, lJoCalc, ikpm2, s0table, !#
     C    s0kpm2,	!#
     P    Pol, Theta, Azm, S0obs, S0KpA, S0KpB, S0KpC, lDataOK, !#
     I    uc, vc, !#
<PER>T    uc5, vc5, !#
<PER>T    U5, D5, S05, S0sd5, E5, Jdepart5, Jvar5, !#
<NLM>O    Nscat, !#
     O    U, D, S0, S0sd, E, Jdepart, Jvar ) !#

<ALL> use types, only: accumulator, s0_table_typ
<ALL> implicit none

c!#   Input constants:
c!#~   N         Length of data vectors
c!#~   lVDCalc   Calculate speed and direction U and D
c!#~   lS0Calc   Calculate backscatter S0
c!#~   lSdCalc   Calculate standard deviation S0sd
c!#~   lNDCalc   Calculate normalized departure, E
c!#~   lSd5Calc  Use trajectory values to calculate standard deviation
c!#~   lNDdBCalc Use dB space to calculate normalized departure
c!#~   lMLECalc  Calculate variance penalty term
c!#~   lJoCalc   Calculate Jo
<ALL> integer N !#
<ALL> type (s0_table_typ), intent(in) :: s0table
<ALL> logical lVDCalc, lS0Calc, lSdCalc, lNDCalc, lSd5Calc, lNDdBCalc !#
<ALL> logical lMLECalc, lJoCalc !#
<ALL> integer :: ikpm2

c!#   Observational data and constants:
c!#~   S0Kpm2    Sigma0 Kpm2 term
c!#~   Pol       Polarization (0=Hpol, 1=Vpol)
c!#~   Theta     Beam incidence angle (rad)
c!#~   Azm       Radar pointing direction (rad)
c!#~   Azm.      From direction, clockwise from north
c!#~   S0obs     Sigma0 observed (normalized)
c!#~   S0KpA     Sigma0 KpA term
c!#~   S0KpB     Sigma0 KpB term
c!#~   S0KpC     Sigma0 KpC term
c!#~   lDataOK   Good data flag
<ALL> real :: s0kpm2
<ALL> integer Pol(N) !#
<ALL> real Theta(N), Azm(N), S0obs(N), S0KpA(N), S0KpB(N), S0KpC(N) !#
<ALL> logical lDataOK(N) !#

c!#   Inputs:
c!#~   uc        u-component wind (m/s)
c!#~   vc        v-component wind (m/s)
<ALL> real uc(N), vc(N) !#

c!#   Outputs:
c!#~   U         Neutral wind speed (m/s)
c!#~   U.        At reference level
c!#~   D         Wind direction (rad)
c!#~   D.        From direction, clockwise from north
c!#~   S0        Sigma0 (radar backscatter) (normalized)
c!#~   S0sd      Sigma0 standard deviation
c!#~   E         Normalized departures (1)
c!#~   Jdepart   Cost function due to departures
c!#~   Jvar      Cost function due to variance penalty term
c!#~   Nscat     Number of sigma0s used
<ALL> real U(N), D(N), S0(N), S0sd(N), E(N) !#
<ALL> real (accumulator) :: Jdepart, Jvar	!#
<NLM> integer Nscat !#
cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
<PER> real uc5(N), vc5(N) !#
<PER> real U5(N), D5(N), S05(N), S0sd5(N), E5(N) !#
<PER> real (accumulator) :: Jdepart5, Jvar5 !#

C!# Local Data:
<ALL> real :: s0kpa_nscat(n)

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   The wind components are transformed to speed and direction.  The
c!#   speed and direction are then used to evaluate the sigma0s.  Then
c!#   the standard deviation is calculated either based on the observed
c!#   values or the trajectory values (lSd5Calc).  The normalized
c!#   departures are calculated either in normal space or in dB space
c!#   (lNDdBCalc). Finally, the components of the obs
c!#   function---Jdepart, Jvar, Nscat are accumulated.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!#   Initialization of the obs. function components---Jdepart and
c!#   Jvar---must be done outside of this routine.

c!##  CHANGE LOG :
c!#   $Log: jsccalc.for,v $
c!#   Revision 1.2  2000/01/24 15:44:10  trn
c!#   Cosmetic changes
c!#
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.5  1998/01/13 19:32:20  rnh
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

c!#   0. Compute NSCAT formula S0KpA from Quikscat Kpalpha and KpM2 if
c!#   so specified:

<ALL> if (lsdcalc) then
<ALL>    if (ikpm2 .eq. 1) then
<ALL>       s0KpA_nscat(:) = s0kpa(:) * (1 + s0kpm2) - 1
<ALL>    elseif (ikpm2 .eq. 0) then
<ALL>       s0KpA_nscat(:) = s0kpa(:)
<ALL>    else
c!#   TBD: for ikpm2=2 (variable KpM2), also evaluate LTM and ADJ 
<ALL>       stop 'jsccalc(td,ad): unsupported value of ikpm2'
<ALL>    endif
<ALL> endif

c!#   1. Evaluate wind speed and direction.

<NLM> if (lVDCalc) call JscEVD
<LTM> if (lVDCalc) call JscEVDtl
<ADJ> if (lVDCalc) call JscEVDad
     C    ( N,
     I    uc, vc,
<PER>T    uc5, vc5, U5, D5,
     O    U, D )

c     ------------------------------------------------------------------

c!#   2. Evaluate sigma0 values.
c!#   TBD: for variable KpM2 (ikpm2=2), also evaluate KpM2 at this point

<NLM> if (lS0Calc) call JscES0
<LTM> if (lS0Calc) call JscES0tl
<ADJ> if (lS0Calc) call JscES0ad
     C   ( N, s0table,
     P   Pol, Theta, Azm,
     I   U, D,
<PER>T   U5, D5, S05,
     O   S0 )

c     ------------------------------------------------------------------

c!#   3. Evaluate standard deviations from trajectory or obs. value.

<NLM> if (lSdCalc) call JscESd
<LTM> if (lSdCalc) call JscESdtl
<ADJ> if (lSdCalc) call JscESdad
     C    ( N, lSd5Calc,
     I    S0obs, S0KpA_nscat, S0KpB, S0KpC, lDataOK,
     I    S0,
<PER>T    S05, S0sd5,
     O    S0sd )

c     ------------------------------------------------------------------

c!#   4. Evaluate normalized departures.

<NLM> if (lNDCalc) call JscEE
<LTM> if (lNDCalc) call JscEEtl
<ADJ> if (lNDCalc) call JscEEad
     C   ( N, lNDdBCalc,
     P   S0obs, lDataOK,
     I   S0, S0sd,
<PER>T   S05, S0sd5, E5,
     O   E )

c     ------------------------------------------------------------------

c!#   5. Accumlate obs. function components.

<NLM> if (lJoCalc) call JscCJo
<LTM> if (lJoCalc) call JscCJotl
<ADJ> if (lJoCalc) call JscCJoad
     C    ( N, lMLECalc, lDataOK,
     I    S0sd, E,
<PER>T    S0sd5, E5, Jdepart5, Jvar5,
<NLM>O    Nscat,
     O    Jdepart, Jvar )

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
