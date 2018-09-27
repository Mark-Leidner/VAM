

c!#   CSU IDENTIFICATION : JscESd
c!#   $Id: jscesd.for,v 1.1 2000/01/20 16:31:39 trn Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Jun 93

c!##  PURPOSE : JscESd calculates the sigma0 standard deviations.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

<NLM> subroutine JscESd   !#
<LTM> subroutine JscESdtl   !#
<ADJ> subroutine JscESdad   !#
     C      ( N, lSd5Calc, !#
     I      S0obs, S0KpA, S0KpB, S0KpC, lDataOK, !#
     I      S0, !#
<PER>T      S05, S0sd5, !#
     O      S0sd ) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

<ALL> use s0_init_mod, only: s0sdmin

<ALL> implicit none

c!#   Input constants:
c!#~   N         Length of data vectors
c!#~   lSd5Calc  Use trajectory values to calculate standard deviation
<ALL> integer N !#
<ALL> logical lSd5Calc !#

c!#   Observational data and constants:
c!#~   S0obs     Sigma0 observed (normalized)
c!#~   S0KpA     Sigma0 KpA term
c!#~   S0KpB     Sigma0 KpB term
c!#~   S0KpC     Sigma0 KpC term
c!#~   lDataOK   Good data flag
<ALL> real S0obs(N), S0KpA(N), S0KpB(N), S0KpC(N) !#
<ALL> logical lDataOK(N) !#
cPER> FTNCHEK: S0obs, lDataOK are not referenced

c!#   Inputs:
c!#~   S0        Sigma0 (radar backscatter) (normalized)
<ALL> real S0(N) !#

c!#   Outputs:
c!#~   S0sd      Sigma0 standard deviation
<ALL> real S0sd(N) !#

cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
<PER> real S05(N), S0sd5(N) !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   When using the observed values to calculate the standard
c!#   deviations, if there is missing data, set the standard deviation
c!#   to one.  

c!##  REFERENCES :

c!##  LIMITATIONS :

c!#   The abs(S0obs) is used to guard against negative observed values.
c!#   This is problematical: The calculation of standard deviations
c!#   should be based on a positive value of sigma0.  It is better to
c!#   use trajectory sigma0 to calculate the standard deviations or to
c!#   apply data editting to the observations to eliminate negative
c!#   values.

c!##  CHANGE LOG :
c!#   $Log: jscesd.for,v $
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.6  1998/01/13 19:32:20  rnh
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

c!#   1. Calculate S0sd from the trajectory value,

      if (lSd5Calc) then
        do 140 j = 1, N
<NLM>     call JscCSd
<LTM>     call JscCSdtl
<ADJ>     call JscCSdad
     C        ( S0sdmin,
     I        S0KpA(j), S0KpB(j), S0KpC(j),
     I        S0(j),
<PER>T        S05(j), S0sd5(j),
     O        S0sd(j) )
  140   continue

c     ------------------------------------------------------------------

c!#   2. Or, calculate S0sd from the observed value.

<NLM> else
<NLM>   do 240 j = 1, N
<NLM>     if (lDataOK(j)) then
<NLM>       call JscCSd
<NLM>C          ( S0sdmin,
<NLM>I          S0KpA(j), S0KpB(j), S0KpC(j),
<NLM>I          abs(S0obs(j)),
<NLM>O          S0sd(j) )
<NLM>     else
<NLM>       S0sd(j) = 1
<NLM>     endif
  240   continue
      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
