
c!#   CSU IDENTIFICATION : JscCSd
c!#   $Id: jsccsdtl.F,v 1.1 2000/11/06 19:52:28 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Jun 93

c!##  PURPOSE : JscCSd calculates the sigma0 standard deviations.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!#   Avoid division by zero by enforcing a minimum value.

c!##  CONSTRAINTS :

c!#   This routine may be inlined.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscCSdtl   !#
     C      ( S0sdmin, !#
     I      S0KpA, S0KpB, S0KpC, !#
     I      S0, !#
     T      S05, S0sd5, !#
     O      S0sd ) !#

      implicit none

c!#~   S0sdmin   Minimum sigma0 standard deviation
      real S0sdmin !#

c!#   Observational data and constants:
c!#~   S0KpA     Sigma0 KpA term
c!#~   S0KpB     Sigma0 KpB term
c!#~   S0KpC     Sigma0 KpC term
c     FTNCHEK: S0KpC is not referenced
      real S0KpA, S0KpB, S0KpC !#

c!#   Inputs:
c!#~   S0        Sigma0 (radar backscatter) (normalized)
c!#   S0 may be either calculated or observed.
      real S0 !#

c!#   Outputs:
c!#~   S0sd      Sigma0 standard deviation
      real S0sd !#

c    
c     Variables with a 5 suffix are the corresponding trajectory !#
c     variables. !#
      real S05, S0sd5 !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#      s0sd = sqrt(KpA*S0**2 + KpB*S0 + KpC)
c!#   
c!#   For JPL style calculation all of KpA, KpB and KpC are specified.
c!#      For Quikscat, KpA = (alpha-prime) [1+KpM^2] - 1,
c!#      where (alpha-prime) = (1+KpcA^2)(1+Kpr^2).
c!#   For simple Kp, KpA = Kp**2 and KpB=KpC=0.
c!#   For a constant error EPS, KpA=KpB=0 and KpC=EPS**2.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: jsccsdtl.F,v $
c!#   Revision 1.1  2000/11/06 19:52:28  mcc
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

c!#   1. Calculate standard deviation.

c!#   If Kpm2, and therefore KpA, really depends on the wind speed,
c!#   then the LTM and ADJ must also consider perturbations in S0KpA

      if (S0sd5 .gt. S0sdmin) then
c     SD**2 = KpA*S0**2 + KpB*S0 + KpC
c     SD*2*dSD = (2*KpA*S0 + KpB)*dS0
        S0sd = ( S0KpA * S05 + S0KpB / 2 ) * S0 / S0sd5
      else
         S0sd = 0
      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
