
c!#   CSU IDENTIFICATION : JscCVD
c!#   $Id: jsccvd.for,v 1.1 2000/01/20 16:31:39 trn Exp $

c     Copyright (c)       Ross Hoffman           AER          16 Feb 93

c!##  PURPOSE : JscCVD converts wind components to speed and direction.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!#   Avoid division by zero by enforcing a minimum wind speed.

c!##  CONSTRAINTS :

c!#   This routine may be inlined.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

<NLM> subroutine JscCVD   !#
<LTM> subroutine JscCVDtl   !#
<ADJ> subroutine JscCVDad   !#
     C     (Vmin, !#
     I     uc, vc, !#
<PER>T     uc5, vc5, !#
<PER>T     V5, D5, !#
     O     V, D) !#

      implicit none

      real Vmin, uc, vc, V, D !#

c!#~   Vmin      Minimum wind speed (m/s)
c!#~   uc        u-component wind (m/s)
c!#~   vc        v-component wind (m/s)
c!#~   V         Wind speed (m/s)
c!#~   D         Wind direction (rad)
c!#~   D.       From direction, clockwise from north
cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
cPER> FTNCHEK: D5 is not referenced
<PER> real uc5, vc5, V5, D5 !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#         V = SQRT(uc**2 + vc**2)
c!#         D = ATAN2(-uc, -vc)

c!#   Winds less than a minimum wind speed of Vmin are treated
c!#   as exactly zero, but reset to Vmin for further calculations.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: jsccvd.for,v $
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.3  1998/01/13 19:32:20  rnh
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

c!#   1. Convert to speed and direction.

<NLM> V = SQRT(uc**2 + vc**2)
<NLM> if (V .gt. Vmin) then
<PER> if (V5 .gt. Vmin) then
<LTM>    V = (uc5/V5)*uc + (vc5/V5)*vc
<ADJ>    V = 0
<ADJ>    uc += (uc5/V5)*V
<ADJ>    vc += (vc5/V5)*V

<NLM>    D = ATAN2(-uc, -vc)
cPER> d(atan(z))=1/(1+z**2) and z=(uc/vc)
<LTM>    D = ((vc5/V5)*uc - (uc5/V5)*vc)/V5
<ADJ>    D = 0
<ADJ>    uc += (vc5/V5)*(D/V5)
<ADJ>    vc += -(uc5/V5)*(D/V5)
      else
<NLM>    V = Vmin
<PER>    V = 0
<ALL>    D = 0
      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
