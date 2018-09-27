
c!#   CSU IDENTIFICATION : JscEVD
c!#   $Id: jscevd.for,v 1.1 2000/01/20 16:31:39 trn Exp $

c     Copyright (c)       Ross Hoffman           AER          16 Feb 93

c!##  PURPOSE : JscEVD converts wind components to speed and direction.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

<NLM> subroutine JscEVD   !#
<LTM> subroutine JscEVDtl   !#
<ADJ> subroutine JscEVDad   !#
     C     (N, !#
     I     uc, vc, !#
<PER>T     uc5, vc5, V5, D5, !#
     O     V, D) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

<ALL> use s0_init_mod, only: vmin

<ALL> implicit none

<ALL> integer N !#
<ALL> real uc(N), vc(N), V(N), D(N) !#

c!#~   N         Length of data vectors
c!#~   uc        u-component wind (m/s)
c!#~   vc        v-component wind (m/s)
c!#~   V         Wind speed (m/s)
c!#~   D         Wind direction (rad)
c!#~   D.        From direction, clockwise from north
cPER>
cPER> Variables with a 5 suffix are the corresponding trajectory !#
cPER> variables. !#
<PER> real uc5(N), vc5(N), V5(N), D5(N) !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: jscevd.for,v $
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.3  1998/01/13 19:32:20  rnh
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

c!#   1. Convert to speed and direction.

      do 150 j = 1, N
<NLM>    call JscCVD
<LTM>    call JscCVDtl
<ADJ>    call JscCVDad
     C        (Vmin,
     I        uc(j), vc(j),
<PER>T        uc5(j), vc5(j), V5(j), D5(j),
     O        V(j), D(j))
 150  continue

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
