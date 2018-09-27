
c!#   CSU IDENTIFICATION : JscEVD
c!#   $Id: jscevdad.F,v 1.1 2000/11/06 19:53:13 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          16 Feb 93

c!##  PURPOSE : JscEVD converts wind components to speed and direction.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscEVDad   !#
     C     (N, !#
     I     uc, vc, !#
     T     uc5, vc5, V5, D5, !#
     O     V, D) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

      use s0_init_mod, only: vmin
      implicit none
      integer N !#
      real uc(N), vc(N), V(N), D(N) !#
      real uc5(N), vc5(N), V5(N), D5(N) !#
      integer j
      do 150 j = N, 1, -1
         call JscCVDad
     C        (Vmin,
     I        uc(j), vc(j),
     T        uc5(j), vc5(j), V5(j), D5(j),
     O        V(j), D(j))
 150  continue
      return
      end
