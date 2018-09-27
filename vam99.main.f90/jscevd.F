
c!#   CSU IDENTIFICATION : JscEVD
c!#   $Id: jscevd.F,v 1.1 2000/11/06 19:53:10 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER          16 Feb 93

c!##  PURPOSE : JscEVD converts wind components to speed and direction.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscEVD   !#
     C     (N, !#
     I     uc, vc, !#
     O     V, D) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

      use s0_init_mod, only: vmin

      implicit none

      integer N !#
      real uc(N), vc(N), V(N), D(N) !#

c!#~   N         Length of data vectors
c!#~   uc        u-component wind (m/s)
c!#~   vc        v-component wind (m/s)
c!#~   V         Wind speed (m/s)
c!#~   D         Wind direction (rad)
c!#~   D.        From direction, clockwise from north

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: jscevd.F,v $
c!#   Revision 1.1  2000/11/06 19:53:10  mcc
c!#   Added for non in-house build
c!#
c!#   Revision 1.1  2000/01/20 16:31:39  trn
c!#   Initial revision
c!#
c!#   Revision 1.3  1998/01/13 19:32:20  rnh
c!#   Major diet complete.
c!#

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!#~   j        Loop variable
      integer j

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Convert to speed and direction.

      do 150 j = 1, N
         call JscCVD
     C        (Vmin,
     I        uc(j), vc(j),
     O        V(j), D(j))
 150  continue

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
