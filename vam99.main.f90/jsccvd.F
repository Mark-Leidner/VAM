
c!#   CSU IDENTIFICATION : JscCVD
c!#   $Id: jsccvd.F,v 1.1 2000/11/06 19:52:31 mcc Exp $

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

      subroutine JscCVD   !#
     C     (Vmin, !#
     I     uc, vc, !#
     O     V, D) !#

      implicit none

      real Vmin, uc, vc, V, D !#

c!#~   Vmin      Minimum wind speed (m/s)
c!#~   uc        u-component wind (m/s)
c!#~   vc        v-component wind (m/s)
c!#~   V         Wind speed (m/s)
c!#~   D         Wind direction (rad)
c!#~   D.       From direction, clockwise from north

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
c!#   $Log: jsccvd.F,v $
c!#   Revision 1.1  2000/11/06 19:52:31  mcc
c!#   Added for non in-house build
c!#
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

      V = SQRT(uc**2 + vc**2)
      if (V .gt. Vmin) then

         D = ATAN2(-uc, -vc)
      else
         V = Vmin
         D = 0
      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
