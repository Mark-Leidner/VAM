
c!#   CSU IDENTIFICATION : JscCS0Tb
c!#   $Id: jsccs0tbad.F,v 1.1 2000/11/06 19:52:16 mcc Exp $

c     Copyright (c)       Ross Hoffman           AER      Nov 93

c!##  PURPOSE : JscCS0Tb interpolates the sigma0 look up table.

c!#   JscCS0Tb calculates the normalized backscatter associated with
c!#   given inputs.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!#   This routine may be inlined.

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine JscCS0Tbad   !#
     C    (MTheta, MU, Pi, lCubic, Theta0, DTheta, NTheta, !#
     C    U0, DU, NU, Phi0, DPhi, NPhi, S0Tbl, S00Tbl, !#
     I    Theta, Azm, gU, gD, !#
     T    U, D, S0, !#
     O    gS0) !#

      implicit none

c!#   Constants of the model function:
c!#~   MTheta     Maximum Theta points
c!#~   MU         Maximum U points
c!#~   Pi        Half circumfrence of unit circle, 3.14159... (rad)
c!#~   lCubic     Use cubic interpolation if true, linear otherwise
c!#~   Theta0     First incidence angle (rad)
c!#~   DTheta     Increment in incidence angle (rad)
c!#~   NTheta     Number of incidence angles
c!#~   U0         First wind speed (m/s)
c!#~   DU         Increment in wind speed (m/s)
c!#~   NU         Number of wind speeds
c!#~   Phi0       First relative azimuth angle (rad)
c!#~   DPhi       Increment in relative azimuth angle (rad)
c!#~   NPhi       Number of relative azimuth angles
c     FTNCHEK: Phi0 is not referenced
c!#~   S0Tbl      Sigma0 table (normalized)
c!#~   S00Tbl     Sigma0 table for zero wind (normalized)
c!#~   S00Tbl.    as function of Theta

      integer MTheta, MU, NTheta, NU, NPhi !#
      logical lCubic !#
      real Pi, Theta0, DTheta, U0, DU, Phi0, DPhi, S00Tbl(MTheta) !#
      real*4 S0Tbl( MTheta, MU, NPhi ) !#

c!#   Constants of the observations:
c!#~   Theta    Beam incidence angle (rad)
c!#~   Azm      Radar pointing direction (rad)
c!#~   Azm.     From direction, clockwise from north
      real Theta, Azm !#

c!#   Inputs:
c!#~   U         Neutral wind speed (m/s)
c!#~   U.        At reference level
c!#~   D         Wind direction (rad)
c!#~   D.        From direction, clockwise from north
      real U, D !#

c!#   Outputs:
c!#~   S0        Sigma0 (radar backscatter) (normalized)
      real S0 !#
c     FTNCHEK: S0 is not referenced
c    
c     Variables with a g prefix are the corresponding perturbation !#
c     variables. !#
      real gU, gD, gS0 !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   Interpolate the look up table, using: Linear interpolation in
c!#   Theta; and linear or cubic interpolation in U and Phi.
c!#   Extrapolate when needed: There is never any extrapolation for Phi,
c!#   since it is circular; constant extrapolation is used for Theta;
c!#   linear extrapolation is used for large U; and to extrapolate U to
c!#   zero a zero derivative is assumed at zero wind with a function
c!#   value interpolated in Theta from S00Tbl (see JscCS00).

c!#   Each interpolation is in the interval [0,1], so indices run from 0
c!#   to 1 for linear interpolation and from -1 to 2 for cubic
c!#   interpolation.

c     NOTE: g-prefix in use, rather than 5-suffix. !#

c!##  REFERENCES :

c!##  LIMITATIONS :

c!#   Table range for Phi can be 0 to Pi or 0 to 2*Pi.
c!#   Phi0 must be zero.

c!#   ALL angles in radians.  But will work if ALL are in degrees.

c!#   D and Azm must both to be in range of -2*Pi to 2*Pi.

c!##  CHANGE LOG :
c!#   $Log: jsccs0tbad.F,v $
c!#   Revision 1.1  2000/11/06 19:52:16  mcc
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

      real xTheta, xU, xPhi
      integer iTheta, iU, iPhi, N2Pi
      real S0k(-1:2), dS0(0:1), cU0, cU1, cU2, cU3, S00
      real cPhi0(-1:2), cPhi1(-1:2), cPhi2(-1:2), cPhi3(-1:2)
      integer kStart, kEnd, k, kU, kPhi, jU(-1:2), jPhi(-1:2)
      real gxU, gxPhi, gS0k(-1:2), gdS0(0:1), gcU0, gcU1, gcU2, gcU3
c     FTNCHEK: CU0 is not referenced

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Determine indices to be used for interpolating.

      if (lCubic) then
        kStart = -1
        kEnd = 2
      else
        kStart = 0
        kEnd = 1
      endif
      xTheta = MIN( MAX( (Theta - Theta0)/DTheta + 1, REAL(1) ),
     &    REAL(NTheta) )
      iTheta = MIN( INT(xTheta), NTheta - 1 )
      xTheta = xTheta - iTheta
      xU = (U - U0)/DU + 1
      iU = MIN( MAX( INT(xU), 1),  NU - 1 )
      xU = xU - iU
      xPhi = MOD( D - Azm + 5*Pi, 2*Pi )/DPhi + 1
      iPhi = INT( xPhi )
      xPhi = xPhi - iPhi
      N2Pi = INT( 2*Pi/DPhi + 0.5 )
      do 315 k = kStart, kEnd
        jU(k) = iU + k
        jPhi(k) = MOD(iPhi - 1 + k + N2Pi, N2Pi) + 1
  315 continue
      if (lCubic .and. iU .eq. 1) jU(-1) = 1
      if (lCubic .and. iU .eq. NU - 1) jU(2) = NU
      do 335 kPhi = kStart, kEnd
        if (jPhi(kPhi) .gt. NPhi) jPhi(kPhi)  = 2*NPhi - jPhi(kPhi)
  335 continue
      do 435 kU = kStart, kEnd
        do 415 kPhi = kStart, kEnd
          S0k(kPhi) = (1-xTheta)*S0Tbl(iTheta,jU(kU),jPhi(kPhi))
     &        + xTheta*S0Tbl(iTheta+1,jU(kU),jPhi(kPhi))
  415   continue
        if (lCubic) then
          do 425 kPhi = 0, 1
            dS0(kPhi) = ( S0k(kPhi+1) - S0k(kPhi-1) )/2
  425     continue
          cPhi0(kU) = S0k(0)
          cPhi1(kU) = dS0(0)
          cPhi2(kU) = 3*(S0k(1) - S0k(0)) - (dS0(1) + 2*dS0(0))
          cPhi3(kU) = dS0(1) + dS0(0) -2*(S0k(1) - S0k(0))
        else
          cPhi0(kU) = S0k(0)
          cPhi1(kU) = S0k(1) - S0k(0)
          cPhi2(kU) = 0
          cPhi3(kU) = 0
        endif
  435 continue
      do 545 kU = kStart, kEnd
        if (lCubic) then
          S0k(kU) = ((cPhi3(kU)*xPhi + cPhi2(kU))*xPhi +
     &         cPhi1(kU))*xPhi + cPhi0(kU)
        else
          S0k(kU) = cPhi1(kU)*xPhi + cPhi0(kU)
        endif
  545 continue
      if (lCubic) then
        do 610 kU = 0, 1
          dS0(kU) = ( S0k(kU+1) - S0k(kU-1) )/2
  610   continue
      endif
      if (iU .eq. 1) then
        S00 = (1-xTheta)*S00Tbl(iTheta) + xTheta*S00Tbl(iTheta+1)
        if (lCubic) then
          dS0(0) = (S0k(1) - S00)/(U0 + DU)
        endif
        if (U .lt. U0) then
          xU = U/U0
          S0k(1) = S0k(0)
          S0k(0) = S00
          if (lCubic) then
            dS0(1) = dS0(0)
            dS0(0) = 0
          endif
        endif
      endif
      if (lCubic .and. iU .lt. NU - 1) then
        cU1 = dS0(0)
        cU2 = 3*(S0k(1) - S0k(0)) - (dS0(1) + 2*dS0(0))
        cU3 = dS0(1) + dS0(0) - 2*(S0k(1) - S0k(0))
      else
        cU1 = S0k(1) - S0k(0)
        cU2 = 0
        cU3 = 0
      endif
      if (lCubic) then
      else
      endif
      gcU3 = 0
      gcU2 = 0
      gcU1 = 0
      gcU0 = 0
      if (lCubic) then
        do 825 kU = 0, 1
          gdS0(kU) = 0
  825   continue
      endif
      do 815 kU = kStart, kEnd
        gS0k(kU) = 0
  815 continue
      gxPhi = 0
      gxU = 0
      if (lCubic) then
        gcU0 = gcU0 + gS0
        gcU1 = gcU1 + xU*gS0
        gcU2 = gcU2 + xU**2*gS0
        gcU3 = gcU3 + xU**3*gS0
        gxU = gxU + ((3*cU3*xU + 2*cU2)*xU + cU1)*gS0
        gS0 = 0
      else
        gcU0 = gcU0 + gS0
        gcU1 = gcU1 + gS0*xU
        gxU = gxU + cU1*gS0
        gS0 = 0
      endif
      if (lCubic .and. iU .lt. NU - 1) then
        gS0k(0) = gS0k(0) + 2*gcU3
        gS0k(1) = gS0k(1) - 2*gcU3
        gdS0(0) = gdS0(0) + gcU3
        gdS0(1) = gdS0(1) + gcU3
        gcU3 = 0
        gdS0(0) = gdS0(0) - 2*gcU2
        gdS0(1) = gdS0(1) - gcU2
        gS0k(0) = gS0k(0) - 3*gcU2
        gS0k(1) = gS0k(1) + 3*gcU2
        gcU2 = 0
        gdS0(0) = gdS0(0) + gcU1
        gcU1 = 0
        gS0k(0) = gS0k(0) + gcU0
        gcU0 = 0
      else
        gcU3 = 0
        gcU2 = 0
        gS0k(0) = gS0k(0) - gcU1
        gS0k(1) = gS0k(1) + gcU1
        gcU1 = 0
        gS0k(0) = gS0k(0) + gcU0
        gcU0 = 0
      endif
      if (iU .eq. 1) then
        if (U .lt. U0) then
          if (lCubic) then
            gdS0(0) = 0
            gdS0(0) = gdS0(0) + gdS0(1)
            gdS0(1) = 0
          endif
          gS0k(0) = 0
          gS0k(0) = gS0k(0) + gS0k(1)
          gS0k(1) = 0
          gU = gU + gxU/U0
          gxU = 0
        endif
        if (lCubic) then
          gS0k(1) = gS0k(1) + gdS0(0)/(U0 + DU)
          gdS0(0) = 0
        endif
      endif
      if (lCubic) then
        do 615 kU = 0, 1
          gS0k(kU-1) = gS0k(kU-1) - gdS0(kU)/2
          gS0k(kU+1) = gS0k(kU+1) + gdS0(kU)/2
          gdS0(kU) = 0
  615   continue
      endif
      do 555 kU = kStart, kEnd
        if (lCubic) then
          gxPhi = gxPhi + ((3*cPhi3(kU)*xPhi + 2*cPhi2(kU))*xPhi +
     &        cPhi1(kU))*gS0k(kU)
          gS0k(kU) = 0
        else
          gxPhi = gxPhi + cPhi1(kU)*gS0k(kU)
          gS0k(kU) = 0
        endif
  555 continue
      gD = gD + gxPhi/DPhi
      gxPhi = 0
      gU = gU + gxU/DU
      gxU = 0
      return
      end
