
c!#   CSU IDENTIFICATION : JscCS0Tb
c!#   $Id: jsccs0tb.for,v 1.1 2000/01/20 16:31:39 trn Exp $

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

<NLM> subroutine JscCS0Tb   !#
<LTM> subroutine JscCS0Tbtl   !#
<ADJ> subroutine JscCS0Tbad   !#
     C    (MTheta, MU, Pi, lCubic, Theta0, DTheta, NTheta, !#
     C    U0, DU, NU, Phi0, DPhi, NPhi, S0Tbl, S00Tbl, !#
<NLM>I    Theta, Azm, U, D, !#
<NLM>O    S0) !#
<PER>I    Theta, Azm, gU, gD, !#
<PER>T    U, D, S0, !#
<PER>O    gS0) !#

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
cPER> FTNCHEK: S0 is not referenced
cPER>
cPER> Variables with a g prefix are the corresponding perturbation !#
cPER> variables. !#
<PER> real gU, gD, gS0 !#

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

cPER> NOTE: g-prefix in use, rather than 5-suffix. !#

c!##  REFERENCES :

c!##  LIMITATIONS :

c!#   Table range for Phi can be 0 to Pi or 0 to 2*Pi.
c!#   Phi0 must be zero.

c!#   ALL angles in radians.  But will work if ALL are in degrees.

c!#   D and Azm must both to be in range of -2*Pi to 2*Pi.

c!##  CHANGE LOG :
c!#   $Log: jsccs0tb.for,v $
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
<PER> real gxU, gxPhi, gS0k(-1:2), gdS0(0:1), gcU0, gcU1, gcU2, gcU3
cPER> FTNCHEK: CU0 is not referenced

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Determine indices to be used for interpolating.

<ALL> if (lCubic) then
<ALL>   kStart = -1
<ALL>   kEnd = 2
<ALL> else
<ALL>   kStart = 0
<ALL>   kEnd = 1
<ALL> endif

c     ------------------------------------------------------------------

c!#   2. Transform input parameters into grid coordinates.

c!#   2.1 Theta with constant extrapolation.

<ALL> xTheta = MIN( MAX( (Theta - Theta0)/DTheta + 1, REAL(1) ),
<ALL>&    REAL(NTheta) )
<ALL> iTheta = MIN( INT(xTheta), NTheta - 1 )
<ALL> xTheta = xTheta - iTheta

c!#   2.2 Wind allowing extrapolation to be defined below.

<ALL> xU = (U - U0)/DU + 1
<LTM> gxU = gU/DU
<ADJ> gxU = 0
<ADJ> gU += gxU/DU
<ALL> iU = MIN( MAX( INT(xU), 1),  NU - 1 )
<ALL> xU = xU - iU

c!#   2.3 Relative azumith angle.

c     Calculate angle Phi between (wind direction from = D) and
c     (radar direction towards = Azm + Pi) in the range 0 to 2*Pi.
c     A value of 0 corresponds to radar pointing into the wind.
c     The additional factor of 4*Pi included here allows D and Azm
c     both to be in range of -2*Pi to 2*Pi.

<ALL> xPhi = MOD( D - Azm + 5*Pi, 2*Pi )/DPhi + 1
<LTM> gxPhi = gD/DPhi
<ADJ> gxPhi = 0
<ADJ> gD += gxPhi/DPhi
<ALL> iPhi = INT( xPhi )
<ALL> xPhi = xPhi - iPhi

c     ------------------------------------------------------------------

c!#   3. Set up mapping from table to extracted table.

c     Theta will be interpolated linearly

c!#   3.1 Wind and relative azimuth angle may be interpolated cubically.

<ALL> N2Pi = INT( 2*Pi/DPhi + 0.5 )
<ALL> do 315 k = kStart, kEnd
<ALL>   jU(k) = iU + k
c     When iPhi = 1, jPhi(-1) = N2Pi, when iPhi = N2Pi, jPhi(1) = 1
<ALL>   jPhi(k) = MOD(iPhi - 1 + k + N2Pi, N2Pi) + 1
  315 continue

c!#   3.2 Repeat wind edge indices if necessary to avoid out of bound
c!#   references. 

c     These values will be referenced later, but the results will be
c     recalculated.

<ALL> if (lCubic .and. iU .eq. 1) jU(-1) = 1
<ALL> if (lCubic .and. iU .eq. NU - 1) jU(2) = NU

c!#   3.3 Fold Phi indices at Pi if necessary.

<ALL> do 335 kPhi = kStart, kEnd
<ALL>   if (jPhi(kPhi) .gt. NPhi) jPhi(kPhi)  = 2*NPhi - jPhi(kPhi)
  335 continue

c     ------------------------------------------------------------------

c!#   4. For each U index:

<ALL> do 435 kU = kStart, kEnd

c!#   4.1 For each Phi index, interpolate the table linearly in Theta.

<ALL>   do 415 kPhi = kStart, kEnd
<ALL>     S0k(kPhi) = (1-xTheta)*S0Tbl(iTheta,jU(kU),jPhi(kPhi))
<ALL>&        + xTheta*S0Tbl(iTheta+1,jU(kU),jPhi(kPhi))
  415   continue

<ALL>   if (lCubic) then

c!#   4.2 Calculate derivatives if Phi interpolation is cubic.

<ALL>     do 425 kPhi = 0, 1
<ALL>       dS0(kPhi) = ( S0k(kPhi+1) - S0k(kPhi-1) )/2
  425     continue

c!#   4.3 Calculate cubic or linear coefficients of the Phi polynomial.

<ALL>     cPhi0(kU) = S0k(0)
<ALL>     cPhi1(kU) = dS0(0)
<ALL>     cPhi2(kU) = 3*(S0k(1) - S0k(0)) - (dS0(1) + 2*dS0(0))
<ALL>     cPhi3(kU) = dS0(1) + dS0(0) -2*(S0k(1) - S0k(0))
<ALL>   else
<ALL>     cPhi0(kU) = S0k(0)
<ALL>     cPhi1(kU) = S0k(1) - S0k(0)
<ALL>     cPhi2(kU) = 0
<ALL>     cPhi3(kU) = 0
<ALL>   endif

  435 continue

c     ------------------------------------------------------------------

c!#   5. Evaluate Phi polynomials at observed Phi for each U index.

<ALL> do 545 kU = kStart, kEnd
<ALL>   if (lCubic) then
<ALL>     S0k(kU) = ((cPhi3(kU)*xPhi + cPhi2(kU))*xPhi +
<ALL>&         cPhi1(kU))*xPhi + cPhi0(kU)
<LTM>     gS0k(kU) = ((3*cPhi3(kU)*xPhi + 2*cPhi2(kU))*xPhi +
<LTM>&         cPhi1(kU))*gxPhi

<ALL>   else
<ALL>     S0k(kU) = cPhi1(kU)*xPhi + cPhi0(kU)
<LTM>     gS0k(kU) = cPhi1(kU)*gxPhi
<ALL>   endif
  545 continue
cADJ>
<ADJ> do 555 kU = kStart, kEnd
<ADJ>   if (lCubic) then
cADJ> ...gS0k(kU) = ((3*cPhi3(kU)*xPhi + 2*cPhi2(kU))*xPhi +
cADJ> ...      cPhi1(kU))*gxPhi
<ADJ>     gS0k(kU) = 0
<ADJ>     gxPhi += ((3*cPhi3(kU)*xPhi + 2*cPhi2(kU))*xPhi +
<ADJ>&        cPhi1(kU))*gS0k(kU)
<ADJ>   else
cADJ> ...gS0k(kU) = cPhi1(kU)*gxPhi
<ADJ>     gS0k(kU) = 0
<ADJ>     gxPhi += cPhi1(kU)*gS0k(kU)
<ADJ>   endif

  555 continue

c     ------------------------------------------------------------------

c!#   6. Interpolate in U.

c!#   At this point the table has been interpolated in Theta and Phi at
c!#   each of the U indices.

c!#   6.1 Calculate derivatives if interpolation is cubic.

<ALL> if (lCubic) then
<ALL>   do 610 kU = 0, 1
<ALL>     dS0(kU) = ( S0k(kU+1) - S0k(kU-1) )/2
<LTM>     gdS0(kU) = ( gS0k(kU+1) - gS0k(kU-1) )/2
  610   continue
<ALL> endif
cADJ>
<ADJ> if (lCubic) then
<ADJ>   do 615 kU = 0, 1
cADJ> ...gdS0(kU) = ( gS0k(kU+1) - gS0k(kU-1) )/2
<ADJ>     gdS0(kU) = 0
<ADJ>     gS0k(kU+1) += gdS0(kU)/2
<ADJ>     gS0k(kU-1) += - gdS0(kU)/2
  615   continue
<ADJ> endif

c!#   6.2 If the wind speed is low:

<ALL> if (iU .eq. 1) then

c!#   6.2.1 Interpolate S00 in Theta.
<ALL>   S00 = (1-xTheta)*S00Tbl(iTheta) + xTheta*S00Tbl(iTheta+1)

c!#   6.2.2 Recalculate derivative at U0.
c     This will be centered differences if U0 = DU !!!
<ALL>   if (lCubic) then
<ALL>     dS0(0) = (S0k(1) - S00)/(U0 + DU)
<LTM>     gdS0(0) = gS0k(1)/(U0 + DU)
<ALL>   endif

c!#   6.2.3 If U is less than U0, recalculate grid coordinate.
<ALL>   if (U .lt. U0) then
<ALL>     xU = U/U0
<LTM>     gxU = gU/U0

c!#   Also shift table values and derivatives so that indices 0 and 1
c!#   correspond to U = 0 and U0.
<ALL>     S0k(1) = S0k(0)
<LTM>     gS0k(1) = gS0k(0)
<ALL>     S0k(0) = S00
<LTM>     gS0k(0) = 0
<ALL>     if (lCubic) then
<ALL>       dS0(1) = dS0(0)
<LTM>       gdS0(1) = gdS0(0)
<ALL>       dS0(0) = 0
<LTM>       gdS0(0) = 0
<ALL>     endif

<ALL>   endif
<ALL> endif
cADJ>
<ADJ> if (iU .eq. 1) then
<ADJ>   if (lCubic) then
cADJ> ...gdS0(0) = gS0k(1)/(U0 + DU)
<ADJ>     gdS0(0) = 0
<ADJ>     gS0k(1) += gdS0(0)/(U0 + DU)
<ADJ>   endif
<ADJ>   if (U .lt. U0) then
cADJ> ...gxU = gU/U0
<ADJ>     gxU = 0
<ADJ>     gU += gxU/U0
cADJ> ...gS0k(1) = gS0k(0)
<ADJ>     gS0k(1) = 0
<ADJ>     gS0k(0) += gS0k(1)
cADJ> ...gS0k(0) = 0
<ADJ>     gS0k(0) = 0
<ADJ>     if (lCubic) then
cADJ> ...gdS0(1) = gdS0(0)
<ADJ>       gdS0(1) = 0
<ADJ>       gdS0(0) += gdS0(1)
cADJ> ...gdS0(0) = 0
<ADJ>       gdS0(0) = 0
<ADJ>     endif
<ADJ>   endif
<ADJ> endif

c!#   6.3 Calculate coefficients of polynomial in U from values and
c!#   derivatives at 0 and 1.

c!#   NOTE: For high winds at and beyond edge of table always use linear
c!#   function.

<ALL> if (lCubic .and. iU .lt. NU - 1) then
<NLM>   cU0 = S0k(0)
<LTM>   gcU0 = gS0k(0)
<ALL>   cU1 = dS0(0)
<LTM>   gcU1 = gdS0(0)
<ALL>   cU2 = 3*(S0k(1) - S0k(0)) - (dS0(1) + 2*dS0(0))
<LTM>   gcU2 = 3*(gS0k(1) - gS0k(0)) - (gdS0(1) + 2*gdS0(0))
<ALL>   cU3 = dS0(1) + dS0(0) - 2*(S0k(1) - S0k(0))
<LTM>   gcU3 = gdS0(1) + gdS0(0) - 2*(gS0k(1) - gS0k(0))
<ALL> else
<NLM>   cU0 = S0k(0)
<LTM>   gcU0 = gS0k(0)
<ALL>   cU1 = S0k(1) - S0k(0)
<LTM>   gcU1 = gS0k(1) - gS0k(0)
<ALL>   cU2 = 0
<LTM>   gcU2 = 0
<ALL>   cU3 = 0
<LTM>   gcU3 = 0
<ALL> endif
cADJ>
<ADJ> if (lCubic .and. iU .lt. NU - 1) then
cADJ> ...gcU0 = gS0k(0)
<ADJ>   gcU0 = 0
<ADJ>   gS0k(0) += gcU0
cADJ> ...gcU1 = gdS0(0)
<ADJ>   gcU1 = 0
<ADJ>   gdS0(0) += gcU1
cADJ> ...gcU2 = 3*(gS0k(1) - gS0k(0)) - (gdS0(1) + 2*gdS0(0))
<ADJ>   gcU2 = 0
<ADJ>   gS0k(1) += 3*gcU2
<ADJ>   gS0k(0) += - 3*gcU2
<ADJ>   gdS0(1) += - gcU2
<ADJ>   gdS0(0) += - 2*gcU2
cADJ> ...gcU3 = gdS0(1) + gdS0(0) - 2*(gS0k(1) - gS0k(0))
<ADJ>   gcU3 = 0
<ADJ>   gdS0(1) += gcU3
<ADJ>   gdS0(0) += gcU3
<ADJ>   gS0k(1) += - 2*gcU3
<ADJ>   gS0k(0) += 2*gcU3
<ADJ> else
cADJ> ...gcU0 = gS0k(0)
<ADJ>   gcU0 = 0
<ADJ>   gS0k(0) += gcU0
cADJ> ...gcU1 = gS0k(1) - gS0k(0)
<ADJ>   gcU1 = 0
<ADJ>   gS0k(1) += gcU1
<ADJ>   gS0k(0) += - gcU1
cADJ> ...gcU2 = 0
<ADJ>   gcU2 = 0
cADJ> ...gcU3 = 0
<ADJ>   gcU3 = 0
<ADJ> endif

c     ------------------------------------------------------------------

c!#   7. Evaluate the U polynomial for the input value of U.

<ALL> if (lCubic) then
<NLM>   S0 = ((cU3*xU + cU2)*xU + cU1)*xU + cU0
<LTM>   gS0 = ((3*cU3*xU + 2*cU2)*xU + cU1)*gxU +
<LTM>&       ((gcU3*xU + gcU2)*xU + gcU1)*xU + gcU0
<ALL> else
<NLM>   S0 = cU1*xU + cU0
<LTM>   gS0 = cU1*gxU + gcU1*xU + gcU0
<ALL> endif
cADJ>
<ADJ> if (lCubic) then
cADJ> ...gS0 = ((3*cU3*xU + 2*cU2)*xU + cU1)*gxU +
cADJ> ...      ((gcU3*xU + gcU2)*xU + gcU1)*xU + gcU0
<ADJ>   gS0 = 0
<ADJ>   gxU += ((3*cU3*xU + 2*cU2)*xU + cU1)*gS0
<ADJ>   gcU3 += xU**3*gS0
<ADJ>   gcU2 += xU**2*gS0
<ADJ>   gcU1 += xU*gS0
<ADJ>   gcU0 += gS0
<ADJ> else
cADJ> ...gS0 = cU1*gxU + gcU1*xU + gcU0
<ADJ>   gS0 = 0
<ADJ>   gxU += cU1*gS0
<ADJ>   gcU1 += gS0*xU
<ADJ>   gcU0 += gS0
<ADJ> endif
cADJ>
cADJ> ------------------------------------------------------------------
cADJ>
cADJ> 8. Local variables must be intialized for adjoint
cADJ>
<ADJ> gxU = 0
<ADJ> gxPhi = 0
<ADJ> do 815 kU = kStart, kEnd
<ADJ>   gS0k(kU) = 0
  815 continue
<ADJ> if (lCubic) then
<ADJ>   do 825 kU = 0, 1
<ADJ>     gdS0(kU) = 0
  825   continue
<ADJ> endif
<ADJ> gcU0 = 0
<ADJ> gcU1 = 0
<ADJ> gcU2 = 0
<ADJ> gcU3 = 0

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
