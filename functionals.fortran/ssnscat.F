
c!#   CSU IDENTIFICATION : ssnscat
c!#   $Id: ssnscat.F,v 1.1 2000/11/13 13:47:53 mcc Exp $

c!##  PURPOSE : Calculates misfit to NSCAT scatterometer sigma0 data.

c!#   The misfit of analysis to NSCAT scatterometer data is computed
c!#   with a sum of squares objective function.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine ssnscat !#
     O     ( ss, swgt, delssu, delssv, !#
     I     u, v, idim, !#
     O     Jdepart, Jpsd ) !#

c!#   Input:
c!#~   u, v      u- and v-wind components of current analysis
c!#~   idim      Number of analysis gridpoints in x-direction
      real u(idim,1), v(idim,1) !#
      integer idim !#

c!#   Output values:
c!#~   ss        Sum of squares from jscat functional
c!#~   ss.       Initialized in sscgr.
c!#~   swgt      Number of data points used to calculate ss
c!#~   swgt.     Initialized in sscgr.
c!#~   delssu    Gradient of functional w.r.t. u-wind components
c!#~   delssv    Gradient of functional w.r.t. v-wind components
c!#~   Jdepart   Cost function due to departures
c!#~   Jpsd      Cost function due to variance penalty term
      real*8 ss !#
      real swgt, delssu(idim,1), delssv(idim,1) !#
      real*8 Jdepart, Jpsd !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   Data are processed in equal sized groups to reduce memory
c!#   requirements.  Each group is processed according to the following
c!#   steps:

c!#      1) the current analysis is interpolated to NSCAT data locations,
c!#      2) Jscat routines are used to compute trajectory sig0 values,
c!#         sum of squares functional and gradient
c!#      3) adjoint of interpolation

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: ssnscat.F,v $
c!#   Revision 1.1  2000/11/13 13:47:53  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.10  1998/04/06 17:26:54  stanr
c!#   change type of Jdepart and Jpsd to real*8, matching sscgr
c!#
c!#   Revision 1.9  1998/04/02 19:23:22  stanr
c!#   ]change type of SS to real*8, matching sscgr
c!#
c!#   Revision 1.8  1998/03/18 16:32:49  leidner
c!#   now using KpC as quality control flag
c!#
c!#   Revision 1.7  1998/02/11 20:12:18  leidner
c!#   revised some of the documentation
c!#
c!#   Revision 1.6  1998/01/26 15:05:19  leidner
c!#   removed obs function accumulation and updated documentation
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c!#   Common block /vam/ defines vam parameters and functions.
#include "vam.h"

c!#   Common block /ns_bscat/ holds the NSCAT data.
#include "ns_bscat.h"

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!#   Data group variables:
c!#~   MGrpSiz   Maximum number of data in each group
c!#~   ui5, vi5  u- and v-wind components for one data group
c!#~   ui5, vi5. Interpolated to NSCAT data locations
c!#~   ui, vi    Adjoint variables corresponding to ui5, vi5
      integer MGrpSiz
      parameter (MGrpSiz = 100)
      real ui5(MGrpSiz), vi5(MGrpSiz), ui(MGrpSiz), vi(MGrpSiz)

c!#   Place holders for call to JscEnv:
c!#~   jSatID    Satellite identifier
c!#~   jSatID.   (1=>ERS1,2=>ERS2,3=>NSCAT1,4=>SASS)
      integer jSatID

c!#   Trajectory variables 
      real E(MGrpSiz)

c!#   Adjoint variables
      real gU(MGrpSiz), gD(MGrpSiz), gS0(MGrpSiz), gS0sd(MGrpSiz)
      real gE(MGrpSiz)

c!#   Counters:
c!#~   j         Loop variable
c!#~   kstart    Array index (marks the beginning of a data group)
c!#~   kend      Array index (marks the end of a data group)
      integer j, kstart, kend

c!#   Derived quantities:
c!#~   lDataOK   Good data flag
      logical lDataOK(MGrpSiz)

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Initialize cost function components, satellite identifier

c     convert real sum-of-weights to integer for passing to JscEnv
      iswgt = nint(swgt)
      Jdepart = 0
      Jpsd = 0
      jSatID = 3

c!#   and adjoint variables

      do 110 j = 1, MGrpSiz
         ui(j) = 0
         vi(j) = 0
         gU(j) = 0
         gD(j) = 0
         gS0(j) = 0
         gS0sd(j) = 0
         gE(j) = 0
 110  continue

c     ------------------------------------------------------------------

c!#   2. For each data group 

      do 500 kstart = 1, ns_nobs, MGrpSiz
         kend = min(kstart + MGrpSiz - 1, ns_nobs)

c!#   2.1 Interpolate analysis to long,lat of observations

         call uvinterp
     c        ( idim, kend - kstart + 1,
     c        ns_ic(kstart), ns_jc(kstart),
     c        ns_xc(kstart), ns_yc(kstart),
     i        u, v,
     o        ui5, vi5 )

c!#   2.2 Set good data flag

         do 400 j = kstart, kend
            lDataOK(j-kstart+1) = ABS(ns_s0obs(j) - NS_FILL) .gt. 0.01
     &           .and. ns_kpc(j) .ge. 0
 400     continue
        
c!#   2.3 Calculate sum of squares and gradient contribution
c!#   w.r.t. wind components at obs locations

         call JscEnv
     C        ( kend - kstart + 1, 1, jSatID,
     I        ns_pol(kstart), ns_theta(kstart), ns_azim(kstart),
     I        ns_s0obs(kstart), ns_kpa(kstart), ns_kpb(kstart),
     I        ns_kpc(kstart), lDataOK,
     I        ui5, vi5,
     O        ns_vel5(kstart), ns_dir5(kstart), ns_s05(kstart),
     O        ns_s0sd(kstart), E,
     O        ui, vi,
     O        gU, gD, gS0, gS0sd, gE,
     O        iswgt, ss, Jdepart, Jpsd )

c!#   2.4 Adjoint of interpolation

         call uvinterpad
     c        ( idim, kend - kstart + 1,
     c        ns_ic(kstart), ns_jc(kstart),
     c        ns_xc(kstart), ns_yc(kstart),
     i        delssu, delssv,
     o        ui, vi )

 500  continue

c     convert integer sum-of-weights to real for passing
c     back to sscgr
      swgt = float(iswgt)

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end
