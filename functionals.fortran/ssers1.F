
c!#   CSU IDENTIFICATION : ssers1
c!#   $Id: ssers1.F,v 1.1 2000/11/13 13:47:42 mcc Exp $

c!##  PURPOSE : Calculates misfit to ERS-1,2 scatterometer sigma0 data.

c!#   The misfit of analysis to ERS-1,2 scatterometer data is computed
c!#   with a sum of squares objective function.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine ssers1 !#
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
c!#   requirements.  Each group is processed by antenna according to
c!#   the following steps:

c!#      1) the current analysis is interpolated to ERS-1,2 data locations,
c!#      2) Jscat routines are used to compute trajectory sig0 values,
c!#         sum of squares functional and gradient
c!#      3) adjoint of interpolation

c!#   Note: A loop over antenna is used to process the data.  Since a
c!#   triplet of ERS-1,2 sigma0 measurements (one from each antenna;
c!#   fore, mid, aft) are assigned one lat/lon location, the analysis wind
c!#   speed interpolated to the data location is used for all three
c!#   measurements.

c!#   Note: To accomodate JPL's form of the Kp equation for NSCAT data
c!#   used in the Jscat routines:
c!#      1) Kp0 is used as a place holder to accomodate unused coefficients
c!#      2) ERS-1,2 Kp values are squared and stored in KpA before
c!#         passing to the Jscat routines

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: ssers1.F,v $
c!#   Revision 1.1  2000/11/13 13:47:42  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.11  1998/04/06 17:22:44  stanr
c!#   change type of Jdepart and Jpsd to real*8, matching sscgr
c!#
c!#   Revision 1.10  1998/04/02 17:37:22  stanr
c!#   change type of SS to real*8, matching sscgr
c!#
c!#   Revision 1.9  1998/02/11 20:11:44  leidner
c!#   revised some of the documentation
c!#
c!#   Revision 1.8  1998/01/26 15:05:19  leidner
c!#   removed obs function accumulation and updated documentation
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c!#   Common block /vam/ defines vam parameters and functions.
#include "vam.h"

c!#   Common block /er_bscat/ holds the ERS-1,2 data.
#include "er_bscat.h"

c     ------------------------------------------------------------------

c!##  LOCAL DATA ELEMENTS :

c!#   Data group variables:
c!#~   MGrpSiz   Maximum number of data in each group
c!#~   ui5, vi5  u- and v-wind components for one data group
c!#~   ui5, vi5. Interpolated to ERS-1,2 data locations
c!#~   ui, vi    Adjoint variables corresponding to ui5, vi5
c!#~   Kp0       Place holder array for Kp terms; filled with zeros
c!#~   VPol      Polarization (0=Hpol, 1=Vpol)
      integer MGrpSiz
      parameter (MGrpSiz = 100)
      real ui5(MGrpSiz), vi5(MGrpSiz), ui(MGrpSiz), vi(MGrpSiz)
      real Kp0(MGrpSiz)
      integer VPol(MGrpSiz)

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
c!#~   KpA       Coefficient A in JPL sigma0 variance eqn
c!#~   lDataOK   Good data flag
      real KpA(MGrpSiz)
      logical lDataOK(MGrpSiz)

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Initialize cost function components, satellite identifier,

c     convert real sum-of-weights to integer for passing to JscEnv
      iswgt = nint(swgt)
      Jdepart = 0
      Jpsd = 0
      jSatID = 1

c!#   Kp0, VPol arguments,

      data Kp0 / MGrpSiz * 0 /
      data VPol / MGrpSiz * 1 /

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

      do 500 kstart = 1, er_nobs, MGrpSiz
         kend = min(kstart + MGrpSiz - 1, er_nobs)
         
c!#   2.1 Interpolate analysis to long,lat of observations

         call uvinterp
     c        ( idim, kend - kstart + 1,
     c        er_ic(kstart), er_jc(kstart),
     c        er_xc(kstart), er_yc(kstart),
     i        u, v,
     o        ui5, vi5 )

c!#   2.2 For each antenna

         do 450 jb = 1, ER_MBEAM

c!#   2.2.1 Set good data flag and square Kp for jscat routines

            do 400 j = kstart, kend
               lDataOK(j-kstart+1) = ABS(er_s0obs(j, jb) - ER_FILL)
     &              .gt. 0.01 .and. er_kp(j, jb) .ge. 0
               KpA(j-kstart+1) = er_kp(j, jb)**2
 400        continue

c!#   2.2.2 Calculate sum of squares and gradient contribution
c!#   w.r.t. wind components at obs locations

            call JscEnv
     C           ( kend - kstart + 1, 1, jSatID,
     I           VPol, er_theta(kstart,jb), er_azim(kstart,jb),
     I           er_s0obs(kstart,jb), KpA, Kp0, Kp0, lDataOK,
     I           ui5, vi5,
     O           er_vel5(kstart), er_dir5(kstart), er_s05(kstart,jb),
     O           er_s0sd(kstart,jb), E,
     O           ui, vi,
     O           gU, gD, gS0, gS0sd, gE,
     O           iswgt, ss, Jdepart, Jpsd )

 450     continue

c!#   2.3 Adjoint of interpolation

         call uvinterpad
     c        ( idim, kend - kstart + 1,
     c        er_ic(kstart), er_jc(kstart),
     c        er_xc(kstart), er_yc(kstart),
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
