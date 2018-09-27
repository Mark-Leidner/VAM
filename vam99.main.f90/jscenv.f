
c!#   CSU IDENTIFICATION : JscEnv
c!#   $Id: jscenv.f,v 1.1 2000/01/20 16:35:17 trn Exp $

c     Copyright (c)       Ross Hoffman           AER          10 Feb 93

c!##  PURPOSE : JscEnv calculates Jscat and its gradient.

c!#   JscEnv is the software environment interface routine for the
c!#   calculations of the sigma0s, lack of fit to the scatterometer data
c!#   and the gradient with respect to the model variables.  All this is
c!#   done at the observation locations for a single batch of data.

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      module jscenv_m	!#

      implicit none

      private

      public jscenv

      CONTAINS

      subroutine JscEnv   !#
     C    ( N, isdcalc, indcalc, ikpm2, s0table, s0kpm2, !#
     I    Pol, Theta, Azm, !#
     I    S0obs, S0KpA, S0KpB, S0KpC, lDataOK, !#
     I    uc5, vc5, !#
     O    U5, D5, S05, S0sd5, E5, !#
     O    uc, vc, !#
     O    U, D, S0, S0sd, E, !#
     O    Nscat, Jscat, Jdepart, Jvar ) !#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :
c     Variables to control the calculation
      use s0_init_mod, only : lcalc, ladcalc, lvdcalc, ls0calc,
     &     iJocalc, jdebug, jniter, jtask

      use types, only: accumulator, s0_table_typ
      use constants, only: pi

c!#   Input constants:
c!#~   N         Length of data vectors
c!#~   iSdCalc   Method to calculating S0sd
c!#~   iSdCalc.  (1=>Using obs,2=>Using traj,>0=>Calculate)
c!#~   iNDCalc   Method to calculating e
c!#~   iNDCalc.  (1=>Linear space,2=>dB space,>0=>Calculate)
c!#~   ikpm2     (1=>use constant value,2=>use lookup table, 
c!#~   ikpm2.    >0=> Compute nscat kpa from kp_alpha and kpm2)
c!#~   s0kpm2    Value of Kpm2 (only used if ikpm2=1)
      integer, intent(in) :: N, isdcalc, indcalc, ikpm2 !#
      real, intent(in) :: s0kpm2

c!#~   s0table  Lookup table for sigma0 for these observations
      type(s0_table_typ), intent(in) :: s0table

c!#   Observational data and constants:
c!#~   Pol       Polarization (0=Hpol, 1=Vpol)
c!#~   Theta     Beam incidence angle (rad)
c!#~   Azm       Radar pointing direction (rad)
c!#~   Azm.      From direction, clockwise from north
c!#~   S0obs     Sigma0 observed (normalized)
c!#~   S0KpA     Sigma0 KpA term
c!#~   S0KpB     Sigma0 KpB term
c!#~   S0KpC     Sigma0 KpC term
c!#~   lDataOK   Good data flag
      integer, intent(in) :: Pol(N) !#
      real, intent(in) :: Theta(N), Azm(N), S0obs(N), S0KpA(N),
     &     S0KpB(N), S0KpC(N)   !#
      logical, intent(in) :: lDataOK(N) !#

c!#   Trajectory values.
c!#~   uc5       u-component wind (m/s)
c!#~   vc5       v-component wind (m/s)
      real, intent(in) :: uc5(N), vc5(N) !#

c!#   Other trajectory variables (nominally these are calculated)
c!#~   U5        Neutral wind speed (m/s)
c!#~   U5.       At reference level
c!#~   D5        Wind direction (rad)
c!#~   D5.       From direction, clockwise from north
c!#~   S05       Sigma0 (radar backscatter) (normalized)
c!#~   S0sd5     Sigma0 standard deviation
c!#~   E5        Normalized departures (1)
      real, intent(inout) :: U5(N), D5(N), S05(N), S0sd5(N), E5(N) !#

c!#   Adjoint variables
c!#   These are the adjoint variables corresponding to the
c!#   trajectory variables defined above.
      real, intent(inout) :: uc(N), vc(N) !#
      real, intent(inout) :: U(N), D(N), S0(N), S0sd(N), E(N) !#

c!#   Output values
c!#~   Nscat     Number of sigma0s used
c!#~   Jscat     Cost function
c!#~   Jdepart   Cost function due to departures
c!#~   Jvar      Cost function due to variance penalty term
      integer, intent(inout) :: Nscat !#
      real(accumulator), intent(out) :: Jscat !#
      real(accumulator), intent(inout) :: Jdepart, Jvar !#

c     ------------------------------------------------------------------

c!##  DATA CONVERSION :

c!##  ALGORITHMS :

c!#   The environmental interface subroutines for the scatterometer
c!#   obs. function are denoted by names starting with JscE.  These
c!#   routines attempt to separate the actual calculations from the
c!#   concerns of

c!#    1) allocating storage for workspace;
c!#    2) determining universal constants, and fixed parameters
c!#       from module variables
c!#    3) performing configuration management;
c!#    4) performing vectorization; and
c!#    5) handling debug outputs.

c!#   No allocation of storage is needed for the current version.

c!#   Configuration management means for example choosing the
c!#   proper model function, method of calculating error statistics,
c!#   etc.

c!#   In many cases, vectorization can be implemented by in-lining
c!#   of short codes.  This technique is not used in this particular
c!#   routine, but in many routines called from here.

c!#   NOTE: By convention adjoint variables will be returned as zero if
c!#   the trajectory variable is calculated within the routine.

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: jscenv.f,v $
c!#   Revision 1.1  2000/01/20 16:35:17  trn
c!#   Initial revision
c!#

c!#   Initial revision based on:
c!#   jscenv.F, Revision 1.6  1998/01/26 15:00:12  leidner
c!#   removed initialization of obs function components

c!##  LOCAL DATA ELEMENTS :

c     Variables to control the calculation
c!#~   lSdCalc   Calculate standard deviation S0sd
c!#~   lNDCalc   Calculate normalized departure, E
c!#~   lJoCalc   Calculate Jo
c!#~   lSd5Calc  Use trajectory values to calculate standard deviation
c!#~   lNDdBCalc Use dB space to calculate normalized departure
c!#~   lMLECalc   Calculate variance penalty term
      logical lSdCalc, lNDCalc, lJoCalc
      logical lSd5Calc, lNDdBCalc, lMLECalc

c!#   For debugging output below:
      integer :: icalc, iadcalc, ivdcalc, is0calc

c     Adjoint variables
      real(accumulator) :: dJdepart, dJvar

c     Loop variable
      integer j

c!##  LOCAL DATA STRUCTURES :

c!##  DATA FILES :

c     ------------------------------------------------------------------

c!##  LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Housekeeping functions.

c!#   1.3 Set variables to control the calculation.

      lSdCalc = iSdCalc .gt. 0
      lNDCalc = iNDCalc .gt. 0
      lJoCalc = iJoCalc .gt. 0
      lSd5Calc = iSdCalc .eq. 2
      lNDdBCalc = iNDCalc .eq. 2
      lMLECalc = iJoCalc .eq. 2

c!#   For debugging output below:
      icalc = 0
      if (lcalc) icalc = 1
      iadcalc = 0
      if (ladcalc) iadcalc = 1
      ivdcalc = 0
      if (lvdcalc) ivdcalc = 1
      is0calc = 0
      if (ls0calc) is0calc = 1
      
c     ------------------------------------------------------------------

c!#   2. Perform forward calculations

      if (lCalc) call JscCalc
     C    ( N, lVDCalc, lS0Calc, lSdCalc, lNDCalc,
     C    lSd5Calc, lNDdBCalc, lMLECalc, lJoCalc, ikpm2, s0table,
     C    s0kpm2, 
     P    Pol, Theta, Azm, S0obs, S0KpA, S0KpB, S0KpC, lDataOK,
     I    uc5, vc5,
     O    Nscat,
     O    U5, D5, S05, S0sd5, E5, Jdepart, Jvar )

c     ------------------------------------------------------------------

c!#   3. Calculate Jscat and start the adjoint calculation.

      Jscat = Jdepart + Jvar !#

      dJdepart = 1
      dJvar = 1

c     ------------------------------------------------------------------

c!#   4. Perform adjoint calculations.

      if (lAdCalc) call JscCalcad
     C    ( N, lVDCalc, lS0Calc, lSdCalc, lNDCalc,
     C    lSd5Calc, lNDdBCalc, lMLECalc, lJoCalc, ikpm2, s0table,
     C    s0kpm2, 
     P    Pol, Theta, Azm, S0obs, S0KpA, S0KpB, S0KpC, lDataOK,
     I    uc, vc,
     T    uc5, vc5,
     T    U5, D5, S05, S0sd5, E5, Jdepart, Jvar,
     O    U, D, S0, S0sd, E, dJdepart, dJvar )

c     ------------------------------------------------------------------

c!#   5. Print debug output if desired.

      if (jDebug .gt. 0) then

c     5.2 Starting message

        write (*, *) '>>>>Jscat debug output starts for jNiter ',
     &      jNiter, ' and jTask ', jTask

c     5.3 Heading

        write (*, '(2a7,a10,3a18)')
     &      'jNiter', 'jTask', 'Nscat', 'Jscat', 'Jdepart', 'Jvar'
        write (*, '(2i7,i10,3e18.7)')
     &      jNiter, jTask, Nscat, Jscat, Jdepart, Jvar
        write (*, '(a10,(t11, 20a5))') '*',
     &      ' Sat','MFun','Iter','Task',
     &      '  VD','  S0','  Sd','  ND',
     &      'Calc','  Jo','  Ad','Dbug'
        write (*, '(a10,(t11, 20i5))') 'j:', (j,j=1,12)
        write (*, '(a10,(t11, 20i5))') 'iCntl(j):', 3, 3,
     &       jniter, jtask, ivdcalc, is0calc, isdcalc, indcalc,
     &       icalc, ijocalc, iadcalc, jdebug
        write (*, '(a6,a5,a5,a8,6a18)') 'Obs:', 'N', 'Pol',
     &      'DataOK', 'Theta', 'Azm', 'S0obs',
     &      'S0KpA', 'S0KpB', 'S0KpC'
        write (*, '(a6,7a18)') 'Calc:', 'uc', 'vc', 'U', 'D',
     &      'S0', 'S0sd', 'E'

c     5.4 Debug info for selected obs

        do 550 j = 1, N, jDebug

          write (*, '(a6,i5,i5,l8,6e18.7)') 'Obs:', j, Pol(j),
     &        lDataOK(j), (180/Pi)*Theta(j), (180/Pi)*Azm(j), S0obs(j),
     &        S0KpA(j), S0KpB(j), S0KpC(j)
          write (*, '(a6,7e18.7)') 'Traj:', uc5(j), vc5(j), U5(j),
     &        (180/Pi)*D5(j), S05(j), S0sd5(j), E5(j)
          write (*, '(a6,7e18.7)') 'Adj:', uc(j), vc(j), U(j),
     &        D(j)/(180/Pi), S0(j), S0sd(j), E(j)

  550   continue
        write (*, *) '<<<<Jscat debug output  ends  for jNiter ',
     &      jNiter, ' and jTask ', jTask
      endif

c     ------------------------------------------------------------------

c!##  ERROR HANDLING :

      return
      end subroutine jscenv

      end module jscenv_m	!#
