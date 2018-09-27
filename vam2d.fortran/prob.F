c!#   $Id: prob.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: prob.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C ROUTINE          PROB  FUNCTION  U0071  VAXFORT  PROB.FOR
C
C PURPOSE          CALCULATE PROBABILITY OF ERROR VS A BACKGROUND
C
C PROJECT          EDIPVS
C
C ARGUMENTS        DIFF  (ENTRY)  R4  OBSERVATION-BACKGROUND DIFFERENCE
C                  SDEV  (ENTRY)  R4  ESTIMATED SDANDARD DEVIATION IN
C                                     BACKGROUND ERRORS.
C                  PROB  (FUNCN)  R4  PROBABILITY DENSITY (0.0 - 1.0)
C
C INTRINSICS       EXP
C
C DESCRIPTION      CALCULATES THE PROBABILITY DENSITY OF A MEASUREMENT 
C                  DIFFERENCE FROM A BACKGROUND GIVEN THE LIKELY 
C                  STANDARD DEVIATION IN BACKGROUND ERRORS (ASSUMED 
C                  GAUSSIAN, WITH ZERO MEAN DIFFERENCE). PROBABILITY
C                  DENSITY IS SCALED TO GIVE UNITY FOR ZERO DIFFERENCE.
C                  EXAMPLES:
C                    DIFF = 0       -->  PROB = 1.000
C                    DIFF = SDEV    -->  PROB = 0.606
C                    DIFF = 2*SDEV  -->  PROB = 0.135
C                    DIFF = 3*SDEV  -->  PROB = 0.011
C
C ALGORITHM        PROB = EXP(-(DIFF/SDEV)**2/2)
C
C VERSION          1.00  NOVEMBER 1992  ORIG:  D. OFFILER  NOV 92
C
c
c Recieved by P.WOICESHYN (JPL/NOAA-NMC) from Dave OFFILER, UKMO, Bracknell, UK
c	for use and modification as part of the ESA calibration
c	and validation team activity -- FEB 18, 1993.
c
c	Transfer of this code to other users is not permitted without receiving
c	prior permission from Woiceshyn and Offiler.
c
c	Errors and modifications of this code are to be reported to Woiceshyn.
c	
c
c
      FUNCTION PROB ( DIFF, SDEV )

      IMPLICIT NONE

      REAL DIFF, SDEV, PROB, VAR

      PROB = EXP ( -0.5 * DIFF * DIFF / MAX ( SDEV * SDEV, 1E-10 ) )

      END
