c!#   $Id: retprob.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: retprob.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C ROUTINE          RETPROB  SUBROUTINE  R0005  FORT77  RETPROB.FOR
C
C PURPOSE          RETRIEVAL AMBIGUITY REMOVAL CONFIDENCE VALUE
C
C PROJECT          EDIPVS
C
C ARGUMENTS        NVS   (ENTRY)  I4  NO. OF VALID SOLUTIONS (2-4)
C                  AIM   (ENTRY)  R4  MID BEAM INCIDENCE ANGLE (15-50DEG)
C                  SPD   (ENTRY)  R4  WIND SPEED (0-50M/S)
C                  PRB   (EXIT)   R4  CALC. PROBABILITY VALUE (0.0-1.0)
C
C DESCRIPTION      CALCULATES AN `AMBIGUITY REMOVAL CONFIDENCE' VALUE
C                  AFTER BASIC RETRIEVAL. PRB IS A FUNCTION OF THE
C                  NUMBER OF VALID SOLUTIONS RETRIEVED, THE MID-BEAM 
C                  INCIDENCE ANGLE AND THE RETRIEVED (RANK 1) WIND SPEED. 
C                  THIS VERSION TUNED TO CMOD4.
C
C REFERENCES       COEFFICIENTS FOUND & VALIDATED USING PROGRAM
C                  ARCVAL (VXXXX).
C
C VERSION          1.00  NOVEMBER 1992  ORIG: D. OFFILER  NOV 92
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
      SUBROUTINE RETPROB ( NVS, AIM, SPD, PRB )

      IMPLICIT NONE

* ARGUMENT LIST PARAMETERS

      integer NVS
      real    AIM, SPD, PRB

* LOCAL PARAMETERS

      integer IA, IS, IV
      real    PRBA(7,4), PRBV(6,4)

* PRBA = MEAN RK1 PERFORMANCE BY INC.ANG (15-20,20-25...), AND BY NO. OF
*        SOLUTIONS, FOR ALL SPEEDS
* PRBV = MEAN RK1 PERFORMANCE BY SPD (0-4,4-8...) AND BY NO. OF
*        SOLUTIONS, FOR ALL INC.ANG.

      DATA PRBA  / 7*0.50,
     $             0.53, 0.59, 0.62, 0.61, 0.63, 0.64, 0.62,
     $             0.51, 0.48, 0.54, 0.51, 0.53, 0.54, 0.54,
     $             0.49, 0.45, 0.48, 0.48, 0.54, 0.55, 0.56 /
      DATA PRBV  / 6*0.50,
     $             0.52, 0.60, 0.65, 0.73, 0.80, 0.90,
     $             0.49, 0.48, 0.60, 0.78, 0.90, 0.90,
     $             0.48, 0.48, 0.53, 0.49, 0.90, 0.90 /

      IS = MIN ( MAX ( NVS, 1 ), 4 )

      IA = INT ( AIM - 15.0 ) / 5 + 1
      IA = MIN ( MAX ( IA, 1 ), 7 )

      IV = INT ( SPD ) / 4 + 1
      IV = MIN ( MAX ( IV, 1 ), 6 )

      PRB = ( PRBA(IA,IS) + PRBV(IV,IS) ) * 0.5

      END
