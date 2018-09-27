c!#   $Id: residual.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: residual.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C ROUTINE          RESID  FUNCTION  R0002  VAXFORT  RESID.FOR
C
C PURPOSE          CALCULATES RESIDUAL SUM OF SQUARES
C
C PROJECT          EDIPVS
C
C ARGUMENTS        VV     (ENTRY)  R4  WIND SPEED (M/S)
C                  DG     (ENTRY)  R4  WIND DIRECTION W.R.T. NORTH (DEG)
C                  S0     (ENTRY)  R4  3 MEASURED SIGMA-0 VALUES (DB)
C                  AI     (ENTRY)  R4  3 BEAM INCIDENCE ANGLES (DEG)
C                  BA     (ENTRY)  R4  3 BEAM AZIMUTH ANGLES WRT NTH (DEG)
C                  RESID  (FUNC)   R4  RESIDUAL SUM OF SQUARES
C             
C SUBPROGRAMS      TSIGDB
C
C DESCRIPTION      CALCULATES THE SUM OVER THE 3 BEAMS OF THE
C                  SQUARES OF THE RESIDUAL DIFFERENCE BETWEEN
C                  THE MEASURED BACKSCATTER AND THEORETICAL
C                  C-BAND MODEL VALUES (BOTH IN DB MEASURE),
C                  GIVEN THE CELL GEOMETRY.
C
C VERSION          1.02  NOVBEMBER  1992  ORIG: D. OFFILER  SEP 89
C
C CHANGES          01  RESIDUAL NO LONGER NORMALISED; USE DB
C                      INSTEAD OF LINEAR MEASURE; RE-ORDER REMAINING
C                      ARGUMENT LIST PARAMETERS.     D.O.   7 OCT 92
C                  02  MISSING BEAMS TAKEN FROM S0 VALUE INSTEAD OF
C                      SEPARATE LOGICAL FLAG ARRAY.  D.O.  26 NOV 92
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
      FUNCTION RESIDUAL ( VV, DD, S0, AI, BA )

      IMPLICIT NONE

      integer NB 
      real    S0MIN
      PARAMETER ( NB = 3, S0MIN = -50.0 )

      real    RESIDUAL, VV, DD, S0(NB), AI(NB), BA(NB)

      integer IB
      real    R, TSIGDB

      RESIDUAL = 0.0
      DO 10 IB = 1, NB
         IF ( S0(IB) .GT. S0MIN ) THEN
            R     = S0(IB) - TSIGDB ( VV, BA(IB)-DD, AI(IB) )
            RESIDUAL = RESIDUAL + R * R
         ENDIF
   10 CONTINUE

      END
