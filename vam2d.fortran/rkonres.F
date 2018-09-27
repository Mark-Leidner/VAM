c!#   $Id: rkonres.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: rkonres.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
*---------------------------------------------------------------------
*   RANK.FOR   --   RANKING ROUTINES
*---------------------------------------------------------------------
*  CALL RKONRES ( VSOL, DSOL, RSOL, S0, AI, BA, KP, NVS, IRK1 )
*
*  CALL RKONBGD ( VSOL, DSOL, RSOL, VBG, DBG, NVS, IRK1 )
*
*  CALL RKONLFD ( VSOL, DSOL, RSOL, DLOCAL, DSD, RKONBG, NVS, IRK1 )
*
*  CALL RANK    ( VSOL, DSOL, RSOL, NVS, IRK1 )
*---------------------------------------------------------------------
C ROUTINE          RKONRES  SUBROUTINE  R0011  VAXFORT  RANK.FOR
C
C PURPOSE          RANK SOLUTIONS ON RESIDUAL PROBABILITIES
C
C PROJECT          EDIPVS
C
C ARGUMENTS        VSOL  (ENT/EX)  R4  4 SPEEDS (M/S)
C                  DSOL  (ENT/EX)  R4  4 DIRECTIONS (DEG.T)
C                  RSOL  (EXIT)    R4  4 RELATIVE PROBABILITIES (0-1)
C                  S0    (ENTRY)   R4  3 SIGMA-0 VALUES (DB)
C                  AI    (ENTRY)   R4  3 BEAM INCIDENCE ANGLES (DEG)
C                  BA    (ENTRY)   R4  3 BEAM AZIMUTH ANGLES (DEG.T)
C                  KP    (ENTRY)   R4  3 BEAM KP VALUES (PERCENT)
C                  NVS   (EXIT)    I4  NO. OF VALID SOLUTIONS (0-4)
C                  IRK1  (EXIT)    I4  PREVIOUS RANK OF NEW RANK 1 (1-4)
C
C SUBPROGRAMS      PROB, RANK, RESID, RETPROB
C
C INTRINSICS       ALOG10, MAX, SQRT
C
C DESCRIPTION      CALCULATES RESIDUAL FROM MODEL EACH SOLUTION,
C                  AND PROBABILITY FROM (RMS) RESIDUAL, GIVEN
C                  AN ESTIMATE OF THE S.DEV IN S0'S. THEN RANKS
C                  IN DECENDING ORDER OF (RELATIVE) PROBABILITIES.
C                  RANK 1 PROBABILITY REPLACED BY LIKELY (VALIDATED)
C                  VALUE & RKS 2-4 ADJUSTED TO KEEP RELATIVE VALUES
C                  AND SO THAT SUM OF ALL RANKS REMAINS UNITY.
C
C VERSION          1.00  NOVBEMBER 1992  ORIG: D.OFFILER  NOV 92
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
      SUBROUTINE RKONRES ( VSOL, DSOL, RSOL, S0, AI, BA, KP, NVS, IRK1 )

      IMPLICIT NONE

      INTEGER NB, NS
      REAL    EPS, S0MIN, CALVAR, FITVAR, KPSC, KPNVAR, VMAX, RMIN
      PARAMETER ( NB = 3, NS = 4 )! NB = no. beams/NS = max no. solns
      PARAMETER ( EPS    =  1E-7 ) ! an arbitrary small value
      PARAMETER ( CALVAR =  0.04 ) ! beam calibration error (0.2dB rms)
      PARAMETER ( FITVAR =  0.04 ) ! c-band model fit error (0.2dB rms)
      PARAMETER ( KPSC   =   3.0 ) ! Kp scaling for `geophysical' noise
      PARAMETER ( KPNVAR =  0.04 ) ! nominal Kp error (approx 5%)
      PARAMETER ( S0MIN  = -50.0 ) ! beam considered missing if s0<-50dB
      PARAMETER ( VMAX   =  50.8 ) ! max wind speed for a valid solution
      PARAMETER ( RMIN   = 0.001 ) ! minimum prob. for valid retrieval
                                   ! (before scaling) - equivalent to an 
                                   ! rms residual of ~2.2db with total s0 
                                   ! s.dev=0.64db (Kp~5%)
* ARGUMENT LIST PARAMETERS

      INTEGER NVS, IRK1
      REAL    VSOL(NS), DSOL(NS), RSOL(NS), S0ERR
      REAL    S0(NB), AI(NB), BA(NB), KP(NB)

* LOCAL PARAMETERS

      INTEGER IB, IS
      REAL    KPSD, RES, RMAX, RTOT, R1, RR
      REAL    PROB, RESIDUAL

* ESTIMATE S.DEV OF ERROR IN S0'S 

      S0ERR = 0.0
      DO 10 IB = 1, NB
         IF ( S0(IB) .GT. S0MIN ) THEN
            KPSD  = 10.0 * ALOG10 ( 1.0 + KPSC*KP(IB)*0.01 )
            S0ERR = S0ERR + KPSD * KPSD
         ELSE
            S0ERR = S0ERR + KPNVAR
         ENDIF
   10 CONTINUE
      S0ERR = SQRT ( ( S0ERR / NB ) + CALVAR + FITVAR )

* CALCULATE PROBABILITIES FOR EACH SOLUTION'S RMS RESIDUAL, GIVEN S0 ERROR

      RMAX = 0.0
      DO 20 IS = 1, NS
         IF ( VSOL(IS) .LE. VMAX ) THEN
            RES      = RESIDUAL( VSOL(IS), DSOL(IS), S0, AI, BA )
            RSOL(IS) = PROB  ( SQRT ( RES / NB ), S0ERR )
            RMAX     = MAX   ( RMAX, RSOL(IS) )
         ELSE
            RSOL(IS) = 0.0
         ENDIF
   20 CONTINUE

* RANK SOLUTIONS IN ORDER OF SCALED (RELATIVE) PROBABILITIES

      CALL RANK ( VSOL, DSOL, RSOL, NVS, IRK1 )

* UNFORTUNATELY, THE RANK 1 RELATIVE PROBABILITY (RSOL(1)) IS NOT A 
* GOOD PREDICTOR OF WHETHER THIS SOLUTION IS CORRECT OR NOT.
* WE THEREFORE CALCULATE A (PRE-VALIDATED) SUBSTITUTE VALUE,
* BASED ON NO. OF VALID SOLUTIONS, WIND SPEED AND INCIDENCE ANGLE. 
* SCALE OTHER RSOLS TO MAINTAIN RELATIVE SIZE AND UNITY SUM

      IF ( RMAX .GE. RMIN .AND. NVS .GT. 0 ) THEN
         CALL RETPROB ( NVS, AI(2), VSOL(1), R1 )
         RR      = R1 / RSOL(1)
         RSOL(1) = R1

         IF ( NVS .GT. 1 ) THEN
            RTOT    = 0.0
            DO 30 IS = 2, NS
               RSOL(IS) = RSOL(IS) * RR
               RTOT     = RTOT + RSOL(IS)
   30       CONTINUE
         
            NVS = 1
            R1  = ( 1.0 - R1 ) / RTOT
            IF ( RTOT .GT. EPS ) THEN
               DO 40 IS = 2, NS
                  RSOL(IS) = MIN ( RSOL(IS) * R1, RSOL(IS-1) * 0.99 )
                  IF ( RSOL(IS) .GT. RMIN ) NVS = NVS + 1
   40          CONTINUE
            ENDIF
         ENDIF

* RETRIEVAL REJECTED IF HIGHEST PROBABILTY 'RMAX' TOOLOW 
* (IE POOR RESIDUAL FIT TO C-BAND MODEL)

      ELSE
         NVS = 0
      ENDIF

      END
