c!#   $Id: winret.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: winret.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C ROUTINE          WINRET  SUBROUTINE  R0001  VAXFORT  WINRET.FOR
C
C PURPOSE          SINGLE-CELL RANKED AMBIGUOUS WIND RETRIEVAL
C
C PROJECT          EDIPVS
C
C ARGUMENTS        S0    (ENTRY)  R4  3 MEASURED SIGMA-0 VALUES (DB)
C                  AI    (ENTRY)  R4  3 BEAM INCIDENCE ANGLES (DEG)
C                  BA    (ENTRY)  R4  3 BEAM AZIMUTH ANGLES (DEG)
C                  KP    (ENTRY)  R4  3 KP NOISE ESTIMATES (PERCENT)
C                  MS    (ENTRY)  I2  3 MISSING SHOT COUNTERS
C                  PCD   (ENT/EX) I2  PRODUCT CONFIDENCE FLAGS
C                  VSOL  (EXIT)   R4  4 RETRIEVED WIND SPEEDS (M/S)
C                  DSOL  (EXIT)   R4  4 RETRIEVED WIND DIRECTIONS (DEG)
C                  RSOL  (EXIT)   R4  4 RELATIVE PROBABILITY VALUES (0-1)
C
C SUBPROGRAMS      FIT3, QSPEED, RESIDUAL, RKONRES
C
C INTRINSICS       BTEST, IBSET, IBCLR, MAX, MIN
C
C DESCRIPTION      GIVEN A SET OF 3 SIGMA-0 MEASUREMENTS ASSOCIATED DATA,
C                  PERFORMS A SEARCH FOR LOCAL MINIMA IN RESIDUAL TO
C                  PRODUCE AN AMBIGUOUS SET OF WIND VECTOR RETRIEVALS. 
C                  THESE ARE THEN RANKED IN ORDER OF PROBABILITY (BASED 
C                  ON SOLUTION RESIDUAL).
C
C ALGORITHM        1) STEP OVER A COMPLETE 360 DEG IN WIND DIRECTION
C                     (PLUS SOME OVERLAP); FOR EACH STEP, ESTIMATE
C                     WIND SPEED DIRECTLY FROM SIGMA-0 TRIPLET & THENCE 
C                     SUM OF RESIDUALS VS. C-BAND MODEL.
C                  2) SEARCH OVER WIND DIRECTION FOR A LOCAL MINIMA IN 
C                     RESIDUAL.
C                  3) ACTUAL POSITION OF EACH MINIMUM (WIND DIRECTION)
C                     IS ESTIMATED FROM A QUADRATIC FITTED TO THE MINIMUM 
C                     AND THE TWO NEIGHBOURING RESIDUALS. THE SPEED 
C                     AND RESIDUALS ARE RE-ESTIMATED AT THIS POINT.
C                  4) REPEAT 2-3 UNTIL WHOLE CIRCLE SEARCHED OR FOUR
C                     MINIMA FOUND.
C                  5) CALCULATE PROBABILITY FOR EACH SOLUTION FROM
C                     RESIDUAL AT SOLUTION
C                  6) RANK THE SOLUTIONS IN DECREASING ORDER OF 
C                     PROBABILITY VALUES.
C                                                
C VERSION          1.09  JANUARY 1993  ORIG: D. OFFILER  SEP 89
C
C CHANGES          01  A) FIX TYPO ERROR SETTING RD(1).
C                      B) MAKE DD ARRAY LINEAR INSTEAD OF 0-360; THIS 
C                         WAS CAUSING MIMIMA DETECTED NEAR ZERO DEG. 
C                         TO GENERATE INCORRECT FIT3 COEFFICIENTS, AND 
C                         HENCE INCORRECT DIRECTION.
C                      C) REMOVE 'ANG360' CALL FOR IN-LINE CODE.
C                      D) CHECK KP LIMIT.                 D.O.     JAN 91
C                  02  TUNE UPDATE ARC VALUES.            D.O.  21 AUG 91
C                  03  A) LIMIT RETRIEVED WIND SPEEDS TO VALID RANGE
C                         0.2 - 50.8 M/S
C                      B) SET AN INVALID SOLUTION IF FIT3 'C2' COEFF IS 
C                         ZERO (WAS SETTING SPEED/DIR TO ZEROS)
C                      C) ONLY CLEAR 'NO RETR' FLAG IF RK 1 GOOD.
C                                                         D.O.  21 OCT 91
C                  04  A) SET ALL SOLUTIONS MISSINGINITIALLY.
C                      B) ABORT PROCESSING EARLIER WHEN LAND SET.
C                      C) RE-SET ALL SOLUTIONS MISSING IF RK 1 NO GOOD.
C                                                         D.O.   4 NOV 91
C                  05  A) SET 'MISSING' RESIDUAL VALUES TO -9999.0
C                         INSTEAD OF -999.0.              D.O.   6 JAN 92
C                      B) IGNORE 'BEAM ARCING' FLAGS.     D.O.  10 JAN 92
C                      C) ASSUME MISSING KP'S = 10%.      D.O.  28 JAN 92
C                  06  NO LONGER SCALE RESIDUAL WITH KP.  D.O.  21 JUL 92
C                  07  A) NO LONGER SCALE QSPEED WITH KP.
C                      B) NO SCALING AT ALL IN RESIDUAL, S0 LEFT
C                         IN DB; RSOL IS RETURNED AS RMS DIFF IN DB.
C                      C) SEARCH ALGORITHM MODIFIED; PERFORM CONSECUTIVE
C                         3-NEIGHBOUR CHECK INSTEAD OF SEARCHING FOR
C                         ABSOLUTE MINIMUM RESIDUAL WITHIN A `QUADRANT'.
C                      D) REJECT CASES WITH LARGE RSOL'S.
C                      E) ARC VALUE CALCULATED BY S/R RESARC.
C                                                         D.O.  19 OCT 92
C                  08  A) REDUCE KP THRESHOLD FROM 20% TO 10% (POOR
C                         SIGMA-0 VALUES ON FIRST ROW OF CELLS AFTER NEW
C                         START OF SWATH).
C                      B) RANK ON RSOL AS RELATIVE RESIDUAL PROBABILITES 
C                         (DONE IN S/R RKONRES). ARC NOW AS RSOL(1)
C                      C) ONLY RETRIEVE 3-BEAM CASES WITH PCD=0
C                                                         D.O.  25 NOV 92
C                  09  SCAN FOR MINIMA WAS STOPPED BEFORE PENULTIMATE
C                      RESIDUAL IN ARRAY (330/345/360 DEG)- CORRECTED.
C                                                         D.O.   5 JAN 92
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
      SUBROUTINE WINRET ( S0, AI, BA, KP, MS, PCD,
     1 VSOL, DSOL, RSOL, nvs )

      IMPLICIT NONE

      INTEGER NB, NS, ND, NF
      REAL    DINC, KPLIM, VMIN, VMAX
      REAL    EPS, MTHRES
      PARAMETER ( NB = 3, NS = 4, ND = 26, NF = 3 )
      PARAMETER ( DINC = 15, KPLIM = 10.0, VMIN = 0.2, VMAX = 50.8 )
      PARAMETER ( EPS = 1.0E-7, MTHRES = 20 )

* ARGUMENT LIST PARAMETERS

      INTEGER*2 MS(NB), PCD
      REAL      S0(NB), AI(NB), BA(NB), KP(NB)
      REAL      VSOL(NS), DSOL(NS), RSOL(NS)

* LOCAL PARAMETERS

      INTEGER ISTART(4)
      INTEGER IB, IS, ID, IRK1, MSTOT, NVS
      REAL    DD(ND), RD(ND), D3(NF), R3(NF)
      REAL    C0, C1, C2, RMAX, VG, DG
      REAL    QSPEED, RESIDUAL
                                         
* ASSUME KP'S OK, NO RETRIEVAL MADE & NO AMBIGUITY REMOVAL DONE;
* INITIALLY SET ALL SOLUTIONS TO 'MISSING' VALUES.

      PCD = IBCLR ( PCD,  6 )
      PCD = IBSET ( PCD,  8 )
      PCD = IBSET ( PCD,  9 )
      PCD = IBSET ( PCD, 11 )
      PCD = IBCLR ( PCD, 12 )
      PCD = IBCLR ( PCD, 13 )
      PCD = IBCLR ( PCD, 14 )

      DO 110 IS = 1, NS 
         VSOL(IS) = 51.0
         DSOL(IS) = 510.0 
         RSOL(IS) = 0.0
  110 CONTINUE                       

* CHECK KP'S; ABORT IF ANY BEAM MISSING OR ARCING OR KP ABOVE 
* THRESHOLD, OR CELL IS FLAGGED AS NOT SEA, OR FRAME CHECKSUM ERROR
* (ANY OF PCD BITS 1-8,11 SET) OR TOO MANY MISSING SHOTS.
cpw
cpw  Kp's and missing shots filtered in calling main program
cpw
cpw      MSTOT  = 0
cpw      DO 120 IB = 1, NB
cpw         IF ( KP(IB) .LT. 0 .OR. KP(IB) .GT. KPLIM ) 
cpw     $      PCD = IBSET ( PCD, 6 )
cpw         MSTOT = MSTOT + ABS ( MS(IB) )
cpw  120 CONTINUE      
cpw      IF ( IAND ( PCD, '04FF'X ) .NE. 0 .OR.
cpw     $     MSTOT .GT. MTHRES ) RETURN

* ESTIMATE SPEED & HENCE RESIDUAL FOR SAMPLED SET OF DIRECTIONS OVER
* THE RANGE 0-345DEG; COPY 0 TO 360 AND 345 TO -15 TO GIVE SOME OVERLAP
* AROUND 0DEG.
      
      DG   = 0.0
      DO 130 ID = 2, ND-1
         VG     = QSPEED ( DG, S0, AI, BA )
         DD(ID) = DG
         RD(ID) = RESIDUAL  ( VG, DG, S0, AI, BA )
         DG     = DG + DINC
  130 CONTINUE
      DD(1)    = DD(ND-1) - 360.0
      RD(1)    = RD(ND-1)
      DD(ND)   = DD(2)    + 360.0
      RD(ND)   = RD(2)

* SEARCH FOR LOCAL RESIDUAL MINIMA

      ID   = 2
      IS   = 0
  210 CONTINUE
         IF ( RD(ID) .LT. RD(ID-1) .AND.             
     $        RD(ID) .LT. RD(ID+1) ) THEN
 
* INTERPOLATE DIRECTION TO RESIDUAL MINIMUM WITH A QUADRATIC FIT
* TO THE THREE POINTS ABOUT THE MINIMUM. 

            D3(1) = DD(ID-1)
            D3(2) = DD(ID)
            D3(3) = DD(ID+1)
            R3(1) = RD(ID-1)
            R3(2) = RD(ID)
            R3(3) = RD(ID+1)
            CALL FIT3 ( D3, R3, C0, C1, C2 )

* DIRECTION ESTIMATE IS AT MINIMUM OF QUADRATIC FROM FIT3
* (IE dR/dD = 0 = C1 + 2*C2*D WHERE R = C0 + C1*D + C2*D**2);
* RE-ESTIMATE SPEED AT THIS SOLUTION.

            IF ( ABS ( C2 ) .GT. EPS ) THEN
               IS = IS + 1
               DSOL(IS) = AMOD ( -C1 / ( 2.0 * C2 ), 360.0 )
               IF ( DSOL(IS) .LT. 0.0 ) DSOL(IS) = DSOL(IS) + 360.0
               VSOL(IS) = QSPEED ( DSOL(IS), S0, AI, BA )
               VSOL(IS) = MIN    ( MAX ( VSOL(IS), VMIN ), VMAX )
               ID = ID + 1
            ENDIF
         ENDIF

         ID = ID + 1
         IF ( ID .LT. ND .AND. IS .LT. NS ) GOTO 210
*     END-CONTINUE-210

* RANK IN DECREASING ORDER OF (RELATIVE) PROBABILITY OF RESIDUALS
               
      CALL RKONRES ( VSOL, DSOL, RSOL, S0, AI, BA, KP, NVS, IRK1 )

* IF AT LEAST TWO PROBABLE SOLUTIONS, FLAG A RETRIEVAL DONE.
* IF ONLY ONE OR NO VALID SOLUTIONS (NONE RETRIEVED OR BEST SOLUTION 
* WAS OF LOW PROBABILITY), RESET ALL SOLUTIONS AS MISSING DATA.

      IF ( NVS .GE. 2 ) THEN
         PCD = IBCLR ( PCD, 11 )
      ELSE
         DO 310 IS = 1, NS 
            VSOL(IS) = 51.0
            DSOL(IS) = 510.0 
            RSOL(IS) = 0.0
  310    CONTINUE
      ENDIF

      END
