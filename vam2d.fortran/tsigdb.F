c!#   $Id: tsigdb.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: tsigdb.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
*-----------------------------------------------------------------------
*   TSIG0.FOR   ---   TABULATED SIGMA0 AS A FUNCTION OF WIND SPEED,
*                     DIRECTION AND INCIDENCE ANGLE, FOR BACKSCATTER
*                     IN DB (TSIGDB) AND LINEAR (TSIGLN)
*-----------------------------------------------------------------------
* S0DB = TSIGDB ( U, D, A )
*
* S0LN = TSIGLN ( U, D, A )
*-----------------------------------------------------------------------
C ROUTINE          TSIGDB  FUNCTION  M0002  VAXFORT  TSIG0.FOR
C
C PURPOSE          INTERPOLATED TABLULAR SIGMA-0 (DB)
C
C PROJECT          EDIPVS
C
C ARGUMENTS        U      (ENTRY)  R4  WIND SPEED (M/S)
C                  D      (ENTRY)  R4  REALATIVE WIND DIRN. (DEG)
C                  A      (ENTRY)  R4  INCIDENCE ANGLE (DEG)
C                  TSIGDB  (FUNC)  R4  SIGMA-0 (DB)
C
C COMMON           /CMODEL/
C                  MODEL  (WRITE)  C10  MODEL ID
C
C FILES            UNIT(11)  FILE(CMOD_DBLUT)  SIGMA-0 L.U.T. (I/P)
C
C INTRINSICS       ABS, INT, MIN, SIGN
C
C RTL              EXIT
C
C DESCRIPTION      INTERPOLATES SIGMA-0 (IN DB MEASURE) FROM 3-D 
C                  LOOK-UP TABLE OF BACKSCATTER VS. WIND SPEED 
C                  (1-30  M/S, IN 1 M/S STEPS), WIND DIRECTION
C                  (0-180 DEG, IN 5 DEG STEPS) AND INCIDENCE ANGLE
C                  (15-60 DEG, IN 1 DEG STEPS). 
C                  THE FIRST CALL TO THE ROUTINE LOADS THE LUT FROM
C                  DISK; THE FILE IS SPECIFIED VIA THE LOGICAL NAME
C                  'CMOD_DBLUT'
C
C ALGORITHM        FROM GIVEN PARAMETERS, FINDS NEAREST TABULATED
C                  VALUES, AND DOES A TRI-LINEAR INTERPOLATION.
C                  THE INTERPOLATION IS CAST IN A FORM WHICH MINIMISES
C                  THE NUMBER OF FLOATING-POINT OPERATORS (IE + - * / ),
C                  AND USES NO TRANSCENDENTAL FUNCTION CALLS.
C                  THE LOCAL GRADIENTS ARE EVALUATED AS THE DIFFERENCE
C                  BETWEEN NEIGHBOURING TABLE VALUES DIVIDED BY THE
C                  TABLE INCREMENT FOR EACH DIMENSION.
C
C REFERENCE        SIGMA-0 L.U.T. CREATED BY PROGRAM LUTGEN (M1000).
C                  AND TESTED BY PROGRAM SIGTEST (M2000).
C
C VERSION          1.02  OCTOBER 1992  ORIG: D. OFFILER  SEP 89
C
C CHANGES          01  LUT FILE REFERENCED VIA LOGICAL NAME.  D.O. APR 91
C                  02  ENHANCED LUT FILE.                     D.O. OCT 92
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
      FUNCTION TSIGDB ( U, D, A )

      IMPLICIT NONE

      integer ioopen,IPCHAN
      integer NU, ND, NA
      real    FU, FD, FA, DU, DD, DA
      PARAMETER ( IPCHAN = 40 )
      PARAMETER ( NU =  30, ND =  37, NA =   46 )
      PARAMETER ( FU = 1.0, FD = 0.0, FA = 15.0 )
      PARAMETER ( DU = 1.0, DD = 5.0, DA =  1.0 )

* ARGUMENT LIST PARAMETERS

      real TSIGDB, U, D, A                

* LOCAL PARAMETERS

      CHARACTER MODEL*10, SIGFIL*10
      LOGICAL FIRST
      integer STATUS, IU0, ID0, IA0, IU1, ID1, IA1
      real    DBLUT(NU,ND,NA)
      real    X, U0, D0, A0, RU0, RD0, RA0, RA1, RUD
      real    W0, X0, Y0, Z0, W1, X1, Y1, Z1

      COMMON / CMODEL / MODEL

      DATA SIGFIL / 'CMOD_DBLUT' /
      DATA FIRST  / .TRUE. /

* READ LOOK-UP TABLE ON FIRST ENTRY.

      IF ( FIRST ) THEN
          if (ioopen(IPCHAN) .ne. 0) then 
             PRINT *, '*** TSIGDB: Unable to open file'
             CALL EXIT(1)
          ENDIF
         READ  ( IPCHAN ) MODEL, DBLUT
         call ioclose(IPCHAN)
         FIRST = .FALSE.
      ENDIF

* FIND NEXT LOWER & NEXT HIGHER TABULATED ELEMENTS FOR EACH PARAMETER
* (RELATIVE DIRECTION 0-180 DEG ONLY, SINCE 180-360 IS SYMMETRICAL)

      X = ABS ( D )
   20 IF ( X .LE. 180.0 ) GOTO 30
         X = ABS ( X - SIGN ( 360.0, X ) )
         GOTO 20
   30 CONTINUE

      U0 = ( U - FU ) / DU + 1.0
      D0 = ( X - FD ) / DD + 1.0
      A0 = ( A - FA ) / DA + 1.0

      IU0 = MIN ( MAX ( INT ( U0 ), 1 ), NU - 1 )
      ID0 = MIN ( MAX ( INT ( D0 ), 1 ), ND - 1 )
      IA0 = MIN ( MAX ( INT ( A0 ), 1 ), NA - 1 )

      IU1 = IU0 + 1
      ID1 = ID0 + 1
      IA1 = IA0 + 1

* FIND OFFSETS FROM TABULATED VALUES

      RU0 = U0 - IU0
      RD0 = D0 - ID0
      RA1 = A0 - IA0
      RA0 = 1.0 - RA1
      RUD = RU0 * RD0

* INTERPOLATION COEFFICIENTS

      W0 = DBLUT(IU0,ID0,IA0)
      X0 = DBLUT(IU0,ID1,IA0) - W0
      Y0 = DBLUT(IU1,ID0,IA0) - W0    
      Z0 = DBLUT(IU1,ID1,IA0) - W0 - X0 - Y0

      W1 = DBLUT(IU0,ID0,IA1)
      X1 = DBLUT(IU0,ID1,IA1) - W1
      Y1 = DBLUT(IU1,ID0,IA1) - W1
      Z1 = DBLUT(IU1,ID1,IA1) - W1 - X1 - Y1

* INTERPOLATED BACKSCATTER VALUE

      TSIGDB = ( W0 + X0 * RD0 + Y0 * RU0 + Z0 * RUD ) * RA0
     $       + ( W1 + X1 * RD0 + Y1 * RU0 + Z1 * RUD ) * RA1

      END
