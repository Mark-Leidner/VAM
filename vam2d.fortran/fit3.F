c!#   $Id: fit3.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: fit3.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C ROUTINE          FIT3  SUBROUTINE  U0031  FORT77  FIT3.FOR
C
C PURPOSE          FITS A QUADRATIC CURVE TO THREE POINTS
C
C ARGUMENTS        X   (ENTRY)  3R4  ARRAY OF 3 X (INEPENDENT) COORDS
C                  Y   (ENTRY)  3R4  ARRAY OF 3 Y (DEPENDENT) COORDS
C                  C0  (EXIT)    R4  CONSTANT COEFFICIENT
C                  C1  (EXIT)    R4  FIRST-ORDER COEFFICIENT
C                  C2  (EXIT)    R4  SECOND-ORDER COEFFICIENT
C
C INTRINSICS       ABS
C
C DESCRIPTION      GIVEN THREE COORDINATE PAIRS (X1,Y1), (X2,Y2)
C                  AND (X3,Y3), FIT3 WILL FIT AN EXACT QUADRATIC
C                  CURVE PASSING THROUGH THESE 3 POINTS, AND RETURN
C                  THE QUADRATIC CURVE COEFFICIENTS C0, C1 AND C2
C                  SUCH THAT:
C                     Y = C0 + C1 * X + C2 * X**2
C                  THE METHOD IS ONE OF SIMULTANEOUS EQUATIONS
C                  WITH SUBSTITUTION, FIRST TO SOLVE FOR C2,
C                  THEN C1 AND FINALLY C0. THE THREE 'X' VALUES
C                  MUST BE DISTINCT AND IN ASCENDING OR DESCENDING
C                  NUMERICAL ORDER.
C
C VERSION          1.01  OCTOBER 1992  ORIG: D. OFFILER  SEPT 89
C
C CHANGES          01  RE-ORDER COEFS IN ARGUMENT LIST.  D.O.  14 OCT 92
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
c
      SUBROUTINE FIT3 ( X, Y, C0, C1, C2 )

* ARGUMENT LIST PARAMETERS

      REAL C0, C1, C2, X(*), Y(*)

      DATA EPS / 1E-10 /

      X12  = X(1) - X(2)
      X13  = X(1) - X(3)
      X1S  = X(1) * X(1)
      X12S = X1S - X(2) * X(2)
      X13S = X1S - X(3) * X(3)

      A1 = X13 * ( Y(1) - Y(2) ) - X12 * ( Y(1) - Y(3) )
      A2 = X13 * X12S - X12 * X13S
      IF ( ABS ( A2 ) .GT. EPS ) THEN
         C2 = A1 / A2
      ELSE
         C2 = 0.0
      ENDIF

      IF ( ABS ( X13 ) .GT. EPS ) THEN
         C1 = ( Y(1) - Y(3) - C2 * X13S ) / X13
      ELSE
         C1 = 0.0
      ENDIF

      C0 = Y(1) - C2 * X1S - C1 * X(1)

      END
