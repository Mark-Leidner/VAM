c!#   $Id: taudat.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: taudat.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      SUBROUTINE TAUDAT(IERR,T,DATES,LOOPA,ICNVRT)
C---------------------------------------------------------------------
C    SUBROUTINE: TAUDAT    DATE: 4/24/84     AUTHOR: UNKNOWN
C
C    ENGLISH NAME:  TAU/DATE TO DATE/TAU
C
C    LANGUAGE: FORTRAN H EXTENDED (ENHANCED)
C
C    PURPOSE: TO CONVERT TAU TO DATE (MM DD YY HH) OR VICE VERSA.
C
C    CALLING SEQUENCE:
C    -----------------
C
C    CALL TAUDAT (IERR, TAU, IDATES, LOOPA, ICNVRT)
C
C    WHERE:  IERR: (I*4) ERROR FLAG (ONE FOR NO ERRORS)
C             TAU: (R*4) GLAS ABSOLUTE TIME COUNTER (TAU=1: 10/29/72 0Z)
C          IDATES: (I*4) DATE ARRAY DIMENSIONED (4)
C                        IDATES(1) = MONTH
C                        IDATES(2) = DAY
C                        IDATES(3) = YEAR
C                        IDATES(4) = HOUR (GREENWICH MEAN TIME)
C           LOOPA: (I*4) NUMBER OF VALUES TO CONVERT (UP TO 2)
C                        (IF LOOPA=2, TAU SHOULD BE DIMENSIONED (2) AND
C                         IDATES SHOULD BE DIMENSIONED (4,2))
C          ICNVRT: (I*4) CONVERSION INDEX --
C                        ICNVRT=1: CONVERT DATE TO TAU
C                        ICNVRT=2: CONVERT TAU TO DATE
C
C    NOTE: THIS IS A DIRECT COPY OF THE OLD 'TAUCNV' ROUTINE EXCEPT THAT
C          ALL INPUTS AND OUTPUTS ARE IBM FULL-PRECISION.
C
C    EXAMPLES:
C    ---------
C
C    1) CONVERT TAU OF 54216. TO A DATE:
C
C           CALL TAUDAT (IERR,54216.,IDATES,1,2)
C
C    2) CONVERT DATE OF 1/5/79 12Z TO TAU:
C
C           CALL TAUDAT (IERR,TAU,IDATES,1,1)
C
C           WHERE: IDATES(1)=1, IDATES(2)=5, IDATES(3)=79, IDATES(4)=12
C
C    3) CONVERT 2 TAUS (54216. AND 55312.) TO DATES:
C
C       INTEGER IDATES(4,2)
C       REAL TAU(2)
C
C       TAU(1) = 54216.
C       TAU(2) = 55312.
C       CALL TAUDAT (IERR,TAU,IDATES,2,2)
C**********************************************************************
      INTEGER   FTHREE(5)
      INTEGER   LPERD,IERR(2),DATES(4)
      INTEGER*2 AMNTHS(12)
      INTEGER*2 LEAPYR(7)
      INTEGER*2 SUBYR,TAUBIS
      REAL T(2)

      data      FTHREE/0,8784,17544,26304,35064/
      data      LPERD/35064/
      data      AMNTHS/744,672,744,720,744,720,744,744,720,744,720,744/
      data      LEAPYR/75,79,83,87,91,95,99/
      data      SUBYR/71/,TAUBIS/7248/
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C             OPERTION
C             --------
C
C             ICNVRT=1, CONVERT DATE (MM/DD/YY - HH) TO TAU.
C             ICNVRT=2, CONVERT TAU TO DATE (MM/DD/YY - HH).
C             LOOPA=1, CONVERSION OF ONE VALUE PER CALL.
C                IE - CONVERSION OF ONE DATE TO ONE TAU OR VICE VERSA.
C             LOOPA=2, CONVERSION OF TWO VALUES PER CALL.
C
C             - - - - - - - - - - - - - - - - - - - - - - -
C
C             IERR=1, NORMAL OPERATION.
C             IERR=2, INPUT TAU IS LESS THAN 0.
C             IERR=3 OR 4, FATAL ERROR.
C
C
C             - - - - - - - - - - - - - - - - - - - - - - - - -
      XXXXX=0.0
C
C
C             CALCULATION COMPONENTS
C             ----------------------
C
C             LPERD= TAUS IN ANY GIVEN FOUR YEAR PERIOD.
C             AMNTHS= TAUS IN EACH MONTH OF THE YEAR.
C             LEAPYR= THE LAST YEAR OF EACH OF SEVEN GIVEN FOUR YEAR
C                PERIODS.
C             FTHREE(2-5)= TAUS AT THE END OF 1,2,3, AND 4TH YEARS
C                RESPECTIVELY.  (YEAR 1 IS LEAP YEAR.)
C             SUBYR= ONE YEAR BEFORE FIRST VALID 'TAU YEAR'.
C             TAUBIS= TAU BIAS.
C
      GO TO(10,100),ICNVRT
   10 DO 40 IA=1,LOOPA
      IERR(IA)=1
      IYR=DATES(3)
      DO 30 I=1,7
      IB=I
      IF(IYR.LE.LEAPYR(I))GO TO 35
   30 CONTINUE
      IERR(IA)=3
   35 B=IB
      IC=4-(LEAPYR(IB)-IYR)
      FTAU= ((B*LPERD)-LPERD)+FTHREE(IC)
      AMNTHS(2)=672
      IF(IC.EQ.1)AMNTHS(2)=696
      MONTH=DATES(1)
      DO 20 I=1,MONTH
      IB=I
   20 FTAU=FTAU+AMNTHS(I)
      FTAU=FTAU-AMNTHS(IB)
      A=DATES(2)
      FTAU=FTAU+(A*24.)-24.
      A=DATES(4)
      FTAU=FTAU+A-TAUBIS
      T(IA)=FTAU+0.000001
   40 CONTINUE
      GO TO 99
  100 DO 130 IA=1,LOOPA
      IERR(IA)=1
      IF(T(IA).GT.(-0.001))GO TO 103
      DO 1050 IB=1,4
 1050 DATES(IB)=0
      IERR(IA)=2
      GO TO 130
  103 TAU=T(IA)+TAUBIS
      A=TAU/LPERD
      IL=A+0.000001
      A=IL
      TAU=TAU-(A*LPERD)
      TA=TAU
      DO 105 I=2,5
      IB=I
      A=TA
      TA =TAU-FTHREE(I)
      IF(TA.LT.(-0.01))GO TO 110
  105 CONTINUE
      IERR(IA)=4
  110 DATES(3)=SUBYR+(IL*4)+IB-1
      AMNTHS(2)=672
      IF(IB.EQ.2)AMNTHS(2)=696
      DO 120 I=1,12
      IB=I
      B=A
      A=A-AMNTHS(I)
      IF(A.LT.(-0.01)   )GO TO 125
  120 CONTINUE
  125 DATES(1)=IB
      DATES(2)=(B/24.)+1.000001
      Z=DATES(2)-1
      A= (B-(Z*24.))+0.2
      DATES(4)=A
  130 CONTINUE
   99 RETURN
      END
