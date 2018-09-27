c     From Mike Navon 14 May 93
      SUBROUTINE VA15AD(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,S,Y,
     *          POINT,W,IFLAG,ITER)
c!#   $Id: va15ad.F,v 1.3 2006/11/22 19:49:07 rnh Exp $
c!#   $Log: va15ad.F,v $
c!#   Revision 1.3  2006/11/22 19:49:07  rnh
c!#   Corrected typos
c!#
c!#   Revision 1.2  2006/11/22 19:38:17  rnh
c!#   Replaced block data with module.  Block data was not loading.
c!#
c!#   Revision 1.1  2000/11/07 20:16:36  mcc
c!#   Routine added to build minimize.f90dir lib. Initial revision
c!#
c!#   Revision 1.1  1997/02/10 14:52:21  leidner
c!#   Initial revision
c!#
      use va15cd
C
      real X(N),G(N),S(M*N),Y(M*N),DIAG(N),W(N+2*M)
      real FTOL,XTOL,STPMIN,STPMAX,STP,F,YS,SQ,
     *              YR,BETA,ONE,ZERO,EPS,XNORM,GNORM,YY,DDOT,STP1
C
      INTEGER BOUND,ITER,NFUN,NFEV,IPRINT(2),POINT,CP,IFLAG
      LOGICAL FINISH,DIAGCO
      SAVE  
      DATA ONE,ZERO/1.0D+0,0.0D+0/
C
C     ------------------------------------------------------------
C     INITIALIZE
C     ------------------------------------------------------------
C
      IF(IFLAG.EQ.0) GO TO 1
      GO TO (72,10) IFLAG
   1  ITER= 0
      IF(N.LE.0.OR.M.LE.0) GO TO 96
      IF(GTOL.LE.1.D-04) THEN 
        IF(LP.GT.0) WRITE(LP,145)
        GTOL=1.D-02 
      ENDIF
      NFUN= 1
      POINT= 0
      FINISH= .FALSE.
      IF(DIAGCO) THEN
         DO 3 I=1,N 
 3       IF (DIAG(I).LE.ZERO) GO TO 95
      ELSE
         DO 4 I=1,N 
 4       DIAG(I)= 1.0D0
      ENDIF
      DO 5 I=1,N
 5    S(I)= -G(I)*DIAG(I)
      GNORM= sqrt(DDOT(N,G,1,G,1))
      STP1= ONE/GNORM
C
C     PARAMETERS FOR LINE SEARCH ROUTINE
C     ----------------------------------
      FTOL= 1.0D-4
      XTOL= 1.0D-17 
      STPMIN= 1.0D-20
      STPMAX= 1.0D+20
      MAXFEV= 20
C
       CALL VA15BD(IPRINT,ITER,NFUN,
     *                     N,M,X,F,G,STP,FINISH)
C
C    ------------------------------------------------------------
C     MAIN ITERATION LOOP
C    --------------------------------------------------------
C
 8    ITER= ITER+1
      INFO=0
      BOUND=ITER-1
      IF (ITER .GT. M)BOUND=M 
      IF(ITER.EQ.1) GO TO 65
C
C     ------------------------------------------------------------
C     COMPUTE -HG AND THE DIAGONAL SCALING MATRIX IN DIAG
C     ------------------------------------------------------------
C
      IF(.NOT.DIAGCO) THEN
CZOU         PRINT*,'form scaling ITER=', ITER
         DO 9 I=1,N 
   9     DIAG(I)= YS/YY
      ELSE
         IFLAG=2
         RETURN
      ENDIF
  10  CONTINUE
      DO 11 I=1,N
  11  IF (DIAG(I).LE.ZERO) GO TO 95
C
      CP= POINT
      IF (POINT.EQ.0) CP=M
      W(N+CP)= ONE/YS
      DO 12 I=1,N
  12  W(I)= -G(I)
      CP= POINT
      DO 25 II= 1,BOUND
         CP=CP-1
         IF (CP.EQ. -1)CP=M-1 
         SQ= DDOT(N,S(CP*N+1),1,W,1)
         W(N+M+CP+1)= W(N+CP+1)*SQ
         DO 20 K=1,N
  20     W(K)= W(K)-W(N+M+CP+1)*Y(CP*N+K)
  25  CONTINUE
C
      DO 30 I=1,N
  30  W(I)=DIAG(I)*W(I)
      DO 45 II=1,BOUND
         YR= DDOT(N,Y(CP*N+1),1,W,1)
         BETA= W(N+CP+1)*YR
         DO 40 K=1,N
  40     W(K)= W(K)+S(CP*N+K)*(W(N+M+CP+1)-BETA)
         CP=CP+1
         IF (CP.EQ.M)CP=0
  45  CONTINUE
C
C     ------------------------------------------------------------
C     STORE THE NEW DIRECTION IN S
C     ------------------------------------------------------------
C
       DO 60 J=1,N
  60   S(POINT*N+J)= W(J)
C
C     ------------------------------------------------------------
C     OBTAIN THE MINIMIZER OF THE FUNCTION ALONG THE
C     DIRECTION S BY USING THE LINE SEARCH ROUTINE OF VD05AD
C     ------------------------------------------------------------
  65  NFEV=0
      STP=ONE
      IF (ITER.EQ.1) STP=STP1 
      DO 70 I=1,N
  70  W(I)=G(I)
  72  CONTINUE
C
      CALL VD05AD(N,X,F,G,S(POINT*N+1),STP,FTOL,GTOL,
     *            XTOL,STPMIN,STPMAX,MAXFEV,INFO,NFEV,DIAG) 
C
      IF (INFO .EQ. -1) THEN
        IFLAG=1
        RETURN
      ENDIF
      IF (INFO .NE. 1) GO TO 90
      NFUN= NFUN + NFEV
C
C     ------------------------------------------------------------
C     COMPUTE THE NEW S AND Y 
C     ------------------------------------------------------------
C
      NPT=POINT*N
      DO 75 I=1,N
      S(NPT+I)= STP*S(NPT+I)
  75  Y(NPT+I)= G(I)-W(I)
      YS= DDOT(N,Y(NPT+1),1,S(NPT+1),1) 
      YY= DDOT(N,Y(NPT+1),1,Y(NPT+1),1) 
      POINT=POINT+1 
      IF (POINT.EQ.M)POINT=0
C
C     ------------------------------------------------------------
C     CONVERGENCE CHECK
C     ------------------------------------------------------------
C
      GNORM= DDOT(N,G,1,G,1)
      GNORM=sqrt(GNORM)
      XNORM= DDOT(N,X,1,X,1)
      XNORM=sqrt(XNORM)
      XNORM= MAX1(1.0,XNORM)
C
CZOU start
C      XIAO=GNORM/XNORM
C      PRINT*,'GNORM/(1, XNORM)=', XIAO
CZOU end
      IF (GNORM/XNORM .LE. EPS) FINISH=.TRUE.
C
      CALL VA15BD(IPRINT,ITER,NFUN,
     *               N,M,X,F,G,STP,FINISH)
      IF (FINISH) THEN
         IFLAG=0
         RETURN
      ENDIF
      GO TO 8
C
C     ------------------------------------------------------------
C     END OF MAIN ITERATION LOOP. ERROR EXITS.
C     ------------------------------------------------------------
C
  90  IF(LP.LE.0) RETURN
      IF (INFO.EQ.0) THEN
           IFLAG= -1
           WRITE(LP,100)IFLAG 
      ELSE IF (INFO.EQ.2) THEN
           IFLAG= -2
           WRITE(LP,105)IFLAG 
      ELSE IF (INFO.EQ.3) THEN
           IFLAG= -3
           WRITE(LP,110)IFLAG 
      ELSE IF (INFO.EQ.4) THEN
           IFLAG= -4
           WRITE(LP,115)IFLAG 
      ELSE IF (INFO.EQ.5) THEN
           IFLAG= -5
           WRITE(LP,120)IFLAG 
      ELSE IF (INFO.EQ.6) THEN
           IFLAG= -6
           WRITE(LP,125)IFLAG 
      ENDIF
      RETURN
C
  95  IFLAG= -7
      IF(LP.GT.0) WRITE(LP,135)IFLAG,I
      RETURN
  96  IFLAG= -8
      IF(LP.GT.0) WRITE(LP,140)IFLAG
C
C     ------------------------------------------------------------
C     FORMATS
C     ------------------------------------------------------------
C
 100  FORMAT(/' IFLAG= ',I2,/' IMPROPER INPUT PARAMETERS DURING',
     .       ' THE LINE SEARCH.')
 105  FORMAT(/' IFLAG= ',I2,/' RELATIVE WIDTH OF THE INTERVAL OF',
     .       ' UNCERTAINTY IN THE LINE SEARCH'/'IS OF THE ORDER OF
     .          MACHINE ROUNDOFF.')
 110  FORMAT(/' IFLAG= ',I2,/' NUMBER OF CALLS TO FUNCTION IN THE',
     .       ' LINE SEARCH HAS REACHED 20.')
 115  FORMAT(/' IFLAG= ',I2,/' THE STEP IN THE LINE SEARCH IS',
     .       ' TOO SMALL.')
 120  FORMAT(/' IFLAG= ',I2,/' THE STEP IN THE LINE SEARCH IS',
     .       ' TOO LARGE.')
 125  FORMAT(/' IFLAG= ',I2,/' ROUNDING ERRORS PREVENT FURTHER',
     .       ' PROGRESS IN THE LINE SEARCH.')
 135  FORMAT(/' IFLAG= ',I2,/' THE',I5,'-TH DIAGONAL ELEMENT OF THE', 
     .       ' INVERSE HESSIAN APPROXIMATION IS NOT POSITIVE')
 140  FORMAT(/' IFLAG= ',I2,/' IMPROPER INPUT PARAMETERS (N OR M',
     .       ' ARE NOT POSITIVE)')
 145  FORMAT(/'  GTOL IS LESS THAN OR EQUAL TO 1.D-04',
     .       / 'IT HAS BEEN RESET TO 1.D-02')
      RETURN
      END 
