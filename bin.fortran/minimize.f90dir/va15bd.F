C
      SUBROUTINE VA15BD(IPRINT,ITER,NFUN,
     *                     N,M,X,F,G,STP,FINISH)
c!#   $Id: va15bd.F,v 1.5 2006/11/22 19:49:08 rnh Exp $
c!#   $Log: va15bd.F,v $
c!#   Revision 1.5  2006/11/22 19:49:08  rnh
c!#   Corrected typos
c!#
c!#   Revision 1.4  2006/11/22 19:38:18  rnh
c!#   Replaced block data with module.  Block data was not loading.
c!#
c!#   Revision 1.3  2005/03/18 18:14:23  leidner
c!#   added grep string to optional printout
c!#
c!#   Revision 1.2  2005/02/15 20:10:12  leidner
c!#   updated printouts for IPRINT(1) >= 0 and IPRINT(2) = 0 to dovetail with
c!#   VAM minimization outputs.
c!#
c!#   Revision 1.1  2000/11/07 20:16:39  mcc
c!#   Routine added to build minimize.f90dir lib. Initial revision
c!#
c!#   Revision 1.1  1997/02/10 14:52:21  leidner
c!#   Initial revision
c!#
      use va15cd
C
C     ---------------------------------------------------------------------
C     THIS ROUTINE PRINTS MONITORING INFORMATION. THE FREQUENCY AND AMOUNT
C     OF OUTPUT ARE SPECIFIED AS FOLLOWS: 
C
C     IPRINT(1) < 0 : NO OUTPUT IS GENERATED
C     IPRINT(1) = 0 : OUTPUT ONLY AT FIRST AND LAST ITERATION
C     IPRINT(1) > 0 : OUTPUT EVERY IPRINT(1) ITERATION
C     IPRINT(2) = 0 : ITERATION COUNT, FUNCTION VALUE, NORM OF THE GRADIENT
C                     ,NUMBER OF FUNCTION CALLS AND STEP LENGTH
C     IPRINT(2) = 1 : + VECTOR OF VARIABLES AND GRADIENT VECTOR AT THE
C                       INITIAL POINT
C     IPRINT(2) = 2 : + VECTOR OF VARIABLES
C     IPRINT(2) = 3 : + GRADIENT VECTOR 
C     ---------------------------------------------------------------------
C
      real X(N),G(N),F,GNORM,STP,FACTOR,DDOT
      INTEGER IPRINT(2),ITER,NFUN,PROB
      LOGICAL FINISH
      COMMON /SET/ FACTOR,PROB
C
      IF (IPRINT(1).LT.0)RETURN
      GNORM= DDOT(N,G,1,G,1)
      RMSG = sqrt(GNORM/N)
      GNORM= sqrt(GNORM)
      XNORM= DDOT(N,X,1,X,1)
      XNORM= sqrt(XNORM)
      XNORM= MAX1(1.0,XNORM)
      IF (ITER.EQ.0)THEN
           WRITE(MP,10)
           WRITE(MP,20) PROB,N,M
           WRITE(MP,30)F,GNORM
                 IF (IPRINT(2).GE.1)THEN
                     WRITE(MP,40)
                     WRITE(MP,50) (X(I),I=1,N)
                     WRITE(MP,60)
                     WRITE(MP,50) (G(I),I=1,N)
                  ENDIF
           WRITE(MP,10)
           WRITE(MP,70) 'Norm(G)/Norm(X)', 'Iter', 'Nfun',
     &       'RMS(Gradient)', 'Sum of Squares', 'Steplength',
     &       'Norm(G)', 'Norm(X)'
      ELSE
          IF ((IPRINT(1).EQ.0).AND.(ITER.NE.1.AND..NOT.FINISH))RETURN 
              IF (IPRINT(1).NE.0)THEN
                   IF(MOD(ITER-1,IPRINT(1)).EQ.0.OR.FINISH)THEN
                         WRITE(MP,80) GNORM/XNORM, ITER, NFUN, 
     &                                RMSG, F, STP, GNORM, XNORM
                   ELSE
                         RETURN
                   ENDIF
              ELSE
                   WRITE(MP,80) GNORM/XNORM, ITER, NFUN, 
     &                          RMSG, F, STP, GNORM, XNORM
              ENDIF 
              IF (IPRINT(2).EQ.2.OR.IPRINT(2).EQ.3)THEN
                    IF (FINISH)THEN
                        WRITE(MP,90)
               WRITE(MP,50) (X(I),I=1,N)
        WRITE(MP,60)
        WRITE(MP,50) (G(I),I=1,N)
                    ENDIF
              ENDIF 
            IF (FINISH) WRITE(MP,100)
      ENDIF
C
 10   FORMAT('*************************************************')
 20   FORMAT(' PROB=',I3,'   N=',I7,'   NUMBER OF CORRECTIONS=',I2)
 30   FORMAT('    ',E15.8,'          ',E15.8) 
 40   FORMAT(' VECTOR X= ')
 50   FORMAT(6(2X,1E15.8))
 60   FORMAT(' GRADIENT VECTOR G= ')
 70   FORMAT (/ 5X, A16,   2(1X,A4), 5A16   /)
 80   FORMAT ('monit', E16.7, 2(1X,i4), 5E16.7 )
 90   FORMAT(' FINAL POINT X= ')
 100  FORMAT(/' THE MINIMIZATION TERMINATED WITHOUT DETECTING ERRORS.',
     .       /' IFLAG = 0')
C
      RETURN
      END 
