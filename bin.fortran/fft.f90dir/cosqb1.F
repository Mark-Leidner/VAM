      SUBROUTINE COSQB1(N,X,W,XH)                                       
C***BEGIN PROLOGUE  COSQB1                                              
c!#   $Id: cosqb1.F,v 1.1 2000/11/07 20:09:26 mcc Exp $
c!#   $Log: cosqb1.F,v $
c!#   Revision 1.1  2000/11/07 20:09:26  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***REFER TO  COSQB                                                     
C***ROUTINES CALLED  RFFTB                                              
C***END PROLOGUE  COSQB1                                                
      DIMENSION       X(1)       ,W(1)       ,XH(1)                     
C***FIRST EXECUTABLE STATEMENT  COSQB1                                  
      NS2 = (N+1)/2                                                     
      NP2 = N+2                                                         
      DO 101 I=3,N,2                                                    
         XIM1 = X(I-1)+X(I)                                             
         X(I) = X(I)-X(I-1)                                             
         X(I-1) = XIM1                                                  
  101 CONTINUE                                                          
      X(1) = X(1)+X(1)                                                  
      MODN = MOD(N,2)                                                   
      IF (MODN .EQ. 0) X(N) = X(N)+X(N)                                 
      CALL RFFTB (N,X,XH)                                               
      DO 102 K=2,NS2                                                    
         KC = NP2-K                                                     
         XH(K) = W(K-1)*X(KC)+W(KC-1)*X(K)                              
         XH(KC) = W(K-1)*X(K)-W(KC-1)*X(KC)                             
  102 CONTINUE                                                          
      IF (MODN .EQ. 0) X(NS2+1) = W(NS2)*(X(NS2+1)+X(NS2+1))            
      DO 103 K=2,NS2                                                    
         KC = NP2-K                                                     
         X(K) = XH(K)+XH(KC)                                            
         X(KC) = XH(K)-XH(KC)                                           
  103 CONTINUE                                                          
      X(1) = X(1)+X(1)                                                  
      RETURN                                                            
      END                                                               
