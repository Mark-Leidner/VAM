      SUBROUTINE COSQF1(N,X,W,XH)                                       
C***BEGIN PROLOGUE  COSQF1                                              
c!#   $Id: cosqf1.F,v 1.1 2000/11/07 20:09:32 mcc Exp $
c!#   $Log: cosqf1.F,v $
c!#   Revision 1.1  2000/11/07 20:09:32  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***REFER TO  COSQF                                                     
C***ROUTINES CALLED  RFFTF                                              
C***END PROLOGUE  COSQF1                                                
      DIMENSION       X(1)       ,W(1)       ,XH(1)                     
C***FIRST EXECUTABLE STATEMENT  COSQF1                                  
      NS2 = (N+1)/2                                                     
      NP2 = N+2                                                         
      DO 101 K=2,NS2                                                    
         KC = NP2-K                                                     
         XH(K) = X(K)+X(KC)                                             
         XH(KC) = X(K)-X(KC)                                            
  101 CONTINUE                                                          
      MODN = MOD(N,2)                                                   
      IF (MODN .EQ. 0) XH(NS2+1) = X(NS2+1)+X(NS2+1)                    
      DO 102 K=2,NS2                                                    
         KC = NP2-K                                                     
         X(K) = W(K-1)*XH(KC)+W(KC-1)*XH(K)                             
         X(KC) = W(K-1)*XH(K)-W(KC-1)*XH(KC)                            
  102 CONTINUE                                                          
      IF (MODN .EQ. 0) X(NS2+1) = W(NS2)*XH(NS2+1)                      
      CALL RFFTF (N,X,XH)                                               
      DO 103 I=3,N,2                                                    
         XIM1 = X(I-1)-X(I)                                             
         X(I) = X(I-1)+X(I)                                             
         X(I-1) = XIM1                                                  
  103 CONTINUE                                                          
      RETURN                                                            
      END                                                               
