      SUBROUTINE COST(N,X,WSAVE)                                        
C***BEGIN PROLOGUE  COST                                                
c!#   $Id: cost.F,v 1.1 2000/11/07 20:09:37 mcc Exp $
c!#   $Log: cost.F,v $
c!#   Revision 1.1  2000/11/07 20:09:37  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***DATE WRITTEN   790601   (YYMMDD)                                    
C***REVISION DATE  830401   (YYMMDD)                                    
C***CATEGORY NO.  J1A3                                                  
C***KEYWORDS  FOURIER TRANSFORM                                         
C***AUTHOR  SWARZTRAUBER, P. N., (NCAR)                                 
C***PURPOSE  COSINE TRANSFORM OF A REAL, EVEN SEQUENCE.                 
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE COST COMPUTES THE DISCRETE FOURIER COSINE TRANSFORM       
C  OF AN EVEN SEQUENCE X(I).  THE TRANSFORM IS DEFINED BELOW AT OUTPUT  
C  PARAMETER X.                                                         
C                                                                       
C  COST IS THE UNNORMALIZED INVERSE OF ITSELF SINCE A CALL OF COST      
C  FOLLOWED BY ANOTHER CALL OF COST WILL MULTIPLY THE INPUT SEQUENCE    
C  X BY 2*(N-1).  THE TRANSFORM IS DEFINED BELOW AT OUTPUT PARAMETER X. 
C                                                                       
C  THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE COST MUST BE             
C  INITIALIZED BY CALLING SUBROUTINE COSTI(N,WSAVE).                    
C                                                                       
C  INPUT PARAMETERS                                                     
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE X.  N MUST BE GREATER THAN 1.     
C          THE METHOD IS MOST EFFICIENT WHEN N-1 IS A PRODUCT OF        
C          SMALL PRIMES.                                                
C                                                                       
C  X       AN ARRAY WHICH CONTAINS THE SEQUENCE TO BE TRANSFORMED       
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15       
C          IN THE PROGRAM THAT CALLS COST.  THE WSAVE ARRAY MUST BE     
C          INITIALIZED BY CALLING SUBROUTINE COSTI(N,WSAVE), AND A      
C          DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT        
C          VALUE OF N.  THIS INITIALIZATION DOES NOT HAVE TO BE         
C          REPEATED SO LONG AS N REMAINS UNCHANGED.  THUS SUBSEQUENT    
C          TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.            
C                                                                       
C  OUTPUT PARAMETERS                                                    
C                                                                       
C  X       FOR I=1,...,N                                                
C                                                                       
C             X(I) = X(1)+(-1)**(I-1)*X(N)                              
C                                                                       
C               + THE SUM FROM K=2 TO K=N-1                             
C                                                                       
C                   X(K)*COS((K-1)*(I-1)*PI/(N-1))                      
C                                                                       
C               A CALL OF COST FOLLOWED BY ANOTHER CALL OF              
C               COST WILL MULTIPLY THE SEQUENCE X BY 2*(N-1).           
C               HENCE COST IS THE UNNORMALIZED INVERSE                  
C               OF ITSELF.                                              
C                                                                       
C  WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT BE       
C          DESTROYED BETWEEN CALLS OF COST.                             
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  RFFTF                                              
C***END PROLOGUE  COST                                                  
      DIMENSION       X(1)       ,WSAVE(1)                              
C***FIRST EXECUTABLE STATEMENT  COST                                    
      NM1 = N-1                                                         
      NP1 = N+1                                                         
      NS2 = N/2                                                         
      IF (N-2) 106,101,102                                              
  101 X1H = X(1)+X(2)                                                   
      X(2) = X(1)-X(2)                                                  
      X(1) = X1H                                                        
      RETURN                                                            
  102 IF (N .GT. 3) GO TO 103                                           
      X1P3 = X(1)+X(3)                                                  
      TX2 = X(2)+X(2)                                                   
      X(2) = X(1)-X(3)                                                  
      X(1) = X1P3+TX2                                                   
      X(3) = X1P3-TX2                                                   
      RETURN                                                            
  103 C1 = X(1)-X(N)                                                    
      X(1) = X(1)+X(N)                                                  
      DO 104 K=2,NS2                                                    
         KC = NP1-K                                                     
         T1 = X(K)+X(KC)                                                
         T2 = X(K)-X(KC)                                                
         C1 = C1+WSAVE(KC)*T2                                           
         T2 = WSAVE(K)*T2                                               
         X(K) = T1-T2                                                   
         X(KC) = T1+T2                                                  
  104 CONTINUE                                                          
      MODN = MOD(N,2)                                                   
      IF (MODN .NE. 0) X(NS2+1) = X(NS2+1)+X(NS2+1)                     
      CALL RFFTF (NM1,X,WSAVE(N+1))                                     
      XIM2 = X(2)                                                       
      X(2) = C1                                                         
      DO 105 I=4,N,2                                                    
         XI = X(I)                                                      
         X(I) = X(I-2)-X(I-1)                                           
         X(I-1) = XIM2                                                  
         XIM2 = XI                                                      
  105 CONTINUE                                                          
      IF (MODN .NE. 0) X(N) = XIM2                                      
  106 RETURN                                                            
      END                                                               
