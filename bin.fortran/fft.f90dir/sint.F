      SUBROUTINE SINT(N,X,WSAVE)                                        
C***BEGIN PROLOGUE  SINT                                                
c!#   $Id: sint.F,v 1.1 2000/11/07 20:11:22 mcc Exp $
c!#   $Log: sint.F,v $
c!#   Revision 1.1  2000/11/07 20:11:22  mcc
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
C***PURPOSE  SINE TRANSFORM OF A REAL, ODD SEQUENCE.                    
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE SINT COMPUTES THE DISCRETE FOURIER SINE TRANSFORM         
C  OF AN ODD SEQUENCE X(I).  THE TRANSFORM IS DEFINED BELOW AT          
C  OUTPUT PARAMETER X.                                                  
C                                                                       
C  SINT IS THE UNNORMALIZED INVERSE OF ITSELF SINCE A CALL OF SINT      
C  FOLLOWED BY ANOTHER CALL OF SINT WILL MULTIPLY THE INPUT SEQUENCE    
C  X BY 2*(N+1).                                                        
C                                                                       
C  THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE SINT MUST BE             
C  INITIALIZED BY CALLING SUBROUTINE SINTI(N,WSAVE).                    
C                                                                       
C  INPUT PARAMETERS                                                     
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE TO BE TRANSFORMED.  THE METHOD    
C          IS MOST EFFICIENT WHEN N+1 IS THE PRODUCT OF SMALL PRIMES.   
C                                                                       
C  X       AN ARRAY WHICH CONTAINS THE SEQUENCE TO BE TRANSFORMED       
C                                                                       
C                                                                       
C  WSAVE   A WORK ARRAY WITH DIMENSION AT LEAST INT(3.5*N+16)           
C          IN THE PROGRAM THAT CALLS SINT.  THE WSAVE ARRAY MUST BE     
C          INITIALIZED BY CALLING SUBROUTINE SINTI(N,WSAVE), AND A      
C          DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT        
C          VALUE OF N.  THIS INITIALIZATION DOES NOT HAVE TO BE         
C          REPEATED SO LONG AS N REMAINS UNCHANGED.  THUS SUBSEQUENT    
C          TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.            
C                                                                       
C  OUTPUT PARAMETERS                                                    
C                                                                       
C  X       FOR I=1,...,N                                                
C                                                                       
C               X(I)= THE SUM FROM K=1 TO K=N                           
C                                                                       
C                    2*X(K)*SIN(K*I*PI/(N+1))                           
C                                                                       
C               A CALL OF SINT FOLLOWED BY ANOTHER CALL OF              
C               SINT WILL MULTIPLY THE SEQUENCE X BY 2*(N+1).           
C               HENCE SINT IS THE UNNORMALIZED INVERSE                  
C               OF ITSELF.                                              
C                                                                       
C  WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT BE       
C          DESTROYED BETWEEN CALLS OF SINT.                             
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  RFFTF                                              
C***END PROLOGUE  SINT                                                  
      DIMENSION       X(1)       ,WSAVE(1)                              
      DATA SQRT3 /1.73205080756888/                                     
C***FIRST EXECUTABLE STATEMENT  SINT                                    
      IF (N-2) 101,102,103                                              
  101 X(1) = X(1)+X(1)                                                  
      RETURN                                                            
  102 XH = SQRT3*(X(1)+X(2))                                            
      X(2) = SQRT3*(X(1)-X(2))                                          
      X(1) = XH                                                         
      RETURN                                                            
  103 NP1 = N+1                                                         
      NS2 = N/2                                                         
      WSAVE(1) = 0.                                                     
      KW = NP1                                                          
      DO 104 K=1,NS2                                                    
1        KW = KW+1                                                      
         KC = NP1-K                                                     
         T1 = X(K)-X(KC)                                                
         T2 = WSAVE(KW)*(X(K)+X(KC))                                    
         WSAVE(K+1) = T1+T2                                             
         WSAVE(KC+1) = T2-T1                                            
  104 CONTINUE                                                          
      MODN = MOD(N,2)                                                   
      IF (MODN .NE. 0) WSAVE(NS2+2) = 4.*X(NS2+1)                       
      NF = NP1+NS2+1                                                    
      CALL RFFTF (NP1,WSAVE,WSAVE(NF))                                  
      X(1) = .5*WSAVE(1)                                                
      DO 105 I=3,N,2                                                    
         X(I-1) = -WSAVE(I)                                             
         X(I) = X(I-2)+WSAVE(I-1)                                       
  105 CONTINUE                                                          
      IF (MODN .NE. 0) RETURN                                           
      X(N) = -WSAVE(N+1)                                                
      RETURN                                                            
      END                                                               
