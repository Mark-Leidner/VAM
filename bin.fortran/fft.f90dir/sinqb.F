      SUBROUTINE SINQB(N,X,WSAVE)                                       
C***BEGIN PROLOGUE  SINQB                                               
c!#   $Id: sinqb.F,v 1.1 2000/11/07 20:11:12 mcc Exp $
c!#   $Log: sinqb.F,v $
c!#   Revision 1.1  2000/11/07 20:11:12  mcc
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
C***PURPOSE  UNNORMALIZED INVERSE OF SINQF.                             
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE SINQB COMPUTES THE FAST FOURIER TRANSFORM OF QUARTER      
C  WAVE DATA.  THAT IS, SINQB COMPUTES A SEQUENCE FROM ITS              
C  REPRESENTATION IN TERMS OF A SINE SERIES WITH ODD WAVE NUMBERS.      
C  THE TRANSFORM IS DEFINED BELOW AT OUTPUT PARAMETER X.                
C                                                                       
C  SINQF IS THE UNNORMALIZED INVERSE OF SINQB SINCE A CALL OF SINQB     
C  FOLLOWED BY A CALL OF SINQF WILL MULTIPLY THE INPUT SEQUENCE X       
C  BY 4*N.                                                              
C                                                                       
C  THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE SINQB MUST BE            
C  INITIALIZED BY CALLING SUBROUTINE SINQI(N,WSAVE).                    
C                                                                       
C                                                                       
C  INPUT PARAMETERS                                                     
C                                                                       
C  N       THE LENGTH OF THE ARRAY X TO BE TRANSFORMED.  THE METHOD     
C          IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.       
C                                                                       
C  X       AN ARRAY WHICH CONTAINS THE SEQUENCE TO BE TRANSFORMED       
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15       
C          IN THE PROGRAM THAT CALLS SINQB.  THE WSAVE ARRAY MUST BE    
C          INITIALIZED BY CALLING SUBROUTINE SINQI(N,WSAVE), AND A      
C          DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT        
C          VALUE OF N.  THIS INITIALIZATION DOES NOT HAVE TO BE         
C          REPEATED SO LONG AS N REMAINS UNCHANGED.  THUS SUBSEQUENT    
C          TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.            
C                                                                       
C  OUTPUT PARAMETERS                                                    
C                                                                       
C  X       FOR I=1,...,N                                                
C                                                                       
C               X(I)= THE SUM FROM K=1 TO K=N OF                        
C                                                                       
C                 4*X(K)*SIN((2K-1)*I*PI/(2*N))                         
C                                                                       
C               A CALL OF SINQB FOLLOWED BY A CALL OF                   
C               SINQF WILL MULTIPLY THE SEQUENCE X BY 4*N.              
C               THEREFORE SINQF IS THE UNNORMALIZED INVERSE             
C               OF SINQB.                                               
C                                                                       
C  WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT          
C          BE DESTROYED BETWEEN CALLS OF SINQB OR SINQF.                
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  COSQB                                              
C***END PROLOGUE  SINQB                                                 
      DIMENSION       X(1)       ,WSAVE(1)                              
C***FIRST EXECUTABLE STATEMENT  SINQB                                   
      IF (N .GT. 1) GO TO 101                                           
      X(1) = 4.*X(1)                                                    
      RETURN                                                            
  101 NS2 = N/2                                                         
      DO 102 K=2,N,2                                                    
         X(K) = -X(K)                                                   
  102 CONTINUE                                                          
      CALL COSQB (N,X,WSAVE)                                            
      DO 103 K=1,NS2                                                    
         KC = N-K                                                       
         XHOLD = X(K)                                                   
         X(K) = X(KC+1)                                                 
         X(KC+1) = XHOLD                                                
  103 CONTINUE                                                          
      RETURN                                                            
      END                                                               
