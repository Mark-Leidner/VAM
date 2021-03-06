      SUBROUTINE COSQF(N,X,WSAVE)                                       
C***BEGIN PROLOGUE  COSQF                                               
c!#   $Id: cosqf.F,v 1.1 2000/11/07 20:09:29 mcc Exp $
c!#   $Log: cosqf.F,v $
c!#   Revision 1.1  2000/11/07 20:09:29  mcc
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
C***PURPOSE  FORWARD COSINE TRANSFORM WITH ODD WAVE NUMBERS.            
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE COSQF COMPUTES THE FAST FOURIER TRANSFORM OF QUARTER      
C  WAVE DATA. THAT IS, COSQF COMPUTES THE COEFFICIENTS IN A COSINE      
C  SERIES REPRESENTATION WITH ONLY ODD WAVE NUMBERS.  THE TRANSFORM     
C  IS DEFINED BELOW AT OUTPUT PARAMETER X                               
C                                                                       
C  COSQF IS THE UNNORMALIZED INVERSE OF COSQB SINCE A CALL OF COSQF     
C  FOLLOWED BY A CALL OF COSQB WILL MULTIPLY THE INPUT SEQUENCE X       
C  BY 4*N.                                                              
C                                                                       
C  THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE COSQF MUST BE            
C  INITIALIZED BY CALLING SUBROUTINE COSQI(N,WSAVE).                    
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
C          IN THE PROGRAM THAT CALLS COSQF.  THE WSAVE ARRAY MUST BE    
C          INITIALIZED BY CALLING SUBROUTINE COSQI(N,WSAVE), AND A      
C          DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT        
C          VALUE OF N.  THIS INITIALIZATION DOES NOT HAVE TO BE         
C          REPEATED SO LONG AS N REMAINS UNCHANGED.  THUS SUBSEQUENT    
C          TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.            
C                                                                       
C  OUTPUT PARAMETERS                                                    
C                                                                       
C  X       FOR I=1,...,N                                                
C                                                                       
C               X(I) = X(1) PLUS THE SUM FROM K=2 TO K=N OF             
C                                                                       
C                  2*X(K)*COS((2*I-1)*(K-1)*PI/(2*N))                   
C                                                                       
C               A CALL OF COSQF FOLLOWED BY A CALL OF                   
C               COSQB WILL MULTIPLY THE SEQUENCE X BY 4*N.              
C               THEREFORE COSQB IS THE UNNORMALIZED INVERSE             
C               OF COSQF.                                               
C                                                                       
C  WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT          
C          BE DESTROYED BETWEEN CALLS OF COSQF OR COSQB.                
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  COSQF1                                             
C***END PROLOGUE  COSQF                                                 
      DIMENSION       X(1)       ,WSAVE(1)                              
      DATA SQRT2 /1.4142135623731/                                      
C***FIRST EXECUTABLE STATEMENT  COSQF                                   
      IF (N-2) 102,101,103                                              
  101 TSQX = SQRT2*X(2)                                                 
      X(2) = X(1)-TSQX                                                  
      X(1) = X(1)+TSQX                                                  
  102 RETURN                                                            
  103 CALL COSQF1 (N,X,WSAVE,WSAVE(N+1))                                
      RETURN                                                            
      END                                                               
