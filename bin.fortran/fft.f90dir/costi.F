      SUBROUTINE COSTI(N,WSAVE)                                         
C***BEGIN PROLOGUE  COSTI                                               
c!#   $Id: costi.F,v 1.1 2000/11/07 20:09:40 mcc Exp $
c!#   $Log: costi.F,v $
c!#   Revision 1.1  2000/11/07 20:09:40  mcc
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
C***PURPOSE  INITIALIZE FOR COST.                                       
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE COSTI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN        
C  SUBROUTINE COST.  THE PRIME FACTORIZATION OF N TOGETHER WITH         
C  A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND         
C  STORED IN WSAVE.                                                     
C                                                                       
C  INPUT PARAMETER                                                      
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE TO BE TRANSFORMED.  THE METHOD    
C          IS MOST EFFICIENT WHEN N-1 IS A PRODUCT OF SMALL PRIMES.     
C                                                                       
C  OUTPUT PARAMETER                                                     
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15.      
C          DIFFERENT WSAVE ARRAYS ARE REQUIRED FOR DIFFERENT VALUES     
C          OF N.  THE CONTENTS OF WSAVE MUST NOT BE CHANGED BETWEEN     
C          CALLS OF COST.                                               
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  RFFTI                                              
C***END PROLOGUE  COSTI                                                 
      DIMENSION       WSAVE(1)                                          
      DATA PI /3.14159265358979/                                        
C***FIRST EXECUTABLE STATEMENT  COSTI                                   
      IF (N .LE. 3) RETURN                                              
      NM1 = N-1                                                         
      NP1 = N+1                                                         
      NS2 = N/2                                                         
      DT = PI/FLOAT(NM1)                                                
      FK = 0.                                                           
      DO 101 K=2,NS2                                                    
         KC = NP1-K                                                     
         FK = FK+1.                                                     
         WSAVE(K) = 2.*SIN(FK*DT)                                       
         WSAVE(KC) = 2.*COS(FK*DT)                                      
  101 CONTINUE                                                          
      CALL RFFTI (NM1,WSAVE(N+1))                                       
      RETURN                                                            
      END                                                               
