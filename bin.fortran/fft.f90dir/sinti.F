      SUBROUTINE SINTI(N,WSAVE)                                         
C***BEGIN PROLOGUE  SINTI                                               
c!#   $Id: sinti.F,v 1.1 2000/11/07 20:11:25 mcc Exp $
c!#   $Log: sinti.F,v $
c!#   Revision 1.1  2000/11/07 20:11:25  mcc
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
C***PURPOSE  INITIALIZE FOR SINT.                                       
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE SINTI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN        
C  SUBROUTINE SINT.  THE PRIME FACTORIZATION OF N TOGETHER WITH         
C  A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND         
C  STORED IN WSAVE.                                                     
C                                                                       
C  INPUT PARAMETER                                                      
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE TO BE TRANSFORMED.  THE METHOD    
C          IS MOST EFFICIENT WHEN N+1 IS A PRODUCT OF SMALL PRIMES.     
C                                                                       
C  OUTPUT PARAMETER                                                     
C                                                                       
C  WSAVE   A WORK ARRAY WITH AT LEAST INT(3.5*N+16) LOCATIONS.          
C          DIFFERENT WSAVE ARRAYS ARE REQUIRED FOR DIFFERENT VALUES     
C          OF N.  THE CONTENTS OF WSAVE MUST NOT BE CHANGED BETWEEN     
C          CALLS OF SINT.                                               
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  RFFTI                                              
C***END PROLOGUE  SINTI                                                 
      DIMENSION       WSAVE(1)                                          
      DATA PI /3.14159265358979/                                        
C***FIRST EXECUTABLE STATEMENT  SINTI                                   
      IF (N .LE. 1) RETURN                                              
      NP1 = N+1                                                         
      NS2 = N/2                                                         
      DT = PI/FLOAT(NP1)                                                
      KS = N+2                                                          
      KF = KS+NS2-1                                                     
      FK = 0.                                                           
      DO 101 K=KS,KF                                                    
         FK = FK+1.                                                     
         WSAVE(K) = 2.*SIN(FK*DT)                                       
  101 CONTINUE                                                          
      CALL RFFTI (NP1,WSAVE(KF+1))                                      
      RETURN                                                            
      END                                                               
