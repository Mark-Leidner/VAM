      SUBROUTINE COSQI(N,WSAVE)                                         
C***BEGIN PROLOGUE  COSQI                                               
c!#   $Id: cosqi.F,v 1.1 2000/11/07 20:09:34 mcc Exp $
c!#   $Log: cosqi.F,v $
c!#   Revision 1.1  2000/11/07 20:09:34  mcc
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
C***PURPOSE  INITIALIZE FOR COSQF AND COSQB.                            
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE COSQI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN        
C  BOTH COSQF AND COSQB.  THE PRIME FACTORIZATION OF N TOGETHER WITH    
C  A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND         
C  STORED IN WSAVE.                                                     
C                                                                       
C  INPUT PARAMETER                                                      
C                                                                       
C  N       THE LENGTH OF THE ARRAY TO BE TRANSFORMED.  THE METHOD       
C          IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.       
C                                                                       
C  OUTPUT PARAMETER                                                     
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15.      
C          THE SAME WORK ARRAY CAN BE USED FOR BOTH COSQF AND COSQB     
C          AS LONG AS N REMAINS UNCHANGED.  DIFFERENT WSAVE ARRAYS      
C          ARE REQUIRED FOR DIFFERENT VALUES OF N.  THE CONTENTS OF     
C          WSAVE MUST NOT BE CHANGED BETWEEN CALLS OF COSQF OR COSQB.   
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  RFFTI                                              
C***END PROLOGUE  COSQI                                                 
      DIMENSION       WSAVE(1)                                          
      DATA PIH /1.57079632679491/                                       
C***FIRST EXECUTABLE STATEMENT  COSQI                                   
      DT = PIH/FLOAT(N)                                                 
      FK = 0.                                                           
      DO 101 K=1,N                                                      
         FK = FK+1.                                                     
         WSAVE(K) = COS(FK*DT)                                          
  101 CONTINUE                                                          
      CALL RFFTI (N,WSAVE(N+1))                                         
      RETURN                                                            
      END                                                               
