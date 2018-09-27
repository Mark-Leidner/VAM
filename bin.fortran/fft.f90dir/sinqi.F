      SUBROUTINE SINQI(N,WSAVE)                                         
C***BEGIN PROLOGUE  SINQI                                               
c!#   $Id: sinqi.F,v 1.1 2000/11/07 20:11:18 mcc Exp $
c!#   $Log: sinqi.F,v $
c!#   Revision 1.1  2000/11/07 20:11:18  mcc
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
C***PURPOSE  INITIALIZE FOR SINQF AND SINQB.                            
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE SINQI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN        
C  BOTH SINQF AND SINQB.  THE PRIME FACTORIZATION OF N TOGETHER WITH    
C  A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND         
C  STORED IN WSAVE.                                                     
C                                                                       
C  INPUT PARAMETER                                                      
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE TO BE TRANSFORMED.  THE METHOD    
C          IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.       
C                                                                       
C  OUTPUT PARAMETER                                                     
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15.      
C          THE SAME WORK ARRAY CAN BE USED FOR BOTH SINQF AND SINQB     
C          AS LONG AS N REMAINS UNCHANGED.  DIFFERENT WSAVE ARRAYS      
C          ARE REQUIRED FOR DIFFERENT VALUES OF N.  THE CONTENTS OF     
C          WSAVE MUST NOT BE CHANGED BETWEEN CALLS OF SINQF OR SINQB.   
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  COSQI                                              
C***END PROLOGUE  SINQI                                                 
      DIMENSION       WSAVE(1)                                          
C***FIRST EXECUTABLE STATEMENT  SINQI                                   
      CALL COSQI (N,WSAVE)                                              
      RETURN                                                            
      END                                                               
