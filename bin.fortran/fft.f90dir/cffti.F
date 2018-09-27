      SUBROUTINE CFFTI(N,WSAVE)                                         
C***BEGIN PROLOGUE  CFFTI                                               
c!#   $Id: cffti.F,v 1.1 2000/11/07 20:09:18 mcc Exp $
c!#   $Log: cffti.F,v $
c!#   Revision 1.1  2000/11/07 20:09:18  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***DATE WRITTEN   790601   (YYMMDD)                                    
C***REVISION DATE  830401   (YYMMDD)                                    
C***CATEGORY NO.  J1A2                                                  
C***KEYWORDS  FOURIER TRANSFORM                                         
C***AUTHOR  SWARZTRAUBER, P. N., (NCAR)                                 
C***PURPOSE  INITIALIZE FOR CFFTF AND CFFTB.                            
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE CFFTI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN        
C  BOTH CFFTF AND CFFTB.  THE PRIME FACTORIZATION OF N TOGETHER WITH    
C  A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND         
C  STORED IN WSAVE.                                                     
C                                                                       
C  INPUT PARAMETER                                                      
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE TO BE TRANSFORMED                 
C                                                                       
C  OUTPUT PARAMETER                                                     
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 4*N+15.      
C          THE SAME WORK ARRAY CAN BE USED FOR BOTH CFFTF AND CFFTB     
C          AS LONG AS N REMAINS UNCHANGED.  DIFFERENT WSAVE ARRAYS      
C          ARE REQUIRED FOR DIFFERENT VALUES OF N.  THE CONTENTS OF     
C          WSAVE MUST NOT BE CHANGED BETWEEN CALLS OF CFFTF OR CFFTB.   
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  CFFTI1                                             
C***END PROLOGUE  CFFTI                                                 
      DIMENSION       WSAVE(1)                                          
C***FIRST EXECUTABLE STATEMENT  CFFTI                                   
      IF (N .EQ. 1) RETURN                                              
      IW1 = N+N+1                                                       
      IW2 = IW1+N+N                                                     
      CALL CFFTI1 (N,WSAVE(IW1),WSAVE(IW2))                             
      RETURN                                                            
      END                                                               
