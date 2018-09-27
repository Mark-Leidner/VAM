      SUBROUTINE EZFFTI(N,WSAVE)                                        
C***BEGIN PROLOGUE  EZFFTI                                              
c!#   $Id: ezffti.F,v 1.1 2000/11/07 20:09:51 mcc Exp $
c!#   $Log: ezffti.F,v $
c!#   Revision 1.1  2000/11/07 20:09:51  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***DATE WRITTEN   790601   (YYMMDD)                                    
C***REVISION DATE  830401   (YYMMDD)                                    
C***CATEGORY NO.  J1A1                                                  
C***KEYWORDS  FOURIER TRANSFORM                                         
C***AUTHOR  SWARZTRAUBER, P. N., (NCAR)                                 
C***PURPOSE  INITIALIZE EZFFTF AND EZFFTB                               
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE EZFFTI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN       
C  BOTH EZFFTF AND EZFFTB.  THE PRIME FACTORIZATION OF N TOGETHER WITH  
C  A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND         
C  STORED IN WSAVE.                                                     
C                                                                       
C  INPUT PARAMETER                                                      
C                                                                       
C  N       THE LENGTH OF THE SEQUENCE TO BE TRANSFORMED.                
C                                                                       
C  OUTPUT PARAMETER                                                     
C                                                                       
C  WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15.      
C          THE SAME WORK ARRAY CAN BE USED FOR BOTH EZFFTF AND EZFFTB   
C          AS LONG AS N REMAINS UNCHANGED.  DIFFERENT WSAVE ARRAYS      
C          ARE REQUIRED FOR DIFFERENT VALUES OF N.                      
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  EZFFT1                                             
C***END PROLOGUE  EZFFTI                                                
      DIMENSION       WSAVE(1)                                          
C***FIRST EXECUTABLE STATEMENT  EZFFTI                                  
      IF (N .EQ. 1) RETURN                                              
      CALL EZFFT1 (N,WSAVE(2*N+1),WSAVE(3*N+1))                         
      RETURN                                                            
      END                                                               
