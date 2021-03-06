      SUBROUTINE CFFTB(N,C,WSAVE)                                       
C***BEGIN PROLOGUE  CFFTB                                               
c!#   $Id: cfftb.F,v 1.1 2000/11/07 20:09:07 mcc Exp $
c!#   $Log: cfftb.F,v $
c!#   Revision 1.1  2000/11/07 20:09:07  mcc
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
C***PURPOSE  UNNORMALIZED INVERSE OF CFFTF.                             
C***DESCRIPTION                                                         
C                                                                       
C  SUBROUTINE CFFTB COMPUTES THE BACKWARD COMPLEX DISCRETE FOURIER      
C  TRANSFORM (THE FOURIER SYNTHESIS).  EQUIVALENTLY, CFFTB COMPUTES     
C  A COMPLEX PERIODIC SEQUENCE FROM ITS FOURIER COEFFICIENTS.           
C  THE TRANSFORM IS DEFINED BELOW AT OUTPUT PARAMETER C.                
C                                                                       
C  A CALL OF CFFTF FOLLOWED BY A CALL OF CFFTB WILL MULTIPLY THE        
C  SEQUENCE BY N.                                                       
C                                                                       
C  THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE CFFTB MUST BE            
C  INITIALIZED BY CALLING SUBROUTINE CFFTI(N,WSAVE).                    
C                                                                       
C  INPUT PARAMETERS                                                     
C                                                                       
C                                                                       
C  N      THE LENGTH OF THE COMPLEX SEQUENCE C.  THE METHOD IS          
C         MORE EFFICIENT WHEN N IS THE PRODUCT OF SMALL PRIMES.         
C                                                                       
C  C      A COMPLEX ARRAY OF LENGTH N WHICH CONTAINS THE SEQUENCE       
C                                                                       
C  WSAVE   A REAL WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 4*N+15  
C          IN THE PROGRAM THAT CALLS CFFTB.  THE WSAVE ARRAY MUST BE    
C          INITIALIZED BY CALLING SUBROUTINE CFFTI(N,WSAVE), AND A      
C          DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT        
C          VALUE OF N.  THIS INITIALIZATION DOES NOT HAVE TO BE         
C          REPEATED SO LONG AS N REMAINS UNCHANGED.  THUS SUBSEQUENT    
C          TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.            
C          THE SAME WSAVE ARRAY CAN BE USED BY CFFTF AND CFFTB.         
C                                                                       
C  OUTPUT PARAMETERS                                                    
C                                                                       
C  C      FOR J=1,...,N                                                 
C                                                                       
C             C(J)=THE SUM FROM K=1,...,N OF                            
C                                                                       
C                   C(K)*EXP(I*J*K*2*PI/N)                              
C                                                                       
C                         WHERE I=SQRT(-1)                              
C                                                                       
C  WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT BE       
C          DESTROYED BETWEEN CALLS OF SUBROUTINE CFFTF OR CFFTB         
C***REFERENCES  (NONE)                                                  
C***ROUTINES CALLED  CFFTB1                                             
C***END PROLOGUE  CFFTB                                                 
      DIMENSION       C(1)       ,WSAVE(1)                              
C***FIRST EXECUTABLE STATEMENT  CFFTB                                   
      IF (N .EQ. 1) RETURN                                              
      IW1 = N+N+1                                                       
      IW2 = IW1+N+N                                                     
      CALL CFFTB1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))                     
      RETURN                                                            
      END                                                               
