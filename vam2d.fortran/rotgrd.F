c!#   $Id: rotgrd.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: rotgrd.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
C********************************************************************** 
      SUBROUTINE ROTGRD(IM,JNP,SLON1,SLON2,RGRID1,RGRID2)               
C***********************************************************************
                                                                        
      REAL RGRID1(IM,JNP),RGRID2(IM,JNP)                              
C                                                                       
C=======================================================================
C     INITIALIZE                                                        
C=======================================================================
C                                                                       
      XLNINC = 360.0 / FLOAT(IM)                                        
                                                                        
      STLON1 = SLON1                                                    
      STLON2 = SLON2                                                    
      IF (STLON1 .LT. 0.0) STLON1 = STLON1 + 360.0                      
      IF (STLON2 .LT. 0.0) STLON2 = STLON2 + 360.0                      
C                                                                       
C=======================================================================
C     ROTATE GRID SUCH THAT DATA WILL START AT "SLON2".                 
C=======================================================================
C                                                                       
      DO 10 J = 1,JNP                                                   
                                                                        
         DO 5 ILON1 = 1,IM                                              
C                                                                       
C           CALCULATE WHAT LONGITUDE CORRESPONDS TO "ILON1" GIVEN THE   
C           CONFIGURATION OF GRID-1.                                    
C           ---------------------------------------------------------   
C                                                                       
            XLON = FLOAT(ILON1-1) * XLNINC + STLON1                     
            IF (XLON .GE. 360.0) XLON = XLON - 360.0                    
C                                                                       
C           CALCULATE WHAT GRID POINT CORRESPONDS TO "XLON" GIVEN       
C           THE CONFIGURATION OF GRID-2.                                
C           -----------------------------------------------------       
C                                                                       
            ILON2 = (XLON - STLON2) / XLNINC + 1.0                      
            IF (ILON2 .LE. 0) ILON2 = IM + ILON2                        
                                                                        
C           WRITE(6,100) XLON,ILON1,ILON2                               
C100        FORMAT(1X,F7.1,2I6)                                         
            RGRID2(ILON2,J) = RGRID1(ILON1,J)                           
                                                                        
 5       CONTINUE                                                       
                                                                        
 10   CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
