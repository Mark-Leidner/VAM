      SUBROUTINE RADF3(IDO,L1,CC,CH,WA1,WA2)                            
C***BEGIN PROLOGUE  RADF3                                               
c!#   $Id: radf3.F,v 1.2 2000/11/27 19:26:07 mcc Exp $
c!#   $Log: radf3.F,v $
c!#   Revision 1.2  2000/11/27 19:26:07  mcc
c!#   Removed {DIR$ IVDEP} from comment lines
c!#
c!#   Revision 1.1  2000/11/07 20:10:38  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***REFER TO  RFFTF                                                     
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  RADF3                                                 
      DIMENSION       CH(IDO,3,L1)           ,CC(IDO,L1,3)           ,  
     1                WA1(1)     ,WA2(1)                                
      DATA TAUR,TAUI /-.5,.866025403784439/                             
C***FIRST EXECUTABLE STATEMENT  RADF3                                   
      DO 101 K=1,L1                                                     
         CR2 = CC(1,K,2)+CC(1,K,3)                                      
         CH(1,1,K) = CC(1,K,1)+CR2                                      
         CH(1,3,K) = TAUI*(CC(1,K,3)-CC(1,K,2))                         
         CH(IDO,2,K) = CC(1,K,1)+TAUR*CR2                               
  101 CONTINUE                                                          
      IF (IDO .EQ. 1) RETURN                                            
      IDP2 = IDO+2                                                      
      IF((IDO-1)/2.LT.L1) GO TO 104                                     
      DO 103 K=1,L1                                                     
C                                                             
         DO 102 I=3,IDO,2                                               
            IC = IDP2-I                                                 
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)               
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)               
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)               
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)               
            CR2 = DR2+DR3                                               
            CI2 = DI2+DI3                                               
            CH(I-1,1,K) = CC(I-1,K,1)+CR2                               
            CH(I,1,K) = CC(I,K,1)+CI2                                   
            TR2 = CC(I-1,K,1)+TAUR*CR2                                  
            TI2 = CC(I,K,1)+TAUR*CI2                                    
            TR3 = TAUI*(DI2-DI3)                                        
            TI3 = TAUI*(DR3-DR2)                                        
            CH(I-1,3,K) = TR2+TR3                                       
            CH(IC-1,2,K) = TR2-TR3                                      
            CH(I,3,K) = TI2+TI3                                         
            CH(IC,2,K) = TI3-TI2                                        
  102    CONTINUE                                                       
  103 CONTINUE                                                          
      RETURN                                                            
  104 DO 106 I=3,IDO,2                                                  
         IC = IDP2-I                                                    
C                                                             
         DO 105 K=1,L1                                                  
            DR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)               
            DI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)               
            DR3 = WA2(I-2)*CC(I-1,K,3)+WA2(I-1)*CC(I,K,3)               
            DI3 = WA2(I-2)*CC(I,K,3)-WA2(I-1)*CC(I-1,K,3)               
            CR2 = DR2+DR3                                               
            CI2 = DI2+DI3                                               
            CH(I-1,1,K) = CC(I-1,K,1)+CR2                               
            CH(I,1,K) = CC(I,K,1)+CI2                                   
            TR2 = CC(I-1,K,1)+TAUR*CR2                                  
            TI2 = CC(I,K,1)+TAUR*CI2                                    
            TR3 = TAUI*(DI2-DI3)                                        
            TI3 = TAUI*(DR3-DR2)                                        
            CH(I-1,3,K) = TR2+TR3                                       
            CH(IC-1,2,K) = TR2-TR3                                      
            CH(I,3,K) = TI2+TI3                                         
            CH(IC,2,K) = TI3-TI2                                        
  105    CONTINUE                                                       
  106 CONTINUE                                                          
      RETURN                                                            
      END                                                               
