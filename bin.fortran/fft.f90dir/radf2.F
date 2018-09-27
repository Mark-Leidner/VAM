      SUBROUTINE RADF2(IDO,L1,CC,CH,WA1)                                
C***BEGIN PROLOGUE  RADF2                                               
c!#   $Id: radf2.F,v 1.2 2000/11/27 19:26:04 mcc Exp $
c!#   $Log: radf2.F,v $
c!#   Revision 1.2  2000/11/27 19:26:04  mcc
c!#   Removed {DIR$ IVDEP} from comment lines
c!#
c!#   Revision 1.1  2000/11/07 20:10:35  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***REFER TO  RFFTF                                                     
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  RADF2                                                 
      DIMENSION       CH(IDO,2,L1)           ,CC(IDO,L1,2)           ,  
     1                WA1(1)                                            
C***FIRST EXECUTABLE STATEMENT  RADF2                                   
      DO 101 K=1,L1                                                     
         CH(1,1,K) = CC(1,K,1)+CC(1,K,2)                                
         CH(IDO,2,K) = CC(1,K,1)-CC(1,K,2)                              
  101 CONTINUE                                                          
      IF (IDO-2) 107,105,102                                            
  102 IDP2 = IDO+2                                                      
      IF((IDO-1)/2.LT.L1) GO TO 108                                     
      DO 104 K=1,L1                                                     
C                                                             
         DO 103 I=3,IDO,2                                               
            IC = IDP2-I                                                 
            TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)               
            TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)               
            CH(I,1,K) = CC(I,K,1)+TI2                                   
            CH(IC,2,K) = TI2-CC(I,K,1)                                  
            CH(I-1,1,K) = CC(I-1,K,1)+TR2                               
            CH(IC-1,2,K) = CC(I-1,K,1)-TR2                              
  103    CONTINUE                                                       
  104 CONTINUE                                                          
      GO TO 111                                                         
  108 DO 110 I=3,IDO,2                                                  
         IC = IDP2-I                                                    
C                                                             
         DO 109 K=1,L1                                                  
            TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)               
            TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)               
            CH(I,1,K) = CC(I,K,1)+TI2                                   
            CH(IC,2,K) = TI2-CC(I,K,1)                                  
            CH(I-1,1,K) = CC(I-1,K,1)+TR2                               
            CH(IC-1,2,K) = CC(I-1,K,1)-TR2                              
  109    CONTINUE                                                       
  110 CONTINUE                                                          
  111 IF (MOD(IDO,2) .EQ. 1) RETURN                                     
  105 DO 106 K=1,L1                                                     
         CH(1,2,K) = -CC(IDO,K,2)                                       
         CH(IDO,1,K) = CC(IDO,K,1)                                      
  106 CONTINUE                                                          
  107 RETURN                                                            
      END                                                               
