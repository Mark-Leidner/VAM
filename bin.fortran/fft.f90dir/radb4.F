      SUBROUTINE RADB4(IDO,L1,CC,CH,WA1,WA2,WA3)                        
C***BEGIN PROLOGUE  RADB4                                               
c!#   $Id: radb4.F,v 1.2 2000/11/27 19:25:56 mcc Exp $
c!#   $Log: radb4.F,v $
c!#   Revision 1.2  2000/11/27 19:25:56  mcc
c!#   Removed {DIR$ IVDEP} from comment lines
c!#
c!#   Revision 1.1  2000/11/07 20:10:26  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***REFER TO  RFFTB                                                     
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  RADB4                                                 
      DIMENSION       CC(IDO,4,L1)           ,CH(IDO,L1,4)           ,  
     1                WA1(1)     ,WA2(1)     ,WA3(1)                    
      DATA SQRT2 /1.414213562373095/                                    
C***FIRST EXECUTABLE STATEMENT  RADB4                                   
      DO 101 K=1,L1                                                     
         TR1 = CC(1,1,K)-CC(IDO,4,K)                                    
         TR2 = CC(1,1,K)+CC(IDO,4,K)                                    
         TR3 = CC(IDO,2,K)+CC(IDO,2,K)                                  
         TR4 = CC(1,3,K)+CC(1,3,K)                                      
         CH(1,K,1) = TR2+TR3                                            
         CH(1,K,2) = TR1-TR4                                            
         CH(1,K,3) = TR2-TR3                                            
         CH(1,K,4) = TR1+TR4                                            
  101 CONTINUE                                                          
      IF (IDO-2) 107,105,102                                            
  102 IDP2 = IDO+2                                                      
      IF((IDO-1)/2.LT.L1) GO TO 108                                     
      DO 104 K=1,L1                                                     
C                                                             
         DO 103 I=3,IDO,2                                               
            IC = IDP2-I                                                 
            TI1 = CC(I,1,K)+CC(IC,4,K)                                  
            TI2 = CC(I,1,K)-CC(IC,4,K)                                  
            TI3 = CC(I,3,K)-CC(IC,2,K)                                  
            TR4 = CC(I,3,K)+CC(IC,2,K)                                  
            TR1 = CC(I-1,1,K)-CC(IC-1,4,K)                              
            TR2 = CC(I-1,1,K)+CC(IC-1,4,K)                              
            TI4 = CC(I-1,3,K)-CC(IC-1,2,K)                              
            TR3 = CC(I-1,3,K)+CC(IC-1,2,K)                              
            CH(I-1,K,1) = TR2+TR3                                       
            CR3 = TR2-TR3                                               
            CH(I,K,1) = TI2+TI3                                         
            CI3 = TI2-TI3                                               
            CR2 = TR1-TR4                                               
            CR4 = TR1+TR4                                               
            CI2 = TI1+TI4                                               
            CI4 = TI1-TI4                                               
            CH(I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2                     
            CH(I,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2                       
            CH(I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3                     
            CH(I,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3                       
            CH(I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4                     
            CH(I,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4                       
  103    CONTINUE                                                       
  104 CONTINUE                                                          
      GO TO 111                                                         
  108 DO 110 I=3,IDO,2                                                  
         IC = IDP2-I                                                    
C                                                             
         DO 109 K=1,L1                                                  
            TI1 = CC(I,1,K)+CC(IC,4,K)                                  
            TI2 = CC(I,1,K)-CC(IC,4,K)                                  
            TI3 = CC(I,3,K)-CC(IC,2,K)                                  
            TR4 = CC(I,3,K)+CC(IC,2,K)                                  
            TR1 = CC(I-1,1,K)-CC(IC-1,4,K)                              
            TR2 = CC(I-1,1,K)+CC(IC-1,4,K)                              
            TI4 = CC(I-1,3,K)-CC(IC-1,2,K)                              
            TR3 = CC(I-1,3,K)+CC(IC-1,2,K)                              
            CH(I-1,K,1) = TR2+TR3                                       
            CR3 = TR2-TR3                                               
            CH(I,K,1) = TI2+TI3                                         
            CI3 = TI2-TI3                                               
            CR2 = TR1-TR4                                               
            CR4 = TR1+TR4                                               
            CI2 = TI1+TI4                                               
            CI4 = TI1-TI4                                               
            CH(I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2                     
            CH(I,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2                       
            CH(I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3                     
            CH(I,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3                       
            CH(I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4                     
            CH(I,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4                       
  109    CONTINUE                                                       
  110 CONTINUE                                                          
  111 IF (MOD(IDO,2) .EQ. 1) RETURN                                     
  105 DO 106 K=1,L1                                                     
         TI1 = CC(1,2,K)+CC(1,4,K)                                      
         TI2 = CC(1,4,K)-CC(1,2,K)                                      
         TR1 = CC(IDO,1,K)-CC(IDO,3,K)                                  
         TR2 = CC(IDO,1,K)+CC(IDO,3,K)                                  
         CH(IDO,K,1) = TR2+TR2                                          
         CH(IDO,K,2) = SQRT2*(TR1-TI1)                                  
         CH(IDO,K,3) = TI2+TI2                                          
         CH(IDO,K,4) = -SQRT2*(TR1+TI1)                                 
  106 CONTINUE                                                          
  107 RETURN                                                            
      END                                                               
