      SUBROUTINE PASSF2(IDO,L1,CC,CH,WA1)                               
C***BEGIN PROLOGUE  PASSF2                                              
c!#   $Id: passf2.F,v 1.2 2000/11/27 19:25:40 mcc Exp $
c!#   $Log: passf2.F,v $
c!#   Revision 1.2  2000/11/27 19:25:40  mcc
c!#   Removed {DIR$ IVDEP} from comment lines
c!#
c!#   Revision 1.1  2000/11/07 20:10:10  mcc
c!#   Routine added to build fft.f90dir lib. Initial rev.
c!#
c!#   Revision 1.1  1997/02/07 16:57:20  leidner
c!#   Initial revision
c!#
C***REFER TO  CFFTF                                                     
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  PASSF2                                                
      DIMENSION       CC(IDO,2,L1)           ,CH(IDO,L1,2)           ,  
     1                WA1(1)                                            
C***FIRST EXECUTABLE STATEMENT  PASSF2                                  
      IF (IDO .GT. 2) GO TO 102                                         
      DO 101 K=1,L1                                                     
         CH(1,K,1) = CC(1,1,K)+CC(1,2,K)                                
         CH(1,K,2) = CC(1,1,K)-CC(1,2,K)                                
         CH(2,K,1) = CC(2,1,K)+CC(2,2,K)                                
         CH(2,K,2) = CC(2,1,K)-CC(2,2,K)                                
  101 CONTINUE                                                          
      RETURN                                                            
  102 IF(IDO/2.LT.L1) GO TO 105                                         
      DO 104 K=1,L1                                                     
C                                                             
         DO 103 I=2,IDO,2                                               
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)                       
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)                               
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)                             
            TI2 = CC(I,1,K)-CC(I,2,K)                                   
            CH(I,K,2) = WA1(I-1)*TI2-WA1(I)*TR2                         
            CH(I-1,K,2) = WA1(I-1)*TR2+WA1(I)*TI2                       
  103    CONTINUE                                                       
  104 CONTINUE                                                          
      RETURN                                                            
  105 DO 107 I=2,IDO,2                                                  
C                                                             
      DO 106 K=1,L1                                                     
            CH(I-1,K,1) = CC(I-1,1,K)+CC(I-1,2,K)                       
            TR2 = CC(I-1,1,K)-CC(I-1,2,K)                               
            CH(I,K,1) = CC(I,1,K)+CC(I,2,K)                             
            TI2 = CC(I,1,K)-CC(I,2,K)                                   
            CH(I,K,2) = WA1(I-1)*TI2-WA1(I)*TR2                         
            CH(I-1,K,2) = WA1(I-1)*TR2+WA1(I)*TI2                       
  106    CONTINUE                                                       
  107 CONTINUE                                                          
      RETURN                                                            
      END                                                               
