c!#   $Id: errsea.F,v 1.2 1997/02/18 17:59:24 leidner Exp $
c!#   $Log: errsea.F,v $
c!#   Revision 1.2  1997/02/18 17:59:24  leidner
c!#   remove redundant code redirecting program flow if IER.LE.512 or IER.GT.532
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE ERRSEA (IER,LNAME)
#include "vam.h"
C***********************************************************************
C*****ERRSEA  IS CALLED WHEN AN ERROR CONDITION OCCURS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : ..INVG PLTEND TCHECK ...... ...... ...... ...... ......
C-----GMSF : ERRTRA ...... ...... ...... ...... ...... ...... ......
C-----FORT : IBCOM# ...... ...... ...... ...... ...... ...... ......
      character*2 LNAME(3),LINVG(3)
      data LINVG/'IN','VG','  '/
      WRITE (6,100) IER,LNAME
  100 FORMAT (/////11X,'*****',10X,'ERROR CONDITION',I5,10X,
     +   'RETURNED BY DEALIAS ROUTINE  ',3A2,'.',10X,'*****'//)
      if (ier .le. 512 .or. ier .gt. 532) then 
         print *, ier,lname
         go to 1000
      endif
      GO TO (13,14,15,16,17,18,19,20,21,22,
     +       23,24,25,26,27,28,29,30,31,32), IER-512
   13 WRITE (6,513)
      GO TO 1000
   14 WRITE (6,514)
      GO TO 1000
   15 WRITE (6,515)
      GO TO 1000
   16 WRITE (6,516)
      GO TO 1000
   17 WRITE (6,517)
      GO TO 1000
   18 WRITE (6,518)
      GO TO 1000
   19 WRITE (6,519)
      GO TO 1000
   20 WRITE (6,520)
      GO TO 1000
   21 WRITE (6,521)
      GO TO 1000
   22 WRITE (6,522)
      GO TO 1000
   23 WRITE (6,523)
      GO TO 1000
   24 WRITE (6,524)
      GO TO 1000
   25 WRITE (6,525)
      GO TO 1000
   26 WRITE (6,526)
      GO TO 1000
   27 WRITE (6,527)
      GO TO 1000
   28 WRITE (6,528)
      GO TO 1000
   29 WRITE (6,529)
      GO TO 1000
   30 WRITE (6,530)
      GO TO 1000
   31 WRITE (6,531)
      GO TO 1000
   32 WRITE (6,532)
 1000 DO 1010 I=1,3
      IF (LNAME(I).NE.LINVG(I)) GO TO 1025
 1010 CONTINUE
      GO TO 1050
 1025 continue     
 1050 continue
c     CALL ERRTRA
      CALL TCHECK
      call exit(1)
  513 FORMAT ('0NO DATA POINTS IN COMMON BLOCK.')
  514 FORMAT ('0INCONSISTENT GRID PARAMETERS.')
  515 FORMAT ('0INPUT GRID SMALLER THAN OUTPUT GRID - FILL NOT',
     +   ' IMPLEMENTED.')
  516 FORMAT ('0NAMED GRID NOT FOUND.')
  517 FORMAT ('0UNAVAILABLE TASK.')
  518 FORMAT ('0ERROR READING INPUT DATA SET.')
  519 FORMAT ('0END OF INPUT DATA SET REACHED UNEXPECTEDLY.')
  520 FORMAT ('0NAMELIST MISSING OR IN ERROR.')
  521 FORMAT ('0DIMENSIONS EXCEEDED - TOO MANY OBSERVATIONS.')
  522 FORMAT ('0GRID EXCEEDS ALLOWABLE DIMENSIONS.')
  523 FORMAT ('0RDLEV2 ERROR 1. INVALID TYPE.')
  524 FORMAT ('0RDLEV2 ERROR 2. SYNOPTIC TIME NOT FOUND.')
  525 FORMAT ('0RDLEV2 ERROR 3. INVALID INDEX.')
  526 FORMAT ('0RDLEV2 ERROR 4. I/O ERROR.')
  527 FORMAT ('0PERFECTLY CALM SASS DATA POINT; PERFECTLY IMPOSSIBLE.')
  528 FORMAT ('0PLOTTER NOT INITIALIZED.')
  529 FORMAT ('0POLE POINT CODE UNAVAILABLE.')
  530 FORMAT ('0IMPOSSIBLE DIVIDE CHECK.')
  531 FORMAT ('0SUBJECTIVELY CHOSEN AMBIGUITY OUT OF BOUNDS.')
  532 FORMAT ('0ERROR CODE NOT ASSIGNED.')
C*****
C*****------------------------------------------------------------------
C*****     ERROR CODE                  CAUSE
C*****------------------------------------------------------------------
C*****        513       NO DATA POINTS IN COMMON BLOCK.
C*****        514       INCONSISTENT GRID PARAMETERS.
C*****        515       INPUT GRID SMALLER THAN OUTPUT GRID - FILL NOT
C*****                  IMPLEMENTED.
C*****        516       NAMED GRID NOT FOUND.
C*****        517       UNAVAILABLE TASK.
C*****        518       ERROR READING INPUT DATA SET.
C*****        519       END OF INPUT DATA SET REACHED UNEXPECTEDLY.
C*****        520       NAMELIST MISSING OR IN ERROR.
C*****        521       DIMENSIONS EXCEEDED - TOO MANY OBSERVATIONS.
C*****        522       GRID EXCEEDS ALLOWABLE DIMENSIONS.
C*****        523       RDLEV2 ERROR 1. INVALID TYPE.
C*****        524       RDLEV2 ERROR 2. SYNOPTIC TIME NOT FOUND.
C*****        525       RDLEV2 ERROR 3. INVALID INDEX.
C*****        526       RDLEV2 ERROR 4. I/O ERROR.
C*****        527       PERFECTLY CALM SASS DATA POINT;
C*****                  PERFECTLY IMPOSSIBLE.
C*****        528       PLOTTER NOT INITIALIZED.
C*****        529       POLE POINT CODE UNAVAILABLE.
C*****        530       IMPOSSIBLE DIVIDE CHECK.
C*****        531       SUBJECTIVELY CHOSEN AMBIGUITY OUT OF BOUNDS.
C*****        532       ERROR CODE NOT ASSIGNED.
C*****------------------------------------------------------------------
C*****
      END
