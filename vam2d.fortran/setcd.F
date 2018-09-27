c!#   $Id: setcd.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: setcd.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE SETCD (CDRAG,IDEBUG)
#include "vam.h"
C***********************************************************************
C*****SETCD SETS DRAG COEFFICIENT IN GRDTA COMMON AREA.  VALUE STORED
C*****      IS ACTUALLY -CD/DEL(Z) , WHERE DEL(Z) IS THE HEIGHT OF THE
C*****      BOUNDARY LAYER WHERE THE STRESS IS PRESUMED TO VANISH
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : RPGRID ...... ...... ...... ...... ...... ...... ......
C-----FORT : IBCOM# ...... ...... ...... ...... ...... ...... ......
#include "gparm.h"
#include "grdta.h"
C-----DEFINE NUMERICAL CONSTANTS
      DATA ONE/1D0/
      DO 100 J=1,NY
      DO 100 I=1,NX
C-----CONSTANT DRAG COEFFICIENT USED
C-----CONSTANT PBL HEIGHT OF 1 KM USED
      CD(I,J)=-CDRAG/1000
  100 CONTINUE
C-----
      SCALE=(ONE*10)**8
      WRITE (6,101) CDRAG,CD(1,1)
  101 FORMAT ('0CD(1,1) = - DRAG COEFFICIENT / DELTA Z = - ',
     +    E15.7,' / 1000 = ',E15.7)
      IF (IDEBUG.EQ.0) RETURN
      WRITE (6,102) SCALE
  102 FORMAT ('0CD = - DRAG COEFFICIENT / DELTA Z * ',E15.7//)
      CALL RPGRID (CD(IP,JP),NIP,NJP,IDIM,SCALE)
      RETURN
      END
