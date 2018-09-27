c!#   $Id: pgrid.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: pgrid.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      SUBROUTINE PGRID (IG,NI,NJ,NDIM)
#include "vam.h"
C***********************************************************************
C*****PGRID PLOTS A GRID - EACH GRID SQUARE IS HALF INCH SQUARE
C-----8 LINES TO THE INCH - 10 CHARACTERS TO THE INCH
C-----SPECIFICATIONS
      character*4 LBLANK(20),LDASH(20)
      integer uptr
      data LBLANK/20*'    '/
      data LDASH/20*'----'/
      data UPTR/6/
      INTEGER IG(NDIM,NJ)
      DIMENSION RG(NDIM,NJ)
      INTEGER ITEMP(20)
C***********************************************************************
C*****ENTRY IPGRID PLOTS AN INTEGER GRID
      ENTRY IPGRID (IG,NI,NJ,NDIM)
      NE=0
   25 NS=NE+1
      NE=NE+20
      IF (NE.GT.NI) NE=NI
      N=NE-NS+1
      DO 100 J=1,NJ
      WRITE (UPTR,101) (LBLANK(I),I=1,N)
      WRITE (UPTR,101) (LBLANK(I),I=1,N)
      WRITE (UPTR,102) (IG(I+NS-1,NJ+1-J),I=1,N)
      IF (MOD(J,5).EQ.0) WRITE (UPTR,103) (LDASH(I),I=1,N)
      IF (MOD(J,5).NE.0) WRITE (UPTR,103) (LBLANK(I),I=1,N)
  100 CONTINUE
      IF (NE.LT.NI) GO TO 25
      RETURN
C***********************************************************************
C*****ENTRY RPGRID SCALES AND PLOTS A REAL GRID
      ENTRY RPGRID (RG,NI,NJ,NDIM,SCALE)
      NE=0
  125 NS=NE+1
      NE=NE+20
      IF (NE.GT.NI) NE=NI
      N=NE-NS+1
      DO 200 J=1,NJ
      WRITE (UPTR,101) (LBLANK(I),I=1,N)
      WRITE (UPTR,101) (LBLANK(I),I=1,N)
      DO 150 I=1,N
C-----MULTILPLY GRID VALUES BY SCALE
      ITEMP(I) = RG(I+NS-1,NJ+1-J) * SCALE
  150 CONTINUE
      WRITE (UPTR,102) (ITEMP(I),I=1,N)
      IF (MOD(J,5).EQ.0) WRITE (UPTR,103) (LDASH(I),I=1,N)
      IF (MOD(J,5).NE.0) WRITE (UPTR,103) (LBLANK(I),I=1,N)
  200 CONTINUE
      IF (NE.LT.NI) GO TO 125
      RETURN
  101 FORMAT (11X,4(4(A4,1X),A4,'|'))
  102 FORMAT (11X,20I5)
  103 FORMAT (11X,20(A4,'+'))
      END
