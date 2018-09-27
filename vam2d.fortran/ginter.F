c!#   $Id: ginter.F,v 1.1.1.1 1997/02/21 21:06:24 leidner Exp $
c!#   $Log: ginter.F,v $
c!#   Revision 1.1.1.1  1997/02/21 21:06:24  leidner
c!#   added formatted debug writes
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE GINTER (NDIM0,F0,XS0,DELX0,NX0,IPER0,YS0,DELY0,NY0,
     +   JPER0,NDIM,F,XS,DELX,NX,YS,DELY,NY,FILL)
#include "vam.h"
C***********************************************************************
C*****GINTER INTERPOLATES (BILINEARLY) FROM ONE GRID TO A NEW GRID
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : .BILIN .GRIDX ERRSEA ...... ...... ...... ...... ......
C-----F0 IS INPUT GRID
C-----F IS OUTPUT GRID
      DIMENSION F0(NDIM0,NY0), F(NDIM,NY)
C-----FOR EACH NEW GRID POINT
c     write(*,200) NDIM0,NY0,
c    &             XS0,DELX0,NX0,IPER0,
c    &             YS0,DELY0,NY0,JPER0
c     write(*,210) NDIM,NY,
c    &             XS,DELX,NX,IPER,
c    &             YS,DELY,NY,JPER
  200 format(/'in ginter, F0(NDIM0,NY0): NDIM0,NY0=',2i4,/
     &        '                XS0,DELX0,NX0,IPER0=',2f8.3,2i4,/
     &        '                YS0,DELY0,NY0,JPER0=',2f8.3,2i4)
  210 format( 'in ginter, F (NDIM, NY ): NDIM, NY =',2i4,/
     &        '                XS, DELX, NX, IPER =',2f8.3,2i4,/
     &        '                YS, DELY, NY, JPER =',2f8.3,2i4/)

      XMIN=1
      XMAX=NX0
      YMIN=1
      YMAX=NY0
      DO 1000 J=1,NY
      DO 1000 I=1,NX
C-----DETERMINE PHYSICAL COORDINATES OF NEW GRIDPOINT
      X=XS+(I-1)*DELX
      Y=YS+(J-1)*DELY
C-----DETERMINE OLD PHYSICAL COORDINATES. IN THIS CASE
C*****BOTH GRIDS ARE BASED ON SAME COORDINATE SYSTEM.
      X0=X
      Y0=Y
c     write(*,230) I,J,X,XS0,DELX0,IPER0,XMIN,XMAX 
  230 format('   I,J,X,XS0,DELX0,IPER0,XMIN,XMAX =',2i4,3f8.3,i4,2f8.3)
C-----TRANSFORM TO OLD GRID COORDINATES
      CALL GRIDX (X0,XS0,DELX0,IPER0,XMIN,XMAX,II,XX,*777)
c     write(*,235) II,XX
  235 format('                             II,XX =',i4,f8.5)
c     write(*,240) Y,YS0,DELY0,JPER0,YMIN,YMAX 
  240 format('       Y,YS0,DELY0,JPER0,YMIN,YMAX =',8x,3f8.3,i4,2f8.3)
      CALL GRIDX (Y0,YS0,DELY0,JPER0,YMIN,YMAX,JJ,YY,*777)
c     write(*,245) JJ,YY
  245 format('                             JJ,YY =',i4,f8.5)
C-----INTERPOLATE
      F(I,J)=BILIN (XX,YY,F0(II,JJ),NDIM0)
c     write(*,250) F0(II,JJ),F(I,J)
  250 format('                  F0(II,JJ),F(I,J) =',2f8.3/)
 1000 CONTINUE
      RETURN
C-----INPUT GRID IS TOO SMALL
  777 CALL ERRSEA (515,6HGINTER)
      RETURN
      END
