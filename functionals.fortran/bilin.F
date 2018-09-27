c!#   $Id: bilin.F,v 1.1 2000/11/13 13:47:19 mcc Exp $
c!#   $Log: bilin.F,v $
c!#   Revision 1.1  2000/11/13 13:47:19  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
      REAL FUNCTION BILIN (X,Y,F,NDIMF)
C***********************************************************************
C*****BILIN PERFORMS BILINEAR INTERPOLATION WITHIN THE UNIT SQUARE
C-----F POINTS TO GRID LOCATION OF LOWER LEFT CORNER
C-----
C-----XREFS: ..NONE
C-----SPECIFICATIONS
      DIMENSION F(NDIMF,1)
      BILIN=X*Y*(F(2,2)-F(2,1)-F(1,2)+F(1,1)) +
     +     X*(F(2,1)-F(1,1)) + Y*(F(1,2)-F(1,1)) + F(1,1)
      RETURN
      END
