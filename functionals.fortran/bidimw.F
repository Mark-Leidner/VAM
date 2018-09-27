c!#   $Id: bidimw.F,v 1.1 2000/11/13 13:47:16 mcc Exp $
c!#   $Log: bidimw.F,v $
c!#   Revision 1.1  2000/11/13 13:47:16  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.1  1997/04/17 18:44:35  rnh
c!#   Initial revision
c!#
c     ******************************************************************
 
      subroutine bidimw
c 
c**** bidimw calculates the weights for bilinear interpolation 
c 
c     PURPOSE 

c     This subroutine calculates the weights needed to perform
c     bilinear interpolation.

c**   INTERFACE
c     
     I     ( xobs, yobs, Nw,
     O     w )

c     xobs       x-coordinate of observation             (Input)
c     yobs       y-coordinate of observation             (Input)
c     w          weights for bilinear interpolation      (Output)
      real xobs, yobs, w(Nw,Nw)

c     ------------------------------------------------------------------

c     1. Calculate the weights for bilinear interpolation.

      w(2,2) = xobs*yobs
      w(2,1) = xobs-w(2,2)
      w(1,2) = yobs-w(2,2)
      w(1,1) = 1 - yobs - w(2,1)

      Return
      End
