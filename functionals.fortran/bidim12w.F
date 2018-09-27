c!#   $Id: bidim12w.F,v 1.1 2000/11/13 13:47:12 mcc Exp $
c!#   $Log: bidim12w.F,v $
c!#   Revision 1.1  2000/11/13 13:47:12  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.2  1997/12/18 20:05:10  rnh
c!#   Added comments detailing the calculation method.
c!#
c!#	Revision 1.1  1997/04/17  18:44:35  rnh
c!#	Initial revision
c!#
c     ******************************************************************
 
      subroutine bidim12w 
c 
c**** bidim12w calculates bidimensional 12 point interpolation weights
c 
c     PURPOSE 

c     This subroutine calculates the weights needed to perform
c     bidimensional 12 point interpolation.

c**   INTERFACE
c     
     I     ( xobs, yobs, Nw,
     O     w )

c     xobs       x-coordinate of observation             (Input)
c     yobs       y-coordinate of observation             (Input)
c     w          weights for bidim 12 pt interpolation   (Output)
      real xobs, yobs, w(Nw,Nw)

c     Local variables.
c     a          intermediate factors used in computing the weights
c     b          intermediate factors used in computing the weights
      real a, b
      dimension a(4), b(4)
      integer i, j

c     ------------------------------------------------------------------

c     1. Calculate the weights for bidim 12 pt interpolation.

c     Use the Lagrange multiplier representation of collocating cubic
c        f(x) = \sum_j f_j (\prod_(i \ne j) (x - x_i))/
c                          (\prod_(i \ne j) (x_j - x_i)) .
c     Taking the points at (-1, 0, 1, 2) to determine the 
c     weights for interpolating cubically to the x and y locations
c     separately.

      a(1) = (xobs * (xobs - 1) * (xobs - 2))/(-6)
      a(2) = ((1 + xobs) * (xobs - 1) * (xobs - 2))/2
      a(3) = ((1 + xobs) * xobs * (xobs - 2))/(-2)
      a(4) = ((1 + xobs) * xobs * (xobs - 1))/6
      b(1) = (yobs * (yobs - 1) * (yobs - 2))/(-6)
      b(2) = ((1 + yobs) * (yobs - 1) * (yobs - 2))/2
      b(3) = ((1 + yobs) * yobs * (yobs - 2))/(-2)
      b(4) = ((1 + yobs) * yobs * (yobs - 1))/6

c     Interpolating in x and then in y,
c         f = b_1 f_1 + b_2 f_2 + ...
c           = b_1 ( (1-x) f_21 + x f_31) + 
c             b_2 ( a_1 f_12 + a_2 f_22 + a_3 f_32 + a_4 f_42) + ...
c     shows that the x and y weights multiply.

      w(1,1) = 0.
      w(2,1) = b(1)*(1 - xobs)
      w(3,1) = b(1)*xobs
      w(4,1) = 0.
      do 20 j=2,3
        do 10 i=1,4
          w(i,j)=b(j)*a(i)
   10   continue
   20 continue
      w(1,4) = 0.
      w(2,4) = b(4)*(1 - xobs)
      w(3,4) = b(4)*xobs
      w(4,4) = 0.

      Return
      End
