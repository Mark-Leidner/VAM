c!#   $Id: bidim.for,v 1.1 2000/11/13 13:47:10 mcc Exp $
c!#   $Log: bidim.for,v $
c!#   Revision 1.1  2000/11/13 13:47:10  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.2  1997/04/17 18:47:09  rnh
c!#   Put into .for archive.
c!#
c!#	Revision 1.1  1997/04/09  15:08:58  rnh
c!#	Initial revision
c!#

c     ******************************************************************

<NLM> subroutine bidim
<LTM> subroutine bidimtl
<ADJ> subroutine bidimad

c**** bidim interpolates the analysis to one observation location

c     PURPOSE

c     Interpolates gridded analysis to an observation point.
c     
c**   INTERFACE

     C     ( idim, Nw,
     I     f, i, j, w,
     O     fi )

cPER> LINEAR ROUTINE - NO TRAJECTORY VALUES !!!

c     idim      dimension of f array                    (Input-Constant)
c     Nw        dimension of weights array              (Input-Constant)
c     f         gridpoint values                        (Input) 
c     i,j       lower left-hand corner of grid box      (Input)
c     w         weights for bidim 12 pt interpolation   (Input)
c     fi        interpolated value at (xobs,yobs)       (Output)
      real f(idim,1), w(Nw,Nw), fi
      integer i,ii,j,jj

c     ------------------------------------------------------------------

c     1. Interpolate analysis to location of observation.

<NLM> fi = 0
<LTM> fi = 0
<ADJ> fi = 0
      do 12 ii=1,Nw
        do 10 jj=1,Nw
<NLM>     fi = fi + w(ii,jj)*f(i-Nw/2+ii,j-Nw/2+jj)
<LTM>     fi = fi + w(ii,jj)*f(i-Nw/2+ii,j-Nw/2+jj)
<ADJ>     f(i-Nw/2+ii,j-Nw/2+jj) += w(ii,jj)*fi
  10    continue
  12  continue
      Return
      End
