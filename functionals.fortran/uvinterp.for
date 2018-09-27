c!#   $Id: uvinterp.for,v 1.1 2000/11/13 13:48:09 mcc Exp $
c!#   $Log: uvinterp.for,v $
c!#   Revision 1.1  2000/11/13 13:48:09  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.4  1998/05/15 15:00:23  stanr
c!#   split "integer NWgts(2)/2,4/" for standard conformance
c!#
c!#   Revision 1.3  1997/04/17 18:47:09  rnh
c!#   Put into .for archive.
c!#
c!#	Revision 1.2  1997/04/17  17:44:15  rnh
c!#	Set adjoint variable to zero after use.
c!#
c!#	Revision 1.1  1997/04/09  15:08:58  rnh
c!#	Initial revision
c!#
c
c     ******************************************************************

<NLM> subroutine uvinterp
<LTM> subroutine uvinterptl
<ADJ> subroutine uvinterpad

c**** uvinterp interpolates the analysis to a set of observation locations

c     PURPOSE

c     Interpolates gridded analysis to set of observation points.
     
c**   INTERFACE

     c     ( idim, n,
     c       ic, jc, xc, yc,
     i       u, v,
     o       uc, vc )

cPER> LINEAR ROUTINE - NO TRAJECTORY VALUES !!!

c     idim      dimension of f array                   (Input-Constant)
c     n         number of observations in this group   (Input-Constant)   
c     ic        i-locations of LLC of obs locations    (Input) 
c     jc        j-locations of LLC of obs locations    (Input) 
c     xc        x-locations obs in a unit grid square  (Input) 
c     yc        y-locations obs in a unit grid square  (Input) 
c     u,v       gridpoint values                       (Input)
c     uc,vc     interpolated values                    (Output)

      integer idim, n, ic(n), jc(n)
      real xc(n), yc(n), u(idim,1), v(idim,1), uc(n), vc(n)

c     Local variables.
      parameter (MaxNW = 4)
      real w(MaxNW*MaxNW)
      integer NWgts(2)
      data    NWgts   /2,4/

#include "vam.h"
#include "gparm.h"

c     ------------------------------------------------------------------

c     1. Interpolate analysis to location of observation.


<ALL> Nw = NWgts(interp)

      do 100 k = 1, n


c-----Calculate weights for interpolation

<ALL>   if (interp .eq. 1) call bidimw  ( xc(k), yc(k), Nw, w )
<ALL>   if (interp .eq. 2) call bidim12w( xc(k), yc(k), Nw, w )

c-----Get analyzed values interpolated to point

<NLM>   call bidim
<LTM>   call bidimtl
<ADJ>   call bidimad
     c      ( idim, Nw,
     i      u, ic(k), jc(k), w,
     o      uc(k) )

<NLM>   call bidim
<LTM>   call bidimtl
<ADJ>   call bidimad
     c      ( idim, Nw,
     i      v, ic(k), jc(k), w,
     o      vc(k) )

  100 continue
      
      return
      end
