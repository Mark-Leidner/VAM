c
c     ******************************************************************
c
      subroutine SSSSMI
c
c**** SSSSMI calculates the sum of squares due to ssmi wind data
c
c     Copyright (c)       Ross Hoffman           AER        Apr 97
c!#   $Id: ssssmi.F,v 1.1 2000/11/13 13:47:58 mcc Exp $
c!#   $Log: ssssmi.F,v $
c!#   Revision 1.1  2000/11/13 13:47:58  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.2  1998/05/15 14:44:58  stanr
c!#   made both args to MOD function REAL (4 occurrences)
c!#
c!#   Revision 1.1  1997/04/17 18:44:35  rnh
c!#   Initial revision
c!#
c
c     PURPOSE
c
c     Calculate lack of fit to ssmi data
c     including directional information.
c
c**   INTERFACE
c
     C    ( idim,
     I    u, v,
     O    delssu, delssv,
     O    Jssmi, Jvelm, Jlosm)
c
c*    Input constants
c     idim      First dimension of grids
      integer idim
c
c*    Input values
c     u         Gridded u-component wind (at 10 m)
c     v         Gridded v-component wind (at 10 m)
      real u(idim,1), v(idim,1)
c
c*    Output variables
c     delssu    Adjoint of u
c     delssv    Adjoint of v
c     Jssmi     Combined cost function
c     Jvelm     Cost function due to wind speed
c     Jlosm     Cost function due to wind speed along line of sight
      real delssu(idim,1), delssv(idim,1)
      real*8 Jssmi, Jvelm, Jlosm
c
c*    Data passed through common.
#include "vam.h"
#include "ssmidata.h"
#include "gparm.h"
c
c**   METHOD
c
c     Calculate increments for Jvelm and Jlosm,
c     and then increment cost functions.
c     Then do adjoint calculation
c
c**   EXTERNALS
c
c     ssmicost, ssmicostad
c
c**   REFERENCES
c
c     None
c
c     ------------------------------------------------------------------
c
c**   0. Define local variables
c
c     Vmin      Minimum wind speed
      real vmin
      parameter (vmin = 1e-4)
c
c*    trajectory variables
c     uc5       u-component wind at obs location
c     vc5       v-component wind at obs location
c     velm5     Wind speed
c     dir5      Wind direction
c     losm5     LOS wind speed
c     dJvelm5   Cost function increment due to wind speed
c     dJlosm5   Cost function increment due to LOS wind
      real uc5, vc5, velm5, dir5, losm5, dJvelm5, dJlosm5
c
c*    corresponding adjoint variables
      real uc, vc, velm, dir, losm, dJvelm, dJlosm
c     which must be initialized to zero
      uc = 0
      vc = 0
      velm = 0
      dir = 0
      losm = 0
      dJvelm = 0
      dJlosm = 0
c
c     ------------------------------------------------------------------
c
c**   1. Initialize cost functions
c
      Jssmi = 0
      Jvelm = 0
      Jlosm = 0
c
c     ------------------------------------------------------------------
c
c**   2. For each ssmi obs with good data
c
      do i = 1, nssmi
        if (vssmi(i,ivelm) .ne. mvi_ssmi) then
c
c     ------------------------------------------------------------------
c
c**   3. Calculate the cost function increments
c
          call ssmicost
     c        ( vmin, idim, vssmi(i,ilosm) .ne. mvi_ssmi,
     c        int(vssmi(i,ixg)), int(vssmi(i,iyg)),
     c        mod(vssmi(i,ixg),1.0), mod(vssmi(i,iyg),1.0),
     c        vssmi(i,ipx), vssmi(i,ipy),
     c        vssmi(i,ivelm), vssmi(i,ilosm),
     i        u, v,
     o        uc5, vc5, velm5, dir5, losm5, dJvelm5, dJlosm5 )
c
c     ------------------------------------------------------------------
c
c**   4. Increment cost functions
c
          Jvelm = Jvelm + dJvelm5
          Jlosm = Jlosm + dJlosm5
          Jssmi = Jssmi + wgt_velm*dJvelm5 + wgt_losm*dJlosm5
c
c     ------------------------------------------------------------------
c
c**   5. Adjoint calculation, starting with adjoint of Jssmi = 1
c
          dJvelm = wgt_velm
          dJlosm = wgt_losm
c
          call ssmicostad
     c        ( vmin, idim, vssmi(i,ilosm) .ne. mvi_ssmi,
     c        int(vssmi(i,ixg)), int(vssmi(i,iyg)),
     c        mod(vssmi(i,ixg),1.0), mod(vssmi(i,iyg),1.0),
     c        vssmi(i,ipx), vssmi(i,ipy),
     c        vssmi(i,ivelm), vssmi(i,ilosm),
     i        delssu, delssv,
     t        u, v,
     t        uc5, vc5, velm5, dir5, losm5, dJvelm5, dJlosm5,
     o        uc, vc, velm, dir, losm, dJvelm, dJlosm )
c
c     ------------------------------------------------------------------
c
        endif
      enddo
c
      return
      end
