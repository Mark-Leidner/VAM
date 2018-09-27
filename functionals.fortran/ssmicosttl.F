c
c     ******************************************************************
c
      subroutine SSMICosttl
c
c**** SSMICost calculates the departures from the ssmi wind data
c
c     Copyright (c)       Ross Hoffman           AER        Apr 97
c!#   $Id: ssmicosttl.F,v 1.1 2000/11/13 14:15:49 mcc Exp $
c!#   $Log: ssmicosttl.F,v $
c!#   Revision 1.1  2000/11/13 14:15:49  mcc
c!#   File added for non in-house build of libss.a . Initial revision.
c!#
c!#   Revision 1.1  2000/11/13 13:47:50  mcc
c!#   File added for build of libss.a. Initial revision.
c!#
c!#   Revision 1.2  1997/08/20 14:45:39  leidner
c!#   corrected trajectory los wind calculation
c!#
c!#	Revision 1.1  1997/04/17  18:44:35  rnh
c!#	Initial revision
c!#
c
c     PURPOSE
c
c     Calculate departures to ssmi wind data
c     including directional information.
c
c**   INTERFACE
c
     c    ( Vmin, idim, lc_losm,
     c       ic, jc, xc, yc, px, py, velm_o, losm_o,
     i       u, v,
     t       u5, v5,
     t       uc5, vc5, velm5, dir5, losm5, Jvelm5, Jlosm5,
     o       uc, vc, velm, dir, losm, Jvelm, Jlosm )
c
c*    Input constants
c     Vmin      Minimum wind speed
c     idim      First dimension of grids
c     lc_losm   Calculate losm?
      real Vmin
      integer idim
      logical lc_losm
c
c     (ic,jc)   Grid cell coordinates of obs
c     (xc,yc)   Intra-grid cell coordinates of obs
c     (px,py)   (sin,cos) of azimuth direction
c     velm_o    Observed wind speed
c     losm_o    Observed LOS wind speed
      integer ic, jc
      real xc, yc, px, py, velm_o, losm_o
c
c*    Input variables
c     u        Gridded u-component wind
c     v        Gridded v-component wind
      real u(idim,1), v(idim,1)
c
c*    Output values
c     uc        u-component wind at obs location
c     vc        v-component wind at obs location
c     velm      Wind speed
c     dir       Wind direction
c     losm      LOS wind speed
c     Jvelm     Cost function due to wind speed
c     Jlosm     Cost function due to LOS wind
      real uc, vc, velm, dir, losm, Jvelm, Jlosm
c    
c     Variables with a 5 suffix are the corresponding trajectory
c     variables.
      real u5(idim,1), v5(idim,1)
      real uc5, vc5, velm5, dir5, losm5, Jvelm5, Jlosm5
c
c**   METHOD
c
c     Interpolate grid to obs locations.
c     Evaluate velm and losm and then cost functions for a single obs.
c     These are standard quadratic loss functions.
c
c**   EXTERNALS
c
c     uvinterp, jsccvd
c     uvinterptl, jsccvdtl
c
c**   REFERENCES
c
c     None
c
c     ------------------------------------------------------------------
c
c**   1. Calculate the analysis wind interpolated to the obs location. 
c     NOTE interpolation is linear in u and v!!!
c
      call uvinterptl
     c    ( idim, 1,
     c    ic, jc, xc, yc,
     i    u, v,
     o    uc, vc )
c
c     ------------------------------------------------------------------
c
c**   2. Calculate trajectory wind speed and los wind
c
      call jsccvdtl
     c    ( vmin,
     i    uc, vc,
     t    uc5, vc5,
     t    velm5, dir5,
     o    velm, dir )
      losm = (-px)*uc + (-py)*vc
c
c     ------------------------------------------------------------------
c
c**   3. Calculate the functionals
c
      Jvelm = 2*(velm5 - velm_o)*velm
      if (lc_losm) then
        Jlosm = 2*(losm5 - losm_o)*losm
      else
        Jlosm = 0
      endif
c
      return
      end
