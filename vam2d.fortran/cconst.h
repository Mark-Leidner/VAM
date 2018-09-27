c!#   $Id: cconst.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: cconst.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally cconst.blk)
c!#

c     control parameters
      parameter (Nl=8, Ni=5, Na=3, Nm=40, NObsDay=2, Nvp=1, Nvv=1)
      real mu, dt
      parameter (Nx=Na+Ni*Nvp, mu=0.5, dt=(Nl/(1.0*NObsDay))/Nm)

c     Nl = Number of Levels with observations
c     Ni = Number of Instances or cases (e.g. forecasts)
c     Na = Number of Alpha parameters
c     Nm = Number of tiMe steps
c     NObsDay = Number of OBServations per DAY
c     Nvp = Number of Variables - Prognostic
c     Nvv = Number of Variables - Verification
c     mu = Implicity time scheme parameter
c     dt = time step (days)
c     Nx = Number of elements in X, the control parameters

      common /scales/ fscale, scale(Nx)
