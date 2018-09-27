c!#   $Id: ssmidata.h,v 1.3 1998/02/11 20:04:04 leidner Exp $
c!#	$Log: ssmidata.h,v $
c!#	Revision 1.3  1998/02/11 20:04:04  leidner
c!#	increased storage space for ssmi data
c!#	
c!#	Revision 1.2  1997/10/23 20:39:07  leidner
c!#	added data to ssmidata buffer
c!#
c!#	Revision 1.1  1997/04/17  18:57:42  rnh
c!#	Initial revision
c!#
c!#
c     The maximum number of SSM/I observations is
      integer mssmi
      parameter (mssmi=50000)

c     and there are
      integer mssmivar
      parameter (mssmivar=16)
c     real variables needed.  These are:
      integer ixg, iyg
      parameter (ixg = 1, iyg = 2)
c     for x and y coordinates in grid units,
      integer ivelm, ilosm
      parameter (ivelm = 3, ilosm = 4)
c     for ordinary and line of sight velocity magnitudes,
      integer ipx, ipy
      parameter (ipx = 5, ipy = 6)
c     for the sine and cosine of the azimuth direction
c     (measured clockwise from north), 
      integer isrow, iscol
      parameter (isrow = 7, iscol = 8)
c     for ssmi scan row and column indices,
      integer isrev
      parameter (isrev = 9)
c     for ssmi fractional rev number,
      integer isst
      parameter (isst = 10)
c     for climatology sea-surface temperature (C),
      integer itice
      parameter (itice = 11)
c     for percent of total sea ice in cell (%),
      integer iwv
      parameter (iwv = 12)
c     for integrated water vapor (mm),
      integer icw
      parameter (icw = 13)
c     for integrated cloud water (mm),
      integer irain
      parameter (irain = 14)
c     for surface rain rate (mm/h), and
      integer ivelm5, ilosm5
      parameter (ivelm5 = 15, ilosm5 = 16)
c     for ordinary and line of sight trajectory velocity magnitudes.
c 
c     Data for the SSM/I loss function is contained in the matrix
      real vssmi(mssmi,mssmivar)

c     With these definitions, if the simulated wind is (uc, vc),
c     then the simulated los wind is simply:
c     vssmi(i,ipx)*uc + vssmi(i,ipy)*vc
        
c     The number of ssmi observations is
      integer nssmi

c     The missing value indicator for ssmi is
      real mvi_ssmi
      parameter (mvi_ssmi = -999)
c
c     Assumed error statistics are that in terms of wind speeds,
c     the rmse for velm and losm are constants (m/s), given by:
      real eps_velm, eps_losm
      parameter (eps_velm = 1.5, eps_losm = 5)
      real wgt_velm
      parameter (wgt_velm = 1/eps_velm**2)
      real wgt_losm
      parameter (wgt_losm = 1/eps_losm**2)

      common /ssmidta/ nssmi, vssmi

c Some of the other constants which follow might eventually be added to
c this common block, as quantities which can be adjusted via namelist.

c     Naming conventions
c
c     Prefix        Usage
c     ------        -----------
c       i           Index
c       m           Maximum
c       n           Number
c       v           Variable
