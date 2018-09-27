c!#   $Id: init_reformat.F,v 1.1 1999/07/21 19:23:54 trn Exp $
c!#   $Log: init_reformat.F,v $
c!#   Revision 1.1  1999/07/21 19:23:54  trn
c!#   Initial revision
c!#
c!#
c!#   initvam.F,v:
c!#   Revision 1.4  1997/04/17 18:57:42  rnh
c!#   Added ssmi los wind cost function
c!#
c!#	Revision 1.3  1997/02/21  21:37:23  leidner
c!#	added ns_bscat.h include
c!#	added initialization of ns_nWVC and ns_nobs
c!#
c!#	Revision 1.2  1997/02/12  19:19:28  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine init_reformat
c**********************************************************************
c English Name: Initialize VAM reformatting program
c -------------
c
c Purpose: Resets all VAM common block parameters to starting values.
c -------- Fills grid parameters for a global lat-lon grid
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 9, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
#include "ptdta.h"
#include "sldta.h"
#include "er_bscat.h"
#include "ns_bscat.h"
#include "weight.h"
c
c     Set defaults for weight parameters
c     ----------------------------------
c
      do i=1,nsos
         lamda(I)  = 0.0
         lscale(I) = 1.0
         scale(I)  = 0.0
      end do

      lscale(1) = (10.)**(-10)
      lscale(5) = (10.)**(+10)
      lscale(8) = (10.)**(+8)

      iteration = 0
c
c     Set defaults for grid parameters: 1x1 deg global grid, origin at (0,-90)
c     --------------------------------
c
      xs   = 0
      delx = 1
      xf   = 359
      iper = 360
      ys   = -90
      dely = 1
      yf   = 90
      xmin = 1
      xmax = 361
      xmin = 1
      xmax = 181

      call grparm

      ipass = 0
c
c     Reset counters.
c     ---------------
c
      er_nobs = 0
      ns_nWVC = 0
      ns_nobs = 0
      npts    = 0
      nptsa   = 0
      nptc    = 0

      return
      end
