c!#   $Id: pt_dealias.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: pt_dealias.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function PT_dealias(ipt,alias,assign_spd,assign_dir,rlat,rlon,
     &                                                  vsol,dsol,fill)
c**********************************************************************
c English Name: Dealias (PT - Point or SASS data)
c -------------
c
c Purpose: Returns desired aliases for a single observation from SASS 
c -------  data buffer (see "ptdta.h") along with index of the alias
c          closest in direction to the analyzed direction. Analyzed 
c          speed and direction may also be assigned to each returned 
c          alias according to the specified flags.
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer PT_dealias,ipt
c     real rlat,rlon,vsol(*),dsol(*),fill
c     logical alias(*),assign_spd,assign_dir
c
c     PT_dealias (O) - function return value:
c
c             PT_dealias > 0 (alias closest to analyzed direction)
c                        = 0 (no aliases found)
c
c     ipt (I) - index pointer into SASS data buffer ("ptdta.h")
c               indicating desired observation.
c
c     alias (I) - flags indicating which aliases are to be considered:
c
c             alias = .false. (ignore alias)
c                      .true. (accept alias)
c
c     assign_spd (I) - flag indicating that the analyzed speed is to be
c                      assigned to each returned alias:
c
c             assign_spd = .true. (assign analyzed speed)
c                         .false. (do not assign speed)
c
c     assign_dir (I) - flag indicating that the analyzed direction is
c                      to be assigned to each returned alias:
c
c             assign_dir = .true. (assign analyzed direction)
c                         .false. (do not assign direction)
c
c     rlat (O) - returned latitude of observation (degrees)
c
c     rlon (O) - returned longitude of observation (degrees)
c
c     vsol (O) - returned speeds for each desired alias (m/s). Missing
c                or undesired aliases are set to "fill".
c
c     dsol (O) - returned direction for each desired alias (degrees).
c                Missing or undesired aliases are set to "fill".
c
c     fill (I) - fill value to be assigned to missing or undesired
c                aliases.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 12, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer ipt
      real rlat,rlon,vsol(*),dsol(*),fill
      logical alias(*),assign_spd,assign_dir
c
c     Local Variables
c     ---------------
c
      integer i,j,ia
      real u0i,v0i,aspeed,adirec,d,dmin
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
#include "ptdta.h"
c
c     Initialize
c     ==========
c
      PT_dealias = 0

      do i = 1,4
         vsol(i) = fill
         dsol(i) = fill
      end do
      if (ipt .le. 0 .or. ipt .gt. npts) return
c
c     Save observation location.
c     ==========================
c
      i    = ill(ipt)
      j    = jll(ipt)
      rlat = ys + (j - 1 + ycoord(ipt)) * dely
      rlon = xs + (i - 1 + xcoord(ipt)) * delx
c
c     Interpolate analysis grid to observation.
c     =========================================
c
      u0i = bilin (xcoord(ipt),ycoord(ipt),u(i,j),idim)
      v0i = bilin (xcoord(ipt),ycoord(ipt),v(i,j),idim)
      call uv2md(u0i,v0i,aspeed,adirec)
c
c     Save desired aliases / determine alias closest to analysis.
c     ===========================================================
c
      dmin = 360.0
      do ia = 1,max(nwinds(ipt),1)

         if (alias(ia)) then 

            call uv2md(uobs(ia,ipt),vobs(ia,ipt),vsol(ia),dsol(ia))
            d = abs(dirdif(adirec,dsol(ia)))

            if (d .lt. dmin) then
               dmin       = d
               PT_dealias = ia
            endif

            if (assign_spd) vsol(ia) = aspeed
            if (assign_dir) dsol(ia) = adirec

         endif

      end do

      return
      end
