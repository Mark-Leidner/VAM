c!#   $Id: copynscat.F,v 1.1 1997/02/21 23:45:10 leidner Exp $
c!#   $Log: copynscat.F,v $
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#
c**********************************************************************
      subroutine copynscat
c**********************************************************************
c English Name: Copy NSCAT
c -------------
c
c Purpose: Copies NSCAT trajectory values calculated by "jscat2d" routine
c -------- into the background data vectors of the NSCAT backscatter
c          data structure ("ns_bscat.h"). See also, "RESIDN.f"
c          routine.
c
c Notes:
c ------
c
c Variable Description:
c ---------------------
c
c Programmer: S. Mark Leidner
c ----------- (Numerical Weather Prediction Group)
c             (Atmospheric and Environmental Research)
c
c Modified: November 6, 1996 - created (after Joe Ardizzone's copyers1).
c ---------
c**********************************************************************
#include "vam.h"
c
c     Common Block - "ns_bscat.h" (NSCAT Backscatter)
c     -------------------------
c
#include "ns_bscat.h"
c
c     Copy trajectory data vectors into background storage
c     ====================================================
c
      do i = 1,ns_nobs
         ns_s0bg(i)  = ns_s05(i)
         ns_velbg(i) = ns_vel5(i)
         ns_dirbg(i) = ns_dir5(i)
      end do

      write(*,100) ns_nobs
 100  format(1x,/,1x,'COPYNSCAT: ',i8,' REPORTS COPIED')

      return
      end
