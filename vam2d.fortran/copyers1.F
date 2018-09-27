c!#   $Id: copyers1.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: copyers1.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine copyers1
c**********************************************************************
c English Name: Copy ERS1
c -------------
c
c Purpose: Copies ERS1 trajectory values calculated by "jscat2d" routine
c -------- into the background data vectors of the ERS1 backscatter
c          data structure ("er_bscat.h"). See also, "RESIDS.f"
c          routine.
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
c Modified: April 13, 1993 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Common Block - "er_bscat" (ERS1 Backscatter)
c     -------------------------
c
#include "er_bscat.h"
c
c     Copy trajectory data vectors into background storage
c     ====================================================
c
      do j = 1,ER_MBEAM
         do i = 1,er_nobs
            er_s0bg(i,j) = er_s05(i,j)
         end do
      end do

      do i = 1,er_nobs
         er_velbg(i) = er_vel5(i)
         er_dirbg(i) = er_dir5(i)
      end do

      write(*,100) er_nobs
 100  format(1x,/,1x,'COPYERS1: ',i8,' REPORTS COPIED')

      return
      end
