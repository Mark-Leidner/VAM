c!#   $Id: showqcn.F,v 1.4 1998/05/18 18:17:41 stanr Exp $
c!#   $Log: showqcn.F,v $
c!#   Revision 1.4  1998/05/18 18:17:41  stanr
c!#   remove extra comma from format 100
c!#
c!#   Revision 1.3  1997/08/05 19:34:49  leidner
c!#   s0 qc info now printed in dB's
c!#
c!#	Revision 1.2  1997/06/23  17:52:39  leidner
c!#	changed ns_kp to 1st JPL kp parameter (kpa) for flagging bad data
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#
c**********************************************************************
      subroutine showqcn(ncnt,iprint,ptr,j1)
c**********************************************************************
c English Name: Show Quality Control
c -------------
c
c Purpose: Prints out information concerning the quality of an NSCAT
c -------- report as determined by "RESIDS" routine.
c
c Notes: 1) This routine is called from "RESIDN" routine and should not 
c ------    be called directly by the user.
c
c Variable Description:
c ---------------------
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 16, 1993 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer ncnt,iprint,ptr
      real j1
c
c     Common Block - "ns_bscat.h" (NSCAT Backscatter)
c     -----------------------------
c
#include "ns_bscat.h"
c
c     Initialize
c     ==========
c
      if (iprint .le. 0) go to 999
      if (mod(ncnt,iprint) .ne. 0) go to 999
      if (ncnt .eq. iprint) write(*,100)
c
c     Print QC info
c     =============
c
         if (ns_kpa(ptr) .gt. 0.0)
     &       write(*,110) ns_qc(ptr),
     &           ns_lat(ptr),ns_lon(ptr),ns_ic(ptr),
     &           ns_jc(ptr),ns_xc(ptr),ns_yc(ptr),
     &           ns_ant(ptr),10*log10(ns_s0obs(ptr)),
     &           10*log10(ns_s0bg(ptr)),10*log10(ns_s05(ptr)),j1

 100  format(1x,102('-'),/,1x,'  Q',
     &      '   LAT  ','   LON  ','   I','  J',
     &      '   X  ','   Y  ','ANT','     S0OBS (dB)',
     &      '      S0BG (dB)','      S05G (dB)','        J1 (dB)',
     &      /,1x,102('-'))
 110  format(/1x,i3,2f8.3,i4,i3,2f6.3,i3,4f15.2)

 999  return
      end
