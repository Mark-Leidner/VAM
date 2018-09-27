c!# CSU IDENTIFICATION : ns_print.F
c!#     $Id: ns_print.F,v 1.5 1997/06/16 17:40:22 leidner Exp $

c!## PURPOSE : Print a formatted sample of NSCAT backscatter data from database.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      subroutine NS_print                                               !#
     I      ( istart, istop, iprint )                                   !#

c!# Input:
c!# istart   index pointer into NSCAT data buffer signifying where to 
c!#          begin sampling
c!# istop    index pointer into NSCAT data buffer signifying where to 
c!#          stop sampling
c!# iprint   sampling frequency (every "iprint" reports are printed):
c!#          > 0 (print)
c!#          <= 0 (no printing)
c!# Output: None
      integer istart, istop, iprint                                     !#

c!## DATA CONVERSION :

c!## ALGORITHMS : 

c!# Samples data according to specified frequency by printing
c!# buffer contents. This routine operates on the VAM NSCAT
c!# backscatter data buffer for the index range specified
c!# (see "ns_bscat.h").

c!## REFERENCES :

c!# Created December 23, 1996 after Joe Ardizzone's er_print.F

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: ns_print.F,v $
c!## CHANGE LOG : Revision 1.5  1997/06/16 17:40:22  leidner
c!## CHANGE LOG : removed printing of Kp and SD and updated documentation
c!## CHANGE LOG :
c!#	Revision 1.4  1997/04/14  16:06:08  leidner
c!#	added stride for WVC print
c!#
c!#	Revision 1.3  1997/04/11  17:20:22  leidner
c!#	added ns_nsig0 to printout
c!#
c!#	Revision 1.2  1997/04/08  19:51:53  leidner
c!#	changed format to print more complete sig0 information
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "ns_bscat.h"

c!## LOCAL DATA ELEMENTS :

c!# i        counter
c!# stride   print frequency for NSCAT Wind Vector Cell data
      integer i, stride                                                 !#

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   0.  Return if printing not requested
c
      if (iprint .le. 0) return
c
c     ------------------------------------------------------------------
c
c!#   1.  Print backscatter sample from NSCAT database
c
      write(*,100)
      do i = istart,istop,iprint
            write(*,110) ns_lat  (i), ns_lon  (i),
     &                   ns_ic   (i), ns_jc   (i),
     &                   ns_xc   (i), ns_yc   (i), ns_ant(i),
     &                   ns_theta(i), ns_azim (i),
     &                   ns_kpa  (i), ns_kpb  (i), 
     &                   ns_kpc  (i), ns_s0obs(i)
   
      end do
      write(*,130) istop-istart+1
c
c     ------------------------------------------------------------------
c
c!#   2.  Print Wind Vector Cell sample from NSCAT database
c
      write(*,200)
c
c!#   2.1.  Set printing stride for WVC's 
c
      if (iprint .gt. 10) then
        stride = iprint/10
      else
        stride = iprint
      endif
      do i = 1,ns_nWVC,stride

            write(*,210) WVC_Lat  (i), WVC_Lon  (i),
     &                   WVC_ic   (i), WVC_jc   (i),
     &                   WVC_xc   (i), WVC_yc   (i),
     &                   ns_nsig0(i)
   
      end do
      write(*,230) ns_nWVC
c
c     ------------------------------------------------------------------
c
c!#   3.  Format statements
c
c Lat    Lon    i  j   x     y  Ant   Theta      Azm       Kpa       Kpb       Kpc      Sig0
cff.ffxfff.ffxiiixiixf.fffxf.fffxiixe.eeeeE+exe.eeeeE+exe.eeeeE+exe.eeeeE+exe.eeeeE+exe.eeeeE+e
 100  format(/,1x,'NSCAT Backscatter Data Inventory',
     &      /,1x,32('-'),
     &      //,1x,120('-'),
     &      /, '   Lat    Lon    i  j   x     y  Ant   Theta  ',
     &         '    Azm       Kpa       Kpb       Kpc      Sig0   ',
     &      /,1x,120('-'))

 110  format(2(1x,f6.2),i4,i3,2f6.3,i3,6e10.4e1)
 130  format(/,1x,'Total Number of Reports: ',i6)
 200  format(//,1x,'NSCAT Wind Vector Cell Inventory',
     &      /,1x,32('-'),
     &      //,1x,120('-'),
     &      /, '   Lat    Lon    i  j   x     y   nsigma0',
     &      /,1x,120('-'))
 210  format(2(1x,f6.2),i4,i3,2f6.3,i5)
 230  format(/,1x,'Total Number of Wind Vector Cells: ',i6)
c
c     ------------------------------------------------------------------
c

c!## ERROR HANDLING :

      return
      end
