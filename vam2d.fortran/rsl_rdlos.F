c!# CSU IDENTIFICATION : rsl_rdlos.F
c!#     $Id: rsl_rdlos.F,v 1.3 1998/03/25 16:58:45 leidner Exp $

c!## PURPOSE : Cover routine for readssmi.F.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      function rsl_rdlos                                                !#
     I    (idate, itime)                                                !#

c!# Input:
c!# idate     desired date in yymmdd format
c!# itime     desired time in hhmmss format
      integer idate, itime                                              !#

c!# Output: Normally 0, otherwise see ERROR HANDLING
      integer rsl_rdlos                                                 !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: rsl_rdlos.F,v $
c!#	Revision 1.3  1998/03/25 16:58:45  leidner
c!#	added option to store ssmi wind speed as sass wind speed
c!#	controlled by namelist parameter store_sass
c!#	
c!#	Revision 1.2  1997/08/14 14:17:29  leidner
c!#	added namelist flag to control use of los directional info
c!#
c!#	Revision 1.1  1997/08/11  17:41:40  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "ssmidata.h"
#include "namelist_rsl_rdlos.h"

c!## LOCAL DATA ELEMENTS :

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   1.  Initialize / retrieve namelist parameters.
c
      rsl_rdlos = 0
      nfiles = 0
      iu = 10
      timwin = 3.0
      datawin_prints = .false.
      ssmiqual_prints = .false.
      use_losm = .true.
      store_sass = .false.
      iprint = 1000
      
      read(IUNML,input,end=901,err=901)
      write(*,input)
c
c     ------------------------------------------------------------------
c
c!#   2.  Call read routines for SSM/I-4 geophysical data.
c
      do n=1,nfiles
         call readssmi
     c       ( idate, itime, timwin,
     c       datawin_prints, ssmiqual_prints, use_losm, store_sass,
     i       n, filenames(n), iu )
      enddo
c
c     ------------------------------------------------------------------
c
c!#   3.  Print out a sample from ssmi data buffer. (see "ssmidata.h")
c
      write(*,100)
      do i=1,nssmi,iprint
         write(*,200) i,(vssmi(i,j),j=1,mssmivar)
      enddo
      write(*, 300) nssmi
c
  100 format( //'ssmi line of sight data inventory',//
     &    137('-'),/
     &    '      n ','   ixg  ','   iyg  ','  ivelm ','  ilosm ',
     &    '   ipx  ','   ipy  ','     row  ','   col  ','     rev  ',
     &    '   sst  ','  tice  ','   iwv  ','   icw  ','  rrate ',
     &    ' ivelm5 ',' ilosm5 ',/
     &    137('-')/)
  200 format(i8,6f8.3,f10.3,f8.3,f10.3,7f8.3)
  300 format(/'Number of stored ssmi:',24x,i6/)
      goto 999
c
c     ------------------------------------------------------------------
c
c!#   4.  End of routine / error control.
c
c!## ERROR HANDLING :
c
c!# rsl_rdlos  =  0 (normal)
c!#              -1 (error reading namelist)

 901  continue
      rsl_rdlos = -1
      print *, 'vam(rsl_rdlos): error reading namelist.'
      go to 999

 999  return
      end
