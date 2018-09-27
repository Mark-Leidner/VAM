c!# CSU IDENTIFICATION : wsl_wrlos.F
c!#     $Id: wsl_wrlos.F,v 1.2 1997/10/23 20:40:21 leidner Exp $

c!## PURPOSE : writes out SSMI los winds to an ascii file

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      function wsl_wrlos                                                !#
     I      ( idate, itime )                                            !#

c!# Input:
c!# idate   date in yymmdd format
c!# itime   time in hhmmss format
      integer wsl_wrlos, idate, itime                                   !#

c!# Output: Normally 0, otherwise see ERROR HANDLING

c!## DATA CONVERSION :

c!# Change longitude from VAM coords (0,360) to Splus map plotting coords
c!# (-180,180)

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: wsl_wrlos.F,v $
c!#	Revision 1.2  1997/10/23 20:40:21  leidner
c!#	added data to ssmidata buffer
c!#	
c!#	Revision 1.1  1997/08/11  17:45:05  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "grdta.h"
#include "ssmidata.h"
#include "namelist_wsl_wrlos.h"

c!## LOCAL DATA ELEMENTS :

      real xlat(mssmi), xlon(mssmi)

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   0.  Initialize function and namelist parameters
c
      wsl_wrlos = 0
      data_fmt = '(10f13.6)'
      header_fmt = '(i10)'
      header_fmt2 = '(f10.2)'
c
c     ------------------------------------------------------------------
c
c!#   1.  Retrieve namelist parameters
c
      read(IUNML,input,end=902,err=902)
      write(*,input)
c
c     ------------------------------------------------------------------
c
c!#   2.  Establish I/O link
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     ------------------------------------------------------------------
c
c!#   3.  Write header from gparm and ssmidata.h
c
      write(iu,header_fmt) idate
      write(iu,header_fmt) itime
      write(iu,header_fmt2) lognum
      write(iu,header_fmt) nssmi
c
c     ------------------------------------------------------------------
c
c!#   4.  Convert grid coords to lats/lons
c
      do i=1,nssmi
        xlon(i) = xs + delx*vssmi(i,ixg)
        xlat(i) = ys + dely*vssmi(i,iyg)
c
c!#   4.1.  Convert longitudes to (-180,180)lon coord
c
        if ( xlon(i) .gt. 180 ) xlon(i) = xlon(i) - 360
      enddo
c
c     ------------------------------------------------------------------
c
c!#   5.  Write SSMI LOS data to ascii file
c
      write(iu,data_fmt) ( xlat(i),i=1,nssmi )
      write(iu,data_fmt) ( xlon(i),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,ivelm),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,ilosm),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,ipx),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,ipy),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,isrow),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,iscol),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,isrev),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,isst),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,itice),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,iwv),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,icw),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,irain),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,ivelm5),i=1,nssmi )
      write(iu,data_fmt) ( vssmi(i,ilosm5),i=1,nssmi )
c
      go to 999
c
c     ------------------------------------------------------------------
c
c!## ERROR HANDLING : return nonzero error codes
c!# wsl_wrlos  =  0 (normal)
c!#              -1 (error returned opening output file)
c!#              -2 (error reading namelist)
c
 901  continue
      wsl_wrlos = -1
      go to 999
c
 902  continue
      wsl_wrlos = -2
      print *, 'vam(wsl_wrlos): error reading namelist'
      go to 999
c
 999  return
      end
