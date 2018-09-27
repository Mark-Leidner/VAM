c!# CSU IDENTIFICATION : rgv_rdgrid.F
c!#     $Id: rgv_rdgrid.F,v 1.4 1997/07/08 20:22:09 leidner Exp $

c!## PURPOSE : Reads U and V gridded wind fields from input "V8interp"
c!# data set into VAM data buffers (see "grdta.h") for the 
c!# specified date/time.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      function RGV_rdgrid                                               !#
     I    (idate,itime)                                                 !#

c!# Input:
c!# idate     desired date in yymmdd format
c!# itime     desired time in hhmmss format
      integer idate, itime                                              !#

c!# Output: Normally 0, otherwise see ERROR HANDLING

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!# This routine invokes the subroutine, "grparm" which will
c!# read important namelist parameters needed by this routine.
c!# The namelist parameters are stored in "gparm.h".

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: rgv_rdgrid.F,v $
c!## CHANGE LOG : Revision 1.4  1997/07/08 20:22:09  leidner
c!## CHANGE LOG : updated documentation
c!## CHANGE LOG :
c!#	Revision 1.3  1997/07/08  17:34:25  leidner
c!#	Added calculation of background vorticity tendency
c!#
c!#	Revision 1.2  1997/02/12  19:40:06  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "grname.h"
#include "grdta.h"

c!## LOCAL DATA ELEMENTS :

c!# fill      fill value for missing grid points
c!# iostack   holds status of data buffer (EMPTY,FULL)
c!# nymd      date (year,month,day) of the requested analysis
c!# nhms      time (hours,min,sec) of the requested analysis
c!# nbefaf    analysis flag indicating the desired analysis
c!# nqnt      number of quantities (records) for current date/time.
c!# im        longitude dimension of wind field.
c!# jnp       latitude dimension of wind field.
c!# ss        sum of squared error from ssdtz
c!# swgt      sum of weights from ssdtz
      real fill                                                         !#
      integer i,iostack(MAXUNIT),nymd(MAXUNIT),nhms(MAXUNIT)            !#
      integer nbefaf(MAXUNIT),nqnt(MAXUNIT),im(MAXUNIT),jnp(MAXUNIT)    !#
      real*8 ss, swgt                                                   !#

c!## LOCAL DATA STRUCTURES :

c!# ihedr     buffer for header record
c!# ibuf      input buffer
c!# rbuf      input buffer (must be atleast im*jnp 4byte words).
c!# usurf,vsurf,idum  work arrays for surface wind analyses
      integer*4 ihedr(6),ibuf(MAXREC-6)                                 !#
      real*4 rbuf(MAXREC)                                               !#
      common /iobuf/ rbuf,ihedr,ibuf                                    !#
      common /work/ usurf(IBF,JBF),vsurf(IBF,JBF),idum(WBF-IBF*JBF*2)   !#

      data fill   /-777D20/
      data nymd   /MAXUNIT * 0/
      data nhms   /MAXUNIT * 0/
      data nbefaf /MAXUNIT * -999/
      data nqnt   /MAXUNIT * 0/
      data im     /MAXUNIT * 0/
      data jnp    /MAXUNIT * 0/
      data iostack/MAXUNIT * EMPTY/

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   1.  Retrieve namelist parameters / initialize
c
      RGV_rdgrid = 0
      call grparm
c
c     ------------------------------------------------------------------
c
c!#   2.  Establish I/O link
c
 5    continue
      ierr = ioopen(iu)
      if (ierr .eq. 2) go to 902
      if (ierr .ne. 0) go to 901
c
c     ------------------------------------------------------------------
c
c!#   3.  Position data set to desired records.
c
      call setclock(idate,itime,ibefaf)
      call settime(nymd(iu),nhms(iu),nbefaf(iu))

      do while (checktime() .lt. 0)
c
c     ------------------------------------------------------------------
c
c!#   4.  Read V8interp Header.
c
         if (iostack(iu) .eq. EMPTY) then
            read(iu,end=900,err=900) ihedr
            nymd(iu)    = ihedr(1)
            nhms(iu)    = ihedr(2)
            nbefaf(iu)  = ihedr(3)
            nqnt(iu)    = ihedr(4)
            im(iu)      = ihedr(5)
            jnp(iu)     = ihedr(6)
            iostack(iu) = FULL
            call settime(nymd(iu),nhms(iu),nbefaf(iu))
         endif

         if (im(iu) .gt. IBF .or. jnp(iu) .gt. JBF) go to 904
c
c     ------------------------------------------------------------------
c
c!#   5.  Check Time.
c
         if (checktime() .lt. 0) then
            if (GEN_rdskip(iu,nqnt(iu)) .ne. 0) go to 901
            iostack(iu) = EMPTY
         endif

      end do

      if (checktime() .ne. 0) go to 903
c
c     ------------------------------------------------------------------
c
c!#   6.  Read in Data
c     
      if (RGV_rddata(iu,nqnt(iu),qlev,qu,qv,im(iu),jnp(iu),u,v,rbuf) 
     &                                               .ne. 0) go to 901
 
      nx0   = im(iu)
      ny0   = jnp(iu)
      jper0 = 0
      xs0   = 0.0
      ys0   = -90.0
      iper0 = im(iu)
      delx0 = 360.0 / float(im(iu))
      dely0 = 180.0 / float(jnp(iu)-1)
c
c     ------------------------------------------------------------------
c
c!#   7.  Rotate grid to VAM orientation.
c
      call rotgrd(im(iu),jnp(iu),-180.0,0.0,u,usurf)
      call rotgrd(im(iu),jnp(iu),-180.0,0.0,v,vsurf)
c
c     ------------------------------------------------------------------
c
c!#   8.  Interpolate to desired background resolution.
c
      call ginter (im(iu),usurf,xs0,delx0,nx0,iper0,ys0,dely0,
     &   ny0,0,IBF,u0,xs,delx,nx,ys,dely,ny,fill)
      call ginter (im(iu),vsurf,xs0,delx0,nx0,iper0,ys0,dely0,
     &   ny0,0,IBF,v0,xs,delx,nx,ys,dely,ny,fill)
c
c     ------------------------------------------------------------------
c
c!#   9.  Initialize analysis grid to background grid.
c
      do j = 1,ny
         do i = 1,nx
            u(i,j) = u0(i,j)
            v(i,j) = v0(i,j)
         end do
      end do
c
c     ------------------------------------------------------------------
c
c!#   10.  Initialize background vorticity tendency arrray.
c
      call ssdtz ( ss, swgt, delssu, delssv, u0, v0,
     &          cd, vel, dtz0, idim, init_dtz0 )
      write(*,*) 'init_dtz0, ss: ', init_dtz0, ss
c
      iostack(iu) = EMPTY
      write (*,100) xs,delx,nx,iper,ys,dely,ny,jper,u(1,1),v(1,1)
      go to 999
c
c     ------------------------------------------------------------------
c
c!#   11.  End of routine / error control
c
c!## ERROR HANDLING :
c
c!# RGV_rdgrid =  0 (normal)
c!#               2 (end of data; requested date/time not found)
c!#               3 (requested date/time not found)
c!#              -1 (error)
c!#              -2 (max dimensions exceeded)

 900  continue
      call ioclose(iu)
      go to 5

 901  continue
      RGV_rdgrid = -1
      go to 999

 902  continue
      RGV_rdgrid = 2
      print *, 'vam(RGV_rdgrid): End of Data'
      print *, 'vam(RGV_rdgrid): date/time not found: ',idate,itime
      go to 999

 903  continue
      RGV_rdgrid = 3
      print *, 'vam(RGV_rdgrid): date/time not found: ',idate,itime
      print *, 'vam(RGV_rdgrid): unit = ',iu
      go to 999

 904  continue
      RGV_rdgrid = -2
      print *, 'vam(RGV_rdgrid): max dimensions exceeded.'
      go to 999

 100  format(/,1x,'Grid Parameters: ',2e18.7,2i6,
     &       /,16x,2e18.7,2i6,
     &       //,1x,'Sample Values: ',2e18.7)

 999  return
      end
