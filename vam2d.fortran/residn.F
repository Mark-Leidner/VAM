c!# CSU IDENTIFICATION : residn.F
c!#     $Id: residn.F,v 1.6 1998/04/06 17:52:44 stanr Exp $

c!## PURPOSE : Performs quality control on NSCAT backscatter data.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      subroutine residn                                                 !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: residn.F,v $
c!## CHANGE LOG : Revision 1.6  1998/04/06 17:52:44  stanr
c!## CHANGE LOG : change type of Jdepart and Jpsd to real*8, matching sscgr
c!## CHANGE LOG :
c!## CHANGE LOG : Revision 1.5  1998/03/18 16:19:02  leidner
c!## CHANGE LOG : now using KpC as quality control flag
c!## CHANGE LOG :
c!## CHANGE LOG : Revision 1.4  1997/08/05 19:22:42  leidner
c!## CHANGE LOG : changed QC methodology to a simple dB space check
c!## CHANGE LOG : updated documentation
c!## CHANGE LOG :
c!#	Revision 1.3  1997/07/25  17:02:42  leidner
c!#	Added penalty and departure terms to ss calc for scatterometer backscatter
c!#
c!#	Revision 1.2  1997/06/23  17:48:57  leidner
c!#	changed ns_kp to 1st JPL kp parameter (kpa) for flagging bad data
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "grdta.h"
#include "ns_bscat.h"
#include "namelist_residn.h"

c!## LOCAL DATA ELEMENTS :

c!# ANALYSIS    Parameter used to set verification source
c!# BACKGROUND  Parameter used to set verification source
c!# ss          Total cost function
c!# Jdepart     Cost function due to departures
c!# Jpsd        Cost function due to penalty term
c!# ntag        Tag marking this data type
c!# j1          Departure for one sigma0
      integer ANALYSIS, BACKGROUND, ntag                                !#
      real*8 Jdepart, Jpsd                                              !#
      real j1                                                           !#
      real*8 ss                                                         !#
      parameter (ANALYSIS = 1,BACKGROUND = 0)                           !#

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   0.  Initialize namelist parameters
c
      iprint = 0
      iver = ANALYSIS
      gamma = 9
c
c     ------------------------------------------------------------------
c
c!#   1.  Read namelist
c
      call isetcon(MAXTAG,itag,ALLTYPE)
      read(IUNML,input,end=900,err=900)
      write(6,input)

      if (iver .eq. BACKGROUND) then 
         write(*,80)
      else
         write(*,90)
      endif
   80 format(/'RESIDUALS FOR NSCAT DATA: USING BACKGROUND'/)
   90 format(/'RESIDUALS FOR NSCAT DATA: USING CURRENT ANALYSIS'/)
c
c     ------------------------------------------------------------------
c
c!#   2.  Calculate trajectory values for all observations.
c
      call ssnscat (ss,xnorm,delssu,delssv, u, v,idim, Jdepart, Jpsd)
c
c     ------------------------------------------------------------------
c
c!#   3.  Begin Quality Control
c
      itoss = 0
      ntag = isrch(MAXTAG,itag,ALLTYPE) - 1
      if (ntag .lt. 0) ntag = MAXTAG

      do n = 1,ns_nobs

        if (ntag.eq.0 .or. isrch(ntag,itag,ns_tag(n)).ne.0) then 
c
c!#   3.1.  Measure the departure, j1, from the verification source
c!#         (dB space)
c
          if (ns_kpc(n) .gt. 0.0) then 
            if (iver .eq. BACKGROUND) then 
              s0calc = 10 * log10( ns_s0bg(n) )
            else
              s0calc = 10 * log10( ns_s05(n) )
            endif
            s0obs = 10 * log10( ns_s0obs(n) )
            j1 = s0obs - s0calc
c
c!#   3.2.  Determine if report is acceptable :
c!#         if (-gamma > displacement > gamma) report fails QC
c
            if ( (j1 .lt. -gamma) .or. (j1 .gt. gamma) ) then
              itoss = itoss + 1
              ns_qc(n) = ior(ns_qc(n),2**ipass)
              call showqcn(itoss,iprint,n,j1)
              ns_kpc(n) = -abs(ns_kpc(n))
            endif
          endif
        endif
      end do
      go to 999
c
c     ------------------------------------------------------------------
c
c!#   4.  End of Routine / Error Control
c
 
c!## ERROR HANDLING :

 900  continue
      call errsea(520,'RESIDN')
      go to 999

 999  continue

      per = 100 * ( float(itoss) / float(ns_nobs) )
      write(*,100) itoss,ns_nobs,per
 100  format(1x,i8,' REPORTS OUT OF ',i8,' FAILED QUALITY CONTROL (',
     &    f5.0,'%)')

      return
      end
