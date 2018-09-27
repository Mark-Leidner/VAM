c!# CSU IDENTIFICATION : flag_rain
c!#     $Id: flag_rain.F,v 1.1 1998/05/05 15:01:44 leidner Exp $

c!## PURPOSE : Eliminate data which may be contaminated by rain.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      subroutine flag_rain(tb_min, al_max)

c!# Input:
c!#~ tb_min     
c!#~ al_max
      real tb_min, al_max

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG :
c!#	$Log: flag_rain.F,v $
c!#	Revision 1.1  1998/05/05 15:01:44  leidner
c!#	Initial revision
c!#	

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "ptdta.h"
#include "ns_winds.h"
#include "ns_bscat.h"

c!## LOCAL DATA ELEMENTS :

c!#~ flag       logical; should this wvc be flagged
      logical flag
c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c!#   1. Loop over all nscat wvc's

      if (nsw_nwvc.gt.0) then

         nwvc_flagged = 0
         do 100 n=1,nsw_nwvc

c!#   2. If GOES data present,

            if (nsw_tb(n).gt.0 .and. nsw_al(n).gt.0) then
c       print *, 'flag_rain: n nsw_tb nsw_al =',nsw_tb(n),nsw_al(n)
               flag = nsw_tb(n).lt.tb_min .and. nsw_al(n).gt.al_max
               if (.not.flag) goto 100

c!#   2.1 flag nscat winds data
 
               if (flag .and. nsw_nambigs(n).gt.0) then
                  WGT(N)=-WGT(N)
                  nwvc_flagged = nwvc_flagged + 1
                  write(*,1000) n,nsw_tb(n),nsw_al(n),nsw_row(n),
     &                 nsw_col(n)
               endif
            endif
 100     enddo

         per = 100.*(float(nwvc_flagged)/float(nsw_nwvc))
         write (*,1100) nwvc_flagged,per

      endif
 1000 format('Rain flagging NSCAT ambiguous winds data: ',
     &     'n Tb albedo row col =',i6,2e16.5,2i6)
 1100 format('After rain flagging NSCAT winds, nwvc_flagged =',i5,
     &     ' ( ',f6.2,' % )')

c!#   3. Loop over all nscat sigma0 measurements

      entry flag_rain_sig(tb_min, al_max)

      if (ns_nobs.gt.0) then

         if (ns_nWVC.ne.nsw_nwvc) stop 'ns_nWVC must equal nsw_nwvc'

         nwvc_flagged = 0
         nsig_flagged = 0

         do 200 n=1,ns_nWVC

c!#   4. If GOES data present,

            if (nsw_tb(n).gt.0 .and. nsw_al(n).gt.0) then
               flag = nsw_tb(n).lt.tb_min .and. nsw_al(n).gt.al_max
               if (.not.flag) goto 200

c!#   4.1 flag nscat sigma0 data

               if (flag .and. ns_nsig0(n).gt.0) then
                  nwvc_flagged = nwvc_flagged + 1

c!#   4.1.1 Determine indices of sigma0 data to flag

                  jstart = 1
                  do i=1,n-1
                     jstart = jstart + ns_nsig0(i)
                  enddo
                  jend = jstart + ns_nsig0(n) - 1

c!#   4.1.2 Set sigma0 kpc to negative number to exclude from analysis

                  write(*,2000) n,nsw_tb(n),nsw_al(n),ns_nsig0(n),
     &                 jstart,jend
                  do j=jstart,jend
                     ns_kpc(j) = -abs(ns_kpc(j))
c       print *, '                           n j row col =',
c     &                    n,j,ns_row(j),ns_col(j)
                     nsig_flagged = nsig_flagged + 1
                  enddo
               endif
            endif
 200     enddo

         per1 = 100.*(float(nwvc_flagged)/float(ns_nWVC))
         per2 = 100.*(float(nsig_flagged)/float(ns_nobs))
         write (*,2100) nwvc_flagged,per1,nsig_flagged,per2

      endif
 2000 format('Rain flagging NSCAT sigma0 data: ',
     &     'n Tb albedo nsig0 jstart jend =',i6,2e16.5,3i6)
 2100 format('After rain flagging NSCAT sigma0, nwvc_flagged =',i5,
     &     ' ( ',f6.2,' % )',
     &      /'                                  nsig_flagged =',i5,
     &     ' ( ',f6.2,' % )')

c!## ERROR HANDLING :
      return
      end



