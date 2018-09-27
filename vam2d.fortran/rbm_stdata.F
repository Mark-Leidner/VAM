
c!#   CSU IDENTIFICATION : rbm_stdata
c!#   $Id: rbm_stdata.F,v 1.4 1998/09/22 19:24:24 leidner Exp $

c!##  PURPOSE : Store NSCAT winds and backscatter from 25km merged files

c!#   NSCAT backscatter data and ephemiris are read directly
c!#   from Merged Geophysical Data Record (MGDR) files.  Data
c!#   important for the vam are stored in the NSCAT data
c!#   structures (see "ns_bscat.h", "ns_winds.h" and "ptdta.h")

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      subroutine rbm_stdata !#
     I     ( nfile, filename, s0min, s0max, s0min_edit, itag, !#
     I     idate, itime, timwin, datawin_prints, sig0qual_prints, !#
     I     alias, rank_by_JPL, nthin)  !#

c!#   Input:
c!#~   nfile     Count of which file is being read in
c!#~   mgdr_file Current filename
c!#~   s0min     Current date in YYMMDD format
c!#~   s0max     Current date in YYMMDD format
c!#~   s0min_edit     Current date in YYMMDD format
c!#~   itag      Current date in YYMMDD format
c!#~   idate     Current date in YYMMDD format
c!#~   itime     Current time in HHMMSS format
c!#~   timwin    1/2 time window in minutes
c!#~   datawin_prints  logical;
c!#~   sig0qual_prints  logical; 
c!#    alias     logical; which ambiguities to use in the analysis
c!#~   rank_by_JPL    logical; rank ambiguities as in JPL HDF files
c!#~   rank_by_JPL.   (pos. 1 - selected ambiguity; remaining positions
c!#~   rank_by_JPL..  filled in order of decreasing mle)
c!#    nthin     Thins WVC's which fall within time and space windowing,
c!#              and pass all QC checks
      integer nfile, idate, itime, itag, nthin !#
      real s0min, s0max, s0min_edit, timwin !#
      character*80 filename !#
      logical datawin_prints, sig0qual_prints, alias(4),rank_by_JPL !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: rbm_stdata.F,v $
c!#	Revision 1.4  1998/09/22 19:24:24  leidner
c!#	added data thinning of NSCAT WVC's
c!#	
c!#	Revision 1.3  1998/05/20 14:22:44  leidner
c!#	moved JPL ranking of ambiguities to subroutine rank_amb_by_JPL.F
c!#	controlled by namelist rank_by_JPL (default is FALSE)
c!#
c!#	Revision 1.2  1998/05/15 20:51:46  stanr
c!#	declare reject logical; change L to L1 in formats
c!#
c!#	Revision 1.1  1998/05/05 15:01:44  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!#   Common block /vam/ defines vam parameters and functions.
#include "vam.h"

c!#   Common block /ns_bscat/ holds NSCAT backscatter data.
#include "ns_bscat.h"

c!#   Common block /ns_bscat/ holds NSCAT winds data.
#include "ns_winds.h"

c!#   Common block /ptdta/ holds ambiguous winds data.
#include "ptdta.h"

c!#   Common blocks for reading 25 km MGDR records.
#include "l20param.h"
#include "mgdr_out.h"
#include "mgdr_com.h"
#include "l20files.h"

c!#   Common block /gparm/ describes grid parameters.
#include "gparm.h"

c!#   Common block /grdta/ describes grid data.
#include "grdta.h"

c!## LOCAL DATA ELEMENTS :

c!# Integer:
c!#~   irec      Record counter
c!#~   i, n      Counters
c!#    wvc_counter  Counts total number of WVC's which fall within time
c!#                 and space windowing, and pass all QC checks
      integer nreject, ngrossfail, ncell_qual, nsubtrack_qual,
     &     nant_qual, npower_qual, negsig0_qual, nsig0_edit,
     &     nland_sfc, nnoicedata_sfc, nice_sfc, wvc_counter
      integer itime_hh, itime_mm, itime_ss, julday
      integer nscat_hh, nscat_mm, nscat_ss
      integer nscat_julday, nscat_idate, nscat_itime
      integer i, irec, n
      integer rank(4)
      logical reject

c!# Real
      real rtime, scat_rtime, dt
      real rlon8, rlat8
      DATA PI/3.1415926535897932D0/

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c     ------------------------------------------------------------------

c!#   1. Open the file

      mgdr_file = filename
      call mgdr_read(0)
      irec = 1

c set wvc_counter for data thinning
      wvc_counter = 0

c     ------------------------------------------------------------------

c!#   2. Read one record

 10   call mgdr_read(irec)

      if (irec.lt.0) return

c     ------------------------------------------------------------------

c!#   3. Process and store data from one record

c!#   3.1 Initialize quality flag counters for this record

      nreject        = 0
      ngrossfail     = 0
      ncell_qual     = 0
      nsubtrack_qual = 0
      nant_qual      = 0
      npower_qual    = 0
      negsig0_qual   = 0
      nsig0_edit     = 0
      nland_sfc      = 0
      nnoicedata_sfc = 0
      nice_sfc       = 0

c!#   3.2 Check time stamp to see if data record is within analysis time window

c!#   3.2.1 Convert NSCAT time tag to yymmdd and hhmmss format

      call converttime(TimeTag,nscat_julday,nscat_idate,nscat_itime,
     &                 nscat_hh,nscat_mm,nscat_ss)

c!#   3.2.2 Convert idate to Julian day

      call yymmdd2julian (idate, julday)

c!#   3.2.3 Convert hhmmss to hours + fraction of hours

      itime_hh    = itime/10000
      itime_mm    = (itime - itime_hh*10000)/100
      itime_ss    = (itime - itime_hh*10000 - itime_mm*100)
      scat_rtime = float(nscat_hh) +
     &              float(nscat_mm)/60. + float(nscat_ss)/3600.
      rtime       = float(itime_hh) + 
     &              float(itime_mm)/60. + float(itime_ss)/3600.

c!#   3.2.4 Check to see if data date is within +/-1 day of idate
c!#   If so, calculate dt.  If not, skip to next data record.

      if (nscat_julday .eq. julday) then
         dt = scat_rtime - rtime
      else if (nscat_julday .eq.(julday-1)) then
         dt = scat_rtime - rtime - 24.
      else if (nscat_julday .eq.(julday+1)) then
         dt = scat_rtime - rtime + 24.
      else 
         if (datawin_prints)
     &   write(*,885) Revnum,irec,idate, julday, itime,
     &                nscat_idate, nscat_julday, nscat_itime
         goto 4001
      endif

c!#   3.2.5 Check to see if data time is within +/-timwin of itime
c!#   If so, process record.  If not, skip to next data record.

      if (abs(dt).gt.timwin) then
        if (datawin_prints)
     &    write(*,887) timwin,Revnum,irec,
     &                 idate,julday,itime,
     &                 itime_hh, itime_mm, itime_ss, rtime,
     &                 nscat_idate, nscat_julday, nscat_itime, 
     &                 nscat_hh, nscat_mm, nscat_ss, scat_rtime,
     &                 dt
          goto 4001
      endif

      if (datawin_prints)
     &  write(*,889) timwin,Revnum,irec,
     &               idate,julday,itime,
     &               itime_hh, itime_mm, itime_ss, rtime,
     &               nscat_idate, nscat_julday, nscat_itime, 
     &               nscat_hh, nscat_mm, nscat_ss, scat_rtime,
     &               dt

c!#   3.3 Loop over all Wind Vector Cells in record

      do 4000 i=1,OUTW

c!#   3.3.1 Transform WVC (long,lat) to grid coordinates

         if (WVCLon(i) .lt. 0) WVCLon(i) = WVCLon(i) + 360.0
         rlon8 = WVCLon(i)
         rlat8 = WVCLat(i)
         
         if (datawin_prints) then
            write(*,900) ys,yf,xs,xf
            write(*,910) i,rlat8,rlon8
         endif

         call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*4000)
         call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*4000)

         if (datawin_prints) write(*,920) WVC_I,i

c!#   3.3.2 Check to see if ocean sigma0 data present

         nocean = 0
         do 1100 islot=1,2
            do 1000 ibeam=1,4
               if (Beam_inx(islot,ibeam,i).ne.0) then
                  j = Beam_inx(islot,ibeam,i)
               else
                  goto 1000
               endif
               if ( ibits(SurfFlag(j,i),0,1) .eq. 0 )
     &              nocean = nocean + 1
 1000       continue
 1100    continue
      
c!#   if no ocean sigma0 info, skip this WVC

         if ( nocean .eq. 0 ) goto 4000

c!#   3.3.3 Increment Wind Vector Cell counter, check WVC storage 

         nsigma0 = 0
         ns_nWVC = ns_nWVC + 1
         nsw_nwvc = nsw_nwvc + 1
         NPTS = NPTS + 1
         NPTSA = NPTSA + 1

         if (ns_nWVC .gt. NS_MWVC) then
            print *, 'not enough space; increase NS_MWVC'
            stop 4000
         endif

         if (nsw_nwvc .gt. NS_MWVC) then
            print *, 'not enough space; increase NS_MWVC'
            stop 4000
         endif

         if (NPTS .gt. PTMAX) then
            print *, 'not enough space; increase PTMAX'
            stop 4000
         endif

c!#   3.3.4 Store WVC latitude and longitude in

c!#         sigma0 data buffer,

         WVC_Lat(ns_nWVC) = WVCLat(i)
         WVC_Lon(ns_nWVC) = WVCLon(i)
         WVC_ic(ns_nWVC)  = ic
         WVC_jc(ns_nWVC)  = jc
         WVC_xc(ns_nWVC)  = xc
         WVC_yc(ns_nWVC)  = yc
         WVC_row(ns_nWVC) = WVC_I
         WVC_col(ns_nWVC) = i

c!#         NSCAT wind data buffer, and

         if (wvclon(i) .lt. 0) wvclon(i) = wvclon(i) + 360.0
         nsw_Lat(nsw_nwvc)     = WVClat(i)
         nsw_Lon(nsw_nwvc)     = WVClon(i)
         nsw_ic(nsw_nwvc)      = ic
         nsw_jc(nsw_nwvc)      = jc
         nsw_xc(nsw_nwvc)      = xc
         nsw_yc(nsw_nwvc)      = yc
         nsw_row(nsw_nwvc)     = WVC_I
         nsw_col(nsw_nwvc)     = i
         nsw_time(nsw_nwvc)    = dt
         nsw_tag(nsw_nwvc)     = itag
         nsw_qc(nsw_nwvc)      = 0

c!#         ambiguous winds data buffer.

         XCOORD(NPTS)          = xc
         YCOORD(NPTS)          = yc
         ILL(NPTS)             = ic
         JLL(NPTS)             = jc
         WGT(NPTS)             = 1
         IOBS(NPTS)            = itag
         pt_qc(NPTS)           = 0

c!#   3.3.5 Check to see if any good sigma0 measurements in this WVC

         if (NumGood(i) .eq. 0) then
            ns_nsig0(ns_nWVC) = 0
            goto 3200
         endif

c!#   3.3.6 Loop over sigma0 measurements (8 data slots) in this WVC

         do 3000 islot=1,2
            do 3100 ibeam=1,4

c!#   3.3.6.1 Is sigma0 measurement present in this data slot?

               if (Beam_inx(islot,ibeam,i).ne.0) then
                  j = Beam_inx(islot,ibeam,i)
               else
                  goto 3100
               endif

c!#   3.3.6.2 Transform sigma0 (long,lat) to grid coordinates

               rlon8 = Sig0Lon(j,i)
               rlat8 = Sig0Lat(j,i)
               call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*3100)
               call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*3100)
               
c!#   3.3.6.3 Check sigma0 QC flags

               reject = .false.
               if (sig0qual_prints) then
                  write(*,9010) Sig0(j,i),Sig0Lat(j,i),Sig0Lon(j,i),
     &                 Cell_Azi(j,i), Inc_Angle(j,i)
                  write(*,9020) ibits(Sig0Qual(j,i),0,1),
     &                 btest(Sig0Qual(j,i),0)
                  write(*,9030) ibits(Sig0Qual(j,i),1,1),
     &                 btest(Sig0Qual(j,i),1)
                  write(*,9040) ibits(Sig0Qual(j,i),2,1),
     &                 btest(Sig0Qual(j,i),2)
                  write(*,9050) ibits(Sig0Qual(j,i),3,1),
     &                    btest(Sig0Qual(j,i),3)
                  write(*,9052) ibits(Sig0Qual(j,i),10,1),
     &                 btest(Sig0Qual(j,i),10)
                  write(*,9070) ibits(SurfFlag(j,i),0,1),
     &                 btest(SurfFlag(j,i),0)
                  write(*,9080) ibits(SurfFlag(j,i),1,1),
     &                 btest(SurfFlag(j,i),1)
                  write(*,9090) ibits(SurfFlag(j,i),2,1),
     &                 btest(SurfFlag(j,i),2)
               endif

c!#   3.3.6.3.1 Gross quality check on sigma0 measurement

               s0lin = 
     &              (-1)**(ibits(Sig0Qual(j,i),10,1)) *
     &              10.**(0.1*Sig0(j,i))
               if (s0lin.lt.s0min .or. s0lin.gt.s0max) then
                  reject = .true.
                  ngrossfail = ngrossfail + 1
                  if (sig0qual_prints) write(*,9200)
               endif

c!#   3.3.6.3.2 Check Sigma0_Quality_Flag bits 0-3 and 10

               if ( ibits(Sig0Qual(j,i),0,1) .eq. 1 ) then
                  reject = .true.
                  ncell_qual = ncell_qual + 1
                  if (sig0qual_prints) write(*,9300)
               else if ( ibits(Sig0Qual(j,i),1,1) .eq. 1 ) then
                  reject = .true.
                  nsubtrack_qual = nsubtrack_qual + 1
                  if (sig0qual_prints) write(*,9310)
               else if ( ibits(Sig0Qual(j,i),2,1) .eq. 1 ) then 
                  reject = .true.
                  nant_qual = nant_qual + 1
                  if (sig0qual_prints) write(*,9320)
               else if ( ibits(Sig0Qual(j,i),3,1) .eq. 1 ) then 
                  reject = .true.
                  npower_qual = npower_qual + 1
                  if (sig0qual_prints) write(*,9330)
               endif
               if ( ibits(Sig0Qual(j,i),10,1) .eq. 1 ) then 
                  negsig0_qual = negsig0_qual + 1
                  if (sig0qual_prints) write(*,9340)
     &                 (ibits(Sig0Qual(j,i),10,1)),
     &                 (-1)**(ibits(Sig0Qual(j,i),10,1)),
     &                 (-1)**(ibits(Sig0Qual(j,i),10,1)) *
     &                 10.**(0.1*sigma0(j,i))
               endif

c!#   3.3.6.3.3 Check Surface_Flag bits 0-2

               if ( ibits(SurfFlag(j,i),0,1) .eq. 1 ) then
                  reject = .true.
                  nland_sfc = nland_sfc + 1
                  if (sig0qual_prints) write(*,9400)
               else if ( ibits(SurfFlag(j,i),1,1) .eq. 1 ) then
                  reject = .true.
                  nnoicedata_sfc = nnoicedata_sfc + 1
                  if (sig0qual_prints) write(*,9410)
               else if ( ibits(SurfFlag(j,i),2,1) .eq. 1 ) then 
                  reject = .true.
                  nice_sfc = nice_sfc + 1
                  if (sig0qual_prints) write(*,9420)
               endif

c!#             If rejected, skip to next sigma0

               if (reject) then
                  nreject = nreject + 1
                  if (sig0qual_prints) write(*,9500) j,nsigma0,ns_nobs
                  go to 3100
               endif

c!#   3.3.6.4 Increment sigma0 obs counter, check sigma0 storage 

               nsigma0 = nsigma0 + 1
               ns_nobs = ns_nobs + 1
               if (ns_nobs .gt. NS_MOBS) then
                  print *, 'not enough space; increase NS_MOBS'
                  stop 3100
               endif

               if (sig0qual_prints) then
                  write(*,9510) 
                  write(*,9500) j,nsigma0,ns_nobs
               endif

c!#   3.3.6.5 Store sigma0 information

               ns_time(ns_nobs)    = dt
               ns_lat(ns_nobs)     = Sig0Lat(j,i)
               ns_lon(ns_nobs)     = Sig0Lon(j,i)
               ns_qc(ns_nobs)      = 0
               ns_ic(ns_nobs)      = ic
               ns_row(ns_nobs)     = WVC_I
               ns_col(ns_nobs)     = i
               ns_jc(ns_nobs)      = jc
               ns_xc(ns_nobs)      = xc
               ns_yc(ns_nobs)      = yc
               ns_tag(ns_nobs)     = itag
               ns_theta(ns_nobs)   = Inc_Angle(j,i) * PI/180.0
               azi                 = Cell_Azi(j,i) + 180.0
               if (azi .ge. 360.0) azi  = azi - 360.0
               ns_azim (ns_nobs)   = azi * PI/180.0

c!#   3.3.6.5.1 Edit negative/very small sigma0 measurements
c               in the range s0min < s0lin < sig0min_edit
c               (see namelist_rnb_rnback.F)

               if ( s0lin .lt. s0min_edit ) then
                  if (sig0qual_prints) write(*,880) j,WVC_I,i,
     &                 s0lin,s0min_edit
                  ns_s0obs(ns_nobs) = s0min_edit
                  nsig0_edit = nsig0_edit + 1
               else
                  ns_s0obs(ns_nobs) = s0lin
               endif

c               Tag negative/very small sigma0 measurements

               if ( s0lin .lt. 1.e-6 ) ns_tag(ns_nobs) = -1
               ns_kpa(ns_nobs)     = Alpham(j,i)
               ns_kpb(ns_nobs)     = Betam(j,i)
               ns_kpc(ns_nobs)     = Gammam(j,i)

c         Polarization: in NSCAT data files,  0=no measurement, 1=Vpol, 2=Hpol

               ns_pol(ns_nobs)     = Polarization(j,i)

c         ..but in VAM, 0=Hpol, 1=Vpol, so reset Hpol to VAM convention

               if (ns_pol(ns_nobs) .gt. 1.5) ns_pol(ns_nobs) = 0

c         Antenna number
c         Scatterometer antennae are numbered according to firing
c         order 1-8: (1) r. forebeam (Vpol)
c                    (2) l. forebeam (Vpol)
c                    (3) r. midbeam (Vpol)
c                    (4) l. midbeam (Vpol)
c                    (5) r. midbeam (Hpol)
c                    (6) l. midbeam (Hpol)
c                    (7) r. aftbeam (Vpol)
c                    (8) l. aftbeam (Vpol)
c         ----------------------------------------------------------

c           Left side of satellite (1 <= cell <= 24)

               if (i .le. 24) then 
                  
                  if (ibeam.eq.1) then
                     ns_ant(ns_nobs) = 2
                  else if (ibeam.eq.2) then
                     ns_ant(ns_nobs) = 4
                  else if (ibeam.eq.3) then
                     ns_ant(ns_nobs) = 6 
                  else if (ibeam.eq.4) then
                     ns_ant(ns_nobs) = 8
                  endif

c           Right side of satellite (25 <= cell <= 48)
c
               else 
                  if (ibeam.eq.1) then
                     ns_ant(ns_nobs) = 1
                  else if (ibeam.eq.2) then
                     ns_ant(ns_nobs) = 3
                  else if (ibeam.eq.3) then
                     ns_ant(ns_nobs) = 5 
                  else if (ibeam.eq.4) then
                     ns_ant(ns_nobs) = 7
                  endif

               endif

 3100       continue
 3000    continue

c!#   3.3.6.6 Store number of sigma0 measurements accepted in this WVC

         ns_nsig0(ns_nWVC) = nsigma0

c     ...begin storing NSCAT winds data

 3200    continue

c!#   3.3.7 Check WVC_Quality_Flag, and if any ambiguities in this WVC

c     Per Frank Wentz, if no ambiguities, and wspd = 0, use as a good
c     report of zero wind from 360.
c
         if (No_ambigs(i).eq.0 .and. Windspeed(1,i).eq.0
     &        .and. WVCqual(i).eq.0 ) then
            print *, 'using zero wind for wvc rev, row, cell ',
     &           Revnum, WVC_I, i
            print *, 'numambigs, wvcqual, windspeed1, winddir1 = ',
     &           No_ambigs(i), WVCqual(i), Windspeed(1,i), Wind_dir(1,i) 
            nsw_nambigs(nsw_nwvc) = 1
            nsw_u(nsw_nwvc,1)   = 0.
            nsw_v(nsw_nwvc,1)   = 0.
            nsw_mle(nsw_nwvc,1) = Likelihood(1,i)
            NWINDS(NPTS)        = 1
            UOBS(1,NPTS)        = 0.
            VOBS(1,NPTS)        = 0.           
            VBAR(NPTS)          = 0.
            Sel(nsw_nwvc)       = 1
            goto 2001
         endif

c     Otherwise, if winds of poor quality or no ambiguities or
c     only one ambiguity, skip

         if ( WVCqual(i).ne.0 .or. No_ambigs(i).eq.0 ) then
            nsw_nambigs(nsw_nwvc) = 0
            NWINDS(NPTS) = 0
            Sel(nsw_nwvc) = -999
            goto 2001
         endif

c!#   3.3.8 Store ambiguities as (u,v) and Maximum Likelihood Estimator

         nsw_nambigs(nsw_nwvc) = No_ambigs(i)
         NWINDS(NPTS) = No_ambigs(i)

c!#   3.3.8.1 Rank ambiguities

         if (rank_by_JPL) then
            call rank_amb_by_JPL
     I           ( nsw_nambigs(nsw_nwvc), Ambselect(i),
     I           Likelihood(1,i),
     O           rank )
            Sel(nsw_nwvc) = 1
         else
            do j=1,nsw_nambigs(nsw_nwvc)
               rank(j) = j
            enddo
            if (nsw_nambigs(nsw_nwvc).eq.1) then
               Sel(nsw_nwvc) = 1
            else
               Sel(nsw_nwvc) = Ambselect(i)
            endif
      if (Ambselect(i).eq.0) then
         print *, 'No_ambigs Ambselect WVCqual ns_nsig0 =',
     &        No_ambigs(i),Ambselect(i),WVCqual(i),ns_nsig0(nsw_nwvc)
      endif
         endif

c!#   3.3.8.2 Store ambiguities according to rank

         sumspd = 0
         do 2000 j=1,nsw_nambigs(nsw_nwvc)
            irank = rank(j)
            if (Windspeed(j,i) .lt. 0) then
               write(*,*) 'Encountered a negative windspeed!!'
               write(*,*) 'windspeed(j,i)=',Windspeed(j,i)
               write(*,*) 'Stopping...'
               STOP 2000
            endif
            wspd = Windspeed(j,i)
            sumspd = sumspd + wspd
            wdir = Wind_dir(j,i) + 180.
            if (wdir .ge. 360.) wdir = wdir - 360.
            wdir = wdir*PI/180.
            call winds(ucomp, vcomp, wspd, wdir, .true.)
            nsw_u(nsw_nwvc,irank)   = ucomp 
            nsw_v(nsw_nwvc,irank)   = vcomp 
            nsw_mle(nsw_nwvc,irank) = Likelihood(j,i)
            UOBS(irank,NPTS)        = ucomp
            VOBS(irank,NPTS)        = vcomp
 2000    continue
         VBAR(NPTS) = sumspd/nsw_nambigs(nsw_nwvc)
 2001    continue

c!#   3.3.9 Set missing ambiguities to NS_FILL

         if ( nsw_nambigs(nsw_nwvc) .lt. 4 ) then
            do 2100 j=nsw_nambigs(nsw_nwvc)+1,4
               nsw_u(nsw_nwvc,j)   = NS_FILL
               nsw_v(nsw_nwvc,j)   = NS_FILL
               nsw_mle(nsw_nwvc,j) = NS_FILL
               UOBS(j,NPTS)        = FILL
               VOBS(j,NPTS)        = FILL
 2100       continue
         endif

c!#   3.3.10 Set ambiguities to NS_FILL if not wanted (see alias)

c     reset ambiguity count holders for this WVC
         nsw_nambigs(nsw_nwvc) = 0
         NWINDS(NPTS) = 0

         do 2200 j=1,4

c     if alias not requested, set winds to FILL value
            if (.not.alias(j)) then
               nsw_u(nsw_nwvc,j)   = NS_FILL
               nsw_v(nsw_nwvc,j)   = NS_FILL
               nsw_mle(nsw_nwvc,j) = NS_FILL
               UOBS(j,NPTS)        = FILL
               VOBS(j,NPTS)        = FILL
            endif

c     if ambiguity not missing, increment alias count

            if (.not.FTEST(nsw_u(nsw_nwvc,j),NS_FILL)) then
               nsw_nambigs(nsw_nwvc) = nsw_nambigs(nsw_nwvc) + 1
               NWINDS(NPTS) = NWINDS(NPTS) + 1
            endif
               
 2200    continue

c!#   if no ambiguities and no sigma0 data stored, decrement counters

         if ( nsw_nambigs(nsw_nwvc) .eq. 0 .and.
     &        ns_nsig0(ns_nWVC) .eq. 0 ) then
            ns_nWVC = ns_nWVC - 1
            nsw_nwvc = nsw_nwvc - 1
            NPTS = NPTS - 1         
            NPTSA = NPTSA - 1

         else

c!#   Skip Wind Vector Cells according to nthin

            wvc_counter = wvc_counter + 1
            if (mod(wvc_counter-1,nthin) .ne. 0) then ! decrement counters
               ns_nWVC = ns_nWVC - 1
               nsw_nwvc = nsw_nwvc - 1
               NPTS = NPTS - 1         
               NPTSA = NPTSA - 1
            endif
         endif

 4000 continue

c     ------------------------------------------------------------------

c!#   4. Print out summary of negative and rejected sigma0 
c!#   measurements for WVC row

      if (nsig0_edit .gt. 0) write(*,884) WVC_I, nsig0_edit
      if (negsig0_qual .gt. 0) write(*,886) WVC_I, negsig0_qual
      if (nreject .gt. 0) write(*,888) WVC_I, nreject,
     &                                 ngrossfail, 
     &                                 ncell_qual, nsubtrack_qual,
     &                                 nant_qual,  npower_qual,
     &                                 nland_sfc,  nnoicedata_sfc,
     &                                 nice_sfc

c     ------------------------------------------------------------------

c!#   5. Increment record counter and process next record

 4001 continue
      irec = irec + 1
      goto 10

c     ------------------------------------------------------------------

c     Formats

  880 format('!! data edit: changing sig0',i3,'/24 row',i4,' col',i3, 
     &    ' from s0obs =',e10.4e1,' to s0min_edit =',e10.4e1)
  884 format( '    # of edited sigma0 obs in WVC row',i4,    9x,'=',i3)
  885 format(/'Observation outside analysis time window +/-1 julday',
     &       '; rev',i4,' record',i4,/
     &       '     background :',i7,i4,i7,/ 
     &       '     observation:',i7,i4,i7)
  886 format( '  # of negative sigma0 obs in WVC row',i4,    9x,'=',i3)
  887 format(/'Observation outside analysis time window +/-',f3.1,' h',
     &       '; rev',i4,' record',i4,/
     &       '     background :',i7,i4,i7,3i3,f8.4,/ 
     &       '     observation:',i7,i4,i7,3i3,f8.4,/
     &       '     difference :',f8.4,' hours')
  888 format(/'# of sigma0 obs rejected from WVC row',i4,    9x,'=',i3,/
     &        '                       failed gross quality check :',i3,/
     &        ' cell location failed to converge (sig0qual bit 0):',i3,/
     &        '   subtrack location out of range (sig0qual bit 1):',i3,/
     &        '       antenna angle out of range (sig0qual bit 2):',i3,/
     &        '     transmit power not available (sig0qual bit 3):',i3,/
     &        '                  negative sigma0 (sig0qual bit10):',i3,/
     &        '                data over land (surfacequal bit 0):',i3,/
     &        '        ice data not available (surfacequal bit 1):',i3,/
     &        '                 data over ice (surfacequal bit 2):',i3)
  889 format(/'Observation within analysis time window +/-',f3.1,' h',
     &       '; rev',i4, ' record',i4,/
     &       '     background :',i7,i4,i7,3i3,f8.4,/ 
     &       '     observation:',i7,i4,i7,3i3,f8.4,/
     &       '     difference :',f8.4,' hours')
  900 format('    Data window : lat[',f8.3,',',f8.3,']  ',
     &                         'lon[',f8.3,',',f8.3,']')
  910 format('     WVC',i3,' loc :         ',f8.3,'                ',
     &        f8.3)
  920 format('                  WVC_row[',i4,']           WVC_col[',
     &       i3,']',/
     &       '                                ...WVC accepted...')
 9010 format(/' SIGMA0 INFO    :',5f8.3)
 9020 format( '      cell_qual : ibits,btest=',i2,1x,l1)
 9030 format( '  subtrack_qual : ibits,btest=',i2,1x,l1)
 9040 format( '       ant_qual : ibits,btest=',i2,1x,l1)
 9050 format( '     power_qual : ibits,btest=',i2,1x,l1)
 9052 format( '       neg sig0 : ibits,btest=',i2,1x,l1)
 9070 format( '       land_sfc : ibits,btest=',i2,1x,l1)
 9080 format( '  noicedata_sfc : ibits,btest=',i2,1x,l1)
 9090 format( '        ice_sfc : ibits,btest=',i2,1x,l1)
 9200 format( '                  failed gross check...')
 9300 format( '                  failed cell quality check...')
 9310 format( '                  failed subtrack quality check...')
 9320 format( '                  failed antennta quality check...')
 9330 format( '                  failed power quality check...')
 9340 format( 'negative sigma0...',/
     &        '    ibits(Sig0Qual(j,i),10,1)          =',i3,/
     &        '    (-1)**(ibits(Sig0Qual(j,i),10,1))  =',i3,/
     &        '    (-1)^ibits * 10^(sig0/10)            =',e10.4e1)
 9400 format( '                  failed land quality check...')
 9410 format( '                  failed ice data quality check...')
 9420 format( '                  failed ice quality check...')
 9500 format( ' SIGMA0 COUNTERS: sig0#',i3,'  nsigma0',i3,
     &       '  ns_nobs',i7)
 9510 format( '    ...sigma0 data accepted...')

      return
      end
