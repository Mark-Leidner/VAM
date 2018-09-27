c!#   $Id: process_record.F,v 1.10 1998/05/18 16:11:03 stanr Exp $
c!#   $Log: process_record.F,v $
c!#   Revision 1.10  1998/05/18 16:11:03  stanr
c!#   declare scale external; change L to L1 in formats
c!#
c!#   Revision 1.9  1998/05/15 20:55:04  stanr
c!#   declare reject logical
c!#
c!#   Revision 1.8  1998/02/11 20:00:18  leidner
c!#   added wvc row and column to backscatter storage
c!#
c!#   Revision 1.7  1997/07/10 18:12:57  leidner
c!#   changed antenna numbering from clockwise (1-6) to firing order (1-8)
c!#
c!#	Revision 1.6  1997/06/23  17:29:18  leidner
c!#	Added neg sig0 editing; made ns_kpb, ns_kpc correction version-
c!#	dependent
c!#
c!#	Revision 1.5  1997/06/10  19:08:40  leidner
c!#	added check for negative sigma0 and moved Kp specification
c!#	to rnb_rdback.F
c!#
c!#	Revision 1.4  1997/04/14  19:08:02  leidner
c!#	removed Kp calculation (now in rnb_rdback.F)
c!#
c!#	Revision 1.3  1997/04/14  11:53:00  leidner
c!#	added quality check for surface_flag, cleaned up code and added documentation
c!#
c!#	Revision 1.2  1997/04/08  19:53:26  leidner
c!#	added storage of coeffs for sig0 variance equation and
c!#	calculation of Kp
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#
********************************************************************************* 
* 										*
* subroutine process_record(fp, recordnum, time_val)				*
*										*
* 	purpose: High-level subroutine which updates the data			*
*		for all the SDS's in the record, scales the data,		*
*		and assigns the data to the appropriate arrays.			*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*		recordnum - record number of record to be processed		*
*			RECORDS ARE 0-BASED!					*
*	output:									*
*		<none>								*
*										*
*	comments: 								*
*		Comment out the lines which refer to SDS's which		*
*		you will not be using.						*
*										*
*		This subprogram is suitable for entering			*
*		scientific calculations.					*
*										*
********************************************************************************* 

      subroutine process_record(fp,recordnum,time_val,row,
     &                  s0min,s0max,s0min_edit,itag,idate,itime,timwin,
     &                   revnum,datawin_prints,sig0qual_prints)
c
#include "rdglob17.h"
#include "vam.h"
#include "gparm.h"
#include "ns_bscat.h"
c
********************************************************************************* 
*	LOCAL VARIABLES	 
*
********************************************************************************* 

*	logical file unit	
	integer fp
*	record number of the record to be processed
	integer recordnum
*	declare type for scale function
	real scale
	external scale
*	counters for loops	
	integer i,j
*	record time
	character*24	time_val
*
        integer row
        integer*4 idate, itime, revnum
        logical datawin_prints,sig0qual_prints
        real nscat_rtime
	logical reject
        DATA PI/3.1415926535897932D0/

********************************************************************************* 
*	Update the SDS 'raw data' memory for the current record			*
********************************************************************************* 

	call updaterec(fp,recordnum)

********************************************************************************* 
*	Scale the values and assign the values to the appropriate arrays	*
*	1-dimensional data							*
*	(outer-most loop)							*
********************************************************************************* 

	do 200,i=1,24							

********************************************************************************* 
*		real numbers							*
********************************************************************************* 

                wvclat(i)=scale(idx_wvclat,raw_wvclat(i))
                wvclon(i)=scale(idx_wvclon,raw_wvclon(i))

********************************************************************************* 
*		integers							*
********************************************************************************* 

                numbeam12(i)=scale(idx_numbeam12,raw_numbeam12(i))
                numbeam34(i)=scale(idx_numbeam34,raw_numbeam34(i))
                numbeam56(i)=scale(idx_numbeam56,raw_numbeam56(i))
                numbeam78(i)=scale(idx_numbeam78,raw_numbeam78(i))
                numgoodsig(i)=scale(idx_numgoodsig,raw_numgoodsig(i))
                numsigma0(i)=scale(idx_numsigma0,raw_numsigma0(i))
                wvcqual(i)=scale(idx_wvcqual,raw_wvcqual(i))

********************************************************************************* 
*		bitflags							*
********************************************************************************* 

                sigma0uf1(i)=int(scale(idx_sigma0uf1,raw_sigma0uf1(i)))
                sigma0uf2(i)=int(scale(idx_sigma0uf2,raw_sigma0uf2(i)))
                sigma0uf3(i)=int(scale(idx_sigma0uf3,raw_sigma0uf3(i)))
        	
********************************************************************************* 
*	2-dimensional data							*
********************************************************************************* 

                do 100,j=1,24
        
********************************************************************************* 
*			real numbers						*
********************************************************************************* 

                        cellazimuth(j,i)=scale(idx_cellazimuth,
     :                          raw_cellazimuth(j,i))
                        cenlat(j,i)=scale(idx_cenlat,raw_cenlat(j,i))
                        cenlon(j,i)=scale(idx_cenlon,raw_cenlon(j,i))
                        coeffa(j,i)=scale(idx_coeffa,raw_coeffa(j,i))
                        coeffb(j,i)=scale(idx_coeffb,raw_coeffb(j,i))
                        coeffc(j,i)=scale(idx_coeffc,raw_coeffc(j,i))
                        incangle(j,i)=scale(idx_incangle,
     :                          raw_incangle(j,i))
                        sigma0(j,i)=scale(idx_sigma0,raw_sigma0(j,i))
                        atmosatten(j,i)=scale(idx_atmosatten,
     :                          raw_atmosatten(j,i))

********************************************************************************* 
*			integers						*
********************************************************************************* 

                        kpolar(j,i)=scale(idx_kpolar,raw_kpolar(j,i))

********************************************************************************* 
*			bit flags						*
********************************************************************************* 

                        sigma0qual(j,i)=int(scale(idx_sigma0qual,
     :	                        raw_sigma0qual(j,i)))
                        surfaceflag(j,i)=int(scale(idx_surfaceflag,
     :                          raw_surfaceflag(j,i)))
100             continue
200     continue

********************************************************************************* 
*	IF YOU NEED TO DO ANY CALCULATIONS THEY GO HERE 			*
*	if you would like to print the rec number, time, lat and lon 		*	
*	uncoment out the following print statement				*
********************************************************************************* 

C******** B E G I N   V A M   C O D E  TO  S T O R E  S I G M A 0  *******
c
c     All NSCAT data is stored in "ns_bscat.h".  See comments therein for
c     more information.
c
c     Initialize quality flag counters for this record
c     ------------------------------------------------
c
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
c
c**   1.  Check time stamp to see if data record is within analysis time window
c         ---------------------------------------------------------------------
c
c**   1.1.  Convert NSCAT time tag to yymmdd and hhmmss format
c           --------------------------------------------------
c
      call converttime(time_val,nscat_julday,nscat_idate,nscat_itime,
     &                 nscat_hh,nscat_mm,nscat_ss)
c
c**   1.2.  Convert idate to Julian day
c           ---------------------------
c
      call yymmdd2julian (idate, julday)
c
c**   1.3.  Convert hhmmss to hours + fraction of hours
c           -------------------------------------------
c
      itime_hh    = itime/10000
      itime_mm    = (itime - itime_hh*10000)/100
      itime_ss    = (itime - itime_hh*10000 - itime_mm*100)
      nscat_rtime = float(nscat_hh) +
     &              float(nscat_mm)/60 + float(nscat_ss)/3600
      rtime       = float(itime_hh) + 
     &              float(itime_mm)/60 + float(itime_ss)/3600
c
c**   1.4.  Check to see if data date is within +/-1 day of idate
c           If so, calculate dt.  If not, skip to next data record.
c           -----------------------------------------------------------
c
      if (nscat_julday .eq. julday) then
         dt = nscat_rtime - rtime
      else if (nscat_julday .eq.(julday-1)) then
         dt = nscat_rtime - rtime - 24.
      else if (nscat_julday .eq.(julday+1)) then
         dt = nscat_rtime - rtime + 24.
      else 
         if (datawin_prints)
     &   write(*,885) revnum,recordnum,idate, julday, itime,
     &                nscat_idate, nscat_julday, nscat_itime
         goto 4001
      endif
c
c**   1.5.  Check to see if data time is within +/-timwin of itime
c           If so, process sigma0 data.  If not, skip to next record.
c           ---------------------------------------------------------
c
      if (abs(dt).gt.timwin) then
        if (datawin_prints)
     &    write(*,887) timwin,revnum,recordnum,
c    &                 idate,julday,itime,
c    &                 itime_hh, itime_mm, itime_ss, rtime,
c    &                 nscat_idate, nscat_julday, nscat_itime, 
c    &                 nscat_hh, nscat_mm, nscat_ss, nscat_rtime,
     &                 dt
          goto 4001
      endif
c
      if (datawin_prints)
     &  write(*,889) timwin,revnum,recordnum,
c    &               idate,julday,itime,
c    &               itime_hh, itime_mm, itime_ss, rtime,
c    &               nscat_idate, nscat_julday, nscat_itime, 
c    &               nscat_hh, nscat_mm, nscat_ss, nscat_rtime,
     &               dt
c
  885 format(/'Observation outside analysis time window +/-1 julday',
     &       '; rev',i4,' record',i4,/
     &       '     background :',i7,i4,i7,/ 
     &       '     observation:',i7,i4,i7)
  887 format(/'Observation outside analysis time window +/-',f3.1,' h',
     &       '; rev',i4,' record',i4,/
c    &       '     background :',i7,i4,i7,3i3,f8.4,/ 
c    &       '     observation:',i7,i4,i7,3i3,f8.4,/
     &       '     difference :',f8.4,' hours')
  889 format(/'Observation within analysis time window +/-',f3.1,' h',
     &       '; rev',i4, ' record',i4,/
c    &       '     background :',i7,i4,i7,3i3,f8.4,/ 
c    &       '     observation:',i7,i4,i7,3i3,f8.4,/
     &       '     difference :',f8.4,' hours')
c
c     This data is within the analysis time window => Process WVC row sigma0's
c     ------------------------------------------------------------------------
c
c**   2.  Loop over all Wind Vector Cells in record
c         -----------------------------------------
c
      do 4000 i=1,24
c
c**   2.1.  Transform WVC (long,lat) to grid coordinates
c           --------------------------------------------
c
        if (wvclon(i) .lt. 0) wvclon(i) = wvclon(i) + 360.0
        rlon8 = wvclon(i)
        rlat8 = wvclat(i)
c 
        if (datawin_prints) then
          write(*,900) ys,yf,xs,xf
          write(*,910) i,rlat8,rlon8
        endif
c
        call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*4000)
        call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*4000)
c
        if (datawin_prints) write(*,920) row,i
c
  900 format('    Data window : lat[',f8.3,',',f8.3,']  ',
     &                         'lon[',f8.3,',',f8.3,']')
  910 format('     WVC',i3,' loc :         ',f8.3,'                ',
     &        f8.3)
  920 format('                  WVC_row[',i4,']           WVC_col[',
     &       i3,']',/
     &       '                                ...WVC accepted...')
c
c**   2.2.  Increment Wind Vector Cell counter, check WVC storage 
c           -----------------------------------------------------
c
        nsigma0 = 0
        ns_nWVC = ns_nWVC + 1
c
        if (ns_nWVC .gt. NS_MWVC) then
          print *, 'not enough space; increase NS_MWVC'
          stop 4000
        endif
c
c**   2.3.  Store WVC latitude and longitude
c           --------------------------------
c
        WVC_Lat(ns_nWVC) = wvclat(i)
        WVC_Lon(ns_nWVC) = wvclon(i)
        WVC_ic(ns_nWVC)  = ic
        WVC_jc(ns_nWVC)  = jc
        WVC_xc(ns_nWVC)  = xc
        WVC_yc(ns_nWVC)  = yc
        WVC_row(ns_nWVC) = row
        WVC_col(ns_nWVC) = i
c
c**   2.4.  Check to see if any good sigma0 measurements in this WVC
c           --------------------------------------------------------
c
        if (numgoodsig(i) .eq. 0) then
          ns_nsig0(ns_nWVC) = 0
          goto 4000
        endif
c
c**   2.5.  Loop over sigma0 measurements (up to 24) in this WVC
c           ----------------------------------------------------
c
        do 3000 j=1,24

c
c**   2.5.1.  Is sigma0 measurement present in this data slot?
c             ------------------------------------------------
c
          if (j.le.6 .and.j.gt.numbeam12(i)) goto 3000
          if (j.ge.7 .and.j.le.12.and.j-6 .gt.numbeam34(i)) goto 3000
          if (j.ge.13.and.j.le.18.and.j-12.gt.numbeam56(i)) goto 3000
          if (j.ge.19.and.j-18.gt.numbeam78(i)) goto 3000
c
c**   2.5.2.  Transform sigma0 (long,lat) to grid coordinates
c             -----------------------------------------------
c
          if (cenlon(j,i) .lt. 0) cenlon(j,i) = cenlon(j,i) + 360.0
          rlon8 = cenlon(j,i)
          rlat8 = cenlat(j,i)
          call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*3000)
          call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*3000)
c
c**   2.5.3.  Perform sigma0 QC
c             -----------------
c
          reject = .false.
c
          if (sig0qual_prints) then
            write(*,9010) sigma0(j,i),cenlat(j,i),cenlon(j,i),
     &                    cellazimuth(j,i), incangle(j,i)
            write(*,9020) ibits(sigma0qual(j,i),0,1),
     &                    btest(sigma0qual(j,i),0)
            write(*,9030) ibits(sigma0qual(j,i),1,1),
     &                    btest(sigma0qual(j,i),1)
            write(*,9040) ibits(sigma0qual(j,i),2,1),
     &                    btest(sigma0qual(j,i),2)
            write(*,9050) ibits(sigma0qual(j,i),3,1),
     &                    btest(sigma0qual(j,i),3)
            write(*,9052) ibits(sigma0qual(j,i),10,1),
     &                    btest(sigma0qual(j,i),10)
c           if (j .le. 8) 
c    &        write(*,9060) ibits(sigma0uf1(i),j-1,1),
c    &                      btest(sigma0uf1(i),j-1)
c           if (j .ge. 9 .and. j .le. 16) 
c    &        write(*,9060) ibits(sigma0uf2(i),j-9,1),
c    &                      btest(sigma0uf2(i),j-9)
c           if (j .ge. 17)
c    &        write(*,9060) ibits(sigma0uf3(i),j-17,1),
c    &                      btest(sigma0uf3(i),j-17)
            write(*,9070) ibits(surfaceflag(j,i),0,1),
     &                    btest(surfaceflag(j,i),0)
            write(*,9080) ibits(surfaceflag(j,i),1,1),
     &                    btest(surfaceflag(j,i),1)
            write(*,9090) ibits(surfaceflag(j,i),2,1),
     &                    btest(surfaceflag(j,i),2)
          endif
c
c**   2.5.3.1.  Gross quality check on sigma0 measurement
c               -----------------------------------------
c
          s0lin = 
     &        (-1)**(ibits(sigma0qual(j,i),10,1))*10.**(0.1*sigma0(j,i))
          if (s0lin.lt.s0min .or. s0lin.gt.s0max) then
            reject = .true.
            ngrossfail = ngrossfail + 1
            if (sig0qual_prints) write(*,9200)
          endif
c
c**   2.5.3.2.  Check Sigma0_Quality_Flag bits 0-3 and 10
c               -----------------------------------------
c
          if ( ibits(sigma0qual(j,i),0,1) .eq. 1 ) then
            reject = .true.
            ncell_qual = ncell_qual + 1
            if (sig0qual_prints) write(*,9300)
          else if ( ibits(sigma0qual(j,i),1,1) .eq. 1 ) then
            reject = .true.
            nsubtrack_qual = nsubtrack_qual + 1
            if (sig0qual_prints) write(*,9310)
          else if ( ibits(sigma0qual(j,i),2,1) .eq. 1 ) then 
            reject = .true.
            nant_qual = nant_qual + 1
            if (sig0qual_prints) write(*,9320)
          else if ( ibits(sigma0qual(j,i),3,1) .eq. 1 ) then 
            reject = .true.
            npower_qual = npower_qual + 1
            if (sig0qual_prints) write(*,9330)
          endif
          if ( ibits(sigma0qual(j,i),10,1) .eq. 1 ) then 
            negsig0_qual = negsig0_qual + 1
            if (sig0qual_prints) write(*,9340)
     &        (ibits(sigma0qual(j,i),10,1)),
     &        (-1)**(ibits(sigma0qual(j,i),10,1)),
     &        (-1)**(ibits(sigma0qual(j,i),10,1))*10.**(0.1*sigma0(j,i))
          endif
c
c**   2.5.3.3.  Check Surface_Flag bits 0-2
c               ---------------------------
c
          if ( ibits(surfaceflag(j,i),0,1) .eq. 1 ) then
            reject = .true.
            nland_sfc = nland_sfc + 1
            if (sig0qual_prints) write(*,9400)
          else if ( ibits(surfaceflag(j,i),1,1) .eq. 1 ) then
            reject = .true.
            nnoicedata_sfc = nnoicedata_sfc + 1
            if (sig0qual_prints) write(*,9410)
          else if ( ibits(surfaceflag(j,i),2,1) .eq. 1 ) then 
            reject = .true.
            nice_sfc = nice_sfc + 1
            if (sig0qual_prints) write(*,9420)
          endif
c
 9010 format(/' SIGMA0 INFO    :',5f8.3)
 9020 format( '      cell_qual : ibits,btest=',i2,1x,l1)
 9030 format( '  subtrack_qual : ibits,btest=',i2,1x,l1)
 9040 format( '       ant_qual : ibits,btest=',i2,1x,l1)
 9050 format( '     power_qual : ibits,btest=',i2,1x,l1)
 9052 format( '       neg sig0 : ibits,btest=',i2,1x,l1)
 9060 format( '  sigma0_usable : ibits,btest=',i2,1x,l1)
 9070 format( '       land_sfc : ibits,btest=',i2,1x,l1)
 9080 format( '  noicedata_sfc : ibits,btest=',i2,1x,l1)
 9090 format( '        ice_sfc : ibits,btest=',i2,1x,l1)
 9200 format( '                  failed gross check...')
 9300 format( '                  failed cell quality check...')
 9310 format( '                  failed subtrack quality check...')
 9320 format( '                  failed antennta quality check...')
 9330 format( '                  failed power quality check...')
 9340 format( 'negative sigma0...',/
     &        '    ibits(sigma0qual(j,i),10,1)          =',i3,/
     &        '    (-1)**(ibits(sigma0qual(j,i),10,1))  =',i3,/
     &        '    (-1)^ibits * 10^(sig0/10)            =',e10.4e1)
 9400 format( '                  failed land quality check...')
 9410 format( '                  failed ice data quality check...')
 9420 format( '                  failed ice quality check...')
 9500 format( ' SIGMA0 COUNTERS: sig0#',i3,'  nsigma0',i3,
     &       '  ns_nobs',i7)
 9510 format( '    ...sigma0 data accepted...')
c
c         If rejected, skip to next sigma0
c         --------------------------------
c
          if (reject) then
            nreject = nreject + 1
            if (sig0qual_prints) write(*,9500) j,nsigma0,ns_nobs
            go to 3000
          endif
c
c**   2.5.4.  Increment sigma0 obs counter, check sigma0 storage 
c             --------------------------------------------------
c
          nsigma0 = nsigma0 + 1
          ns_nobs = ns_nobs + 1
          if (ns_nobs .gt. NS_MOBS) then
            print *, 'not enough space; increase NS_MOBS'
            stop 3000
          endif
c
          if (sig0qual_prints) then
            write(*,9510) 
            write(*,9500) j,nsigma0,ns_nobs
          endif
c
c**   2.5.5.  Store sigma0 information
c             ------------------------
c
          ns_time(ns_nobs)    = dt
          ns_lat(ns_nobs)     = cenlat(j,i)
          ns_lon(ns_nobs)     = cenlon(j,i)
          ns_qc(ns_nobs)      = 0
          ns_ic(ns_nobs)      = ic
          ns_row(ns_nobs)     = row
          ns_col(ns_nobs)     = i
          ns_jc(ns_nobs)      = jc
          ns_xc(ns_nobs)      = xc
          ns_yc(ns_nobs)      = yc
          ns_tag(ns_nobs)     = itag
          ns_theta(ns_nobs)   = incangle(j,i) * PI/180.0
          azi                 = cellazimuth(j,i) + 180.0
          if (azi .ge. 360.0) azi  = azi - 360.0
          ns_azim (ns_nobs)   = azi * PI/180.0
c
c**   2.5.5.1.  Edit sigma0 measurements in the range
c**             s0min < s0lin < sig0min_edit (see namelist_rnb_rnback.F)
c               --------------------------------------------------
c
          if ( s0lin .lt. s0min_edit ) then
            if (sig0qual_prints) write(*,880) j,row,i,s0lin,s0min_edit
            ns_s0obs(ns_nobs) = s0min_edit
            nsig0_edit = nsig0_edit + 1
          else
            ns_s0obs(ns_nobs) = s0lin
          endif
c
c**             Tag negative/very small sigma0 measurements
c
          if ( s0lin .lt. 1.e-6 ) ns_tag(ns_nobs) = -1
c
          ns_kpa(ns_nobs)     = coeffa(j,i)
c
c**             Correct coeffb and coeffc scaling for data earlier
c**             than Build_ID 4.0.0
c
          if ( Ver(1) .lt. 4) then
            ns_kpb(ns_nobs)     = coeffb(j,i)*10.   !correction factor to KpB
            ns_kpc(ns_nobs)     = coeffc(j,i)*1000. !correction factor to KpC
          else
            ns_kpb(ns_nobs)     = coeffb(j,i)
            ns_kpc(ns_nobs)     = coeffc(j,i)
          endif
c
c         Polarization: in HDF files,  0=no measurement, 1=Vpol, 2=Hpol
c         -------------------------------------------------------------
c
          ns_pol(ns_nobs)     = kpolar(j,i)
c
c         ..but in VAM, 0=Hpol, 1=Vpol, so reset Hpol to VAM convention
c         -------------------------------------------------------------
c
          if (ns_pol(ns_nobs) .gt. 1.5) ns_pol(ns_nobs) = 0
c
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
c
c           Left side of satellite (1 <= cell <= 12)
c
          if (i .le. 12) then 
            if (j .le. 6) then 
              ns_ant(ns_nobs) = 2
            else if (j .ge.  7 .and. j .le. 12) then
              ns_ant(ns_nobs) = 4
            else if (j .ge. 13 .and. j .le. 18) then
              ns_ant(ns_nobs) = 6 
            else if (j .ge. 19) then
              ns_ant(ns_nobs) = 8
            endif
c
c           Right side of satellite (13 <= cell <= 24)
c
          else 
            if (j .le. 6) then   
              ns_ant(ns_nobs) = 1
            else if (j .ge.  7 .and. j .le. 12) then 
              ns_ant(ns_nobs) = 3 
            else if (j .ge. 13 .and. j .le. 18) then
              ns_ant(ns_nobs) = 5  
            else 
              ns_ant(ns_nobs) = 7 
            endif 
          endif
c
 3000   continue
c
c**   2.6.  Store number of sigma0 measurements accepted in this WVC
c           --------------------------------------------------------
c    
        ns_nsig0(ns_nWVC) = nsigma0
c
 4000 continue
 4001 continue

c
c**   3.  Print out summary of negative and rejected sigma0 
c**       measurements for WVC row
c         -------------------------------------------------
c
      if (nsig0_edit .gt. 0) write(*,884) row, nsig0_edit
      if (negsig0_qual .gt. 0) write(*,886) row, negsig0_qual
      if (nreject .gt. 0) write(*,888) row, nreject,
     &                                 ngrossfail, 
     &                                 ncell_qual, nsubtrack_qual,
     &                                 nant_qual,  npower_qual,
     &                                 nland_sfc,  nnoicedata_sfc,
     &                                 nice_sfc

  880 format('!! data edit: changing sig0',i3,'/24 row',i4,' col',i3, 
     &    ' from s0obs =',e10.4e1,' to s0min_edit =',e10.4e1)
  884 format( '    # of edited sigma0 obs in WVC row',i4,    9x,'=',i3)
  886 format( '  # of negative sigma0 obs in WVC row',i4,    9x,'=',i3)
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
c
      return
      end
