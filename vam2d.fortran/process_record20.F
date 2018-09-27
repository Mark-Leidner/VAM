c!#   $Id: process_record20.F,v 1.8 1998/05/20 14:33:25 leidner Exp $
c!#   $Log: process_record20.F,v $
c!#   Revision 1.8  1998/05/20 14:33:25  leidner
c!#   added option to rank 50km NSCAT winds by MLE
c!#   controlled by namelist parameter /rank_by_MLE/ (default is TRUE)
c!#
c!#   Revision 1.7  1998/05/05 14:57:16  leidner
c!#   added alias selection to NSCAT winds read from JPL HDF files
c!#
c!#   Revision 1.6  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.5  1998/02/11 20:01:14  leidner
c!#   added setting NWINDS to zero for WVC's with no ambiguities
c!#
c!#   Revision 1.4  1997/12/18 22:17:26  leidner
c!#   added zero wind speed check per F. Wentz
c!#
c!#   Revision 1.3  1997/07/25 16:45:20  leidner
c!#   Added code to put NSCAT wind ambiguities in "ptdta.h"
c!#
c!#	Revision 1.2  1997/04/11  17:27:45  leidner
c!#	added code to fill ns_winds.h
c!#
c!#	Revision 1.1  1997/04/10  15:00:41  leidner
c!#	Initial revision
c!#
********************************************************************************* 
* 										*
* subroutine process_record20(fp, recordnum, time_val)				*
*										*
* 	purpose: High-level subroutine which updates the data			*
*		for all the SDS's in the record, scales the data,		*
*		and assigns the data to the appropriate arrays.			*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*		recordnum - record number of record to be processed		*
*			RECORDS ARE 0-BASED!					*
*		lgoes - logical; is there any goes data in this HDF file	*
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
c
        subroutine process_record20 (fp, recordnum, time_val, row,
     &                        itag,idate,itime,timwin,
     &                        revnum,datawin_prints,lgoes,alias,
     &                        rank_by_MLE)
c
#include "rdglob20.h"
#include "vam.h"
#include "gparm.h"
#include "ns_winds.h"
#include "ptdta.h"
c
********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 

*	logical file unit	
        integer fp
*	record number of the record to be processed
        integer recordnum
*	declare type for scale function
        real scale20
*	counters for loops	
        integer i,j
*	variable containing the time
	character*24 time_val
        logical lgoes
c!#    alias     logical; which ambiguities to use in the analysis
c!#~   rank_by_MLE    logical; rank ambiguities by MLE value
c!#~   rank_by_MLE.   (pos. 1 - highest MLE; remaining positions
c!#~   rank_by_MLE..  filled in order of decreasing MLE)
      logical alias(4), rank_by_MLE
      integer amb_rank(4)
c
        integer row
        integer*4 idate, itime, revnum
        logical datawin_prints
        real nscat_rtime
        DATA PI/3.1415926535897932D0/
        DATA FILL/-777D20/
c
********************************************************************************* 
*	Update the SDS 'raw data' memory for the current recordS		*
********************************************************************************* 

        call updaterec20(fp,recordnum,lgoes)

********************************************************************************* 
*	Scale the values and assign the values to the appropriate arrays	*
*	1-dimensional data							*
*	(outer-most loop: 24 WVC columns)					*
********************************************************************************* 
        if (lgoes) wvcdt=scale20(idx_wvcdt,raw_wvcdt)

        do 200,i=1,24

********************************************************************************* 
*		real numbers							*
********************************************************************************* 

                wvclat(i)=scale20(idx_wvclat,raw_wvclat(i))
                wvclon(i)=scale20(idx_wvclon,raw_wvclon(i))
                if (lgoes) then
                   wvctb(i)=scale20(idx_wvctb,raw_wvctb(i))
                   wvcal(i)=scale20(idx_wvcal,raw_wvcal(i))
                endif

********************************************************************************* 
* 		integers							*
********************************************************************************* 

                meanwind(i)=scale20(idx_meanwind,raw_meanwind(i))
                numambigs(i)=scale20(idx_numambigs,raw_numambigs(i))
                numbeam12(i)=scale20(idx_numbeam12,raw_numbeam12(i))
                numbeam34(i)=scale20(idx_numbeam34,raw_numbeam34(i))
                numbeam56(i)=scale20(idx_numbeam56,raw_numbeam56(i))
                numbeam78(i)=scale20(idx_numbeam78,raw_numbeam78(i))
                numsigma0(i)=scale20(idx_numsigma0,raw_numsigma0(i))
                wvcqual(i)=scale20(idx_wvcqual,raw_wvcqual(i))

********************************************************************************* 
*		bitflags							*
*		<none>								*
********************************************************************************* 
                
********************************************************************************* 
*	2-dimensional data							*
*	(inner loop: 4 ambiguities)						*
********************************************************************************* 

 		do 100,j=1,4        

********************************************************************************* 
*			real numbers						*
********************************************************************************* 

                        direrr(j,i)=scale20(idx_direrr,raw_direrr(j,i))
                        speederr(j,i)=scale20(idx_speederr,
     :                          raw_speederr(j,i))
                        mlelikelihood(j,i)=scale20(idx_mlelikelihood,
     :                          raw_mlelikelihood(j,i))
                        winddir(j,i)=scale20(idx_winddir,
     :                          raw_winddir(j,i))
                        windspeed(j,i)=scale20(idx_windspeed,
     :                          raw_windspeed(j,i))

********************************************************************************* 
*			integers						*
*			<none>							*
********************************************************************************* 

********************************************************************************* 
*			bit flags						*
*			<none>							*
********************************************************************************* 
100                continue
200        continue
c
c***** B E G I N   V A M   C O D E   TO   S T O R E   N S C A T   W I N D S *****
c
c     All NSCAT wind data is stored in "ns_winds.h".  See comments therein for
c     more information.
c
c**   1.  Check time stamp to see if data record is within analysis time window
c         ---------------------------------------------------------------------
c
c**   1.1.  Convert NSCAT time tag to yymmdd and hhmmss format
c           --------------------------------------------------
c
      call converttime(time_val,nscat_julday,nscat_idate,
     &                nscat_itime, nscat_hh,nscat_mm,nscat_ss)
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
c           -------------------------------------------------------
c
      if (nscat_julday .eq. julday) then
         dt = nscat_rtime - rtime
      else if (nscat_julday .eq.(julday-1)) then
         dt = nscat_rtime - rtime - 24.
      else if (nscat_julday .eq.(julday+1)) then
         dt = nscat_rtime - rtime + 24.
      else 
         if (datawin_prints)
     &   write(*,885) revnum, recordnum, idate, julday, itime,
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
     &       '; rev',i5,' record',i4,/
     &       '     background :',i7,i4,i7,/ 
     &       '     observation:',i7,i4,i7)
  887 format(/'Observation outside analysis time window +/-',f3.1,' h',
     &       '; rev',i5,' record',i4,/
c    &       '     background :',i7,i4,i7,3i3,f8.4,/ 
c    &       '     observation:',i7,i4,i7,3i3,f8.4,/
     &       '     difference :',f8.4,' hours')
  889 format(/'Observation within analysis time window +/-',f3.1,' h',
     &       '; rev',i5, ' record',i4,/
c    &       '     background :',i7,i4,i7,3i3,f8.4,/ 
c    &       '     observation:',i7,i4,i7,3i3,f8.4,/
     &       '     difference :',f8.4,' hours')
c
c     This data is within the analysis time window => Process WVC's
c     -------------------------------------------------------------
c
c**   2.  Loop over all Wind Vector Cells in record
c         -----------------------------------------
c
      do 4000 i=1,24
c
c**   2.1.  Transform WVC (long,lat) to grid coordinates
c           --------------------------------------------
c
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
        nsw_nwvc = nsw_nwvc + 1
        NPTS = NPTS + 1
c
        if (nsw_nwvc .gt. NS_MWVC) then
          print *, 'not enough space; increase NS_MWVC'
          stop 900
        endif

        if (NPTS .gt. PTMAX) then
          print *, 'not enough space; increase PTMAX'
          stop 901
        endif
c
c**   2.3.  Store WVC Latitude and Longitude
c           --------------------------------
c
        if (wvclon(i) .lt. 0) wvclon(i) = wvclon(i) + 360.0
        nsw_Lat(nsw_nwvc)     = wvclat(i)
        nsw_Lon(nsw_nwvc)     = wvclon(i)
        nsw_ic(nsw_nwvc)      = ic
        nsw_jc(nsw_nwvc)      = jc
        nsw_xc(nsw_nwvc)      = xc
        nsw_yc(nsw_nwvc)      = yc
        nsw_row(nsw_nwvc)     = row
        nsw_col(nsw_nwvc)     = i
        nsw_time(nsw_nwvc)    = dt
        nsw_tag(nsw_nwvc)     = itag
        nsw_qc(nsw_nwvc)      = 0
        if (lgoes) then
           nsw_tb(nsw_nwvc)      = wvctb(i)
           nsw_al(nsw_nwvc)      = wvcal(i)
           nsw_dt(nsw_nwvc)      = wvcdt
        else
           nsw_tb(nsw_nwvc)      = NS_FILL
           nsw_al(nsw_nwvc)      = NS_FILL
           nsw_dt(nsw_nwvc)      = NS_FILL
        endif
        XCOORD(NPTS)          = xc
        YCOORD(NPTS)          = yc
        ILL(NPTS)             = ic
        JLL(NPTS)             = jc
        WGT(NPTS)             = 1
        IOBS(NPTS)            = itag
        pt_qc(NPTS)           = 0
c
c**   2.4.  Check WVC_Quality_Flag, and if any ambiguities in this WVC
c           ----------------------------------------------------------
c
        if (numambigs(i).eq.0 .and. windspeed(1,i).eq.0) then
c
c**     Per Frank Wentz, if no ambiguities, and wspd = 0, use as a good
c       report of zero wind from 360.
c
           print *, 'using zero wind for wvc rev, row, cell ',
     &          revnum, row, i
           print *, 'numambigs, wvcqual, windspeed1, winddir1 = ',
     &          numambigs(i), wvcqual(i), windspeed(1,i), winddir(1,i) 
           nsw_nambigs(nsw_nwvc) = 1
           nsw_u(nsw_nwvc,1)   = 0.
           nsw_v(nsw_nwvc,1)   = 0.
           nsw_mle(nsw_nwvc,1) = mlelikelihood(1,i)
           UOBS(1,NPTS)        = 0.
           VOBS(1,NPTS)        = 0.           
           Sel(nsw_nwvc)       = 1
           goto 3001
        endif
        if ( wvcqual(i).ne.0 .or. numambigs(i).eq.0 ) then
          nsw_nambigs(nsw_nwvc) = 0
          NWINDS(NPTS) = 0
          Sel(nsw_nwvc) = -999
          goto 3001
        endif
c
c**   2.5.  Store ambiguities as (u,v) and Maximum Likelihood Estimator
c           -----------------------------------------------------------
c
        nsw_nambigs(nsw_nwvc) = numambigs(i)
        NWINDS(NPTS) = numambigs(i)
        NPTSA = NPTSA + 1
c
c!#   2.5.1 Rank ambiguities by MLE (if requested)

        if (rank_by_MLE) then
           call rank_amb_by_MLE
     I          ( nsw_nambigs(nsw_nwvc), mlelikelihood(1,i),
     O          amb_rank )
         else
            do j=1,nsw_nambigs(nsw_nwvc)
               amb_rank(j) = j
            enddo
         endif

c!#   2.5.2 Store ambiguities according to rank
           
        sumspd = 0
        do 3000 j=1,nsw_nambigs(nsw_nwvc)
           irank = amb_rank(j)
           if (windspeed(j,i) .lt. 0) then
              write(*,*) 'Encountered a negative windspeed!!'
              write(*,*) 'windspeed(j,i)=',windspeed(j,i)
              write(*,*) 'Stopping...'
              STOP 3000
           endif
           wspd = windspeed(j,i)
           sumspd = sumspd + wspd
           wdir = winddir(j,i) + 180.
           if (wdir .ge. 360.) wdir = wdir - 360.
           wdir = wdir*PI/180.
           call winds(ucomp, vcomp, wspd, wdir, .true.)
           nsw_u(nsw_nwvc,irank)   = ucomp 
           nsw_v(nsw_nwvc,irank)   = vcomp 
           nsw_mle(nsw_nwvc,irank) = mlelikelihood(j,i)
           UOBS(irank,NPTS)        = ucomp
           VOBS(irank,NPTS)        = vcomp
           if (j.eq.1) Sel(nsw_nwvc) = irank
 3000   continue
        VBAR(NPTS) = sumspd/nsw_nambigs(nsw_nwvc)
 3001   continue
c
c**   2.6.  Set missing ambiguities to NS_FILL
c           ----------------------------------
c
        if ( nsw_nambigs(nsw_nwvc) .lt. 4 ) then
          do 3100 j=nsw_nambigs(nsw_nwvc)+1,4
            nsw_u(nsw_nwvc,j)   = NS_FILL
            nsw_v(nsw_nwvc,j)   = NS_FILL
            nsw_mle(nsw_nwvc,j) = NS_FILL
            UOBS(j,NPTS)        = FILL
            VOBS(j,NPTS)        = FILL
 3100     continue
        endif

c!#   2.7 Set ambiguities to NS_FILL if not wanted (see alias)

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
c
 4000 continue
 4001 continue
c
      return
      end
