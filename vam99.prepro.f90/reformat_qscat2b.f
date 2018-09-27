c    
c!# CSU IDENTIFICATION : reformat_qscat2b

c!# PURPOSE : Read in quikscat hdf file, reformat data and
c!#  write out to binary file for input to the vam. 
c!#  Logical jplrank controls ranking of ambiguities. 
c!#

c!# CSU SPECIFICATION AND CONSTRAINTS:
c  execution:
c  reformat_qscat2b  reformat-namelist

c!# REQUIREMENTS : 

c!# CONSTRAINTS : not compatible with Fortran 90

c!# LANGUAGE : Fortran 77

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :

c====================================================================
c
      program  reformat_qscat2b
c
c====================================================================

c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# REFERENCES : None

c!# LIMITATIONS : 
c!#   namelist contains quikscat filename, reformatted obs 
c!#   output filename, and logical jplrank.

c!# CHANGE LOG : 
c
c $Id: reformat_qscat2b.f,v 1.15 2005/06/15 20:33:14 jhalland Exp $
c 
c $Log: reformat_qscat2b.f,v $
c Revision 1.15  2005/06/15 20:33:14  jhalland
c Declared new variables.
c
c Revision 1.14  2005/01/21 16:55:53  leidner
c updated bit map to include proximity bits for data contaminated by
c land, rain and ice.
c
c Revision 1.13  2005/01/06 16:20:07  leidner
c added capibility for REGULAR thinning.
c
c Revision 1.12  2004/09/30 19:57:35  leidner
c added qc_bit_map to output obs data structure, including setting
c appropriate bit flags.  Also, fixed a problem with format of the
c output header element, itime.
c
c Revision 1.11  2004/09/16 17:01:34  leidner
c added analysis date and time to input namelist parameters, and output
c headers.
c
c Revision 1.10  2004/03/23 20:28:51  leidner
c Changed calculation of time associated with obs.  Time is calculated
c as a delta:
c
c     calculated time = ( obs time - reference time )
c
c where "obs time" and "reference time" are the number of seconds since
c the beginning of the current year.
c
c By default, reference time is the middle of the the current rev.  So,
c the calculated time would range from about -50 minutes (beginning of
c rev) to +50 minutes (end of rev).
c
c Optionally, namelist variables anal_julday and anal_time can be
c used to specify the reference time.
c
c Revision 1.9  2004/03/19 20:47:33  rnh
c FGAT: u_fgat=vfgat=0, alpha=1.  Still TBD: are times useful
c
c Revision 1.8  2002/06/28 18:14:22  mcc
c Added check for iamb ge 1 before processing wvc data.
c
c Revision 1.7  2002/05/22 18:40:32  mcc
c Added coastlnd, rain, rainflguse flags to control obs write.
c This control must be hardwired by the user as needed.
c
c Revision 1.6  2001/10/24 11:54:51  mcc
c Added lat/lon windowing and DIRTH data request capabilities via namelist.
c DIRTH data availabilty based on L2B_algorithm descriptor.
c
c Revision 1.5  2001/01/04 19:20:16  mcc
c Changed parecl from 4 to 1 for SGI.
c
c Revision 1.4  1999/11/18 13:11:42  mcc
c Made upgrades for Build3 including addition of mle
c into main output data array.
c
c Revision 1.2  1999/10/04 14:15:39  mcc
c Changes made to generate timing information
c for native file time interpolation.
c
c

c!# LOCAL DATA ELEMENTS : TBD

c!# LOCAL DATA STRUCTURES : None

c!# DATA FILES : None

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c
c     programmer:
c     Mark C. Cerniglia - AER, Inc.
c     created: Sept 1999.
c 
c     Define Variables

      implicit none

      integer lunin, ierr
      parameter (lunin = 8)
      integer ierr,iostat,idate,itime
      real dfac_const, xmin, xmax, ymin, ymax

      logical jplrank, wdirth
      character*132 l2b_file, fname1, hdr_fname, bin_fname
      integer anal_julday, anal_date, anal_time, thin


      namelist /qsfn/ 
     &     l2b_file, hdr_fname, bin_fname, jplrank, wdirth, 
     &     dfac_const, xmin, xmax, ymin, ymax,
     &     anal_julday, anal_date, anal_time, thin

c     Set namelist defaults
      anal_julday = -1
      anal_time = -1
      thin = 1

c     Retrieve Input Arguments

      call getarg(1,fname1)

      open(unit=lunin,file=fname1,status='old',err=900)
      rewind lunin

c     Execute as directed by namelist.

      read (lunin,qsfn,iostat=ierr) 

      if (ierr .ne. 0) then
         print*, 'Error reading namelist: iostat=',ierr
         goto 999
      endif

      print*,' ' 
      print*,'Quikscat HDF Filename = ',l2b_file
      print*,'jplrank = ',jplrank
      print*,'wdirth = ',wdirth
      print*,'longitude min max range = ',xmin,' ',xmax
      print*,'latitude min max range = ',ymin,' ',ymax
      print*,'analysis Julian day= ',anal_julday
      print*,'analysis time (hours UTC)= ',anal_time
      print*,' ' 

c     read in qscat data in hdf format

      call rd_qscat2b(l2b_file)

c     reformat data and write out qscat data in present form

      call wt_qscat2b(hdr_fname, bin_fname, jplrank, wdirth, 
     &     dfac_const, xmin, xmax, ymin, ymax,
     &     anal_julday, anal_date, anal_time, thin)

      go to 999

 900  continue
      print*,'reformat_qscat2b: unable to access namelist file.'
      call exit(1)

 999  continue

      close(lunin)
      stop
      end
c

c====================================================================
c
      function jpltim2secs(jpltime)
c
c====================================================================
c
c     convert jpl time stamp to seconds from YYYY.

c     JPL TimeTags is in the form of
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field
c     YYYY is the year, DDD is day of year (Julian),
c     hh is hour (UTC), mm is minutes and ss.sss is seconds

      implicit none
      integer ihrb, imnb, iscb, jdb
      integer secm, sech, secd, jpltim2secs

      parameter (secm=60,sech=60*secm,secd=24*sech)

      character*21 jpltime
c
      read (jpltime(06:08),'(i3.3)') jdb
      read (jpltime(10:11),'(i2.2)') ihrb
      read (jpltime(13:14),'(i2.2)') imnb
      read (jpltime(16:17),'(i2.2)') iscb

      jpltim2secs = (jdb-1)*secd + ihrb*sech + imnb*secm + iscb

      return
      end
c
c    
c====================================================================
c     
      subroutine wt_qscat2b(hdr_fname, bin_fname, jplrank, wdirth, 
     &     dfac_const, xmin, xmax, ymin, ymax,
     &     anal_julday, anal_date, anal_time, thin)
c     
c====================================================================
c
c     notes: 

c     TimeTags(MAX_ROWS) is in the form of
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field
c     YYYY is the year, DDD is day of year (Julian),
c     hh is hour (UTC), mm is minutes and ss.sss is seconds

      implicit none
    
      integer MAX_ROWS,MAX_CELLS,MAX_SIG,rev
      integer MAX_WVC,MAX_AMBIG
      parameter (MAX_ROWS = 1624)
      parameter (MAX_WVC = 76)
      parameter (MAX_AMBIG = 4)

      integer lunout, lunrev, jpltim2secs, reftime
      parameter (lunout = 9, lunrev = 10)

      integer idate, itime, nnloc
      integer i, j, namb, jamb, iamb
      integer ierr, lierr
      integer nloc, nocc, nvar, nobs
      integer anal_julday, anal_date, anal_time, thin

      parameter (nloc = MAX_WVC * MAX_ROWS)
      parameter (nocc = MAX_AMBIG)
      parameter (nvar = 5, nobs = nvar * MAX_AMBIG * nloc)
      character*132 hdr_fname, bin_fname
      character*21 TimeTags(MAX_ROWS), revnum
      character*21 btime, etime, ytime

      integer isel, rank(4,4)
c
c     rank matrix (isel cols, namb rows)
c      1 2 3 4
c      2 1 1 1
c      3 3 2 2
c      4 4 4 3

      real time(nloc), latdeg(nloc)
      real londeg(nloc), qsdata(nvar,nocc,nloc)
      real wspd, wdir, uwnd, vwnd, pi
      logical jplrank, yrchng, qcflag(nocc,nloc)
      integer qc_bit_map(nocc,nloc)
c     qcflag = F for good retrieval
c     qcflag = T for bad retrieval

      logical dirth, wdirth, gdirth
c     dirth  = T for dirth data available
c     wdirth = T for user wants dirth data
c     gdirth = T for program control to get dirth data

      real wvc_lat(MAX_WVC,MAX_ROWS)
      real wvc_lon(MAX_WVC,MAX_ROWS)
      real num_ambigs(MAX_WVC,MAX_ROWS)
      real wind_speed(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wind_dir(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wind_speed_err(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wind_dir_err(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wvc_selection(MAX_WVC,MAX_ROWS)
      real model_speed(MAX_WVC,MAX_ROWS)
      real model_dir(MAX_WVC,MAX_ROWS)
      real max_likelihood_est(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wvc_quality_flag(MAX_WVC,MAX_ROWS)
      real wind_speed_selection(MAX_WVC,MAX_ROWS)
      real wind_dir_selection(MAX_WVC,MAX_ROWS)

      real dfac_const, uint(nloc), vint(nloc)
      real ufgat(nloc), vfgat(nloc), alpha(nloc)
      real xmin, xmax, ymin, ymax
      integer rain, rainflguse, coastlnd
      integer ice, wind_not_retrieved

      integer date1, date2, time1, time2, recl
      integer jdb, jde, sb, se, sb_hold
      integer nconst, lname, parecl, iocc

c     Note, parecl=1 for SGI, parecl=4 for SUN.

      parameter (nconst=1,lname=10,parecl=4)

      character*(lname) cobsid, namcon(nconst), namvar(nvar)
      real vconst(nconst)

      integer numocc(nloc), recid(nloc)
      logical thin_row, thin_col

c     definitions of preprocessing QC bits
      integer ib_generic,ib_prepro,ib_land,ib_rain,ib_ice,
     &         ib_near_land, ib_near_rain, ib_near_ice
      PARAMETER (ib_generic=0, ib_prepro=1, ib_land=2, ib_near_land=3,
     &      ib_rain=4, ib_near_rain=5, ib_ice=6, ib_near_ice=7)

      common /sds_qscat/ wvc_quality_flag, wvc_lat, wvc_lon, wind_speed, 
     1     wind_dir, num_ambigs, wvc_selection, wind_speed_err, 
     1     wind_dir_err, model_speed, model_dir, max_likelihood_est,
     1     wind_dir_selection, wind_speed_selection, TimeTags,
     1     revnum, dirth

      data rank /1,2,3,4,2,1,1,1,3,3,2,2,4,4,4,3/
      data qsdata /nobs * 0./
      data cobsid/'ambiguous'/
      data namcon/'dfac'/
      data namvar/'u_wind','v_wind','sd(spd)','sd(dir)','mle'/

c     determine if dirth data should be processed

      gdirth = dirth .and. wdirth
      if (wdirth .and. (.not. dirth)) stop 
     &       'Dirth data not available for this rev'

      pi = 4.*atan(1.e0)
      vconst(1) = dfac_const

c     get start and end times for this rev and write to file.
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field

      read (revnum(1:8),'(i8)') rev
      btime=TimeTags(1)
      etime=TimeTags(MAX_ROWS)

      open  (unit=lunrev, file='rev_times.dat', form='formatted')
      write (lunrev,101)
      write (lunrev,102) rev, btime, etime
      close (lunrev)
c
c     check to see if end rev time crosses into next year.
c     adjust end time accordingly.

      sb = jpltim2secs(btime)
      se = jpltim2secs(etime)
      if ( se .lt. sb ) then
         read (btime(06:08),'(i3.3)') jdb 
         read (etime(06:08),'(i3.3)') jde
         print*,' rev time crosses into next year ',jdb,' ',jde
         if ( jdb .le. jde ) stop 
     &       'begin julday le end julday at year change'
         se = se + jdb * 86400
      endif

c     set reference time.  The reference time is subtracted from each
c     obs time (i.e., WVC average time) to determine the time tag stored
c     in the obs data structure.

      if (anal_julday .gt. -1 .and. anal_time .gt. -1) then
         reftime = (anal_julday-1)*86400 + anal_time*60*60
      else    ! default reftime is the central time of the rev.
         reftime = ( sb + se ) / 2
      endif

c     check to see if rev time crosses into next year, 
c     set yrchng to .true. if it does.

      yrchng = .false.
      sb_hold = 0 
      nnloc = 0 

      do i = 1, MAX_ROWS

         ytime=TimeTags(i)
         sb = jpltim2secs(ytime)
         if( sb_hold .gt. sb ) yrchng = .true.
         if( yrchng ) sb = sb + jdb * 86400
         sb_hold = sb

         thin_row = .FALSE.
         thin_row = mod(i,thin) .ne. 0

         do 800 j = 1, MAX_WVC

            thin_col = .FALSE.
            thin_col = mod(j,thin) .ne. 0

            iamb = int(num_ambigs(j,i)) 

c!!!! process all WVCs, setting appropritate bits in qc_bit_map (comment
c!!!! out if checks immediately below)

c     only process data within lat/lon window and with #ambs ge 1
            if((iamb.ge.1).and.
     &         (wvc_lat(j,i).ge.ymin).and.(wvc_lat(j,i).le.ymax))then
               if((wvc_lon(j,i).ge.xmin).and.(wvc_lon(j,i).le.xmax))then


c     coastlnd, rain, rainflguse can be used to control obs write
c     as determined by user.

                  coastlnd = 0    ! assume %wvc over land
c     coastlnd = 1.               ! %wvc over land
c     coastlnd = 0.               ! no land mass

                  rain = 0        ! assume rain and rain flag not useable
c     rain = 1.                   ! rain
c     rain = 0.                   ! no rain

                  rainflguse = 0  ! assume rain and rain flag not useable
c     rainflguse = 1              ! rain flag is not useable
c     rainflguse = 0              ! rain flag is useable

                  ice = 0

                  wind_not_retrieved = 
     $                  ibits(int(wvc_quality_flag(j,i)),9,1)

                  coastlnd = 
     $                  ibits(int(wvc_quality_flag(j,i)),7,1)

                  rainflguse = 
     $                  ibits(int(wvc_quality_flag(j,i)),12,1)
                  rain = 
     $                  ibits(int(wvc_quality_flag(j,i)),13,1)

                  ice = 
     $                  ibits(int(wvc_quality_flag(j,i)),8,1)

                  nnloc = nnloc + 1
                  numocc(nnloc) = iamb

                  if (thin_row .or. thin_col) then
                     do iocc=1,nocc
                        qc_bit_map(iocc,nnloc)=
     &                     IBSET(qc_bit_map(iocc,nnloc), ib_prepro)
                     enddo
                  endif

                  if (coastlnd) then
                     do iocc=1,nocc
                        qc_bit_map(iocc,nnloc)=
     &                     IBSET(qc_bit_map(iocc,nnloc), ib_land)
                     enddo
                  endif

                  if (rain) then
                     do iocc=1,nocc
                        qc_bit_map(iocc,nnloc)=
     &                     IBSET(qc_bit_map(iocc,nnloc), ib_rain)
                     enddo
                  endif

                  if (ice) then
                     do iocc=1,nocc
                       qc_bit_map(iocc,nnloc)=
     &                     IBSET(qc_bit_map(iocc,nnloc), ib_ice)
                     enddo
                  endif
     
                  if (wind_not_retrieved) then
                     do iocc=1,nocc
                       qc_bit_map(iocc,nnloc)=
     &                     IBSET(qc_bit_map(iocc,nnloc), ib_prepro)
                     enddo
                  endif

                  time(nnloc) = float(sb)-float(reftime)
                  latdeg(nnloc) = wvc_lat(j,i)
                  londeg(nnloc) = wvc_lon(j,i)  ! 0 -> 360
                  recid(nnloc) = 100*i + j  

                  if (wind_not_retrieved) goto 800

c     load u and v model wind components 

                  wspd = model_speed(j,i)
                  wdir = model_dir(j,i) + 180.
                  if (wdir .ge. 360.) wdir = wdir - 360.
                  wdir = wdir*pi/180.
                  call winds(uwnd, vwnd, wspd, wdir, .true.)
                  uint(nnloc) = uwnd
                  vint(nnloc) = vwnd
                  ufgat(nnloc) = 0.
                  vfgat(nnloc) = 0.
                  alpha(nnloc) = 1.

c     isel selects proper row in rank array

                  isel = 1
                  if(jplrank) isel = int(wvc_selection(j,i))

c     load u and v wind components then load errors in 
c     wind speed and direction. jamb controls ambiguity 
c     ordering for load based on isel.

                  do namb = 1, iamb

                     jamb = rank(isel,namb)
                     wspd = wind_speed(jamb,j,i)
                     wdir = wind_dir(jamb,j,i) + 180.

c     load dirth data if applicable
                     if(gdirth.and.(namb.eq.1).and.(.not.jplrank))then 
                        wspd = wind_speed_selection(j,i)
                        wdir = wind_dir_selection(j,i) + 180.0
                     endif


                     if (wdir .ge. 360.) wdir = wdir - 360.
                     wdir = wdir*pi/180.
                     call winds(uwnd, vwnd, wspd, wdir, .true.)

                     wspd = wind_speed_err(jamb,j,i)
                     wdir = wind_dir_err(jamb,j,i) + 180.
                     if (wdir .ge. 360.) wdir = wdir - 360.

                     qsdata(1,namb,nnloc) = uwnd
                     qsdata(2,namb,nnloc) = vwnd
                     qsdata(3,namb,nnloc) = wspd
                     qsdata(4,namb,nnloc) = wdir
                     qsdata(5,namb,nnloc) = max_likelihood_est(jamb,j,i)  

                  enddo

               endif   ! endif for data within spatial window
            endif    ! endif for data with one or more ambiguities

  800    enddo
      enddo

c     
c     Write header/data to ascii/binary file

      recl = nnloc * parecl 
      idate = anal_date
c       convert analysis time from hours to hhmmss format
      itime = anal_time * 10000 
      call wrtobs(lunout, hdr_fname, bin_fname,
     &     cobsid, idate, itime, nconst, nnloc, nocc, nvar,
     &     namcon, vconst, namvar, recl,
     &     numocc, recid, time, latdeg, londeg,
     &     uint, vint, ufgat, vfgat, alpha,
     &     qcflag, qc_bit_map, qsdata, 
     &     lierr)

      ierr = lierr
      if (ierr .eq. 0) then
         print*,'reformat_qscat2b: wrote obs to ',
     &        hdr_fname,bin_fname
      else
         print*,'reformat_qscat2b: wrtobs error ierr= ',ierr
         print*,'writing obs to ',hdr_fname,bin_fname
      endif
c
 101  format('  Rev         UTC Start                UTC Stop')
 102  format(2x,i4,3x,a21,3x,a21)

      return
      end     
     
c====================================================================
c
      subroutine rd_qscat2b(l2b_file)
c     
c====================================================================
     
c     original hdf reader taken from quikscat www site
c     altered by M. C. Cerniglia  9/99
c     
c     7/1/1999 R.S. Dunbar, K.L. Perry
c     Copyright 1999, California Institute of Technology
c====================================================================

c     Set Parameters

      integer MAX_ROWS,MAX_CELLS,MAX_SIG
      parameter (MAX_ROWS = 1624)
      parameter (MAX_WVC = 76)
      parameter (MAX_AMBIG = 4)

      integer obsid, nloc, nocc, nvar
      parameter (obsid = 2)
      parameter (nloc = max_rows * max_wvc)
      parameter (nocc = 4)
      parameter (nvar = 4)

      integer DFACC_RDONLY
      parameter (DFACC_RDONLY = 1)

c     Define Variables

      character product*8
      character*132 l2b_file
      character*82 l2b_algrthm_descp(8)
      character*21 TimeTags(MAX_ROWS), revnum

      integer sd_id,retn,sfstart,sfend
      integer irec1,irec2,itmp,irow,iwvc,iamb

      real wvc_row(MAX_ROWS)
      real wvc_lat(MAX_WVC,MAX_ROWS),wvc_lon(MAX_WVC,MAX_ROWS)
      real wvc_index(MAX_WVC,MAX_ROWS)
      real num_in_fore(MAX_WVC,MAX_ROWS),num_in_aft(MAX_WVC,MAX_ROWS)
      real num_out_fore(MAX_WVC,MAX_ROWS),num_out_aft(MAX_WVC,MAX_ROWS)
      real wvc_quality_flag(MAX_WVC,MAX_ROWS)
      real atten_corr(MAX_WVC,MAX_ROWS),model_speed(MAX_WVC,MAX_ROWS)
      real model_dir(MAX_WVC,MAX_ROWS),num_ambigs(MAX_WVC,MAX_ROWS)
      real wind_speed(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wind_dir(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wind_speed_err(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wind_dir_err(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real max_likelihood_est(MAX_AMBIG,MAX_WVC,MAX_ROWS)
      real wvc_selection(MAX_WVC,MAX_ROWS)
      real wind_speed_selection(MAX_WVC,MAX_ROWS)
      real wind_dir_selection(MAX_WVC,MAX_ROWS)

c     logical dirth = T if dirth data is available otherwise F
      logical dirth

      common /sds_qscat/ wvc_quality_flag, wvc_lat, wvc_lon, wind_speed, 
     1     wind_dir, num_ambigs, wvc_selection, wind_speed_err, 
     1     wind_dir_err, model_speed, model_dir, max_likelihood_est,
     1     wind_dir_selection, wind_speed_selection, TimeTags,
     1     revnum, dirth

c     Open the HDF input file and initiate the SD interface
      sd_id=sfstart(l2b_file,DFACC_RDONLY)

c     Make sure that the file is a QuikSCAT Level 2B file
      call read_attrib_byname(sd_id,'ShortName',ntype,nval,product)
      if (product.ne.'QSCATL2B') then
         print *,'The input file is not a QuikSCAT Level 2B file'
         print *,'*** Aborting program ***'
         stop
      endif

c     Get rev number
      call read_attrib_byname(sd_id,'rev_number',ntype,nval,revnum)

c     Get l2b_algorithm_descriptor
      call read_attrib_byname
     $   (sd_id,'l2b_algorithm_descriptor',ntype,nval,l2b_algrthm_descp)
      print*,'l2b_algrthm_descp = ',l2b_algrthm_descp

c     Look for DIRTH processing descriptor
      dirth = .false.
      do il2b = 1, 8
         ifindt = 0
         ifindt = index(l2b_algrthm_descp(il2b),
     $        'Enhances the direction of the selected ambiguity')
         if (ifindt .eq. 1) dirth = .true.
      enddo

c     Read the timetag info contained in the HDF VDATA
      call read_timetags(l2b_file, TimeTags)

c     Read each SDS in its entirety.  For an example of reading
c     the QuikSCAT SDS data in its entirety, please refer to
c     read_qscat2a.f.

      irow=1
      call extract_sds(sd_id,'wvc_row',irow,MAX_ROWS,wvc_row)
      call extract_sds(sd_id,'wvc_lat',irow,MAX_ROWS,wvc_lat)
      call extract_sds(sd_id,'wvc_lon',irow,MAX_ROWS,wvc_lon)
      call extract_sds(sd_id,'wvc_index',irow,MAX_ROWS,wvc_index)
      call extract_sds(sd_id,'num_in_fore',irow,MAX_ROWS,num_in_fore)
      call extract_sds(sd_id,'num_in_aft',irow,MAX_ROWS,num_in_aft)
      call extract_sds(sd_id,'num_out_fore',irow,MAX_ROWS,num_out_fore)
      call extract_sds(sd_id,'num_out_aft',irow,MAX_ROWS,num_out_aft)
      call extract_sds(sd_id,'wvc_quality_flag',irow,MAX_ROWS,
     &     wvc_quality_flag)
      call extract_sds(sd_id,'atten_corr',irow,MAX_ROWS,atten_corr)
      call extract_sds(sd_id,'model_speed',irow,MAX_ROWS,model_speed)
      call extract_sds(sd_id,'model_dir',irow,MAX_ROWS,model_dir)
      call extract_sds(sd_id,'num_ambigs',irow,MAX_ROWS,num_ambigs)
      call extract_sds(sd_id,'wind_speed',irow,MAX_ROWS,wind_speed)
      call extract_sds(sd_id,'wind_dir',irow,MAX_ROWS,wind_dir)
      call extract_sds(sd_id,'wind_speed_err',irow,MAX_ROWS,
     &     wind_speed_err)
      call extract_sds(sd_id,'wind_dir_err',irow,MAX_ROWS,wind_dir_err)
      call extract_sds(sd_id,'max_likelihood_est',irow,MAX_ROWS,
     &     max_likelihood_est)
      call extract_sds(sd_id,'wvc_selection',irow,MAX_ROWS,
     &     wvc_selection)
      call extract_sds(sd_id,'wind_speed_selection',irow,MAX_ROWS,
     &     wind_speed_selection)
      call extract_sds(sd_id,'wind_dir_selection',irow,MAX_ROWS,
     &     wind_dir_selection)
      retn=sfend(sd_id)

      return
      end
c     
c====================================================================
c     READ_ATTRIB_BYNAME:  a subroutine to read the name and
c     value(s) of a global attribute
c     referenced by its name.
c     
c     5/14/1998 R.S. Dunbar
c====================================================================

      subroutine read_attrib_byname(sd_id,in_attr_name,
     $     num_type,n_values,fvalues)
      
      integer MAX_NC_NAME
      parameter (MAX_NC_NAME=256)

      integer sd_id,num_type,n_values
      integer attr_index,count,retn,n,oldn
      integer sffattr,sfgainfo,sfrattr
      character*(*) in_attr_name
      character*(*) fvalues(*)
      character attr_name*(MAX_NC_NAME),attr_data*512
      character*(MAX_NC_NAME) values(20)
      character cr
      
c     Find the attribute assigned to in_attr_name
      attr_index = sffattr(sd_id,in_attr_name)

c     Get information about the  file attribute
      retn = sfgainfo(sd_id,attr_index,attr_name,num_type,count)

c     Read the attribute data
      retn = sfrattr(sd_id,attr_index,attr_data)

      cr = char(10)
      ival = 0
      oldn = 1
 5    continue

c     QuikSCAT attributes have atleast three lines: 
c     metadata type, array size and metadata contents
c     Use "blank spaces" to identify the end of a line

      n = index(attr_data(oldn:(count-1)),cr)

c     Read all of the metadata lines
      if (n .eq. 0) then
         ival=ival+1
         values(ival) = attr_data(oldn:(count-1))
         goto 99
      else
         ival=ival+1
         values(ival) = attr_data(oldn:(oldn+n-2))
      endif
      oldn=n+oldn
      goto 5

 99   continue
      n_values = ival - 2
      do i=1,n_values
         fvalues(i) = values(i+2)
      enddo
      return
      end

c====================================================================
c     READ_TIMETAGS:  a subroutine to read the timetag info
c     contained in the HDF VDATA
c     
c     5/1998 R.S. Dunbar
c     
c     Revisions:
c     7/1999 Code adapted to read timetags in their entirety.
c     Commenter were also added.  K.L. Perry
c====================================================================
      subroutine read_timetags(filename,timetags)

      character*80 filename
      character*21 timetags(*)
      character*60 fields
      character vdata_name*30
      integer file_id,vdata_ref,vdata_id
      integer n_records,interlace,vdata_size
      integer hopen,vsfgid,vsfatch,vsfinq,vsfread,vfsdtch,hclose

      integer DFACC_RDONLY,FULL_INTERLACE
      parameter(DFACC_RDONLY=1)
      parameter(FULL_INTERLACE=0)

c     Open the HDF file
      file_id = hopen(filename,DFACC_RDONLY,0)

c     Initialize the VS interface
      call vfstart(file_id)

c     Get the reference number for the first vdata in the file
      vdata_ref = -1
      vdata_ref = vsfgid(file_id,vdata_ref)

c     Attach to the vdata for reading if it is found, otherwise 
c     exit the program.
      if (vdata_ref.eq.0) then
         print *,'No Timetags were found in the HDF VDATA'
         print *,'*** Aborting program ***'
         stop
      endif

      vdata_id = vsfatch(file_id,vdata_ref,'r')

c     Get n_records
      retn=vsfinq(vdata_id,n_records,interlace,fields,
     &     vdata_size,vdata_name)

c     Read the timetags
      retn = vsfread(vdata_id,timetags,n_records,FULL_INTERLACE)

c     Terminate access to the vdata and to the VS interface, 
c     then close the HDF file.

      retn =  vsfdtch(vdata_id)
      call vfend(file_id)
      retn = hclose(file_id)

      return
      end

c====================================================================
c     EXTRACT_SDS:  a subroutine to read the contents of an
c     SDS from an HDF file
c     
c     5/12/1998 R.S. Dunbar
c     
c     Revisions:
c     7/1999   Code adapted to read input in bytes as well as ints 
c     and floats.  Comments were also added.  K.L. Perry
c====================================================================
      subroutine extract_sds(sd_id,in_var,irec,slab_size,out_var)

      integer MAX_BUF_SIZE
      parameter (MAX_BUF_SIZE=1000000)
      integer sd_id,sds_index,sds_id,retn
      integer rank,dim_sizes(3),data_type,nattrs,num_type
      integer edge(3),stride(3),start(3),irec,slab_size
      double precision cal,cal_err,off,off_err
      integer iprod,i

      character*(*) in_var
      character name*256
      integer sfn2index,sfselect,sfginfo,sfrdata,sfgcal,sfendacc

      integer*2 buffer(MAX_BUF_SIZE)
      byte buffer2(MAX_BUF_SIZE)
      real out_var(MAX_BUF_SIZE)

c     Search for the index of "in_var"
      sds_index = sfn2index(sd_id, in_var)

c     Select data set corresponding to the returned index
      sds_id = sfselect(sd_id,sds_index)
      retn = sfginfo(sds_id,name,rank,dim_sizes,data_type,nattrs)

      do i=1,rank
         edge(i)=dim_sizes(i)
         start(i)=0
         stride(i)=1
      enddo
      edge(rank)=slab_size
      start(rank)=irec-1

      iprod=1
      do i=1,rank
         iprod=iprod*edge(i)
      enddo

c     Get the calibration and offset values of input
      retn = sfgcal(sds_id,cal,cal_err,off,off_err,num_type)

c     Read Arrays which are not float32 or int8 or uint8
      if ((data_type.ne.5).and.(data_type.ne.20).and.
     &     (data_type.ne.21)) then

c     Read the data set into the "buffer" array
         retn=sfrdata(sds_id,start,stride,edge,buffer)

c     Calibrate the output
         do i=1,iprod
            
c     Correct for 16-bit unsigned integers
            if ((data_type.eq.23).and.(buffer(i).lt.0)) then
               out_var(i)=buffer(i)+65536.0

c     Correct for 32-bit unsigned integers
            else if ((data_type.eq.25).and.(buffer(i).lt.0)) then
               out_var(i)=buffer(i)+4294967296.0

c     No correction needed for signed or positive unsigned integers
            else
               out_var(i)=buffer(i)
            endif

            out_var(i)=out_var(i)*cal
         enddo

c     Read int8 and uint8 arrays. 
      else if ((data_type.eq.20).or.(data_type.eq.21)) then

c     Read the data set into the "buffer2" byte-array
         retn=sfrdata(sds_id,start,stride,edge,buffer2)

c     Calibrate the output
         do i=1,iprod

c     Correct for 8-bit unsigned integers
            if ((data_type.eq.21).and.(buffer(i).lt.0)) then
               out_var(i)=buffer(i)+256.0

c     No correction needed for signed or positive unsigned integers
            else
               out_var(i)=buffer(i)
            endif

            out_var(i)=buffer2(i)*cal
         enddo

      else
c     Read float32 arrays directly into the "out_var" array
         retn=sfrdata(sds_id,start,stride,edge,out_var)

c     Calibrate the output
         do i=1,iprod
            out_var(i)=out_var(i)*cal
         enddo
      endif

c     Terminate access to the array data set.
      retn = sfendacc(sds_id)
      end
c
c!#
