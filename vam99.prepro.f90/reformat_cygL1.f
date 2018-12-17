c
c!# CSU IDENTIFICATION : reformat_cygL1

c!# PURPOSE : 

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!# REQUIREMENTS : 

c!# CONSTRAINTS : 

c!# LANGUAGE : Fortran 77

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :
c====================================================================
c
      program reformat_cygL1
c
c====================================================================
c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# REFERENCES : None

c!# LIMITATIONS : 

c!# CHANGE LOG : 

c!# LOCAL DATA ELEMENTS :

c     Define Variables

      implicit none

      integer lunin, ierr, iostat
      parameter (lunin = 8)
      real satid_const, modfn_const, kpm2_const
      real xmin, xmax, ymin, ymax

      character*132 cygL1_file, fname1, hdr_fname, bin_fname

      namelist /cygfn/ l2a_file, hdr_fname, bin_fname, 
     & satid_const, modfn_const, kpm2_const, xmin, xmax, ymin, ymax

c!# LOCAL DATA STRUCTURES : None

c!# DATA FILES : CYGNSS Level 1 (netCDF format) only

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c
c  programmer:
c  S. Mark Leidner - AER Inc.
c  created: Nov. 2018.
c 
c     Retrieve Input Arguments

      call getarg(1,fname1)

      open(unit=lunin,file=fname1,status='old',err=900)
      rewind lunin

c     Execute as directed by namelist.

      read (lunin,cygfn,iostat=ierr) 
      if (ierr .ne. 0) then
         print*, 'Error reading namelist: iostat=',ierr
         goto 999
      endif

      print*,' ' 
      print*,'Quikscat HDF Filename =  ',l2a_file
      print*,'longitude min max range = ',xmin,' ',xmax
      print*,'latitude min max range = ',ymin,' ',ymax
      print*,' '

c     read in qscat data in hdf format

      call rd_qscat2a(l2a_file)

c     reformat data and write out qscat data in present form

      call wt_qscat2a (hdr_fname, bin_fname, satid_const, modfn_const, 
     &     kpm2_const, xmin, xmax, ymin, ymax)
      go to 999

 900  continue
      print*, 'reformat_qscat2a: unable to access namelist file.'
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
c   convert jpl time stamp to seconds from YYYY.

c   JPL TimeTags is in the form of
c   YYYY-DDDThh:mm:ss.sss - 21 byte character field
c   YYYY is the year, DDD is day of year (Julian),
c   hh is hour (UTC), mm is minutes and ss.sss is seconds

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
      subroutine wt_qscat2a (hdr_fname, bin_fname, satid_const, 
     &     modfn_const, kpm2_const, xmin, xmax, ymin, ymax)
c     
c====================================================================
c
c   notes: 

c   TimeTags(MAX_ROWS) is in the form of
c   YYYY-DDDThh:mm:ss.sss - 21 byte character field
c   YYYY is the year, DDD is day of year (Julian),
c   hh is hour (UTC), mm is minutes and ss.sss is seconds

      implicit none
    
      integer SLAB_SIZE,MAX_ROWS,MAX_CELLS,MAX_SIG
      parameter (SLAB_SIZE = 1702)
      parameter (MAX_ROWS = 1702)
      parameter (MAX_CELLS = 76)

c     see NOTES from reader.
cc    parameter (MAX_SIG = 3240)
      parameter (MAX_SIG = 810)

      integer lunout, lunrev, jpltim2secs, avetim
      parameter (lunout = 9, lunrev = 10)

      integer i, j, nnloc
      integer ierr, lierr, rev
      integer nloc, nocc, nlocc, nvar, nobs

      parameter (nloc = MAX_SIG * MAX_ROWS, nocc=1)
      parameter (nlocc = nloc * nocc)
      parameter (nvar = 7, nobs = nvar * nocc * nloc)
      character*132 hdr_fname, bin_fname
      character*21 TimeTags(MAX_ROWS), revnum
      character*21 btime, etime, ytime

      real time(nloc), latdeg(nloc), pi, d2r
      real londeg(nloc), qsdata(nvar,nocc,nloc)
      real xqsdata(nvar,nocc,nloc), xlatdeg(nloc),xlondeg(nloc)
      real xtime(nloc), xmin, xmax, ymin, ymax

      logical yrchng

c     qcflag = F for good retrieval
c     qcflag = T for bad retrieval
      logical qcflag(nocc,nloc)

      real row_number(MAX_ROWS),num_sigma0(MAX_ROWS)
      real num_sigma0_per_cell(MAX_CELLS,MAX_ROWS)
      real cell_lat(MAX_SIG,MAX_ROWS),cell_lon(MAX_SIG,MAX_ROWS)
      real cell_azimuth(MAX_SIG,MAX_ROWS)
      real cell_incidence(MAX_SIG,MAX_ROWS)
      real sigma0(MAX_SIG,MAX_ROWS),sigma0_attn_amsr(MAX_SIG,MAX_ROWS)
      real sigma0_attn_map(MAX_SIG,MAX_ROWS)
      real kp_alpha(MAX_SIG,MAX_ROWS),kp_beta(MAX_SIG,MAX_ROWS)
      real kp_gamma(MAX_SIG,MAX_ROWS)
      real sigma0_qual_flag(MAX_SIG,MAX_ROWS)
      real sigma0_mode_flag(MAX_SIG,MAX_ROWS)
      real surface_flag(MAX_SIG,MAX_ROWS),cell_index(MAX_SIG,MAX_ROWS)
      real polarization(MAX_SIG,MAX_ROWS)

      integer jdb, jde, sb, se, sb_hold, recl
      integer nconst, lname, parecl
      integer numocc(nloc), recid(nloc)

c     Note, parcel=1 for SGI, parcel=4 for SUN.

      parameter (nconst=3,lname=10,parecl=1)
      real uint(nloc), vint(nloc), vconst(nconst)
      real ufgat(nloc), vfgat(nloc), alpha(nloc)
      real satid_const, modfn_const, kpm2_const
      character*(lname) cobsid, namcon(nconst), namvar(nvar)

      common /sds_qscat_l2a/ cell_azimuth, cell_incidence, sigma0,
     &   kp_alpha, kp_beta, kp_gamma, sigma0_mode_flag, cell_lat,
     &   cell_lon, sigma0_attn_map, surface_flag, sigma0_qual_flag,
     &   TimeTags, revnum

      data qsdata /nobs * 0./
      data uint /nloc * 0./
      data numocc /nloc * 1/
      data vint /nloc * 0./
      data ufgat /nloc * 0./
      data alpha /nloc * 1./
      data vfgat /nloc * 0./
      data qcflag /nlocc * .false./   ! all good obs
      data cobsid/'sigma0'/
      data namcon/'satid', 'modfn', 'kpm2'/

      data namvar/'hv-polar','incidence','azimuth','sigma0',
     &  'kpa','kpb','kpc'/

      pi = 4.*atan(1.e0)
      d2r = pi/180.0

      vconst(1) = satid_const
      vconst(2) = modfn_const
      vconst(3) = kpm2_const

c  get start and end times for this rev and write to file.
c  YYYY-DDDThh:mm:ss.sss - 21 byte character field

      read (revnum(1:8),'(i8)') rev
      btime=TimeTags(1)
      etime=TimeTags(MAX_ROWS)

      open  (unit=lunrev, file='rev_times.dat', form='formatted')
      write (lunrev,101)
      write (lunrev,102) rev, btime, etime
      close (lunrev)
c
c  check if end rev time crosses into next year.
c  adjust end time accordingly.

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
      avetim = ( sb + se ) / 2

c  check if rev time crosses into next year, 
c  set yrchng to .true. if it does.

      yrchng = .false.
      sb_hold = 0 
      nnloc = 0

c     loop over all wvc cells

      do  i = 1, MAX_ROWS

         ytime=TimeTags(i)
         sb = jpltim2secs(ytime)
         if( sb_hold .gt. sb ) yrchng = .true.
         if( yrchng ) sb = sb + jdb * 86400
         sb_hold = sb

         do  j = 1, MAX_SIG

c     only process data with window described by namelist

            if((cell_lat(j,i).ge.ymin).and.(cell_lat(j,i).le.ymax))then
               if((cell_lon(j,i).ge.xmin).and.
     &              (cell_lon(j,i).le.xmax))then

                  nnloc = nnloc + 1
                  time(nnloc) = float(sb)-float(avetim)  
                  recid(nnloc) = 100*i + j  
                  latdeg(nnloc) = cell_lat(j,i)
                  londeg(nnloc) = cell_lon(j,i)

c     perform quality control on wvc cell(j,i)

c     set wvc cells at polar latitudes to bad retrieval
                  if((latdeg(nnloc).lt.-70.).or.
     &                 (latdeg(nnloc).gt.70.)) then
                     qcflag(nocc,nnloc) = .true.

c     check kpa, kpb, and kpc
                  elseif(kp_alpha(j,i) .eq. 0.) then
                     qcflag(nocc,nnloc) = .true.
                  elseif(kp_beta(j,i) .eq. 0.) then
                     qcflag(nocc,nnloc) = .true.
                  elseif(kp_gamma(j,i) .eq. 0.) then
                     qcflag(nocc,nnloc) = .true.

c     check sigma0_qual_flag
                  elseif(sigma0_qual_flag(j,i) .ne. 0.) then
                     qcflag(nocc,nnloc) = .true.

c     check surface_flag
                  elseif(surface_flag(j,i) .ne. 0.) then
                     qcflag(nocc,nnloc) = .true.

c     check sigma0_mode_flag; bits 0->1 and 4->5
                  elseif(ibits(int(sigma0_mode_flag(j,i)),0,1).ne.0)then
                     qcflag(nocc,nnloc) = .true.
                  elseif(ibits(int(sigma0_mode_flag(j,i)),1,1).ne.0)then
                     qcflag(nocc,nnloc) = .true.
                  elseif(ibits(int(sigma0_mode_flag(j,i)),4,1).ne.0)then
                     qcflag(nocc,nnloc) = .true.
                  elseif(ibits(int(sigma0_mode_flag(j,i)),5,1).ne.0)then
                     qcflag(nocc,nnloc) = .true.
                  endif

c     determine polarization from sigma0_mode_flag,
c     = 0. -> horz. pol. ; = 1. -> vert. pol.
                  polarization(j,i) = 
     &                 ibits(int(sigma0_mode_flag(j,i)),2,1)

c     correct for atmospheric effect, sigma0 (surface) = 
c     sigma0 (toa) + attenuation correction * secant of incidence angle;
c     convert from db to linear space

c     db space sigma0
                  sigma0(j,i) = sigma0(j,i) + 
     &                 sigma0_attn_map(j,i)/cos(cell_incidence(j,i)*d2r)

c     linear space sigma0
                  sigma0(j,i) = 10.**(sigma0(j,i)/10.) *
     &                 (-1.)**(ibits(int(sigma0_qual_flag(j,i)),2,1)) 

                  qsdata(1,nocc,nnloc) = polarization(j,i)
                  qsdata(2,nocc,nnloc) = cell_incidence(j,i) * d2r
                  qsdata(3,nocc,nnloc) = cell_azimuth(j,i) * d2r
                  qsdata(4,nocc,nnloc) = sigma0(j,i)
                  qsdata(5,nocc,nnloc) = kp_alpha(j,i)
                  qsdata(6,nocc,nnloc) = kp_beta(j,i)
                  qsdata(7,nocc,nnloc) = kp_gamma(j,i)

               endif
            endif

         enddo
      enddo

c     
c     Write header/data to ascii/binary file

      recl = nnloc * parecl 
      call wrtobs(lunout, hdr_fname, bin_fname,
     *     cobsid, nconst, nnloc, nocc, nvar,
     *     namcon, vconst, namvar, recl,
     *     numocc, recid, time, latdeg, londeg,
     *     uint, vint, ufgat, vfgat, alpha,
     *     qcflag, qsdata, 
     *     lierr)

      ierr = lierr
      if (ierr .eq. 0) then
         print *,'reformat_qscat2a: wrote obs to ',
     *        hdr_fname,bin_fname
      else
         print *,'reformat_qscat2a: wrtobs error ierr= ',ierr
         print *,'writing obs to ',hdr_fname,bin_fname
      endif
c

 101  format('  Rev         UTC Start                UTC Stop')
 102  format(2x,i4,3x,a21,3x,a21)

      return
      end     

c====================================================================
      subroutine rd_qscat2a(l2a_file)
c====================================================================

c NOTES:
c   The L2A data which have been distributed thus far are the 
c   egg data (MAX_SIG=810).  The composite data will have
c   MAX_SIG=3240.  Even though 4 times as much data are read,
c   3240 works with the "eggs in slabs", because the read starts 
c   in the correct place and only "num_sigma0(1)" values are printed.
c   3240 does not with the "eggs in their entirety", because the 
c   read will not be in the correct place for the next row.

c   If you need to read the data in its entirety, please use the
c   read_qscat_info.f program to make sure that the MAX_SIG parameter
c   is correct in your data reader.
   
c  7/1/1999 R.S. Dunbar, K.L. Perry
c  Copyright 1999, California Institute of Technology

c   original hdf reader taken from quikscat www site
c   altered by M. C. Cerniglia  11/99

c====================================================================

c     Set Parameters

      integer SLAB_SIZE,MAX_ROWS,MAX_CELLS,MAX_SIG
      parameter (SLAB_SIZE = 1702)
      parameter (MAX_ROWS = 1702)
      parameter (MAX_CELLS = 76)

c    see NOTES above.
cc      parameter (MAX_SIG = 3240)
      parameter (MAX_SIG = 810)

      integer DFACC_RDONLY
      parameter (DFACC_RDONLY = 1)

c     Define Variables

      character l2a_file*128,product*8
      character*21 TimeTags(MAX_ROWS), revnum

      integer sd_id,retn,sfstart,sfend,irow

      real row_number(MAX_ROWS),num_sigma0(MAX_ROWS)
      real num_sigma0_per_cell(MAX_CELLS,MAX_ROWS)
      real cell_lat(MAX_SIG,MAX_ROWS),cell_lon(MAX_SIG,MAX_ROWS)
      real cell_azimuth(MAX_SIG,MAX_ROWS)
      real cell_incidence(MAX_SIG,MAX_ROWS)
      real sigma0(MAX_SIG,MAX_ROWS),sigma0_attn_amsr(MAX_SIG,MAX_ROWS)
      real sigma0_attn_map(MAX_SIG,MAX_ROWS)
      real kp_alpha(MAX_SIG,MAX_ROWS),kp_beta(MAX_SIG,MAX_ROWS)
      real kp_gamma(MAX_SIG,MAX_ROWS)
      real sigma0_qual_flag(MAX_SIG,MAX_ROWS)
      real sigma0_mode_flag(MAX_SIG,MAX_ROWS)
      real surface_flag(MAX_SIG,MAX_ROWS),cell_index(MAX_SIG,MAX_ROWS)

      common /sds_qscat_l2a/ cell_azimuth, cell_incidence, sigma0,
     &   kp_alpha, kp_beta, kp_gamma, sigma0_mode_flag, cell_lat,
     &   cell_lon, sigma0_attn_map, surface_flag, sigma0_qual_flag,
     &   TimeTags, revnum

c     Open the HDF input file and initiate the SD interface
      sd_id=sfstart(l2a_file,DFACC_RDONLY)

c     Make sure that the file is a QuikSCAT Level 2A file
      call read_attrib_byname(sd_id,'ShortName',ntype,nval,product)
      if (product.ne.'QSCATL2A') then
         print *,'The input file is not a QuikSCAT Level 2A file'
         print *,'*** Aborting program ***'
         stop
      endif

c     Get rev number
      call read_attrib_byname(sd_id,'rev_number',ntype,nval,revnum)

c     Read the timetag info contained in the HDF VDATA
      call read_timetags(l2a_file, TimeTags)

c     Select all wind vector cell rows for read

c     Read each SDS using slabs.  For an example of reading
c     the QuikSCAT SDS data in its entirety, please refer to
c     read_qscat2b.f.

      irow = 1
      call extract_sds(sd_id,'row_number',irow,SLAB_SIZE,
     &     row_number)
      call extract_sds(sd_id,'num_sigma0',irow,SLAB_SIZE,
     &     num_sigma0)
      call extract_sds(sd_id,'num_sigma0_per_cell',irow,
     &     SLAB_SIZE,num_sigma0_per_cell)
      call extract_sds(sd_id,'cell_lat',irow,SLAB_SIZE,cell_lat)
      call extract_sds(sd_id,'cell_lon',irow,SLAB_SIZE,cell_lon)
      call extract_sds(sd_id,'cell_azimuth',irow,SLAB_SIZE,
     &     cell_azimuth)
      call extract_sds(sd_id,'cell_incidence',irow,SLAB_SIZE,
     &     cell_incidence)
      call extract_sds(sd_id,'sigma0',irow,SLAB_SIZE,sigma0)
      call extract_sds(sd_id,'sigma0_attn_amsr',irow,SLAB_SIZE,
     &     sigma0_attn_amsr)
      call extract_sds(sd_id,'sigma0_attn_map',irow,SLAB_SIZE,
     &     sigma0_attn_map)
      call extract_sds(sd_id,'kp_alpha',irow,SLAB_SIZE,kp_alpha)
      call extract_sds(sd_id,'kp_beta',irow,SLAB_SIZE,kp_beta)
      call extract_sds(sd_id,'kp_gamma',irow,SLAB_SIZE,kp_gamma)
      call extract_sds(sd_id,'sigma0_qual_flag',irow,SLAB_SIZE,
     &     sigma0_qual_flag)
      call extract_sds(sd_id,'sigma0_mode_flag',irow,SLAB_SIZE,
     &     sigma0_mode_flag)
      call extract_sds(sd_id,'surface_flag',irow,SLAB_SIZE,
     &     surface_flag)
      call extract_sds(sd_id,'cell_index',irow,SLAB_SIZE,
     &     cell_index)


      retn=sfend(sd_id)
      return
      end

c====================================================================
c    READ_ATTRIB_BYNAME:  a subroutine to read the name and
c                         value(s) of a global attribute
c                         referenced by its name.
c    
c    5/14/1998 R.S. Dunbar
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

c     QuikSCAT attributes have at least three lines: 
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
c    READ_TIMETAGS:  a subroutine to read the timetag info
c                    contained in the HDF VDATA
c    
c    5/1998 R.S. Dunbar
c
c    Revisions:
c    7/1999 Code adapted to read timetags in their entirety.
c           Commenter were also added.  K.L. Perry
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
c    EXTRACT_SDS:  a subroutine to read the contents of an
c                  SDS from an HDF file
c    
c    5/12/1998 R.S. Dunbar
c
c    Revisions:
c    7/1999   Code adapted to read input in bytes as well as ints 
c             and floats.  Comments were also added.  K.L. Perry
c====================================================================
      subroutine extract_sds(sd_id,in_var,irec,slab_size,out_var)

      integer MAX_BUF_SIZE
c      parameter (MAX_BUF_SIZE=1000000)
      parameter  (MAX_BUF_SIZE=10000000)
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
