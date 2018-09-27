c
c====================================================================
      program readqscat2a
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
   
c====================================================================
c     Filename: readqscat2a.f
c
c     Usage: 
c
c       To run this program, use the following command:
c     
c       your_computer% readqscat2a <filename>
c
c       where "<filename>" is the name of the QuikSCAT Level 2A
c       input file
c
c     Description:
c
c       This file contains 3 subroutines in order to read the 
c       QuikSCAT Level 2A data in Hierarchical Data Format (HDF).  
c       The subroutines are as follows.
c
c       1. read_attrib_byname():  a subroutine to read the name 
c                                 and value(s) of a global attribute
c                                 referenced by its name.
c       
c       2. read_timetags():  a subroutine to read the timetag info
c                            contained in the HDF VDATA
c
c       3. extract_sds():  a subroutine to read the contents of an
c                          SDS from an HDF file
c
c     NOTES:
c     1. Please refer all questions concerning this program and
c        QuikSCAT data obtained from the JPL PO.DAAC to
c        qscat@podaac.jpl.nasa.gov.
c
c     2. The HDF library must be installed before this program will 
c        work properly.  The HDF library and further information 
c        about HDF may be obtained from the National Center for 
c        Supercomputing Applications (NCSA) at http://hdf.ncsa.uiuc.edu.
c
c     3. The L2A data are read in slabs.  If you would like to read 
c        these data in their entirety, change the value of "slab_size"
c        to 1702.
c
c  7/1/1999 R.S. Dunbar, K.L. Perry
c  Copyright 1999, California Institute of Technology
c
c Modified by Mark Cerniglia, AER, Inc. 11/99
c====================================================================
c $Id: readqscat2a.f,v 1.2 1999/12/22 16:48:16 mcc Exp $
c $Log: readqscat2a.f,v $
c Revision 1.2  1999/12/22 16:48:16  mcc
c added documentation
c

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
      character*21 TimeTags(1702)

      integer sd_id,retn,sfstart,sfend
      integer irec1,irec2,itmp,irow,isig

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

c     Read the input filename.
      call GETARG(1,l2a_file)
      if (l2a_file .eq. ' ') then
         print *,'Usage: readqscat2a <Level 2A file>'
         stop
      endif

c     Open output file

      open(unit=10,file='qscat2a.out',form='formatted')
      write(6,*)
      write(6,*) 'FILENAME: ',l2a_file

      write(10,*)
      write(10,*) 'FILENAME: ',l2a_file


c     Open the HDF input file and initiate the SD interface
      sd_id=sfstart(l2a_file,DFACC_RDONLY)

c     Make sure that the file is a QuikSCAT Level 2A file
      call read_attrib_byname(sd_id,'ShortName',ntype,nval,product)
      if (product.ne.'QSCATL2A') then
         print *,'The input file is not a QuikSCAT Level 2A file'
         print *,'*** Aborting program ***'
         stop
      endif

c     Read the timetag info contained in the HDF VDATA
      call read_timetags(l2a_file, TimeTags)

c     Select the wind vector cell rows to be read
      write(6,12)
 12   format('Enter the first and last record numbers [1-1702]: '$)

      read(*,*) irec1, irec2
      write(10,*) '  '
      write(10,*) irec1, irec2

      write(6,*) '  '
      write(6,*) irec1, irec2

c     Make sure that "first comes before last"
      if (irec1.gt.irec2) then
         itmp=irec1
         irec1=irec2
         irec2=itmp
      endif

c     Check to see if selected rows are within limits
      if ((irec1.lt.1).or.(irec2.gt.1702)) then
         write(10,*) 'Number of rows must be between 1 and 1702'
         print *,'*** Aborting program ***'
         stop
      endif

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


c     Print results to screen
      do irow=irec1,irec2
         print*,'irow = ',irow    
         write(10,*) ' '
         write(10,*) 'TIME: ', TimeTags(irow)
         write(10,100) row_number(irow)

c         write(6,*) ' '
c         write(6,*) 'TIME: ', TimeTags(irow)
c         write(6,100) row_number(irow)

c         write(6,101) num_sigma0(irow)
c         write(6,102) num_sigma0_per_cell

 100     format('WVC ROW: ',f5.0)
         write(10,101) num_sigma0(irow)
 101     format('Number of Sigma0: ',f5.0)
         write(10,102) (num_sigma0_per_cell(icell,irow),
     &        icell=1,MAX_CELLS)
 102     format(3(20f4.0/),1(16f4.0/))

         write(10,105)
 105     format(' WVC    Lat   Lon    Azi  IncAng Sigma0  Atten  ',
     &        'Kp_alpha    Kp_beta    Kp_gamma  Qual  Mode Surf')
         do isig = 1,num_sigma0(irow)
            write(10,110) cell_index(isig,irow),cell_lat(isig,irow),
     &        cell_lon(isig,irow),cell_azimuth(isig,irow),
     &        cell_incidence(isig,irow),sigma0(isig,irow),
     &        sigma0_attn_map(isig,irow),kp_alpha(isig,irow),
     &        kp_beta(isig,irow),kp_gamma(isig,irow),
     &        sigma0_qual_flag(isig,irow),sigma0_mode_flag(isig,irow),
     &        surface_flag(isig,irow)

 110        format(f4.0,x,5(f6.2,x),f6.3,x,f9.6,2x,2(e10.4,2x),
     &           f3.0,x,f6.0,x,f5.0)
         enddo
      enddo
      close(10)

      retn=sfend(sd_id)
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
