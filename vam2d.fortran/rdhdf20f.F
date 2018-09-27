c!#   $Id: rdhdf20f.F,v 1.5 1998/05/20 14:33:25 leidner Exp $
c!#   $Log: rdhdf20f.F,v $
c!#   Revision 1.5  1998/05/20 14:33:25  leidner
c!#   added option to rank 50km NSCAT winds by MLE
c!#   controlled by namelist parameter /rank_by_MLE/ (default is TRUE)
c!#
c!#   Revision 1.4  1998/05/05 14:57:16  leidner
c!#   added alias selection to NSCAT winds read from JPL HDF files
c!#
c!#   Revision 1.3  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.2  1997/04/11 17:41:38  leidner
c!#   added code to make compatible with VAM
c!#
c!#	Revision 1.1  1997/04/10  13:44:22  leidner
c!#	Initial revision
c!#
*
* Program:  	rdHDF20.f -- NSCAT Level 2.0 HDF Data Reader			*
* Version:	1.0								*
*										*
* Author:  	J. Joseph Benavidez						*
*		                                				*
* Release 									*
* Date:		7/28/95	v0.90 Initial Release					*
*		4/29/96	v1.0 							*
*										*
* Purpose:  	Read NSCAT HDF data by record.					*
*										*
*		First, all the global attributes are displayed.			*
*										*
* 		Second, all the Data set names are displayed.			*
*										*
*		Next, local attributes for a subset of the file's		*
*		SDS's are displayed.						*
*										*
*		The program then loops through the function			*
*		process_record20, which retrieves data from the subset of	*
*		SDS's and assigns them to the appropriate arrays. 		*
*		Data from all the records of the file are displayed. (A 	*
*		record refers to a slice of data from rank 1. The data		*
*		contained in the record depends on which SDS's are		*
*		selected.)							*
*										*
*		Finally, a summary including information regarding the number	*
*		of records, the number of SDS's received per record, and 	*
*		number of buffers allocated is printed.				*
*										*
*										*
* Comment:									*
*										*
*		NOTE! This source code uses the "include" directive		*
*			 as defined by Sun's FORTRAN Version 3.0.1 for Solaris	*
*			 to include global variables.				*
*										*
*			 Please be sure that the appropriate version		*
*			 of the "include" directive is used for your machine.	*
*										*
*         No scientific calculations are made in this program.			*
*										*
*		Increasing the buffer size (see buffsize parameter below) 	*
*		increases program performance (speed) asymptotically but 	*
*		sacrifices memory.						*
*		Allocating too much memory may cause excessive 			*
*		swapping.							*
*										*
*               If you would like to print out the data, look for the following *
*               near the end of the program                                     *
*                                                                               *
*********************************************************************************
*               IF YOU NEED TO DO ANY CALCULATIONS THEY GO HERE                 *
*               if you would like to print the rec number, time, lat and lon    *
*               uncomment out the following print statement                     *
*********************************************************************************
*                                                                               *
* Usage:		rdHDF20F (inputfile)     				*
*										*
* Modified	4/26/96								*
*		by Rose-Marie Kurian						*
*		rmk@shrimp.jpl.nasa.gov						*
*		Changes:							*
*			Added code to read the (vdata) time tags		*
*			Clean up the code and comments				*
*										*
* Copyright (c) 1996, California Institute of Technology.                       *
*                     U.S. Government Sponsorship under NASA Contract           *
*                     NAS7-1260 is acknowledged.                                *
********************************************************************************* 
       subroutine rdhdf20f(nfile,filename,itag,
     &        idate,itime,timwin,datawin_prints,alias,rank_by_MLE)

#include "rdglob20.h"
#include "vam.h"
#include "gparm.h"
#include "ns_winds.h"
#include "hdf.h"
#include "dffunc.h"
#include "netcdf.h"

********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 

*	"logical file unit"       
       	integer 	fp
*	loop counters
       	integer 	i,j
*	indices of character substrings
       	integer 	c1,c2
*	data filename
       	character*256 	filename
        character*8	arg
        integer		iargc
*	length of filename
	integer 	filenamelen
*	size of data buffer
       	integer 	buffsize
*	general character buffer
       	character*32000 	buffer
*	holds return value of hdfshell functions
       	integer 	ret
*	number of records reported from Number_Actual_Output_Records attribute
       	integer 	numrecords
*	number of SDS's used as reported from setindices20 function
        integer 	maxsdsidx
*	declare setindices20 function
        integer 	setindices20
*	program name
        character*8 	progname
*	Level number of data which this program reads
        character*3 	datalevel
*	version number of program
        character*4 	version
*	vdata variables
	integer		found
	integer*4	file_id
	integer		access
	integer*2	n_dds
	integer*4	vdata_ref
	integer*4	vdata_id
	integer*4	n_records
	integer*4	vdata_nrec
	integer*4	interlace
	integer*4	vdata_size
	character*30	fields
	character*30	vdata_name
*	time stamp is 24 characters and a maximum of 820 records
	character*24	time_buf(820)
	character*24	time_val
        character*25    modified_by, modification
        logical         lgoes
 	integer*2	ibuf(820)
        integer         row(820)
*
        integer*4 idate, itime, revnum
        logical datawin_prints
c!#    alias     logical; which ambiguities to use in the analysis
c!#~   rank_by_MLE    logical; rank ambiguities by MLE value
c!#~   rank_by_MLE.   (pos. 1 - highest MLE; remaining positions
c!#~   rank_by_MLE..  filled in order of decreasing MLE)
      logical alias(4), rank_by_MLE
       
********************************************************************************* 
*	Set the buffer size here. (See comments above on buffer size.)		*
********************************************************************************* 

       parameter(
     :          buffsize=20,
     :	        progname='rdHDF20f',
     :	        datalevel='2.0',
     :	        version='0.90'
     :          )
       
       write(*,*)''
       write(*,50) progname,datalevel,version
50     format(A,' - NSCAT Level ',A,' HDF Data Reader v',A)
       
       fp=nfopen(filename,buffsize)
       filenamelen=index(filename,' ')
       if (fp.lt.0) then
                write(*,100) filename
100             format('Could not open file ',A)
	        write(*,102) fp
102             format('Error code:  ',I)
                stop
       end if
*      write(*,*)''
       write(*,110) datalevel,filename(:filenamelen)
110    format('NSCAT Level ',A,' HDF data file: ',A)
*      write(*,*)''

********************************************************************************* 
*	Run through and display global attributes				*
********************************************************************************* 

*       write(*,'(A)')'Global attributes:'
        ret=nfattr(fp, '',buffer)
        if(ret.lt.0) then
                write(*,*) 'There was a problem getting the global attributes.
     :'
                write(*,200) ret
200             format('Error code:  ',I)
        else

********************************************************************************* 
*		Before displaying the names, grab the attribute			*
*		that gives the number of records in the file.			*
********************************************************************************* 

                c1=INDEX(buffer,'Num_Actual_Output_Records')
                c1=c1+INDEX(buffer(c1:),'=')
                c2=c1+INDEX(buffer(c1:),char(10))-2
                read(buffer(c1:c2),'(I)') numrecords

********************************************************************************* 
*             ...and  grab the attribute that gives the rev number              *
*********************************************************************************
                c1=INDEX(buffer,'First_Rev_Number')
                c1=c1+INDEX(buffer(c1:),'=')
                c2=c1+INDEX(buffer(c1:),char(10))-2
                read(buffer(c1:c2),'(I)') revnum

********************************************************************************* 
*             ...and see if attribute "Modified_by" is present
*********************************************************************************
                c1=INDEX(buffer,'Modified_by')

                if (c1.eq.0) then   ! file not modified
                   lgoes = .false.
                   goto 205
                endif

                c1=c1+INDEX(buffer(c1:),'=')
                c2=c1+INDEX(buffer(c1:),char(10))-2
                read(buffer(c1:c2),'(A)') modified_by

                c1=INDEX(buffer,'Modification')
                c1=c1+INDEX(buffer(c1:),'=')
                c2=c1+INDEX(buffer(c1:),char(10))-2
                read(buffer(c1:c2),'(A)') modification
                lgoes = .true.

                write(*,202) modified_by, modification
 202            format('HDF file modified by ',a,'.  Modification:',a)
 205            continue

********************************************************************************* 
*		Display the attributes						*
*		The following loop tokenizes (separates) each attribute		*
********************************************************************************* 

                c1=1
                do 210,i=1,ret
                        c2=c1+INDEX(buffer(c1:),char(10))-2
*                       write(*,'(T9,A)') buffer(c1:c2)
                        c1=c2+2
210                continue
        end if
	
       maxsdsidx=setindices20(lgoes)

********************************************************************************* 
*	Read SDS info for all the SDS's in the record				*
********************************************************************************* 

       call readrecinfo20(fp,lgoes)	

********************************************************************************* 
*	Read the vdata containing the time tags and store time in		*
*	time_buf array.  These can later be displayed if desired		*
********************************************************************************* 

	vdata_ref = -1

	file_id = hopen(filename, access, n_dds)
	ret = vfstart(file_id)

	vdata_ref = vsffnd(file_id, "SwathIndex")
	if (vdata_ref .NE. -1) then
	   vdata_id = vsfatch(file_id, vdata_ref, "r")
	   if (vdata_id .NE. -1) then
              ret = vsfgnam(vdata_id, vdata_name) 
	      ret = vsfgint(vdata_id, interlace) 
	      vdata_nrec = 820 
	         ret = vsfread(vdata_id, ibuf, vdata_nrec, interlace)
* this next three lines will print Swath data indeces
*	      do 218, i=1, vdata_nrec 	
*	         write (*, '(T9,I)') ibuf(i)
*218           continue
	   else
	      write(*,*) 'Error no vdata id was found'
	   endif
	else
	      write(*,*) 'Error no SwathIndex vdata was found'
        endif
         

********************************************************************************* 
* extracting the time tags vdata
********************************************************************************* 

	vdata_ref = -1
	found = 0

	vdata_ref = vsffnd(file_id, "NSCAT L2")
	if (vdata_ref .NE. -1) then
	   vdata_id = vsfatch(file_id, vdata_ref, "r")
	   if (vdata_id .NE. -1) then
              ret = vsfgnam(vdata_id, vdata_name) 
	      if (ret .NE. -1) then
	         ret = vsfsfld(vdata_id, "Mean_Time")
	         if (ret .NE. -1) then
	            ret = vsfgint(vdata_id, interlace) 
	            if (ret .NE. -1) then
	      	       vdata_nrec = 1
* uncomment these lines to read and print the time tags
 	               do 219, i=1, numrecords
 	            ret=vsfread(vdata_id,time_val,vdata_nrec,interlace)
                    read(time_val,'(24A)') time_buf(i)
*	            write(*, '(24A)') time_buf(i)
 219                     continue
		    else
		       write(*,*) 'Error finding vdata interlace'
		    endif 
		 else
		    write(*,*) 'Error finding vdata Mean_Time field'
                 endif
              else
	         write(*,*) 'Error finding vdata Mean_Time field'
	      endif
	   else
	      write(*,*) 'Error no time tags vdata id was found'
	   endif
	else
	      write(*,*) 'Error no NSCAT L2 (time tags) vdata was found'
        endif
         

************************************************************************* 
*	Run through and display all Data set names			*
************************************************************************* 

*       write(*,'(A)') 'Data set names:'
        ret=nfnames(fp,buffer)
        if(ret.lt.0) then
                write(*,*) 'There was a problem getting the data set na
     :mes.'
                write(*,250) ret
250             format('Error code:  ',I)
        else

********************************************************************************* 
*               The following loop tokenizes (separates) each nameS		*
********************************************************************************* 
                c1=1
                do 300,i=1,ret
                        c2=c1+INDEX(buffer(c1:),char(10))-2
*                       write(*,'(T9,A)') buffer(c1:c2)
                        c1=c2+2
300                continue
        end if

********************************************************************************* 
*	Run through and display local attributes				*
********************************************************************************* 

        do 500 i=1,maxsdsidx
*               write(*,*)''
                ret=nfattr(fp,allsdsnames(i),buffer)
                if(ret.lt.0) then
                        write(*,320) allsdsnames(i)(:namelen(i))
320                     format('There was a problem getting the local a
     :ttributes for',A)
                        write(*,350) ret
350                     format('Error code:  ',I)
                else
*                       write(*,375) allsdsnames(i)(:namelen(i))
375                     format('Local attributes for ',A,':')

********************************************************************************* 
*	The following loop tokenizes (separates) each attribute			*
********************************************************************************* 

                        c1=1
                        do 400,j=1,ret
                                 c2=c1+INDEX(buffer(c1:),char(10))-2
*                                write(*,380) buffer(c1:c2)
380                              format(T9,A)
                                 c1=c2+2
400                     continue
                 end if                        
500     continue
*       write(*,*) ''

********************************************************************************* 
*	Process all the records (records are 0-based)				*
********************************************************************************* 
*
*       prepare SwathIndex data for passing to process_record20
        n=0
        do i=1,820
           if (ibuf(i).gt.0) then
              n=n+1
              row(n)=i
           endif
        enddo

*       write(*,'(A)')'Processing data...'
        nsw_revs(nfile) = revnum

	do 600 i=0,numrecords-1
           call process_record20(fp, i, time_buf(i+1),row(i+1),
     &                        itag,idate,itime,timwin,
     &                        revnum,datawin_prints,lgoes,alias,
     &                        rank_by_MLE)
600	continue
	
********************************************************************************* 
*	relinquish control of the Vdata's						*
********************************************************************************* 

	ret = vfend(file_id)
	ret = hclose(file_id)

********************************************************************************* 
*	relinquish control of the SDS's						*
********************************************************************************* 

        do 650 i=1,maxsdsidx
                ret=nfclosesd(fp,allsdsnames(i))
                if (ret.lt.0) then
                        write(*,610) allsdsnames(i)
610                     format('There was a problem closing SDS ',A,'.'
     :)
                        write(*,620) ret
620                     format('Return code: ',I)
                end if              
650     continue

********************************************************************************* 
*	close the file								*
********************************************************************************* 

        ret=nfclose(fp)
        if(ret.lt.0) then
                write(*,670) filename(:filenamelen)
670             format('There was a problem closing file ',A,'.')             
                write(*,680) ret
680             format('Return code: ',I)
        end if              

********************************************************************************* 
*	Give quick summary							*
********************************************************************************* 

        write(*,'(A)')'Processing complete.'
        write(*,*)''
        write(*,700)datalevel,filename(:filenamelen)
700     format('NSCAT Level ',A,' HDF file: ',T33,A)
        write(*,800) numrecords
800     format('Records processed:',T33,I)
        write(*,'(A)')'Number of SDS''s retrieved'

********************************************************************************* 
* NOTE! THE FOLLOWING NUMBER WILL BE WRONG IF UNUSED SDS'S WERE NOT		*
*       PROPERLY COMMENTED OUT OR DELETED (SEE README.for FOR DETAILS)		*
********************************************************************************* 

        write(*,900) maxsdsidx
900     format(T3,'in each record:',T33,I,' SDS''s')
        write(*,1000) buffsize
1000    format('Buffer size: ',T33,I,' records')
        END



