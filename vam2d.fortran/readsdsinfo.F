c!#   $Id: readsdsinfo.F,v 1.1 1997/02/21 23:45:10 leidner Exp $
c!#   $Log: readsdsinfo.F,v $
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#


********************************************************************************* 
* 										*
* subroutine readsdsinfo(fp,sdsidx,sdsname)					*
*										*
* 	purpose: Reads important information about an SDS.			*
*		It updates global variables rank, dimsizes,			*
*		data_type, bytes_per_cell, bytes_per_line,			*
*		and allsdsnames given a particular sdsidx.			*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*		sdsidx - the global index corresponding to the			*
*			particular sds.						*
*		sdsname - the name of the SDS to search for in the		*
*			data file.						*
*	output:									*
*		rank - the rank of the SDS					*
*		dimsizes - the boundaries for each rank				*
*		data_type - type of data as defined in hdf.h or hdfshell.h	*
*		bytes_per_cell - number of bytes used for storing each datum	*
*		bytes_per_line - number of bytes used to store all data		*
*										*
*	comments: This function can also be used to search for			*
*		additional information. For instance, scale_factor		*
*		is retrieved from the SDS's local attributes if it		*
*		exists. Otherwise scale_factore defaults to 1.0			*
*										*
********************************************************************************* 

        subroutine readsdsinfo(fp,sdsidx,sdsname)
		
#include "rdglob17.h"

********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 

*	logical file unit	
        integer fp
*	global index for the SDS
        integer sdsidx
*	name of the SDS
        character*(*) sdsname
*	character buffer
        character*32000 buffer
*	hold return values of hdfshell functions
        integer ret
*	indices of character substrings
        integer c1,c2
	
********************************************************************************* 
*	query the SDS file for SDS information					*
********************************************************************************* 

       	ret=nfquery(fp,sdsname,rank(sdsidx),dimsizes(1,sdsidx),
     :	        data_type(sdsidx),bytes_per_cell(sdsidx),
     :	        bytes_per_line(sdsidx))

********************************************************************************* 
*	Check to see if there was an error.					*
********************************************************************************* 

       	if (ret.lt.0) then
                write(*,100) sdsname
100             format('Could not read the SDS information for ',A)
                write(*,150) ret
150             format('Error code: ',I)
                stop
        end if

********************************************************************************* 
*	Run through the local attributes of the SDS				*
********************************************************************************* 

       	ret=nfattr(fp,sdsname,buffer)
        if(ret.lt.0) then
                write(*,175) sdsname
175             format('There was an error when trying to get local att
     :ributes for ',A)
	        stop
        else

********************************************************************************* 
*		Search for the scale_factor attribute				*
********************************************************************************* 

                c1=INDEX(buffer,'scale_factor')

********************************************************************************* 
*		If it exists, assign that scale_factor to the			*
*		scale_factor array. Otherwise, default to 1.0			*
********************************************************************************* 

                if(c1.ne.0) then
                        c1=c1+INDEX(buffer(c1:),'=')
                        c2=c1+INDEX(buffer(c1:),char(10))-2
                        read(buffer(c1:c2),'(E)') scale_factor(sdsidx)
                else
                        scale_factor(sdsidx)=1.0
                end if
               end if        
               
        allsdsnames(sdsidx)=sdsname
        namelen(sdsidx)=len(sdsname)
        end        
