c!#   $Id: setindices20.F,v 1.3 1998/03/18 16:29:56 leidner Exp $
c!#   $Log: setindices20.F,v $
c!#   Revision 1.3  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.2  1997/04/11 17:47:41  leidner
c!#   made changes for compatability with VAM
c!#
c!#	Revision 1.1  1997/04/10  14:52:32  leidner
c!#	Initial revision
c!#
********************************************************************************* 
* 										*
* integer function setindices20(lgoes)						*
*										*
* 	purpose: This function sets the index value for each selected		*
*		SDS.								*
*										*
*	input:									*
*		lgoes   logical; is goes data present in this HDF file		*
*	output:									*
*		return value - the number of indices selected			*
*										*
*	comments:								*
*		Comment out the lines which refer to SDS`s			*
*		which you are not going to use in your program.			*
*		For instance, if you are not going to use			*
*		WVC_Lat, then comment out the lines:				*
*										*
*     			i=i+1							*
*      		 	idx_wvclat=i       					*
*										*
********************************************************************************* 

       integer function setindices20(lgoes)

#include "rdglob20.h"

********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 

*	index
        integer i
        logical lgoes

       i=0	

       i=i+1
       idx_direrr=i
       
       i=i+1
       idx_speederr=i
       
       i=i+1
       idx_meanwind=i
       
       i=i+1
       idx_mlelikelihood=i
       
       i=i+1
       idx_numambigs=i
       
       i=i+1
       idx_numbeam12=i
       
       i=i+1
       idx_numbeam34=i
       
       i=i+1
       idx_numbeam56=i
       
       i=i+1
       idx_numbeam78=i
       
       i=i+1
       idx_numsigma0=i
       
       i=i+1
       idx_winddir=i
       
       i=i+1
       idx_windspeed=i
       
       i=i+1
       idx_wvclat=i
       
       i=i+1
       idx_wvclon=i
       
       i=i+1
       idx_wvcqual=i

       if (lgoes) then
          i=i+1
          idx_wvctb=i
       
          i=i+1
          idx_wvcal=i

          i=i+1
          idx_wvcdt=i
       endif

       setindices20=i
       			
       end
