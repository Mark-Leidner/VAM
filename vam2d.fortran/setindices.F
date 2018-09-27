c!#   $Id: setindices.F,v 1.3 1997/04/11 17:47:00 leidner Exp $
c!#   $Log: setindices.F,v $
c!#   Revision 1.3  1997/04/11 17:47:00  leidner
c!#   uncommented remaining fields; all are now processed
c!#
c!#	Revision 1.2  1997/04/08  20:17:42  leidner
c!#	now using coeffb and coeffc
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#


********************************************************************************* 
* 										*
* integer function setindices()							*
*										*
* 	purpose: This function sets the index value for each selected		*
*		SDS.								*
*										*
*	input:									*
*		<none>								*
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

       integer function setindices()

#include "rdglob17.h"


********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 


*	index
        integer i

       i=0	

       i=i+1
       idx_cellazimuth=i
       
       i=i+1
       idx_cenlat=i
       
       i=i+1
       idx_cenlon=i
       
       i=i+1
       idx_coeffa=i
       
       i=i+1
       idx_coeffb=i
       
       i=i+1
       idx_coeffc=i
       
       i=i+1
       idx_incangle=i
       
       i=i+1
       idx_kpolar=i
       
       i=i+1
       idx_numbeam12=i
       
       i=i+1
       idx_numbeam34=i
       
       i=i+1
       idx_numbeam56=i
       
       i=i+1
       idx_numbeam78=i
       
       i=i+1
       idx_numgoodsig=i
       
       i=i+1
       idx_numsigma0=i
       
       i=i+1
       idx_sigma0=i
       
       i=i+1
       idx_sigma0qual=i
       
       i=i+1
       idx_sigma0uf1=i
       
       i=i+1
       idx_sigma0uf2=i
       
       i=i+1
       idx_sigma0uf3=i
       
       i=i+1
       idx_surfaceflag=i
       
       i=i+1
       idx_atmosatten=i
       
       i=i+1
       idx_wvclat=i
       
       i=i+1
       idx_wvclon=i
       
       i=i+1
       idx_wvcqual=i

      	
       setindices=i
       			
       end
