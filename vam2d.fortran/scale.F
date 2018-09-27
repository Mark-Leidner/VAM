c!#   $Id: scale.F,v 1.1 1997/02/21 23:45:10 leidner Exp $
c!#   $Log: scale.F,v $
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#
	
********************************************************************************* 
* 										*
* real function scale(sdsidx,rawdat)						*
*										*
* 	purpose: Scales the "raw datum". This function is an intermediate 	*
*		function which calls the C function cscale.			*
*										*
*	input:									*
*		sdsidx - the global index corresponding to the			*
*			particular sds.						*
*		rawdat - reference (pointer) to the raw datum which		*
*			will be passed along to the C function cscale.		*
*	output:									*
*		return value - The scaled datum value.				*
*										*
********************************************************************************* 

	real function scale(sdsidx,rawdat)

#include "rdglob17.h"


********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 


*	declare the external function
        external cscale
*	declare cscale function type
c     	real cscale
      	real*4 cscale
*	reference (pointer) to the raw datum which will be passed 
*	along to the C function cscale.
        integer*4 rawdat
*	the global index corresponding to the particular sds
        integer sdsidx
	
c     print*,'sdsidx,data_type(sdsidx),rawdat,scale_factor(sdsidx)=',
c    :        sdsidx,data_type(sdsidx),rawdat,scale_factor(sdsidx)
        scale=cscale(data_type(sdsidx),rawdat,scale_factor(sdsidx))
	
        end
