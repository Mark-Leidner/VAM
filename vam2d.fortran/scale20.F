c!#   $Id: scale20.F,v 1.2 1997/04/11 17:46:18 leidner Exp $
c!#   $Log: scale20.F,v $
c!#   Revision 1.2  1997/04/11 17:46:18  leidner
c!#   made changes for compatability with VAM
c!#
c!#	Revision 1.1  1997/04/10  14:46:24  leidner
c!#	Initial revision
c!#
********************************************************************************* 
* 										*
* real function scale20(sdsidx,rawdat)						*
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
*		return value - The scaled datum value.				*		*
*										*
********************************************************************************* 

        real function scale20(sdsidx,rawdat)

#include "rdglob20.h"

********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 


*	declare the external function
        external cscale
*	declare cscale function type
      	real*4 cscale
*	reference (pointer) to the raw datum which will be passed 
*	along to the C function cscale.
        integer*4 rawdat
*	the global index corresponding to the particular sds
        integer sdsidx
	
        scale20=cscale(data_type(sdsidx),rawdat,scale_factor(sdsidx))
	
        end
