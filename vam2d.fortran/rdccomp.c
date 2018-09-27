/* 
 $Id: rdccomp.c,v 1.1 1997/02/27 16:07:23 leidner Exp $
 $Log: rdccomp.c,v $
 Revision 1.1  1997/02/27 16:07:23  leidner
 Initial revision

 * rdccomp.c - C Component for NSCAT HDF FORTRAN Readers v0.90 and higher
 *	
 * Author:	J. Joseph M. Benavidez
 *		Jet Propulsion Laboratories
 *		hasuf@seastore.jpl.nasa.gov
 *
 * Release info:
 *		7/26/95 v0.90 Initial Release
 *
 * Purpose:	Handle the scaling of the values, multiplying the scale
 *		factor for the particular SDS times the "raw" datum
 *
 * Usage:
 *	input:	
 *		-long *datatype: storage data type as defined in NCSA's hdf.h
 *		-void *rawdat:	one data element of the SDS
 *		-float *scalefactor: scale factor
 *	output:
 *		-float cscale: returns the scaled datum
 *
 * Comments:
 *
 *	NOTE: Beware that accuracy issues my come up since
 *	      standard FORTRAN does not handle unsigned integers.
 *
 *	NOTE: The biggest number that this function can handle
 *	      is a a 4-byte stored number (ie INT32 or FOAT32)
 *
 *	The included header file may be either hdfshell.h (from NSCAT)
 *	or hdf.h (from NCSA).
 *
 *	Declarations:
 *		-All long types should be declared in the calling
 *		 FORTRAN program as integer (or integer*4), and all float  
 *		 types should be declared as real (or real*4).
 *			
 *	If scaled_dat is a variable in the calling FORTRAN program of type
 *	real, the calling statement would look like the following:
 *
 *		scaled_dat=cscale(datatype,rawdat,scalefactor)
 *
 */
#include "hdfshell.h"
#include <hdf.h>
#define ERRVALUE -999999

#define fcscale   FNAME(cscale)

float fcscale(long *datatype,void* rawdat, float *scalefactor){

	float retvalue;
	unsigned long *pulong;
	unsigned short *pushort;
	unsigned char *puchar;
	
	float *pfloat;
	long *plong;
	short *pshort;
	char *pchar;
	
		
	switch(*datatype){

		case DFNT_FLOAT32:
			pfloat=(float*)rawdat;
			retvalue=(float)((*pfloat) * (*scalefactor));
			break;
		case DFNT_INT32:
			plong=(long*)rawdat;
			retvalue=(float)((*plong) * (*scalefactor));
			break;
		case DFNT_UINT32:
			pulong=(unsigned long*)rawdat;
			retvalue=(float)((*pulong) * (*scalefactor));
			break;
		case DFNT_INT16:
			pshort=(short*)rawdat;
			retvalue=(float)((*pshort) * (*scalefactor));
			break;
		case DFNT_UINT16:
			pushort=(unsigned short*)rawdat;
			retvalue=(float)(((unsigned)(*pushort)) * (*scalefactor));
			break;
		case DFNT_INT8:
		case DFNT_CHAR8:
			pchar=(char*)rawdat;
			retvalue=(float)((*pchar) * (*scalefactor));
			break;
		case DFNT_UINT8:
		case DFNT_UCHAR8:
			puchar=(unsigned char*)rawdat;
			retvalue=(float)(((unsigned)(*puchar)) * (*scalefactor));
			break;
		default:
			retvalue=(float)ERRVALUE;
		}
	
	
	return retvalue;
	}
