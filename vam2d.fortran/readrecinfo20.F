c!#   $Id: readrecinfo20.F,v 1.3 1998/03/18 16:29:56 leidner Exp $
c!#   $Log: readrecinfo20.F,v $
c!#   Revision 1.3  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.2  1997/04/11 17:44:16  leidner
c!#   made changes for compatability with VAM
c!#
c!#	Revision 1.1  1997/04/10  14:26:02  leidner
c!#	Initial revision
c!#

********************************************************************************* 
* 										*
* subroutine readrecinfo20(fp,lgoes)						*
*										*
* 	purpose: Call readsdsinfo20 function for each SDS in the record.	*
*		 This function is called once during program execution.		*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*		lgoes - logical; is goes data present in this HDF file		*
*	output:									*
*		<none>								*
*										*
********************************************************************************* 

        subroutine readrecinfo20(fp,lgoes)
	
#include "rdglob20.h"


********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 


*	logical file unit
	integer fp
        logical lgoes
	
********************************************************************************* 
*	Call readsdsinfo20 for each sds in the record.				*
*	Comment out the lines which are unnecessary.				*
********************************************************************************* 

        call readsdsinfo20(fp,idx_direrr,"Error_Dir")
        call readsdsinfo20(fp,idx_speederr,"Error_Speed")
        call readsdsinfo20(fp,idx_meanwind,"Mean_Wind")
        call readsdsinfo20(fp,idx_mlelikelihood,"MLE_Likelihood")
        call readsdsinfo20(fp,idx_numambigs,"Num_Ambigs")
        call readsdsinfo20(fp,idx_numbeam12,"Num_Beam_12")
        call readsdsinfo20(fp,idx_numbeam34,"Num_Beam_34")
        call readsdsinfo20(fp,idx_numbeam56,"Num_Beam_56")
        call readsdsinfo20(fp,idx_numbeam78,"Num_Beam_78")
        call readsdsinfo20(fp,idx_numsigma0,"Num_Sigma0")
        call readsdsinfo20(fp,idx_winddir,"Wind_Dir")
        call readsdsinfo20(fp,idx_windspeed,"Wind_Speed")
        call readsdsinfo20(fp,idx_wvclat,"WVC_Lat")
        call readsdsinfo20(fp,idx_wvclon,"WVC_Lon")
        call readsdsinfo20(fp,idx_wvcqual,"WVC_Quality_Flag")
        if (lgoes) then
           call readsdsinfo20(fp,idx_wvctb,"GOES_brightness_T")        
           call readsdsinfo20(fp,idx_wvcal,"GOES_albedo")
           call readsdsinfo20(fp,idx_wvcdt,"GOES_delta_time")
        endif

        end
