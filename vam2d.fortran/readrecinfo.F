c!#   $Id: readrecinfo.F,v 1.3 1997/04/11 17:43:11 leidner Exp $
c!#   $Log: readrecinfo.F,v $
c!#   Revision 1.3  1997/04/11 17:43:11  leidner
c!#   uncommented remaining fields; all are now processed
c!#
c!#	Revision 1.2  1997/04/08  20:15:25  leidner
c!#	now using coeffb and coeffc
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#


********************************************************************************* 
* 										*
* subroutine readrecinfo(fp)							*
*										*
* 	purpose: Call readsdsinfo function for each SDS in the record.		*
*		 This function is called once during program execution.		*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*	output:									*
*		<none>								*
*										*
********************************************************************************* 

        subroutine readrecinfo(fp)
	
#include "rdglob17.h"

********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 

*	logical file unit
        integer fp
	
********************************************************************************* 
*	Call readsdsinfo for each sds in the record.				*
*	Comment out the lines which are unnecessary.				*
********************************************************************************* 

        call readsdsinfo(fp,idx_cellazimuth,"Cell_Azimuth")
        call readsdsinfo(fp,idx_cenlat,"Cen_Lat")
        call readsdsinfo(fp,idx_cenlon,"Cen_Lon")
        call readsdsinfo(fp,idx_coeffa,"Coeff_A")
        call readsdsinfo(fp,idx_coeffb,"Coeff_B")
        call readsdsinfo(fp,idx_coeffc,"Coeff_C")
        call readsdsinfo(fp,idx_incangle,"Incidence_Angle")
        call readsdsinfo(fp,idx_kpolar,"K_Polar")
        call readsdsinfo(fp,idx_numbeam12,"Num_Beam_12")
        call readsdsinfo(fp,idx_numbeam34,"Num_Beam_34")
        call readsdsinfo(fp,idx_numbeam56,"Num_Beam_56")
        call readsdsinfo(fp,idx_numbeam78,"Num_Beam_78")
        call readsdsinfo(fp,idx_numgoodsig,"Num_Good_Sigma0")
        call readsdsinfo(fp,idx_numsigma0,"Num_Sigma0")
        call readsdsinfo(fp,idx_sigma0,"Sigma0")
        call readsdsinfo(fp,idx_sigma0qual,"Sigma0_Quality_Flag")
        call readsdsinfo(fp,idx_sigma0uf1,"Sigma0_Usable_Flag_1")
        call readsdsinfo(fp,idx_sigma0uf2,"Sigma0_Usable_Flag_2")
        call readsdsinfo(fp,idx_sigma0uf3,"Sigma0_Usable_Flag_3")
        call readsdsinfo(fp,idx_surfaceflag,"Surface_Flag")
        call readsdsinfo(fp,idx_atmosatten,"Mean_Atmos_Atten")
        call readsdsinfo(fp,idx_wvclat,"WVC_Lat")
        call readsdsinfo(fp,idx_wvclon,"WVC_Lon")
        call readsdsinfo(fp,idx_wvcqual,"WVC_Quality_Flag")
	
        end
