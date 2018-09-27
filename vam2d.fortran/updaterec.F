c!#   $Id: updaterec.F,v 1.4 1998/03/18 16:27:35 leidner Exp $
c!#   $Log: updaterec.F,v $
c!#   Revision 1.4  1998/03/18 16:27:35  leidner
c!#   bug fix
c!#
c!#   Revision 1.3  1997/04/11 17:48:14  leidner
c!#   uncommented remaining fields; all are now processed
c!#
c!#	Revision 1.2  1997/04/08  20:18:53  leidner
c!#	now using coeffb and coeffc
c!#
c!#	Revision 1.1  1997/02/21  23:45:10  leidner
c!#	Initial revision
c!#


********************************************************************************* 
* 										*
* subroutine updaterec(fp,i)							*
*										*
* 	purpose: Updates the "raw data" in the arrays for each			*
*		selected SDS.							*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*		i - record number						*
*	output:									*
*		<none>								*
*										*
********************************************************************************* 

	subroutine updaterec(fp,i)

#include "rdglob17.h"
     

********************************************************************************* 
*	LOCAL VARIABLES								*
********************************************************************************* 

*	record number	
	integer i
*	array index
	integer x
*	logical file unit
	integer fp
*	store return values from nfgetpar calls
	integer ret(24)
	
        x=0
	
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_cellazimuth),i,
     : 	        raw_cellazimuth)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_cenlat),i,raw_cenlat)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_cenlon),i,raw_cenlon)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_coeffa),i,raw_coeffa)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_coeffb),i,raw_coeffb)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_coeffc),i,raw_coeffc)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_incangle),i,raw_incangle)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_kpolar),i,raw_kpolar)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam12),i,raw_numbeam12)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam34),i,raw_numbeam34)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam56),i,raw_numbeam56)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam78),i,raw_numbeam78)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numgoodsig),i,
     :          raw_numgoodsig)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numsigma0),i,raw_numsigma0)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_sigma0),i,raw_sigma0)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_sigma0qual),i,
     :          raw_sigma0qual)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_sigma0uf1),i,raw_sigma0uf1)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_sigma0uf2),i,raw_sigma0uf2) !ML
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_sigma0uf3),i,raw_sigma0uf3) !ML
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_surfaceflag),i,
     :          raw_surfaceflag)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_atmosatten),i,
     :          raw_atmosatten)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_wvclat),i,raw_wvclat)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_wvclon),i,raw_wvclon)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_wvcqual),i,raw_wvcqual)

********************************************************************************* 
*      	The following loop checks to see if there are errors when 		*
*      	updating the data. Since this function is called for every		*
*      	record, the following loop may cause significant slow-down		*
*   	in program execution.							*
********************************************************************************* 

        do 100,x=1,24
        if (ret(x).lt.0) then
                write(*,'(I)') 'Record: ',i
                write(*,*)'Error trying to update SDS data.'
                write(*,50) x
50              format('x=',I,' in subroutine updaterec.')
        end if
100     continue

	end
