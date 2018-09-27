c!#   $Id: updaterec20.F,v 1.3 1998/03/18 16:29:56 leidner Exp $
c!#   $Log: updaterec20.F,v $
c!#   Revision 1.3  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.2  1997/04/11 17:48:42  leidner
c!#   made changes for compatability with VAM
c!#
c!#	Revision 1.1  1997/04/10  14:56:54  leidner
c!#	Initial revision
c!#
********************************************************************************* 
* 										*
* subroutine updaterec20(fp,i,lgoes)						*
*										*
* 	purpose: Updates the "raw data" in the arrays for each			*
*		selected SDS.							*
*										*
*	input:									*
*		fp - logical file unit for data file				*
*		i - record number						*
*		lgoes - logical; is there any goes data in this HDF file	*
*	output:									*
*		<none>								*
*										*
********************************************************************************* 

       subroutine updaterec20(fp,i,lgoes)
										
#include "rdglob20.h"
     
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
        integer ret(23)
        logical lgoes
	
        x=0
	
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_direrr),i,raw_direrr)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_speederr),i,raw_speederr)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_meanwind),i,raw_meanwind)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_mlelikelihood),i,
     :          raw_mlelikelihood)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numambigs),i,raw_numambigs)

        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam12),i,raw_numbeam12)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam34),i,raw_numbeam34)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam56),i,raw_numbeam56)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numbeam78),i,raw_numbeam78)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_numsigma0),i,raw_numsigma0)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_winddir),i,raw_winddir)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_windspeed),i,raw_windspeed)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_wvclat),i,raw_wvclat)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_wvclon),i,raw_wvclon)
        
        x=x+1
        ret(x)=nfgetpar(fp,allsdsnames(idx_wvcqual),i,raw_wvcqual)

        if (lgoes) then
           x=x+1
           ret(x)=nfgetpar(fp,allsdsnames(idx_wvctb),i,raw_wvctb)
           
           x=x+1
           ret(x)=nfgetpar(fp,allsdsnames(idx_wvcal),i,raw_wvcal)
           
           x=x+1
           ret(x)=nfgetpar(fp,allsdsnames(idx_wvcdt),i,raw_wvcdt)
        endif

********************************************************************************* 
*	The following loop checks to see if there are errors when 		*
*	updating the data. Since this function is called for every		*
*	record, the loop may cause significant slow-down			*
*	in program execution.							*
********************************************************************************* 

        do 100,x=1,23
        if (ret(x).lt.0) then
                write(*,'(I)') 'Record: ',i
                write(*,*)'Error trying to update SDS data.'
                write(*,50) x
50              format('x=',I,' in subroutine updaterec20.')
        end if
100     continue

        end
