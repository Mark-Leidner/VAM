c!#   $Id: rdglob17.h,v 1.3 1997/04/11 17:30:48 leidner Exp $
c!#   $Log: rdglob17.h,v $
c!#   Revision 1.3  1997/04/11 17:30:48  leidner
c!#   all fields now available
c!#
c!#	Revision 1.2  1997/04/08  20:11:23  leidner
c!#	now using coeffb and coeffc
c!#
c!#	Revision 1.1  1997/02/21  23:45:35  leidner
c!#	Initial revision
c!#
*------------global variables for rdHDF17.f---------------------


*******************************************************        
*
*   	SDS info
*
*******************************************************

*
*        Additional SDS info
*
c       real        scale_factor(24)
        real*4      scale_factor(24)

        common /more_sdsinfo/
     :          scale_factor

*
*        Required SDS information variables        
*
       integer*4
     :          rank(24),
     :          dimsizes(3,24),
     :          data_type(24),
     :          bytes_per_cell(24),
     :          bytes_per_line(24)

       character*256 allsdsnames(24)
       integer namelen(24)

       common /sdsinfo/
     :          rank,
     :          dimsizes,
     :          data_type,
     :          bytes_per_cell,
     :          bytes_per_line,
     :          allsdsnames, namelen


*******************************************************        
*
*	SDS's that will be included in the record
*
*     	All the SDS's defined in the June 1996
*     	Level 1.7 Data SIS-HDF Version are included.
*
*     	Comment out the sets of SDS variables that
*     	you don't need. (See README file for more 
*    	information.)
*
*******************************************************



        integer idx_cellazimuth
        real*4 cellazimuth(24,24)
        integer*2 raw_cellazimuth(24,24)
       
        integer idx_cenlat
        real*4 cenlat(24,24)
        integer*4 raw_cenlat(24,24)
        
        integer idx_cenlon
 	real*4 cenlon(24,24)
        integer*4 raw_cenlon(24,24)
        
        integer idx_coeffa
        real*4 coeffa(24,24)
        integer*2 raw_coeffa(24,24)

        integer idx_coeffb
        real*4  coeffb(24,24)
        integer*2 raw_coeffb(24,24)

        integer idx_coeffc
        real*4  coeffc(24,24)
        integer*2 raw_coeffc(24,24)

        integer idx_incangle
        real*4  incangle(24,24)
        integer*2 raw_incangle(24,24)

        integer idx_kpolar
        byte kpolar(24,24)
        byte raw_kpolar(24,24)

        integer idx_numbeam12
        byte numbeam12(24)
        byte raw_numbeam12(24)

        integer idx_numbeam34
        byte numbeam34(24)
        byte raw_numbeam34(24)

        integer idx_numbeam56
        byte numbeam56(24)
        byte raw_numbeam56(24)

        integer idx_numbeam78
        byte numbeam78(24)
        byte raw_numbeam78(24)

        integer idx_numgoodsig
        byte numgoodsig(24)
        byte raw_numgoodsig(24)

        integer idx_numsigma0
        byte numsigma0(24)
        byte raw_numsigma0(24)
        
        integer idx_sigma0
        real*4 sigma0(24,24)
        integer*2 raw_sigma0(24,24)
        
        integer idx_sigma0qual
        integer*2 sigma0qual(24,24)
        integer*2 raw_sigma0qual(24,24)

        integer idx_sigma0uf1
        logical*1 sigma0uf1(24)
        byte raw_sigma0uf1(24)

        integer idx_sigma0uf2
        logical*1 sigma0uf2(24)
        byte raw_sigma0uf2(24)

        integer idx_sigma0uf3
        logical*1 sigma0uf3(24)
        byte raw_sigma0uf3(24)

        integer idx_surfaceflag
        logical*1 surfaceflag(24,24)
        byte raw_surfaceflag(24,24)

        integer idx_atmosatten
        real*4  atmosatten(24,24)
        byte raw_atmosatten(24,24)

        integer idx_wvclat
        real*4      wvclat(24)
        integer*2 raw_wvclat(24)

        integer idx_wvclon
        real*4      wvclon(24)
        integer*2 raw_wvclon(24)

        integer idx_wvcqual
        byte wvcqual(24)
        byte raw_wvcqual(24)

                        
        common /nscatrec/
               
     :         idx_cellazimuth,
     :         cellazimuth,
     :         raw_cellazimuth,
       
     :         idx_cenlat,
     :         cenlat,
     :         raw_cenlat,
        
     :         idx_cenlon,
     :         cenlon,
     :         raw_cenlon,
        
     :         idx_coeffa,
     :         coeffa,
     :         raw_coeffa,

     :         idx_coeffb,
     :         coeffb,
     :         raw_coeffb,

     :         idx_coeffc,
     :         coeffc,
     :         raw_coeffc,

     :         idx_incangle,
     :         incangle,
     :         raw_incangle,

     :         idx_kpolar,
     :         kpolar,
     :         raw_kpolar,

     :         idx_numbeam12,
     :         numbeam12,
     :         raw_numbeam12,

     :         idx_numbeam34,
     :         numbeam34,
     :         raw_numbeam34,

     :         idx_numbeam56,
     :         numbeam56,
     :         raw_numbeam56,

     :         idx_numbeam78,
     :         numbeam78,
     :         raw_numbeam78,

     :         idx_numgoodsig,
     :         numgoodsig,
     :         raw_numgoodsig,

     :         idx_numsigma0,
     :         numsigma0,
     :         raw_numsigma0,
        
     :         idx_sigma0,
     :         sigma0,
     :         raw_sigma0,
        
     :         idx_sigma0qual,
     :         sigma0qual,
     :         raw_sigma0qual,

     :         idx_sigma0uf1,
     :         sigma0uf1,
     :         raw_sigma0uf1,

     :         idx_sigma0uf2,
     :         sigma0uf2,
     :         raw_sigma0uf2,

     :         idx_sigma0uf3,
     :         sigma0uf3,
     :         raw_sigma0uf3,

     :         idx_surfaceflag,
     :         surfaceflag,
     :         raw_surfaceflag,

     :         idx_atmosatten,
     :         atmosatten,
     :         raw_atmosatten,

     :         idx_wvclat,
     :         wvclat,
     :         raw_wvclat,

     :         idx_wvclon,
     :         wvclon,
     :         raw_wvclon,

     :         idx_wvcqual,
     :         wvcqual,
     :         raw_wvcqual

*----------------------------end global variables-------------------------
