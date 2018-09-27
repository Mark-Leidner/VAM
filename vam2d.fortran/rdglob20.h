c!#   $Id: rdglob20.h,v 1.3 1998/03/18 16:29:56 leidner Exp $
c!#   $Log: rdglob20.h,v $
c!#   Revision 1.3  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.2  1997/04/11 17:31:46  leidner
c!#   renamed and redefined variables for compatability with VAM
c!#
c!#	Revision 1.1  1997/04/10  14:09:50  leidner
c!#	Initial revision
c!#

*-------------global variables for rdHDF20.f-------------------

*******************************************************	
*
*	SDS info
*
*******************************************************

*
*	Additional SDS info
*
        integer NSDS
        parameter (NSDS=18)

        real*4  scale_factor(NSDS)

        common /more_sdsinfo20/
     :        scale_factor

*
*	Required SDS information variables	
*
       integer*4
     :        rank(NSDS),
     :        dimsizes(3,NSDS),
     :        data_type(NSDS),
     :        bytes_per_cell(NSDS),
     :        bytes_per_line(NSDS)

       character*256 allsdsnames(NSDS)
       integer namelen(NSDS)

       common /sdsinfo20/
     :        rank,
     :        dimsizes,
     :        data_type,
     :        bytes_per_cell,
     :        bytes_per_line,
     :        allsdsnames, namelen


*******************************************************	
*
*	SDS's that will be included in the record
*
*	All the SDS's defined in the Dec. 1994
*	Level 2 Data SIS-HDF Version are included.
*
*	Comment out the sets of SDS variables that
*	you don't need. (See README file for more 
*	information.)
*
*******************************************************

        integer idx_direrr
        integer*2 raw_direrr(4,24)
        real*4  direrr(4,24)
       
        integer idx_speederr
        integer*2 raw_speederr(4,24)
        real*4  speederr(4,24)
        
        integer idx_meanwind
        integer*2 raw_meanwind(24)
        real*4  meanwind(24)
        
        integer idx_mlelikelihood
        integer*2 raw_mlelikelihood(4,24)
        real*4  mlelikelihood(4,24)

        integer idx_numambigs
        character raw_numambigs(24)
        byte    numambigs(24)

        integer idx_numbeam12
        character raw_numbeam12(24)
        byte    numbeam12(24)

        integer idx_numbeam34
        character raw_numbeam34(24)
        byte    numbeam34(24)

        integer idx_numbeam56
        character raw_numbeam56(24)
        byte    numbeam56(24)

        integer idx_numbeam78
        character raw_numbeam78(24)
        byte    numbeam78(24)

        integer idx_numsigma0
        character raw_numsigma0(24)
        byte    numsigma0(24)
        
        integer idx_winddir
        integer*2 raw_winddir(4,24)
        real*4  winddir(4,24)
        
        integer idx_windspeed
        integer*2 raw_windspeed(4,24)
        real*4  windspeed(4,24)

        integer idx_wvclat
        integer*2 raw_wvclat(24)
        real*4  wvclat(24)

        integer idx_wvclon
        integer*2 raw_wvclon(24)
        real*4  wvclon(24)

        integer idx_wvcqual
        character raw_wvcqual(24)
        byte    wvcqual(24)

        integer idx_wvctb
        integer*2 raw_wvctb(24)
        real*4  wvctb(24)

        integer idx_wvcal
        integer*2 raw_wvcal(24)
        real*4  wvcal(24)

        integer idx_wvcdt
        integer*2 raw_wvcdt
        real*4  wvcdt

        common /nscatrec20/
               
     :          idx_direrr,
     :          raw_direrr,
     :          direrr,
       
     :          idx_speederr,
     :          raw_speederr,              
     :          speederr,
        
     :          idx_meanwind,
     :          raw_meanwind,
     :          meanwind,
        
     :          idx_mlelikelihood,
     :          raw_mlelikelihood,
     :          mlelikelihood,
     
     :          idx_numambigs,
     :          raw_numambigs,
     :          numambigs,

     :          idx_numbeam12,
     :          raw_numbeam12,
     :          numbeam12,

     :          idx_numbeam34,
     :          raw_numbeam34,
     :          numbeam34,

     :          idx_numbeam56,
     :          raw_numbeam56,
     :          numbeam56,

     :          idx_numbeam78,
     :          raw_numbeam78,
     :          numbeam78,

     :          idx_numsigma0,
     :          raw_numsigma0,
     :          numsigma0,
        
     :          idx_winddir,
     :          raw_winddir,
     :          winddir,
        
     :          idx_windspeed,
     :          raw_windspeed,
     :          windspeed,

     :          idx_wvclat,
     :          raw_wvclat,
     :          wvclat,

     :          idx_wvclon,
     :          raw_wvclon,
     :          wvclon,

     :          idx_wvcqual,
     :          raw_wvcqual,
     :          wvcqual,

     :          idx_wvctb,
     :          raw_wvctb,
     :          wvctb,

     :          idx_wvcal,
     :          raw_wvcal,
     :          wvcal,

     :          idx_wvcdt,
     :          raw_wvcdt,
     :          wvcdt
                       
*----------------------------end global variables-------------------------------
