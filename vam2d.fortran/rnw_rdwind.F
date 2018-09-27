c!#   $Id: rnw_rdwind.F,v 1.8 1998/05/20 14:33:25 leidner Exp $
c!#   $Log: rnw_rdwind.F,v $
c!#   Revision 1.8  1998/05/20 14:33:25  leidner
c!#   added option to rank 50km NSCAT winds by MLE
c!#   controlled by namelist parameter /rank_by_MLE/ (default is TRUE)
c!#
c!#   Revision 1.7  1998/05/05 14:57:16  leidner
c!#   added alias selection to NSCAT winds read from JPL HDF files
c!#
c!#   Revision 1.6  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.5  1998/02/11 20:02:47  leidner
c!#   added printing of common /ptdta/
c!#
c!#   Revision 1.4  1997/07/25 16:48:56  leidner
c!#   Added defaults for using NSCAT wind ambiguities
c!#
c!#	Revision 1.3  1997/04/17  13:34:41  leidner
c!#	minor corrections to prints and documentation
c!#
c!#	Revision 1.2  1997/04/14  19:21:46  leidner
c!#	first complete version
c!#
c**********************************************************************
      function RNW_rdwind(idate,itime,itype)
c**********************************************************************
c English Name: Read NSCAT Wind (RNW -Read NSCAT Wind from HDF file)
c -------------
c
c Purpose: Reads NSCAT wind data from Level 2.0 JPL HDF
c -------- file into VAM data buffers (see "ns_bscat.h") for
c          the specified date/time.
c
c Notes: 
c ------
c
c Variable Description:
c ---------------------
c
c     Argument List
c     -------------
c
c     integer RNW_rdwind,idate,itime,itype
c
c     RNW_rdwind (O) - function return value:
c
c          RNW_rdwind = -2 (error reading namelist)
c                       -1 (error returned from called subroutine)
c                        0 (normal)
c                        2 (end of file; requested date/time not found)
c                        3 (requested date/time not found)
c
c   **Soft errors (retcode > 0) will not be reported if data set
c     is designated as non-essential (see "critical" flag in namelist).
c
c     idate (I) - desired date in yymmdd format.
c     itime (I) - desired time in hhmmss format.
c     itype (I) - data type:
c
c             itype = NSCAT_WINDS (JPL NSCAT winds, Level 2.0 data, from an HDF file)
c
c Programmer: S. Mark Leidner
c ----------- (Numerical Weather Prediction Group)
c             (Atmospheric and Environmental Research)
c
c Modified: December 17, 1996 - created (from Joe Ardizzone's RBO_rdback).
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer idate,itime,itype
c
c     Local Variables
c     ---------------
c
      integer nrep,nbuff
      real*4 xdate,xtime
c
c     Common Blocks
c     -------------
c
#include "ns_winds.h"
#include "ptdta.h"
#include "rdglob20.h"

c
c     Namelist
c     --------
c
c!#~   rank_by_MLE    logical; rank ambiguities by MLE value
c!#~   rank_by_MLE.   (pos. 1 - highest MLE; remaining positions
c!#~   rank_by_MLE..  filled in order of decreasing MLE)
      logical critical,superob,grosscheck,alias(4),rank_by_MLE
      integer iu,iprint,itag(MAXKX),nkx,kx(MAXKX),nthin,revnum
      real minspeed,maxspeed,scfact(MAXKX),wfact(MAXKX),minprob
c
      logical diagnostic_prints,datawin_prints,rainqc
      integer nfiles
      real timwin
      character*256 filenames(NS_MAXREVS)
      namelist /input/ iu,iprint,itag,nkx,kx,scfact,minspeed,maxspeed,
     &          critical,superob,grosscheck,wfact,alias,minprob,nthin,
     &          diagnostic_prints,datawin_prints,nfiles,timwin,
     &          filenames,rainqc,tb_min,al_max,rank_by_MLE
c 
      RNW_rdwind = 0
      nrep       = 0 
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      iu       = 11
      iprint   = 0
      ra         = 0.5
      dfac       = 0.25
      idims      = PTMAX     
      rainqc = .false.
      tb_min = 240.
      al_max = 62.
      rank_by_MLE = .true.
      do i = 1,MAXKX
         itag(i) = i
         wfact(i) = 1.0
      end do
      critical = .false.
      if (RCO_default(itype,nkx,kx,scfact,minspeed,maxspeed,
     &        superob,grosscheck,alias,minprob,nthin) .ne. 0) go to 901
c
      read(IUNML,input,end=904,err=904)
      write(*,input)
c
      if (diagnostic_prints) then
        write(*,*) ' RNW_ xs,delx,iper,xmin,xmax=',
     :      xs,delx,iper,xmin,xmax
        write(*,*) ' RNW_ ys,dely,   0,ymin,ymax=',
     :      ys,dely,   0,ymin,ymax
      endif
c
c     Initialize Rev info Holders
c     ===========================
c
      do i=1,NS_MAXREVS
        nsw_revs(i)= 0
        nsw_nwvc_rev(i) = 0
      enddo
c
c     Call L20 HDF Reader
c     ===================
c
      do n=1,nfiles
         call rdhdf20f(n,filenames(n),itag,
     &        idate,itime,timwin,datawin_prints,alias, rank_by_MLE)
c
         nsw_nwvc_rev(n)=nsw_nwvc
         if (n.gt.1) then
            do i=1,n-1
               nsw_nwvc_rev(n) = nsw_nwvc_rev(n)-nsw_nwvc_rev(i)
            enddo
         else
            nsw_nwvc_rev(n)=nsw_nwvc
         endif
c
         write(*,100) nsw_nwvc_rev(n)
         write(*,105) (nsw_nwvc_rev(i),i=1,n)
         write(*,110) nsw_nwvc
100      format('Number of WVC this rev:',26x,i6)
105      format('Number of WVC by rev:',28x,15(i6))
110      format('Total number of WVC for all revs:',16x,i6)
c
      enddo
c
c     QC for rain contamination (if requested)
c
      if (rainqc) call flag_rain(tb_min,al_max)
c
c     Print Sample
c     ============
c
      call NSW_print(1,nsw_nwvc,iprint)
      call pt_print(1,NPTS,iprint)

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue

 901  continue
      RNW_rdwind = -1
      go to 999

 902  continue
      if (critical) RNW_rdwind = 2
      print *, 'vam(RNW_rdwind): End of Data'
      print *, 'vam(RNW_rdwind): date/time not found: ',idate,itime
      print *, 'vam(RNW_rdwind): type = ',itype
      go to 999

 903  continue
      if (critical) RNW_rdwind = 3
      print *, 'vam(RNW_rdwind): date/time not found: ',idate,itime
      print *, 'vam(RNW_rdwind): type = ',itype,' unit = ',iu
      go to 999

 904  continue
      RNW_rdwind = -2
      print *, 'vam(RNW_rdwind): error reading namelist.'
      go to 999

 999  return
      end
