c!#   $Id: rss_rdsass.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rss_rdsass.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSS_rdsass(idate,itime,itype)
c**********************************************************************
c English Name: Read SASS (RSS - Read SASS from SASS Repack)
c -------------
c
c Purpose: Reads SASS observations from SASS Repack data set into VAM
c -------- data buffers (see "ptdta.h") for the specified date/time.
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
c     integer RSS_rdsass,idate,itime
c
c     RSS_rdsass (O) - function return value:
c
c          RSS_rdsass = -1 (error returned from called subroutine)
c                       -2 (error reading namelist)
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
c             itype = STANDARD (standard Seasat data set).
c                     ERS1 (ERS1 ambiguous winds).
c                     SSMI (SSMI wind speeds).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: February 6, 1995 - created.
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
      real tau
      real*4 rtau
      integer nrep,nbuff
      integer iostack(MAXUNIT),nymd(MAXUNIT),nhms(MAXUNIT)
c
c     Common Blocks
c     -------------
c
#include "ptdta.h"
c
c     Namelist
c     --------
c
      real ra,dfac,scfact,wfact
      integer iu,iprint,itag,iform
      logical critical,speedonly,alias(4)
      namelist /input/ iu,itag,iprint,iform,critical,alias,speedonly,
     &                 ra,dfac,scfact,wfact

      data nymd/MAXUNIT*0/
      data nhms/MAXUNIT*0/
      data iostack/MAXUNIT*EMPTY/

      ra         = 0.5
      dfac       = 0.25
      RSS_rdsass = 0
      nrep       = 0 
      idims      = PTMAX     
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      iu         = 10
      itag       = 0
      iprint     = 0
      wfact      = 1.0
      critical   = .false.
      if (RSS_default(itype,speedonly,scfact,alias,iform).ne.0)go to 901

      read(IUNML,input,end=904,err=904)
      write(*,input)
c
c     Establish I/O link
c     ==================
c
 5    continue
      ierr = ioopen(iu)
      if (ierr .eq. 2) go to 902
      if (ierr .ne. 0) go to 901
c
c     Position data set to desired records.
c     =====================================
c
      call setclock(idate,itime,0)
      call settime(nymd(iu),nhms(iu),0)

      do while (checktime() .lt. 0)
c
c        Read SASS repack header.
c        ------------------------
c
         if (iostack(iu) .eq. EMPTY) then
            read(iu,end=900,err=900) rtau
            tau = rtau
            call tau2date(tau,nymd(iu),nhms(iu))
            call settime(nymd(iu),nhms(iu),0)
            iostack(iu) = FULL
         endif
c
c        Check Time.
c        -----------
c
         if (checktime() .lt. 0) then 
            if (RPK_rdskip(iu,0) .lt. 0) go to 901
            iostack(iu) = EMPTY
         endif

      end do

      if (checktime() .ne. 0) go to 903
c
c     Read in Data
c     ============
c
      nbuff = RPK_rdskip(iu,iform)
      if (nbuff .lt. 0) go to 901
      nrep = RSS_rddata(iu,nbuff,speedonly,scfact,wfact,alias,itag)

      if (nrep .lt. 0) go to 901
      if (RPK_rdskip(iu,0) .lt. 0) go to 901
      iostack(iu) = EMPTY
c
c     Print sample.
c     =============
c
      call PT_print(npts+1,npts+nrep,iprint)
      npts = npts + nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      call ioclose(iu)
      go to 5

 901  continue
      RSS_rdsass = -1
      go to 999

 902  continue
      if (critical) RSS_rdsass = 2
      print *, 'vam(RSS_rdsass): End of Data'
      print *, 'vam(RSS_rdsass): date/time not found: ',idate,itime
      print *, 'vam(RSS_rdsass): type = ',itype
      go to 999

 903  continue
      if (critical) RSS_rdsass = 3
      print *, 'vam(RSS_rdsass): date/time not found: ',idate,itime
      print *, 'vam(RSS_rdsass): type = ',itype,' unit = ',iu
      go to 999

 904  continue
      RSS_rdsass = -2
      print *, 'vam(RSS_rdsass): error reading namelist.'
      go to 999

 999  return
      end
