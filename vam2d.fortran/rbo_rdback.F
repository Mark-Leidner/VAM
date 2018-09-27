c!#   $Id: rbo_rdback.F,v 1.4 1998/01/26 16:46:55 leidner Exp $
c!#   $Log: rbo_rdback.F,v $
c!#   Revision 1.4  1998/01/26 16:46:55  leidner
c!#   added polynomial Kp model as default ers1 Kp
c!#
c!#   Revision 1.3  1997/06/23 17:40:54  leidner
c!#   added parameters for sig0 QC in linear space (s0min,s0max,s0min_edit)
c!#
c!#	Revision 1.2  1997/02/12  18:26:09  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RBO_rdback(idate,itime,itype)
c**********************************************************************
c English Name: Read Backscatter (RBO -Read Backscatter from OIrepack)
c -------------
c
c Purpose: Reads backscatter data from OIrepack dataset
c -------- into VAM data buffers (see "er_bscat.h") for the
c          specified date/time.
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
c     integer RBO_rdback,idate,itime,itype
c
c     RGV_rdconv (O) - function return value:
c
c          RBO_rdback = -2 (error reading namelist)
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
c             itype = STANDARD (standard backscatter data)
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
      integer nrep,nbuff
      real*4 xdate,xtime
      integer iostack(MAXUNIT),nymd(MAXUNIT),nhms(MAXUNIT)
c
c     Common Blocks
c     -------------
c
#include "er_bscat.h"
c
c     Namelist
c     --------
c
      real dbmin,dbmax,s0min,s0max,s0min_edit
      logical critical
      integer iu,minsig0,nthin,iprint,itag
      namelist /input/ iu,dbmin,dbmax,minsig0,s0min,s0max,s0min_edit,
     &    nthin,iprint,itag,critical,iKpType

      data nymd/MAXUNIT*0/
      data nhms/MAXUNIT*0/
      data iostack/MAXUNIT*EMPTY/

      RBO_rdback = 0
      nrep       = 0 
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      iu       = 13
      iprint   = 0
      itag     = 0
      critical = .false.
      if ( RBO_default 
     I    ( itype,
     O    dbmin, dbmax, s0min, s0max, s0min_edit, minsig0, nthin,
     O     iKpType )
     &    .ne. 0) go to 901

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
c        Read OI repack header.
c        ----------------------
c
         if (iostack(iu) .eq. EMPTY) then
            read(iu,end=900,err=900) xdate,xtime
            nymd(iu) = int(xdate)
            nhms(iu) = int(xtime)
            iostack(iu) = FULL
            call settime(nymd(iu),nhms(iu),0)
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
      nbuff = RPK_rdskip(iu,7)
      if (nbuff .lt. 0) go to 901
      nrep = RBO_rddata(iu,nbuff,dbmin,dbmax,minsig0,nthin,itag,iKpType)

      if (nrep .lt. 0) go to 901
      if (RPK_rdskip(iu,0) .lt. 0) go to 901
      iostack(iu) = EMPTY
c
c     Print Sample
c     ============
c
      call ER_print(er_nobs+1,er_nobs+nrep,iprint)
      er_nobs = er_nobs + nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      call ioclose(iu)
      go to 5

 901  continue
      RBO_rdback = -1
      go to 999

 902  continue
      if (critical) RBO_rdback = 2
      print *, 'vam(RBO_rdback): End of Data'
      print *, 'vam(RBO_rdback): date/time not found: ',idate,itime
      print *, 'vam(RBO_rdback): type = ',itype
      go to 999

 903  continue
      if (critical) RBO_rdback = 3
      print *, 'vam(RBO_rdback): date/time not found: ',idate,itime
      print *, 'vam(RBO_rdback): type = ',itype,' unit = ',iu
      go to 999

 904  continue
      RBO_rdback = -2
      print *, 'vam(RBO_rdback): error reading namelist.'
      go to 999

 999  return
      end
