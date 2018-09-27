c!#   $Id: rco_rdconv.F,v 1.4 1997/04/04 21:44:19 leidner Exp $
c!#   $Log: rco_rdconv.F,v $
c!#   Revision 1.4  1997/04/04 21:44:19  leidner
c!#   Merging NSCATo and VAM1
c!#
c!#	Revision 1.3  1997/02/18  19:28:30  leidner
c!#	namelist TEST added; allows conv obs (up to 5) to be specified via namelist
c!#
c!#	Revision 1.2  1997/02/12  18:26:09  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RCO_rdconv(idate,itime,itype)
c**********************************************************************
c English Name: Read Conventional (RCO -Read Conventional from OIrepack)
c -------------
c
c Purpose: Reads conventional wind reports from OIrepack dataset
c -------- into VAM data buffers (see "sldta.h") for the specified
c          date/time.
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
c     integer RCO_rdconv,idate,itime,itype
c
c     RGV_rdconv (O) - function return value:
c
c          RCO_rdconv = -3 (illegal value specified in namelist)
c                       -2 (error reading namelist)
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
c             itype = STANDARD (standard ship and buoy data).
c                   = ERS1 (ERS1 dealiased winds).
c                     UKMO (dealiased winds using UKMO method).
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
      real*4 xdate,xtime
      integer i,nrep,nbuff
      integer iostack(MAXUNIT),nymd(MAXUNIT),nhms(MAXUNIT)
c
c     Common Blocks
c     -------------
c
#include "sldta.h"
#include "gparm.h"
#include "grdta.h"
c
c     Namelist
c     --------
c
      logical critical,superob,grosscheck,alias(4)
      integer iu,iprint,itag(MAXKX),nkx,kx(MAXKX),nthin
      real minspeed,maxspeed,scfact(MAXKX),wfact(MAXKX),minprob
      namelist /input/ iu,iprint,itag,nkx,kx,scfact,minspeed,maxspeed,
     &          critical,superob,grosscheck,wfact,alias,minprob,nthin
     &    ,itest

c     itest   >0 adds test data via input namelist to other data
c             =0 implies no test data
c             <0 implies only test data

      integer NNOBS,MNOBS
      parameter (MNOBS=100)
      real TLAT(MNOBS), TLON(MNOBS), SPD(MNOBS), DIR(MNOBS)
      namelist /TEST/ NNOBS, TLAT, TLON, SPD, DIR
      data PI/3.1415926535897932D0/

      data nymd/MAXUNIT*0/
      data nhms/MAXUNIT*0/
      data iostack/MAXUNIT*EMPTY/

      RCO_rdconv = 0
      idimc      = SLMAX
      nrep       = 0 
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      iu       = 11
      iprint   = 0
      itest = 0
      do i = 1,MAXKX
         itag(i) = i
         wfact(i) = 1.0
      end do
      critical = .false.
      if (RCO_default(itype,nkx,kx,scfact,minspeed,maxspeed,
     &        superob,grosscheck,alias,minprob,nthin) .ne. 0) go to 901

      read(IUNML,input,end=904,err=904)
      write(*,input)
      if (nkx .gt. MAXKX) go to 905

      if (itest .ne. 0) then
        read(IUNML,TEST,END=904,ERR=904)
        write(*,TEST)
        if (NNOBS .GT. MNOBS) go to 904
      endif
c
c     Establish I/O link
c     ==================
c
 5    continue
      if (itest .ge. 0) then
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
        if (itype .eq. STANDARD) then 
          nbuff = RPK_rdskip(iu,2)
          if (nbuff .lt. 0) go to 901
          nrep = RCO_rddata(iu,nbuff,nkx,kx,scfact,wfact,itag)
        elseif (itype .eq. ERS1) then 
          nbuff = RPK_rdskip(iu,2)
          if (nbuff .lt. 0) go to 901
          nrep = RCO_rddata(iu,nbuff,nkx,kx,scfact,wfact,itag)
        elseif (itype .eq. UKMO) then 
          nbuff = RPK_rdskip(iu,8)
          if (nbuff .lt. 0) go to 901
          nrep = RCO_rdukmo(iu,nbuff,scfact,wfact,alias,itag,
     &        minprob,nthin)
        endif

        if (nrep .lt. 0) go to 901
        if (RPK_rdskip(iu,0) .lt. 0) go to 901
        iostack(iu) = EMPTY
      endif

      if (itest .ne. 0) then
c
c     Store conventional obs input via namelist in 
c     conventional data buffer
c     --------------------------------------------
c
        do 10 i = 1, NNOBS
c
c        Transform (long,lat) to grid coordinates.
c        -----------------------------------------
c
          if (TLON(i) .LT. 0.0) TLON(i) = TLON(i) + 360.0
          TLON8 = TLON(i)
          TLAT8 = TLAT(i)
          call gridx(TLON8,xs,delx,iper,xmin,xmax,ic,xc,*10)
          call gridx(TLAT8,ys,dely,   0,ymin,ymax,jc,yc,*10)
c     
c        Store grid, weight, qc info and u,v components 
c        in conventional data buffer (see "sldta.h")
c        ----------------------------------------------
c
          ilc(i)   = ic
          jlc(i)   = jc
          xconv(i) = xc
          yconv(i) = yc
          iconv(i) = itag(i)
          wconv(i) = wfact(i)
          sl_qc(i) = 0
          tdir = DIR(i)*PI/180.
          call winds(uconv(i),vconv(i),SPD(i),tdir,.true.)

  10    continue

        nrep = nrep + NNOBS
      endif

c
c     Perform requested post-processing functions.
c     ============================================
c
      if (superob)    nrep = SL_superob(nptc+1,nptc+nrep)
      if (grosscheck) nrep = SL_grosscheck(nptc+1,nptc+nrep,
     &                                         minspeed,maxspeed)
c
c     Print Sample
c     ============
c
      call SL_print(nptc+1,nptc+nrep,iprint)
      nptc = nptc + nrep
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      call ioclose(iu)
      go to 5

 901  continue
      RCO_rdconv = -1
      go to 999

 902  continue
      if (critical) RCO_rdconv = 2
      print *, 'vam(RCO_rdconv): End of Data'
      print *, 'vam(RCO_rdconv): date/time not found: ',idate,itime
      print *, 'vam(RCO_rdconv): type = ',itype
      go to 999

 903  continue
      if (critical) RCO_rdconv = 3
      print *, 'vam(RCO_rdconv): date/time not found: ',idate,itime
      print *, 'vam(RCO_rdconv): type = ',itype,' unit = ',iu
      go to 999

 904  continue
      RCO_rdconv = -2
      print *, 'vam(RCO_rdconv): error reading namelist.'
      go to 999

 905  continue
      RCO_rdconv = -3
      print *, 'vam(RCO_rdconv): namelist in error: nkx too large.'
      go to 999

 999  return
      end
