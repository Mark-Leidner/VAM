c!#   $Id: wbo_wrback.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: wbo_wrback.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WBO_wrback(idate,itime,itype)
c**********************************************************************
c English Name: Write Backscatter (WBO - Write Backscatter in OI Rpk)
c -------------
c
c Purpose: Writes observations stored in VAM backscatter data buffer
c -------- (see "er_bscat.h") to output OI Repack data set.
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
c     integer WBO_wrback,idate,itime
c
c     WBO_wrback (O) - function return value:
c
c          WBO_wrback = -1 (error returned from called subroutine)
c                       -2 (error reading namelist)
c                        0 (normal)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c     itype (I) - data type:
c
c             itype = STANDARD (write analyzed backscatter + winds)
c                     ERS1 (write analyzed winds from backscatter)
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 20, 1995 - created.
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
      integer nrep,inchrs
      real*4 fdate,ftime,ftinc
c
c     Namelist
c     --------
c
      character*24 descr
      integer iu,itag(MAXTAG)
      logical assign_spd,assign_dir
      namelist /input/ iu,itag,assign_spd,assign_dir,descr

      data inchrs/6/

      WBO_wrback = 0
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      iu         = 0
      call isetcon(MAXTAG,itag,ALLTYPE)
      if (WBO_default(itype,assign_spd,assign_dir,descr).ne.0) go to 901

      read(IUNML,input,end=902,err=902)
      write(*,input)
c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     Create OI repack header.
c     ========================
c
      fdate = float(idate)
      ftime = float(itime)
      ftinc = float(inchrs*60)
      write(iu) fdate,ftime,ftinc,descr
c
c     Create OI repack data.
c     ======================
c
      nrep = 0
      if (itype.eq.STANDARD)nrep = WBO_wrdata(iu,itag)
      if (itype.eq.ERS1)nrep = WBO_wrwind(iu,itag,assign_spd,assign_dir)
      print *, 'vam(WBO_wrback): ',nrep,' reports written.'

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WBO_wrback = -1
      go to 999

 902  continue
      WBO_wrback = -2
      print *, 'vam(WBO_wrback): error reading namelist.'
      go to 999

 999  return
      end
