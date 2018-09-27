c!#   $Id: wbo_wrnback.F,v 1.1 1997/02/21 23:45:10 leidner Exp $
c!#   $Log: wbo_wrnback.F,v $
c!#   Revision 1.1  1997/02/21 23:45:10  leidner
c!#   Initial revision
c!#

c**********************************************************************
      function WBO_wrnback(idate,itime,itype)
c**********************************************************************
c English Name: Write Backscatter (WBO - Write Backscatter in OI Rpk)
c -------------
c
c Purpose: Writes observations stored in VAM backscatter data buffer
c -------- (see "ns_bscat.h") to output OI Repack data set.
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
c     integer WBO_wrnback,idate,itime
c
c     WBO_wrnback (O) - function return value:
c
c          WBO_wrnback = -1 (error returned from called subroutine)
c                        -2 (error reading namelist)
c                         0 (normal)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c     itype (I) - data type:
c
c             itype = STANDARD_NSCAT (write analyzed backscatter + winds)
c                     NSCAT_WINDS (write analyzed winds from backscatter)
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

      WBO_wrnback = 0
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
c     if (itype.eq.STANDARD_NSCAT) nrep = WBO_wrndata(iu,itag)
      if (itype.eq.NSCAT_WINDS)
     &    nrep = WBO_wrnwind(iu,itag,assign_spd,assign_dir)
      print *, 'vam(WBO_wrnback): ',nrep,' reports written.'

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WBO_wrnback = -1
      go to 999

 902  continue
      WBO_wrnback = -2
      print *, 'vam(WBO_wrnback): error reading namelist.'
      go to 999

 999  return
      end
