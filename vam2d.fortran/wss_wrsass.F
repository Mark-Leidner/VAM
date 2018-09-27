c!#   $Id: wss_wrsass.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: wss_wrsass.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function WSS_wrsass(idate,itime,itype)
c**********************************************************************
c English Name: Write SASS (WSS - Write SASS in SASS Repack)
c -------------
c
c Purpose: Writes observations stored in VAM SASS data buffer (see
c -------- "ptdta.h") to output SASS Repack data set.
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
c     integer WSS_wrsass,idate,itime
c
c     WSS_wrsass (O) - function return value:
c
c          WSS_wrsass = -1 (error returned from called subroutine)
c                       -2 (error reading namelist)
c                        0 (normal)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
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
c Modified: April 12, 1995 - created.
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
      real*4 rtau
      integer nrep
c
c     Common Blocks
c     -------------
c
#include "ptdta.h"
c
c     Namelist
c     --------
c
      character*16 descr
      integer iu,itag(MAXTAG),iform
      logical assign_spd,assign_dir,alias(4)
      namelist /input/ iu,itag,iform,alias,assign_spd,assign_dir,descr

      WSS_wrsass = 0
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      iu         = 0
      call isetcon(MAXTAG,itag,ALLTYPE)
      if (WSS_default(itype,iform,alias,assign_spd,assign_dir,
     &                                        descr) .ne. 0) go to 901

      read(IUNML,input,end=902,err=902)
      write(*,input)
c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     Create SASS repack header.
c     ==========================
c
      rtau = date2tau(idate,itime)
      write(iu) rtau,descr,'00000000'
c
c     Create SASS repack data.
c     ========================
c
      nrep = WSS_wrdata(iu,iform,itag,alias,assign_spd,assign_dir)
      print *, 'vam(WSS_wrsass): ',nrep,' reports written.'

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      WSS_wrsass = -1
      go to 999

 902  continue
      WSS_wrsass = -2
      print *, 'vam(WSS_wrsass): error reading namelist.'
      go to 999

 999  return
      end
