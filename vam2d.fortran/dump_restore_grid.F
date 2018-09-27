c!#   $Id: dump_restore_grid.F,v 1.3 1998/04/28 21:01:38 leidner Exp $
c!#   $Log: dump_restore_grid.F,v $
c!#   Revision 1.3  1998/04/28 21:01:38  leidner
c!#   allow for grid periodicity around GM
c!#
c!#   Revision 1.2  1997/04/08 19:35:03  leidner
c!#   replaced all occurrences of arguement dump_restore with
c!#   dump_restore_grid
c!#
c!#	Revision 1.1  1997/02/18  20:36:59  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function dump_restore_grid (idate,itime,dump)
c**********************************************************************
c English Name: Dumps or restores a gridded U,V analysis
c -------------
c
c Purpose: Writes/reads U and V gridded wind fields from VAM data buffer
c -------- (see "grdta.h") to output data set in native format.
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
c     integer dump_restore,idate,itime
c
c     dump_restore (O) - function return value:
c
c          dump_restore =  0 (normal)
c                         -1 (error returned opening data set)
c                         -2 (error reading namelist)
c                         -3 (I/O error)
c                         -4 (grid parameter mismatch)
c
c     idate (I) - date in yymmdd format.
c     itime (I) - time in hhmmss format.
c     dump (L)  - dump or restore
c
c Programmer: Ross N Hoffman
c ----------- (AER, Inc.)
c
c Modified: Dec 5, 1996 (created)
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer dump_restore_grid,idate,itime
      logical dump
c
c     Common Blocks
c     -------------
c
#include "gparm.h"
#include "grdta.h"
c
c     Namelist
c     --------
c
      integer iu
      namelist /input/ iu
c
c     Retrieve namelist parameters / initialize
c     =========================================
c
      dump_restore_grid = 0

      read(IUNML,input,end=902,err=902)
      write(*,input)

c
c     Establish I/O link
c     ==================
c
      if (ioopen(iu) .ne. 0) go to 901
      if (dump) then
c
c     Write header from gparm
c     =======================
c
      write(iu,err=902) idate,itime,nx,xs,delx,ny,ys,dely
      print *, 'vam(dump_restore): writing header'
      print *, idate,itime,nx,xs,delx,ny,ys,dely
c
c     Write data from grdta (u, then v)
c     =================================
c     
      write(iu,err=902) ((u(i,j),i=1,nx),j=1,ny)
      write(iu,err=902) ((v(i,j),i=1,nx),j=1,ny)
c
      else
c
c     Read header and compare to gparm
c     ================================
c
      read(iu,end=902,err=902) idate0,itime0,nx0,xs0,delx0,ny0,ys0,dely0
c !   allow for 1-gridlength periodicity around GM
c !   else convert W lon to E lon
      if (xs0.lt.(-delx0)) xs0=xs0+360. 
      print *, 'vam(dump_restore): request'
      print *, idate,itime,nx,xs,delx,ny,ys,dely
      print *, 'vam(dump_restore): reading header'
      print *, idate0,itime0,nx0,xs0,delx0,ny0,ys0,dely0
      if (idate .ne. idate0 .or. itime .ne. itime0 .or.
     &    nx .ne. nx0 .or. ny .ne. ny0 .or.
     &    xs .ne. xs0 .or. ys .ne. ys0 .or.
     &    delx .ne. delx0 .or. dely .ne. dely0) go to 904
c
c     Read data to grdta (u, then v)
c     =================================
c     
      read(iu,end=902,err=902) ((u(i,j),i=1,nx),j=1,ny)
      read(iu,end=902,err=902) ((v(i,j),i=1,nx),j=1,ny)
c
      endif
c
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 901  continue
      dump_restore_grid = -1
      go to 999

 902  continue
      dump_restore_grid = -2
      print *, 'vam(dump_restore): error reading namelist'
      go to 999

 903  continue
      dump_restore_grid = -3
      print *, 'vam(dump_restore): I/O error'
      go to 999

  904 continue
      dump_restore_grid = -4
      print *, 'vam(dump_restore): grid parameters mismatch'
      go to 999

 999  return
      end
