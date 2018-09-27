c!#   $Id: rpk_rdskip.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: rpk_rdskip.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function RPK_rdskip(iu,itype)
c**********************************************************************
c English Name: Skip Data (RPK - Repack)
c -------------
c
c Purpose: Skips records on Repack data set until desired buffer type
c -------- is located.
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
c     integer RPK_rdskip,iu,itype
c
c     RPK_rdskip (O) - function return value:
c
c          RPK_rdskip = >0 (number of records for located buffer type)
c                       -1 (Unexpected EOF)
c                       -2 (Unable to locate desired buffer type)
c
c     iu (I) - fortran unit number linking input data set.
c     itype (I) - desired buffer type:
c 
c            itype = 0 (skip remaining records for current date/time).
c                 != 0 (skip records up to buffer type).
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 3, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      integer iu,itype
c
c     Local Variables
c     ---------------
c
      integer i,nform,nbuff
      real*4 form,buff,rdum
c
c     Skip data / Locate desired buffer type.
c     =======================================
c
      read(iu,end=900,err=900) form,buff
      nform = form
      nbuff = buff

      do while (nform .ne. 0 .and. nform .ne. itype)

         do i = 1,nbuff
            read(iu,end=900,err=900) rdum
         end do

         read(iu,end=900,err=900) form,buff
         nform = form
         nbuff = buff

      end do

      if (nform .ne. itype) go to 901
      RPK_rdskip = nbuff
      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RPK_rdskip = -1
      print *, 'vam(RPK_rdskip): unexpected EOF'
      go to 999

 901  continue
      RPK_rdskip = -2
      print *, 'vam(RPK_rdskip): unable to locate buffer type: ',itype
      go to 999

 999  return
      end
