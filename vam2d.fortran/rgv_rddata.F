c!#   $Id: rgv_rddata.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: rgv_rddata.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function RGV_rddata(iu,nqnt,qlev,qu,qv,im,jnp,uwind,vwind,rbuf)
c**********************************************************************
c English Name: Read Data  (RGV - Read Grid from V8 Interp)
c -------------
c
c Purpose: Reads in gridded wind field (U and V) from "v8interp"
c -------- data set for the specified date/time.
c
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
c     character*4 qu,qv
c     real qlev,uwind(*),vwind(*)
c     integer RGV_rddata,iu,idate,itime,im,jnp,nqnt
c
c     RGV_rddata (O) - function return value:
c
c          RGV_rddata = 0 (successful)
c                      -1 (unexpected EOF)      
c                      -2 (U component not found)
c                      -3 (V component not found)
c                      -4 (error returned)
c
c     iu (I) - fortran unit number assigned to input "v8interp" data.
c     idate (I) - desired date in YYMMDD format.
c     itime (I) - desired time in HHMMSS format.
c     qlev (I) - designates level to extract wind fields.
c     qu (I) - 4-character mnemonic designating U-component wind field
c              to be extracted.
c     qv (I) - 4-character mnemonic designating V-component wind field
c              to be extracted.
c     im (I) - longitude dimension of wind field.
c     jnp (I) - latitude dimension of wind field.
c     nqnt (I) - number of quantities (records) for current date/time.
c     uwind (O) - U-component wind field.
c     vwind (O) - V-component wind field.
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)
c             (NASA Goddard Space Flight Center)
c
c Modified: January 17, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      real qlev
      character*4 qu,qv
      real*4 rbuf(im*jnp)
      integer iu,nqnt,im,jnp
      real uwind(im*jnp),vwind(im*jnp)
c
c     Local Variables
c     ---------------
c
      real*4 rlev
      character*4 cqnt
      integer i,idim,nrec

      RGV_rddata = 0
      nrec       = nqnt
      idim       = im * jnp
c
c     Locate U-component.
c     ===================
c
      cqnt = '    '
      rlev = -1.0
      do while (nrec .gt. 0 .and. (cqnt .ne. qu .or. rlev .ne. qlev))
         nrec = nrec - 1
         read(iu,end=900,err=900) cqnt,rlev,rbuf
      end do
      if (cqnt .ne. qu .or. rlev .ne. qlev) go to 901

      do i = 1,idim
         uwind(i) = rbuf(i)
      end do
c
c     Locate V-component.
c     ===================
c
      cqnt = '    '
      rlev = -1.0
      do while (nrec .gt. 0 .and. (cqnt .ne. qv .or. rlev .ne. qlev))
         nrec = nrec - 1
         read(iu,end=900,err=900) cqnt,rlev,rbuf
      end do
      if (cqnt .ne. qv .or. rlev .ne. qlev) go to 902

      do i = 1,idim
         vwind(i) = rbuf(i)
      end do
c
c     Skip remaining data.
c     ====================
c
      if (GEN_rdskip(iu,nrec) .ne. 0) go to 998

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      RGV_rddata = -1
      print *, 'vam(RGV_rddata): unexpected EOF'
      go to 999

 901  continue
      RGV_rddata = -2
      print *, 'vam(RGV_rddata): unable to locate "',qu,'"'
      go to 999

 902  continue
      RGV_rddata = -3
      print *, 'vam(RGV_rddata): unable to locate "',qv,'"'
      go to 999
 
 998  continue
      RGV_rddata = -4

 999  return
      end
