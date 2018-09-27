c!#   $Id: gen_rdskip.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: gen_rdskip.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function GEN_rdskip(iu,nqnt)
c**********************************************************************
c English Name: Skip Data (GEN - Generic)
c -------------
c
c Purpose: Skips specified number of records.
c --------
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
c     integer GEN_rdskip,iu,nqnt
c
c     GEN_rdskip (O) - function return value:
c
c          GEN_rdskip =  0 (normal)
c                       -1 (Unexpected EOF)
c
c     iu (I) - fortran unit number linking input data set.
c     nqnt (I) - number of quantities (records) to skip.
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
      integer iu,nqnt
c
c     Local Variables
c     ---------------
c
      integer nrec

      GEN_rdskip = 0
c
c     Skip remaining data.
c     ====================
c
      nrec = nqnt
      do while (nrec .gt. 0)
         nrec = nrec - 1
         read(iu,end=900,err=900)
      end do

      go to 999
c
c     End of Routine / Error Control
c     ==============================
c
 900  continue
      GEN_rdskip = -1
      print *, 'vam(GEN_rdskip): unexpected EOF'
      go to 999

 999  return
      end
