c!#   $Id: wco_default.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: wco_default.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function WCO_default(itype,assign_spd,assign_dir,descr)
c**********************************************************************
c English Name: Default (WCO - Write Conventional in OI Repack)
c -------------
c
c Purpose: Sets default attributes for writing data to OI
c -------- repack data set (see "WCO_wrconv").
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
c     character*24 descr
c     integer WCO_default,itype
c     logical assign_spd,assign_dir
c
c     WCO_default (O) - function return value:
c
c          WCO_default =  0 (normal)
c                        -1 (unknown type)
c
c     itype (I) - data type (STANDARD,ERS1) (see vam.h).
c
c     descr (O) - 24-character description.
c
c     assign_spd (O) - flag indicating that the analyzed speed is to be
c                      assigned to each report:
c
c             assign_spd = .true. (assign analyzed speed)
c                         .false. (do not assign speed)
c
c     assign_dir (O) - flag indicating that the analyzed direction is
c                      to be assigned to each report:
c
c             assign_dir = .true. (assign analyzed direction)
c                         .false. (do not assign direction)
c
c Programmer: Joseph V. Ardizzone
c ----------- (Satellite Data Utilization Office)      
c             (NASA Goddard Space Flight Center)
c
c Modified: April 5, 1995 - created.
c ---------
c**********************************************************************
#include "vam.h"
c
c     Argument List
c     -------------
c
      character*24 descr
      integer itype
      logical assign_spd,assign_dir

      WCO_default = 0
c
c     Set attributes based on data type. 
c     ==================================
c
      if (itype .eq. STANDARD) then 

         assign_spd = .false.
         assign_dir = .false.
         descr      = 'VAM CONVENTIONAL        '

      elseif (itype .eq. ERS1) then 

         assign_spd = .true.
         assign_dir = .true.
         descr      = 'VAM ERS1                '

      else

         WCO_default = -1
         print *, 'vam(WCO_default): unknown type: ',itype
         
      endif

      return
      end
