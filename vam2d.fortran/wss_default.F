c!#   $Id: wss_default.F,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: wss_default.F,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision
c!#
c**********************************************************************
      function WSS_default(itype,iform,alias,assign_spd,assign_dir,
     &                                                           descr)
c**********************************************************************
c English Name: Default (WSS - Write SASS in SASS Repack)
c -------------
c
c Purpose: Sets default attributes for writing data to SASS 
c -------- repack data set (see "WSS_wrsass").
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
c     character*16 descr
c     integer WSS_default,itype,iform
c     logical alias(*),assign_spd,assign_dir
c
c     WSS_default (O) - function return value:
c
c          WSS_default =  0 (normal)
c                        -1 (unknown type)
c
c     itype (I) - data type (STANDARD,ERS1,SSMI) (see vam.h).
c
c     alias (O) - contains flags for each alias indicating if data is
c                 to be accepted:
c
c              alias(ia) = .true. (alias accepted)
c                          .false. (alias ignored)
c
c     iform (O) - SASS repack buffer type.
c
c     descr (O) - 16-character description.
c
c     assign_spd (O) - flag indicating that the analyzed speed is to be
c                      assigned to each alias:
c
c             assign_spd = .true. (assign analyzed speed)
c                         .false. (do not assign speed)
c
c     assign_dir (O) - flag indicating that the analyzed direction is
c                      to be assigned to each alias:
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
      character*16 descr
      integer itype,iform
      logical alias(4),assign_spd,assign_dir
c
c     Local Variables
c     ---------------
c
      character*4 cunit

      WSS_default = 0
c
c     Set attributes based on data type. 
c     ==================================
c
      if (itype .eq. STANDARD) then 

         iform      = 6
         assign_spd = .false.
         assign_dir = .false.
         descr      = 'VAM SEASAT      '
         cunit      = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias

      elseif (itype .eq. SSMI) then 

         iform      = 6
         assign_spd = .false.
         assign_dir = .true.
         descr      = 'VAM SSMI        '
         cunit      = 'TFFF'
         read(unit=cunit,fmt='(4l1)') alias

      elseif (itype .eq. ERS1) then 

         iform      = 6
         assign_spd = .false.
         assign_dir = .false.
         descr      = 'VAM ERS1        '
         cunit      = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias

      else

         WSS_default = -1
         print *, 'vam(WSS_default): unknown type: ',itype
         
      endif

      return
      end
