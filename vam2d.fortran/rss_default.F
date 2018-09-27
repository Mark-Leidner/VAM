c!#   $Id: rss_default.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rss_default.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSS_default(itype,speedonly,scfact,alias,iform)
c**********************************************************************
c English Name: Default (RSS - Read SASS from SASS repack)
c -------------
c
c Purpose: Sets default attributes for extracting data from SASS
c -------- repack data set (see "RSS_rdsass").
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
c     real scfact
c     integer RSS_default,itype,iform
c     logical speedonly,alias(4)
c
c     RSS_default (O) - function return value:
c
c          RSS_default =  0 (normal)
c                        -1 (unknown type)
c
c     itype (I) - data type (STANDARD,ERS1,SSMI) (see vam.h).
c
c     speedonly (O) - flag indicating if directional information is to
c                     be ignored:
c
c              speedonly = .true. (directional information ignored).
c                          .false. (directional information saved).
c
c     scfact (O) - wind speed scaling factor for adjusting data to
c                  analysis height ("grid_height" is defined in
c                  "grparm" subroutine):
c
c          scfact = log(grid_height/1.52e-04)/log(DATA_HEIGHT/1.52e-04)
c
c     alias (O) - contains flags for each alias indicating if data is
c                 to be accepted:
c
c              alias(ia) = .true. (alias accepted)
c                          .false. (alias ignored)
c
c     iform (O) - SASS repack buffer type.
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
      real scfact
      integer itype,iform
      logical speedonly,alias(4)
c
c     Local Variables
c     ---------------
c
      character*4 cunit
c
c     Common Blocks
c     -------------
c
#include "grname.h"

      RSS_default = 0
c
c     Set attributes based on data type. 
c     ==================================
c
      if (itype .eq. STANDARD) then 

         iform     = 6
         speedonly = .false.
         scfact    = 1.0
         cunit     = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias

      elseif (itype .eq. SSMI) then 

         iform     = 6
         speedonly = .true.
         cunit     = 'TFFF'
         read(unit=cunit,fmt='(4l1)') alias
         scfact = log(grid_height/1.52e-04)/log(SSMI_HEIGHT/1.52e-04)

      elseif (itype .eq. ERS1) then 

         iform     = 6
         speedonly = .false.
         cunit     = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias
         scfact = log(grid_height/1.52e-04)/log(ERS1_HEIGHT/1.52e-04)

      else

         RSS_default = -1
         print *, 'vam(RSS_default): unknown type: ',itype
         
      endif

      return
      end
