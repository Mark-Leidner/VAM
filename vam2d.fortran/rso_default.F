c!#   $Id: rso_default.F,v 1.2 1997/02/12 18:26:09 leidner Exp $
c!#   $Log: rso_default.F,v $
c!#   Revision 1.2  1997/02/12 18:26:09  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RSO_default(itype,speedonly,scfact,alias,dbmin,dbmax,
     &                               minsig0,minprob,nthin,kx,dirinc)
c**********************************************************************
c English Name: Default (RSO - Read SASS from OI Repack)
c -------------
c
c Purpose: Sets default attributes for extracting data from Backscatter
c -------- repack data set (see "RSO_rdsass").
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
c     logical speedonly,alias(4)
c     real dbmin,dbmax,minprob,scfact,dirinc(4)
c     integer RSO_default,itype,nthin,minsig0,kx
c
c     RSO_default (O) - function return value:
c
c          RSO_default =  0 (normal)
c                        -1 (unknown type)
c
c     itype (I) - data type (BACKSCATTER,ERS1,SSMI,...) (see vam.h).
c     speedonly (O) - flag indicating if directional information is to
c                     be ignored:
c
c              speedonly = .true. (directional information ignored).
c                          .false. (directional information saved).
c
c     scfact (O) - wind speed scaling factor for adjusting data to
c                  analysis height.
c
c     alias (O) - contains flags for each alias indicating if data is
c                 to be accepted:
c
c              alias(ia) = .true. (alias accepted)
c                          .false. (alias ignored)
c
c     dbmin (O) - minimum acceptable value for s0 (DB)
c     dbmax (O) - maximum acceptable value for s0 (DB)
c     minsig0 (O) - minimum number of valid s0's needed to accept 
c                   the report.
c     minprob (O) - minimum probability of correctness for accepting
c                   alias (%).
c     nthin (O) - specifies how to thin (reduce) the data:
c
c                    nthin = 1 (every report used; every swath)
c                            2 (every other report used> 19,17,15,...
c                               from every other swath)
c                            .               .
c                            .               .
c
c     kx (O) - OI Repack data type index designating which reports are
c              to be accepted.
c
c     dirinc (O) - directional increment for each alias. For each 
c                  desired alias, this is the clockwise increment from
c                  the primary direction read from the data set (i.e.
c                  there is only one direction stored in an OI repack
c                  surface buffer). Essentially, this allows one to
c                  bogus aliases to create ambiguous winds:
c
c                  dirinc = 0,180.0; alias = T,T,F,F
c  
c                  This creates two aliases in opposite directions, the
c                  first alias being the extracted wind direction and
c                  speed with the second having the same speed but 180
c                  degree opposite direction.
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
      logical speedonly,alias(4)
      integer itype,nthin,minsig0,kx
      real dbmin,dbmax,minprob,scfact,dirinc(4)
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

      RSO_default = 0
      call setcon(4,dirinc,0.0)
c
c     Set attributes based on data type. 
c     ==================================
c
      if (itype .eq. BACKSCATTER) then 

         dbmin       = -50.0
         dbmax       = 20.0
         minsig0     = 1
         nthin       = 3
         minprob     = 0.0
         speedonly = .false.
         cunit     = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias
         scfact = log(grid_height/1.52e-04)/log(BACK_HEIGHT/1.52e-04)

      elseif (itype .eq. ERS1) then 

         kx          = 28
         dirinc(2) = 180.0
         speedonly = .false.
         cunit     = 'TTFF'
         read(unit=cunit,fmt='(4l1)') alias
         scfact = log(grid_height/1.52e-04)/log(ERS1_HEIGHT/1.52e-04)

      elseif (itype .eq. SSMI) then 

         kx          = 28
         speedonly = .true.
         cunit     = 'TFFF'
         read(unit=cunit,fmt='(4l1)') alias
         scfact = log(grid_height/1.52e-04)/log(SSMI_HEIGHT/1.52e-04)

      elseif (itype .eq. UKMO) then 

         nthin       = 3
         minprob     = 0.0
         speedonly = .false.
         cunit     = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias
         scfact = log(grid_height/1.52e-04)/log(ERS1_HEIGHT/1.52e-04)
         
      else

         RSO_default = -1
         print *, 'vam(RSO_default): unknown type: ',itype
         
      endif

      return
      end
