c!#   $Id: rco_default.F,v 1.4 1998/05/05 14:57:16 leidner Exp $
c!#   $Log: rco_default.F,v $
c!#   Revision 1.4  1998/05/05 14:57:16  leidner
c!#   added alias selection to NSCAT winds read from JPL HDF files
c!#
c!#   Revision 1.3  1997/04/11 17:29:48  leidner
c!#   added defaults for NSCAT winds
c!#
c!#	Revision 1.2  1997/02/12  18:26:09  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RCO_default(itype,nkx,kx,scfact,minspeed,maxspeed,
     &                      superob,grosscheck,alias,minprob,nthin)
c**********************************************************************
c English Name: Default (RCO - Read Conventional from OI repack)
c -------------
c
c Purpose: Sets default attributes for extracting data from OI
c -------- repack data set (see "RCO_rdconv").
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
c     logical superob,grosscheck,alias(4)
c     real scfact,minspeed,maxspeed,minprob
c     integer RCO_default,itype,nkx,kx(*),nthin
c
c     RCO_default (O) - function return value:
c
c          RCO_default =  0 (normal)
c                        -1 (unknown type)
c
c     itype (I) - data type (STANDARD,ERS1,...) (see vam.h).
c     nkx (O) - number of observation types.
c     kx (O) - OI repack observation types.
c     scfact (O) - wind speed scaling factors for adjusting data to
c                  analysis height ("grid_height" is defined in
c                  "grparm" subroutine):
c
c        scfact(ikx)=log(grid_height/1.52e-04)/log(DATA_HEIGHT/1.52e-04)
c
c     grosscheck (O) - flag signifying if gross quality control check
c                      is to be performed (see "minspeed","maxspeed").
c     superob (O) - flag signifying if multiple observations at the
c                   same location having the same type are to be 
c                   averaged together.
c
c     alias (O) - contains flags for each alias indicating if data is
c                 to be accepted:
c
c              alias(ia) = .true. (alias accepted)
c                          .false. (alias ignored)
c
c     minprob (O) - minimum probability of correctness for accepting
c                   alias (%).
c
c     minspeed (O) - minimum wind speed threshold for accepting reports.
c
c                      speed <= minspeed (report eliminated)
c
c     maxspeed (O) - maximum wind speed threshold for accepting reports.
c
c                      speed >= maxspeed (report eliminated)
c
c     nthin - specifies how to thin (reduce) the data:
c
c                    nthin = 1 (every report used; every swath)
c                            2 (every other report used> 19,17,15,...
c                               from every other swath)
c                            .               .
c                            .               .
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
      integer itype,nkx,kx(*),nthin
      logical superob,grosscheck,alias(4)
      real scfact(*),minspeed,maxspeed,minprob
c
c     Local Variables
c     ---------------
c
      integer i
      character*10 cunit
c
c     Common Blocks
c     -------------
c
#include "grname.h"
c
c     Set attributes based on data type. 
c     ==================================
c
      RCO_default = 0
      call setcon(MAXKX,scfact,1.0)

      if (itype .eq. STANDARD) then 

         nkx = 4
         cunit='3456'
         read(unit=cunit,fmt='(4i1)') (kx(i),i=1,4)
         scfact(3)=log(grid_height/1.52e-04)/log(SHIP_HEIGHT/1.52e-04)
         scfact(4)=log(grid_height/1.52e-04)/log(SHIP_HEIGHT/1.52e-04)
         scfact(5)=log(grid_height/1.52e-04)/log(BUOY_HEIGHT/1.52e-04)
         scfact(6)=log(grid_height/1.52e-04)/log(BUOY_HEIGHT/1.52e-04)

         minspeed   = -1.0
         maxspeed   = 30.0
         grosscheck = .true.

         superob    = .true.

      elseif (itype .eq. ERS1) then 

         nkx = 1
         kx(1) = 28
         scfact(28)=log(grid_height/1.52e-04)/log(ERS1_HEIGHT/1.52e-04)
        
         minspeed   = -1.0
         maxspeed   = 999.0
         grosscheck = .false.
         superob    = .false.

      elseif (itype .eq. UKMO) then 

         nthin     = 3
         cunit     = 'TFFF'
         minprob   = 0.0
         read(unit=cunit,fmt='(4l1)') alias
         scfact(1)=log(grid_height/1.52e-04)/log(ERS1_HEIGHT/1.52e-04)
        
         minprob    = 0.0   
         minspeed   = -1.0
         maxspeed   = 999.0
         grosscheck = .false.
         superob    = .false.

      elseif (itype .eq. NSCAT_WINDS) then 

         cunit     = 'TTTT'
         read(unit=cunit,fmt='(4l1)') alias
         minspeed   = -1.0
         maxspeed   = 999.0
         grosscheck = .false.
         superob    = .false.

      else

         RCO_default = -1
         print *, 'vam(RCO_default): unknown type: ',itype
         
      endif

      return
      end
