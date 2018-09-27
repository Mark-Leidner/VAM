c!#   $Id: rbo_default.F,v 1.5 1998/01/26 16:46:55 leidner Exp $
c!#   $Log: rbo_default.F,v $
c!#   Revision 1.5  1998/01/26 16:46:55  leidner
c!#   added polynomial Kp model as default ers1 Kp
c!#
c!#   Revision 1.4  1997/08/05 18:54:09  leidner
c!#   changed s0min default for NSCAT data to exclude large negative sigma0's
c!#
c!#	Revision 1.3  1997/06/23  17:36:28  leidner
c!#	added defaults for sig0 QC in linear space (s0min,s0max,s0min_edit)
c!#
c!#	Revision 1.2  1997/04/11  17:40:11  leidner
c!#	added STANDARD_NSCAT defaults
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
c**********************************************************************
      function RBO_default
     I    ( itype,
     O    dbmin, dbmax, s0min, s0max, s0min_edit, minsig0, nthin,
     O     iKpType)
c**********************************************************************
c English Name: Default (RBO - Read Backscatter from OI Repack)
c -------------
c
c Purpose: Sets default attributes for extracting data from Backscatter
c -------- repack data set (see "RBO_rdback").
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
c     real dbmin,dbmax
c     integer RBO_default,itype,nthin,minsig0
c
c     RBO_default (O) - function return value:
c
c          RBO_default =  0 (normal)
c                        -1 (unknown type)
c
c     itype (I) - data type (STANDARD) (see vam.h).
c     dbmin (O) - minimum acceptable value for s0 (DB)
c     dbmax (O) - maximum acceptable value for s0 (DB)
c     s0min (O) - minimum acceptable value for s0 (linear)
c     s0max (O) - maximum acceptable value for s0 (linear)
c     s0min_edit(O)-  data edit value for s0obs:
c                     if  s0min < s0obs < s0min_edit, then set s0obs equal
c                     to s0min_edit
c     minsig0 (O) - minimum number of valid s0's needed to accept 
c                   the report.
c     nthin (O) - specifies how to thin (reduce) the data:
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
      real dbmin,dbmax
      integer itype,nthin,minsig0

      RBO_default = 0
c
c     Set attributes based on data type. 
c     ==================================
c
      if (itype .eq. STANDARD) then 

         dbmin   = -50.0
         dbmax   = 20.0
         s0min   = -1.e+3
         s0max   = 1.e+3
         s0min_edit = 1.e-6
         minsig0 = 1
         nthin   = 3
         iKpType = 2

      elseif (itype .eq. STANDARD_NSCAT) then 

         dbmin   = -70.0
         dbmax   = 30.0
         s0min   = -1.e-3
         s0max   = 1.e+3
         s0min_edit = 1.e-6
         minsig0 = 1
         nthin   = 1

      else

         RBO_default = -1
         print *, 'vam(RBO_default): unknown type: ',itype
         
      endif

      return
      end
