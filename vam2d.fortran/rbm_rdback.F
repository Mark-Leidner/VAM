
c!#   CSU IDENTIFICATION : rbm_rdback
c!#   $Id: rbm_rdback.F,v 1.3 1998/09/22 19:24:24 leidner Exp $

c!##  PURPOSE : Read NSCAT backscatter from 25km merged files

c!#   NSCAT backscatter data and ephemiris are read directly
c!#   from Merged Geophysical Data Record (MGDR) files.  Data
c!#   important for the vam are stored in the NSCAT data
c!#   structures (see "ns_bscat.h", "ns_winds.h" and "ptdta.h").

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      function rbm_rdback !#
     I     ( idate, itime, itype ) !#

c!#   Input:
c!#~   idate     Current date in YYMMDD format
c!#~   itime     Current time in HHMMSS format
c!#~   itype     Data type = STANDARD (standard backscatter data)
      integer idate, itime, itype !#

c!#   Output:
c!#~   rbm_rdback Function return value (<0 signals error)
c                   = -2 (error reading namelist)
c                   = -1 (error returned from called subroutine)
c                   =  0 (normal)
      integer rbm_rdback !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: rbm_rdback.F,v $
c!#	Revision 1.3  1998/09/22 19:24:24  leidner
c!#	added data thinning of NSCAT WVC's
c!#	
c!#	Revision 1.2  1998/05/20 14:25:12  leidner
c!#	changed namelist logical rank_amb -> rank_by_JPL
c!#
c!#	Revision 1.1  1998/05/05 15:01:44  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!#   Common block /vam/ defines vam parameters and functions.
#include "vam.h"

c!#   Common block /ns_bscat/ holds NSCAT backscatter data.
#include "ns_bscat.h"

c!#   Common block /ns_bscat/ holds NSCAT winds data.
#include "ns_winds.h"

c!#   Common block /ptdta/ holds ambiguous winds data.
#include "ptdta.h"

c!#   Common blocks for reading 25 km MGDR records.
#include "l20param.h"
#include "mgdr_out.h"
#include "mgdr_com.h"
#include "l20files.h"

c!#   Common block /gparm/ describes grid parameters.
#include "gparm.h"

c!#   Common block /grdta/ describes grid data.
#include "grdta.h"

c!## LOCAL DATA ELEMENTS :

c!# Integer:
c!#~   i, n      Counters
      integer i, n

c!## LOCAL DATA STRUCTURES :

c!#   Namelist /input/ defines parameters for reading NSCAT mgdr
c!#   data files.

c!#~   critical  Is data set designated as essential?
c!#~   datawin_prints   Flag for prints of data time/location info
c!#~   dbmin     Minimum acceptable value for s0 (dB)
c!#~   dbmax     Maximum acceptable value for s0 (dB)
c!#~   filenames File names of NSCAT mgdr data to be read
c!#~   iprint    Print interval for NSCAT sig0 data
c!#~   itag      Tag useful for identifying different types of data
c!#~   minsig0   Minimum number of valid s0's needed to accept the report
c!#~   MAXREVS   Maximum no. of revs (see vam.h)
c!#~   nfiles    Number of NSCAT mgdr files to read
c!#~   nthin     Specifies how to thin (reduce) the data:
c!#~   nthin.     = 1 (every report used)
c!#~   nthin..    = 2 (every other report used)
c!#    nthin...   note: thins only those WVC's which fall within time
c!#    nthin....  and space windowing, and pass all QC checks
c!#~   rank_by_JPL    logical; rank ambiguities as in JPL HDF files
c!#~   rank_by_JPL.   (pos. 1 - selected ambiguity; remaining positions
c!#~   rank_by_JPL..  filled in order of decreasing mle)
c!#~   s0min     Minimum acceptable value for s0 (linear)
c!#~   s0max     Maximum acceptable value for s0 (linear)
c!#~   s0min_edit   Data edit value for s0obs:
c!#~   s0min_edit.  If  s0min < s0obs < s0min_edit, then s0obs = s0min_edit
c!#~   sig0qual_prints  Flag for prints of sig0 qc info
c!#~   timwin    Time window for data acceptance (+/- timwin)
      integer minsig0, nthin, iprint, itag, nfiles
      real dbmin, dbmax, s0min, s0max, s0min_edit, timwin
      logical critical, datawin_prints, sig0qual_prints, alias(4),
     &     rank_by_JPL
      character*80 filenames(NS_MAXREVS)

      namelist /input/ minsig0, nthin, iprint, itag, nfiles,
     &    dbmin, dbmax, s0min, s0max, s0min_edit, timwin, 
     &    critical, datawin_prints, sig0qual_prints, filenames,
     &    alias, rank_by_JPL

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c     ------------------------------------------------------------------

c!#   1. Initialize variables

      rbm_rdback = 0
      do i=1,NS_MAXREVS
         revs(i)= 0
         ns_nWVC_rev(i) = 0
      enddo

c     ------------------------------------------------------------------

c!#   2. Retrieve namelist parameters

c!#   2.1 Defaults

      lun20    = 67
      iprint   = 0
      itag     = 0
      timwin   = 3.
      critical = .false.
      datawin_prints = .false.
      sig0qual_prints = .false.
      alias(1) = .true.
      alias(2) = .true.
      alias(3) = .true.
      alias(4) = .true.
      rank_by_JPL = .false.
      if ( RBO_default(itype, dbmin, dbmax, s0min, s0max,
     &     s0min_edit, minsig0, nthin ) .ne. 0) go to 901

c!#   2.2 Alter defaults via input namelist

      read(IUNML, input, end=902, err=902)
      write( *, input)

c     ------------------------------------------------------------------

c!#   3. Read NSCAT data from Merged Geophysical Data Record (MGDR) files

      do n=1,nfiles
         call rbm_stdata(n,filenames(n),s0min,s0max,s0min_edit,itag,
     &        idate,itime,timwin,datawin_prints,sig0qual_prints,
     &        alias,rank_by_JPL, nthin)

         revs(n) = Revnum
         nsw_revs(n) = Revnum

         ns_nWVC_rev(n)=ns_nWVC
         nsw_nwvc_rev(n)=nsw_nwvc
         if (n.gt.1) then
            do i=1,n-1
               ns_nWVC_rev(n) = ns_nWVC_rev(n)-ns_nWVC_rev(i)
               nsw_nwvc_rev(n) = nsw_nwvc_rev(n)-nsw_nwvc_rev(i)
            enddo
         endif

         write(*, 95) ns_nobs
         write(*,100) ns_nWVC_rev(n)
         write(*,105) (ns_nWVC_rev(i),i=1,n)
         write(*,110) ns_nwvc
 95      format('Number of stored sigma0s:',24x,i6)
100      format('Number of WVC this rev:',26x,i6)
105      format('Number of WVC by rev:',28x,15(i6))
110      format('Total number of WVC for all revs:',16x,i6)

         write(*,200) nsw_nwvc_rev(n)
         write(*,205) (nsw_nwvc_rev(i),i=1,n)
         write(*,210) nsw_nwvc
200      format(/'Number of WVC this rev:',26x,i6)
205      format('Number of WVC by rev:',28x,15(i6))
210      format('Total number of WVC for all revs:',16x,i6)

      enddo

c     ------------------------------------------------------------------

c!#   4. Print sample

      call NS_print(1, ns_nobs, iprint)  ! ns_bscat data buffer
      call NSW_print(1, nsw_nwvc, iprint)  ! ns_winds data buffer
      call pt_print(1,NPTS,iprint)  ! ambiguous winds data buffer

      go to 999

c     ------------------------------------------------------------------

c!## ERROR HANDLING :

 900  continue

 901  continue
      print *, 'vam(rmb_rdback): error setting namelist defaults.'
      rmb_rdback = -1
      go to 999

 902  continue
      rmb_rdback = -2
      print *, 'vam(rmb_rdback): error reading namelist.'
      go to 999

 999  return
      end

