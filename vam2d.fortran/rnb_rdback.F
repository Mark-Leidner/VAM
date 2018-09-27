
c!#   CSU IDENTIFICATION : rnb_rdback
c!#   $Id: rnb_rdback.F,v 1.11 1998/05/15 21:11:39 stanr Exp $

c!##  PURPOSE : Read NSCAT backscatter from Level 1.7 JPL HDF files.

c!#   NSCAT backscatter data and ephemiris are read directly from JPL
c!#   HDF files.  Data important for the vam are stored in the NSCAT
c!#   data structure (see "ns_bscat.h").

c!#   CSU SPECIFICATION AND CONSTRAINTS :

c!##  REQUIREMENTS :

c!##  CONSTRAINTS :

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c     ------------------------------------------------------------------

c!##  INPUT/OUTPUT INTERFACE :

      function rnb_rdback !#
     I     ( idate, itime, itype ) !#

c!#   Input:
c!#~   idate     Current date in YYMMDD format
c!#~   itime     Current time in HHMMSS format
c!#~   itype     Data type = STANDARD (standard backscatter data)
      integer idate, itime, itype !#

c!#   Output:
c!#~   rnb_rdback Function return value (<0 signals error)
c                   = -2 (error reading namelist)
c                   = -1 (error returned from called subroutine)
c                   =  0 (normal)
c     integer rnb_rdback !#

c!## DATA CONVERSION :

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: rnb_rdback.F,v $
c!#	Revision 1.11  1998/05/15 21:11:39  stanr
c!#	comment out function type declaration (also is in vam.h)
c!#	
c!#	Revision 1.10  1998/03/18 16:29:56  leidner
c!#	added rain flagging QC option for NSCAT data
c!#
c!#	Revision 1.9  1998/01/27 15:59:54  leidner
c!#	updated documentation
c!#

c!## GLOBAL AND SHARED DATA :

c!#   Common block /vam/ defines vam parameters and functions.
#include "vam.h"

c!#   Common block /ns_bscat/ holds NSCAT backscatter data.
#include "ns_bscat.h"

c!#   Common block /ns_bscat/ holds NSCAT winds data.
#include "ns_winds.h"

c!#   Common blocks /sds_info/, /more_sdsinfo/ and /nscatrec/ hold
c!#   variables for reading L17 NSCAT HDF data.
#include "rdglob17.h"

c!#   Common block /gparm/ describes grid parameters.
#include "gparm.h"

c!#   Common block /grdta/ describes grid data.
#include "grdta.h"

c!## LOCAL DATA ELEMENTS :

c!# Integer:
c!#~   i, n      Counters
      integer i, n

c!## LOCAL DATA STRUCTURES :

c!#   Namelist /input/ defines parameters for reading L17 NSCAT HDF
c!#   data files.
#include "namelist_rnb_rdback.h"

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c     ------------------------------------------------------------------

c!#   1. Initialize variables

      rnb_rdback = 0
      do i=1,NS_MAXREVS
         revs(i)= 0
         ns_nWVC_rev(i) = 0
      enddo

c     ------------------------------------------------------------------

c!#   2. Retrieve namelist parameters

c!#   2.1 Defaults

      iprint   = 0
      itag     = 0
      timwin   = 3.
      critical = .false.
      tb_min = 240.
      al_max = 62.
      datawin_prints = .false.
      sig0qual_prints = .false.
      if ( RBO_default(itype, dbmin, dbmax, s0min, s0max,
     &     s0min_edit, minsig0, nthin ) .ne. 0) go to 901

c!#   2.2 Alter defaults via input namelist
c!#   (see "namelist_rnb_rdback.h")

      read(IUNML, input, end=902, err=902)
      write( *, input)

c     ------------------------------------------------------------------

c!#   3. Read in NSCAT sigma0 data from Level 1.7 HDF files

      do n=1,nfiles
         call rdHDF17F(n,filenames(n),s0min,s0max,s0min_edit,itag,
     &        idate,itime,timwin,datawin_prints,sig0qual_prints)

         ns_nWVC_rev(n)=ns_nWVC
         if (n.gt.1) then
            do i=1,n-1
               ns_nWVC_rev(n) = ns_nWVC_rev(n)-ns_nWVC_rev(i)
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

      enddo

c     ------------------------------------------------------------------

c!#   4. QC for rain contamination (if requested)

      if (rainqc) then

c     Dummy check for GOES data needed for rain flagging

         if (nsw_nwvc.le.0) then
            write(*,*) 'Cannot perform sigma0 rain flagging without',
     &           ' reading Level 2.0 HDF files first.  Stopping...'
            stop 'error in rnb_rdback.F'
         endif

         call flag_rain_sig(tb_min, al_max)

      endif

c     ------------------------------------------------------------------

c!#   5. Print sample

      call NS_print(1, ns_nobs, iprint)
      
      go to 999

c     ------------------------------------------------------------------

c!## ERROR HANDLING :

 900  continue

 901  continue
      print *, 'vam(rnb_rdback): error setting namelist defaults.'
      rnb_rdback = -1
      go to 999

 902  continue
      rnb_rdback = -2
      print *, 'vam(rnb_rdback): error reading namelist.'
      go to 999

 999  return
      end

