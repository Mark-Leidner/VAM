
c!#   CSU IDENTIFICATION : namelist_rnb_rdback
c!#   $Id: namelist_rnb_rdback.h,v 1.4 1998/03/18 16:29:56 leidner Exp $

c!##  PURPOSE : Defines namelist which controls reading NSCAT backscatter data.

c!#   CSU SPECIFICATION AND CONSTRAINTS:

c!##  LANGUAGE : Fortran

c!#   CSU DESIGN :

c!##  REFERENCES :

c!##  LIMITATIONS :

c!##  CHANGE LOG : 
c!#   $Log: namelist_rnb_rdback.h,v $
c!#   Revision 1.4  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.3  1998/01/27 15:59:54  leidner
c!#   updated documentation
c!#
c!#   Revision 1.2  1998/01/26 16:48:17  leidner
c!#   removed Kp calculations; controlled by sig0err
c!#
c!#   Revision 1.1  1997/06/23 16:37:06  leidner
c!#   Initial revision
c!#

c     ------------------------------------------------------------------

c!##  GLOBAL AND SHARED DATA :

c!#~   al_max    Threshold GOES albedo for rain flagging
c!#~   critical  Is data set designated as essential?
c!#~   datawin_prints   Flag for prints of data time/location info
c!#~   dbmin     Minimum acceptable value for s0 (dB)
c!#~   dbmax     Maximum acceptable value for s0 (dB)
c!#~   filenames File names of NSCAT L17 data to be read
c!#~   iprint    Print interval for NSCAT sig0 data
c!#~   itag      Tag useful for identifying different types of data
c!#~   minsig0   Minimum number of valid s0's needed to accept the report
c!#~   MAXREVS   Maximum no. of revs (see vam.h)
c!#~   nfiles    Number of NSCAT L17 files to read
c!#~   nthin     Specifies how to thin (reduce) the data:
c!#~   nthin.     = 1 (every report used)
c!#~   nthin..    = 2 (every other report used)
c!#~   rainqc    Logical; flag sigma0 data for rain contamination?
c!#~   s0min     Minimum acceptable value for s0 (linear)
c!#~   s0max     Maximum acceptable value for s0 (linear)
c!#~   s0min_edit   Data edit value for s0obs:
c!#~   s0min_edit.  If  s0min < s0obs < s0min_edit, then s0obs = s0min_edit
c!#~   sig0qual_prints  Flag for prints of sig0 qc info
c!#~   tb_min    Threshold GOES brightness temp for rain flagging
c!#~   timwin    Time window for data acceptance (+/- timwin)
      integer minsig0, nthin, iprint, itag, nfiles
      real al_max, dbmin, dbmax, s0min, s0max, s0min_edit
      real tb_min, timwin
      logical critical, datawin_prints, rainqc, sig0qual_prints
      character*256 filenames(NS_MAXREVS)

      namelist /input/ minsig0, nthin, iprint, itag, nfiles,
     &    al_max, dbmin, dbmax, s0min, s0max, s0min_edit,
     &    tb_min, timwin, critical, datawin_prints, rainqc,
     &    sig0qual_prints, filenames







