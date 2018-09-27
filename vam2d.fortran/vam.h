c!#   $Id: vam.h,v 1.6 1999/11/01 18:42:23 trn Exp $
c!#   $Log: vam.h,v $
c!#   Revision 1.6  1999/11/01 18:42:23  trn
c!#   Changed ptmax to 90000
c!#
c!#   Revision 1.5  1997/04/04 19:29:33  rnh
c!#   Merging NSCATo and VAM1
c!#
c!#	Revision 1.4  1997/02/21  22:37:51  leidner
c!#	added parameters needed for nscat data
c!#
c!#	Revision 1.3  1997/02/18  17:46:25  leidner
c!#	MAXLATS assignment increased from 181 -> 361
c!#	MAXVEC parameter removed
c!#	WBF increased from 400000 -> 3200000
c!#
c!#	Revision 1.2  1997/02/10  19:16:28  leidner
c!#	iosubs.h include now CPP-compatible
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
#include "iosubs.h"

      integer MAXLONS
      parameter (MAXLONS=360)

      integer MAXLATS
      parameter (MAXLATS=181)

      integer PTMAX
      parameter (PTMAX=90000)

      integer SLMAX
      parameter (SLMAX=25000)

      integer MAXREP
      parameter (MAXREP=800000)

      integer WBF
      parameter (WBF=2500000)

      integer IBF
      parameter (IBF=MAXLONS+3)
 
      integer JBF
      parameter (JBF=MAXLATS)
 
      integer MAXREC
      parameter (MAXREC = IBF * JBF)

      integer MAXKX
      parameter (MAXKX=100)

      integer MAXTAG
      parameter (MAXTAG = 5)
 
      integer IUNML
      parameter (IUNML = 8)
 
      integer IULST
      parameter (IULST = 9)

      real SHIP_HEIGHT
      parameter (SHIP_HEIGHT = 19.5)

      real BUOY_HEIGHT
      parameter (BUOY_HEIGHT = 5.0)

      real ERS1_HEIGHT
      parameter (ERS1_HEIGHT = 10.0)

      real BACK_HEIGHT
      parameter (BACK_HEIGHT = 10.0)

      real SSMI_HEIGHT
      parameter (SSMI_HEIGHT = 10.0)
 
      integer ALLTYPE
      parameter (ALLTYPE = -999)
 
      integer STANDARD
      parameter (STANDARD = 1)
 
      integer ERS1
      parameter (ERS1 = 2)
 
      integer SSMI
      parameter (SSMI = 3)
 
      integer BACKSCATTER
      parameter (BACKSCATTER = 4)
 
      integer UKMO
      parameter (UKMO = 5)
 
      integer STANDARD_NSCAT
      parameter (STANDARD_NSCAT = 6)
 
      integer NSCAT_WINDS
      parameter (NSCAT_WINDS = 7)
 
      integer EMPTY
      parameter (EMPTY = 0)
 
      integer FULL
      parameter (FULL = 1)

      real ER_FILL
      parameter (ER_FILL = 1.E+15)

      integer ER_MBEAM
      parameter (ER_MBEAM = 3)

      integer ER_MOBS
      parameter (ER_MOBS = 7500)

      integer ER_UNIT
      parameter (ER_UNIT = 13)

      real ER_S0MIN
      parameter (ER_S0MIN = 10.0**(-80/10))

      real NS_FILL
      parameter (NS_FILL = 1.E+15)

      integer NS_MBEAM
      parameter (NS_MBEAM = 1)

      integer NS_MWVC
      parameter (NS_MWVC = 80000)

      integer NS_MOBS
      parameter (NS_MOBS =800000)

      integer NS_UNIT
      parameter (NS_UNIT = 13)

      integer NS_MAXREVS
      parameter (NS_MAXREVS = 15)

      real NS_S0MIN
      parameter (NS_S0MIN = 10.0**(-80/10))
c
c     function declarations
c     ---------------------
c
      logical FTEST
      integer strlen,checktime
      character*3 getstatus
  
      integer read_event
      integer write_event
      integer execute_event
      integer ers1wind
      integer RGV_rdgrid
      integer RGV_rddata
      integer GEN_rdskip
      integer RCO_rdconv
      integer RCO_rddata
      integer RCO_rdukmo
      integer RPK_rdskip
      integer SL_superob
      integer SL_grosscheck
      integer RSS_rdsass
      integer RSS_rddata
      integer RSS_default
      integer RCO_default
      integer RBO_default
      integer RBO_rddata
      integer RBO_rdback
      integer RNB_rdback
      integer RNW_rdwind
      integer RSO_default
      integer RSO_rdsass
      integer RSO_rdback
      integer RSO_rddata
      integer WSS_wrsass
      integer WSS_wrdata
      integer WSS_default
      integer PT_dealias
      integer WGP_wrgrid
      integer WCO_wrconv
      integer WCO_wrdata
      integer WCO_default
      integer WBO_wrback
      integer WBO_wrdata
      integer WBO_wrwind
      integer WBO_wrnscat
      integer WBO_default
