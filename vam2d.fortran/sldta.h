c!#   $Id: sldta.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: sldta.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally sldta.com)
c!#
C-----COMMON BLOCK DESCRIBING CONV. DATA WITH IDIMC=SLMAX
      integer sl_qc(SLMAX)
      COMMON /SLDTA/ XCONV(SLMAX),
     + YCONV(SLMAX),WCONV(SLMAX),UCONV(SLMAX),VCONV(SLMAX),
     + IDIMC,NPTC,ILC(SLMAX),JLC(SLMAX),ICONV(SLMAX),sl_qc
