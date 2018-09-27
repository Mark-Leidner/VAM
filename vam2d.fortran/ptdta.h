c!#   $Id: ptdta.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: ptdta.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally ptdta.com)
c!#
C-----COMMON BLOCK DESCRIBING POINT DATA WITH IDIMS=PTMAX
      integer pt_qc(PTMAX)
      COMMON /PTDTA/ RA,DFAC,
     +   XCOORD(PTMAX),YCOORD(PTMAX),WGT(PTMAX),
     +   UOBS(4,PTMAX),VOBS(4,PTMAX),VBAR(PTMAX),IOBS(PTMAX),
     +   IDIMS,NPTS,ILL(PTMAX),JLL(PTMAX),NWINDS(PTMAX),NPTSA,
     +   pt_qc
