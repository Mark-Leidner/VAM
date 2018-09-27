c!#   $Id: dgrid.F,v 1.2 1997/02/12 19:19:28 leidner Exp $
c!#   $Log: dgrid.F,v $
c!#   Revision 1.2  1997/02/12 19:19:28  leidner
c!#   all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE DGRID
#include "vam.h"
C***********************************************************************
C*****DGRID DIFFERENCES THE TWO GRIDS: ANALYSIS = ANALYSIS - FORECAST
C-----SPECIFICATIONS
C-----XREFS:
C-----FORT : FRDNL# FWRNL# IBCOM# ...... ...... ...... ...... ......
C-----COMMON BLOCK DESCRIBING GRID DATA WITH IDIM=IBF,IDIMY= JBF - MAY 8
#include "gparm.h"
#include "grdta.h"
C-----DEFINE NUMERICAL CONSTANTS
      DATA ONE/1D0/
      character*8 NNAME
      NAMELIST /INPUT/C,C0,NNAME
      C=ONE
      C0=-ONE
      NNAME=NAME
      READ (30,INPUT,END=1520,ERR=1520)
      WRITE (6,INPUT)
      DO 200 J=1,NY
      DO 200 I=1,NX
      U(I,J)=C*U(I,J)+C0*U0(I,J)
      V(I,J)=C*V(I,J)+C0*V0(I,J)
  200 CONTINUE
C-----
      WRITE (6,201) NNAME,C,NAME,C0,NAME0
  201 FORMAT ('0GRIDS COMBINED: ',A8,' = ',F10.2,' * ',A8,
     +     ' + ',F10.2,' * ',A8/)
      NAME=NNAME
      RETURN
 1520 CALL ERRSEA (520,6HDGRID )
      RETURN
      END
