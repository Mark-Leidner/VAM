c!#   $Id: grparm.F,v 1.7 1997/07/08 16:44:19 leidner Exp $
c!#   $Log: grparm.F,v $
c!#   Revision 1.7  1997/07/08 16:44:19  leidner
c!#   Added parameter to control initialization of background
c!#   vorticity tendency array
c!#
c!#	Revision 1.6  1997/05/21  14:28:20  leidner
c!#	Added grid shifting, controlled by (gshift_dlon, gshift_dlat)
c!#
c!#	Revision 1.5  1997/04/09  15:14:42  rnh
c!#	Added 12 point bicubic interpolation,
c!#	controlled by parameter interp in /gparm/.
c!#
c!#	Revision 1.4  1997/04/08  19:47:08  leidner
c!#	added lognum to INPUT namelist
c!#
c!#	Revision 1.3  1997/04/04  15:47:33  rnh
c!#	Merging NSCATo and VAM1
c!#
c!#	Revision 1.2.1.1  1997/02/21  21:13:11  leidner
c!#	added debug prints
c!#
c!#	Revision 1.2  1997/02/12  19:19:28  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE GRPARM
#include "vam.h"
C***********************************************************************
C*****GRPARM SETS UP GRID PARAMETERS
C-----SPECIFICATIONS
C-----XREFS:
C-----USER : .GRIDX .SETCD .TRIG1 ERRSEA ...... ...... ...... ......
C-----FORT : FRDNL# FWRNL# IBCOM# ...... ...... ...... ...... ......
C-----COMMON BLOCK DESCRIBING GRID PARMS WITH IDIM=IBF, IDIMY= JBF MAY82
#include "gparm.h"
#include "grdta.h"
#include "grname.h"
C-----DEFINE NUMERICAL CONSTANTS
      DATA ONE/1D0/
C-----DEFINE NUMERICAL CONSTANTS
      DATA FILL/-777D20/
C-----DEFINE NUMERICAL CONSTANTS
      REAL NINETY
      DATA ISTAN/1/,IEXT/1/,IDEBUG/0/,CDRAG/1D-3/
      NAMELIST /DEFALT/ XS,DELX,XF,YS,DELY,YF,CDRAG,ISTAN,IEXT
      NAMELIST /INPUT/ XS,DELX,XF,YS,DELY,YF,CDRAG,XMIN,XMAX,YMIN,YMAX,
     +   NAME0,NAME,ISTAN,IEXT,IDEBUG,iu,ibefaf,qlev,qu,qv,grid_height,
     +   lognum, interp, init_dtz0, gshift_dlon, gshift_dlat
c     interp        =(1,2) for (bilinear, bicubic) interpolation
c     init_dtz0     =(0,1) to initialize DTZ0 to (zero,background values)
c     gshift_dlon   = deg. lon to shift grid relative to curr position
c     gshift_dlat   = deg. lat to shift grid relative to curr position
      NAMELIST /GRDTA/ XS,DELX,XF,YS,DELY,YF,CDRAG,XMIN,XMAX,YMIN,YMAX,
     +   IPER,LIPER,LNP,LSP,NX,NY,KMIN,KMAX,LMIN,LMAX,IP,IPMAX,NIP,
     +   JP,JPMAX,NJP,IMIN,IMAX,NI,JMIN,JMAX,NJ,NGRDPT,NAME0,NAME,
     +   lognum, interp, init_dtz0, gshift_dlon, gshift_dlat
      data NINETY/90D0/
C-----SET DEFAULTS
      iu     = 0
      ibefaf = 0
      qlev   = 0.0
      qu     = 'U10M    '
      qv     = 'V10M    '
      IDIM   = IBF
      IDIMY  = JBF
      XMIN   = FILL
      XMAX   = FILL
      YMIN   = FILL
      YMAX   = FILL
      grid_height = 10.0
      lognum=990229.01
      Interp = 1
      init_dtz0 = 0
      gshift_dlon = 0
      gshift_dlat = 0
      IF (IEXT.NE.1) GO TO 10
C-----UNDO EXTENSION
      XS=XS+DELX
      XF=XF-DELX
      IF (.NOT.LSP) YS=YS+DELY
      IF (.NOT.LNP) YF=YF-DELY
C-----
   10 WRITE (6,101)
  101 FORMAT ('0SETTING UP GRID PARAMETERS.'/)
      WRITE (6,DEFALT)
      WRITE (6,102) NAME0,NAME
  102 FORMAT ('0FORECAST GRID IS ',A8,10X,'1ST GUESS GRID IS ',A8/)
C-----INPUT SHOULD BE XS,DELX,XF,YS,DELY,YF
C-----OPTIONAL INPUT MAY BE XMIN,XMAX,YMIN,YMAX,ISTAN,IEXT
      READ (IUNML,INPUT,END=1520,ERR=1520)
      WRITE (6,INPUT)
      print *, 'xmin,xmax,ymin,ymax,fill=',xmin,xmax,ymin,ymax,fill
      IF (abs(XMIN-FILL) .le. 0.01) XMIN=XS
      IF (abs(XMAX-FILL) .le. 0.01) XMAX=XF
      IF (abs(YMIN-FILL) .le. 0.01) YMIN=YS
      IF (abs(YMAX-FILL) .le. 0.01) YMAX=YF
C-----360/DELX SHOULD BE AN INTEGER VALUE
      IPER=4*NINETY/DELX
      LIPER=( (XF-XS).EQ.4*NINETY )
      LNP=( YF.EQ.+NINETY )
      LSP=( YS.EQ.-NINETY )
      IF (IEXT.NE.1) GO TO 20
C-----EXTEND GRID TO ACCOMMODATE FILTER
      XS=XS-DELX
      XF=XF+DELX
      IF (.NOT.LSP) YS=YS-DELY
      IF (.NOT.LNP) YF=YF+DELY
C-----DEFINE GRID PARAMETERS NX, NY
   20 XN=1+(XF-XS)/DELX
      NX = XN
      print *, 'xn,nx=',xn,nx
      IF (XN-NX.NE.0) CALL ERRSEA (514,6HGRPA01)
      YN=1+(YF-YS)/DELY
      NY=YN
      print *, 'ny,yn=',ny,yn
      IF (YN-NY.NE.0) CALL ERRSEA (514,6HGRPA02)
C-----DEFINE INTEGRATION AREA
      KMIN=1
      KMAX=NX
      LMIN=1
      LMAX=NY
      IF (IEXT.NE.1) GO TO 30
      KMIN=2
      KMAX=NX-1
      LMIN=2
      LMAX=NY-1
      IF (LSP) LMIN=1
      IF (LNP) LMAX=NY
C-----DEFINE PLOT AREA
   30 IP=KMIN
      IPMAX=KMAX
      NIP=IPMAX-IP+1
      JP=LMIN
      JPMAX=LMAX
      NJP=JPMAX-JP+1
C-----TRANSFORM DATA WINDOW
      print *, 'xmin,xs,delx,one,xn,i=',xmin,xs,delx,one,xn,i 
      print *, 'xmax,xs,delx,one,xn,i=',xmax,xs,delx,one,xn,i
      print *, 'ymin,ys,dely,one,yn,j=',ymin,ys,dely,one,yn,j
      print *, 'ymax,ys,dely,one,yn,j=',ymax,ys,dely,one,yn,j
      CALL GRIDX(XMIN,XS,DELX,0,ONE,XN,I,XMIN,*666)
      XMIN=XMIN+I
      CALL GRIDX(XMAX,XS,DELX,0,ONE,XN,I,XMAX,*666)
      XMAX=XMAX+I
      CALL GRIDX(YMIN,YS,DELY,0,ONE,YN,J,YMIN,*666)
      YMIN=YMIN+J
      CALL GRIDX(YMAX,YS,DELY,0,ONE,YN,J,YMAX,*666)
      YMAX=YMAX+J
C-----DEFINE ACTIVE GRID POINTS
      IMIN=1
      IMAX=NX
C-----POLES NOT INCLUDED IN JMIN, JMAX
      JMIN=1
      JMAX=NY
      IF (LSP) JMIN=2
      IF (LNP) JMAX=NY-1
      IF (IEXT.NE.1) GO TO 40
      IMIN=2
      IMAX=NX-1
C-----POLES NOT INCLUDED IN JMIN, JMAX
      JMIN=2
      JMAX=NY-1
C-----360 IS NOT INCLUDED
   40 IF (LIPER) IMAX=IMAX-1
      NI=IMAX-IMIN+1
      NJ=JMAX-JMIN+1
      NGRDPT=NI*NJ
      IF (LSP) NGRDPT=NGRDPT+1
      IF (LNP) NGRDPT=NGRDPT+1
      WRITE (6,GRDTA)
C-----OPPORTUNITY TO ALTER VALUES
      IF (ISTAN.EQ.1) GO TO 200
      READ (IUNML,GRDTA,END=1520,ERR=1520)
      XN=NX
      YN=NY
      WRITE (6,GRDTA)
  200 WRITE (6,102) NAME0,NAME
C-----CHECK DIMENSIONS
      IF (NX.LT.1 .OR. NY.LT.1) CALL ERRSEA (522,6HGRPA19)
      IF (NX.GT.IDIM .OR. NY.GT.IDIMY) CALL ERRSEA (522,6HGRPA20)
C-----CHECK GRID LIMITS/PARAMETERS
      IF (XN .NE. (XF-XS)/DELX+ONE) CALL ERRSEA (514,6HGRPA03)
      IF (YN .NE. (YF-YS)/DELY+ONE) CALL ERRSEA (514,6HGRPA04)
      IF (XMIN.LT.1 .OR. XMAX.GT.NX) CALL ERRSEA (514,6HGRPA05)
      IF (YMIN.LT.1 .OR. YMAX.GT.NY) CALL ERRSEA (514,6HGRPA06)
      IF (IMIN.LT.1 .OR. IMAX.GT.NX) CALL ERRSEA (514,6HGRPA07)
      IF (JMIN.LT.1 .OR. JMAX.GT.NY) CALL ERRSEA (514,6HGRPA08)
      IF (KMIN.LT.1 .OR. KMAX.GT.NX) CALL ERRSEA (514,6HGRPA09)
      IF (LMIN.LT.1 .OR. LMAX.GT.NY) CALL ERRSEA (514,6HGRPA10)
      IF (IP.LT.1 .OR. IPMAX.GT.NX) CALL ERRSEA (514,6HGRPA11)
      IF (JP.LT.1 .OR. JPMAX.GT.NY) CALL ERRSEA (514,6HGRPA12)
      IF (NI.NE.IMAX-IMIN+1) CALL ERRSEA (514,6HGRPA13)
      IF (NJ.NE.JMAX-JMIN+1) CALL ERRSEA (514,6HGRPA14)
      IF (NIP.NE.IPMAX-IP+1) CALL ERRSEA (514,6HGRPA15)
      IF (NJP.NE.JPMAX-JP+1) CALL ERRSEA (514,6HGRPA16)
      IF (LIPER .AND. (IMIN+IPER.NE.IMAX+1) ) CALL ERRSEA (514,6HGRPA16)
      IF (LSP .AND. (YS.NE.-NINETY) ) CALL ERRSEA (514,6HGRPA17)
      IF (LNP .AND. (YF.NE.+NINETY) ) CALL ERRSEA (514,6HGRPA18)
C-----CALCULATE TRIGONOMETRIC FACTORS AND SAVE IN GPARM COMMON AREA
      CALL TRIG1(IDEBUG)
C-----SET VALUE OF DRAG COEFFICIENT CD
C-----ACTUALLY THIS COEF. IS CD=-CDRAG/DELTAZ
      CALL SETCD(CDRAG,IDEBUG)
      RETURN
  666 WRITE (6,GRDTA)
      CALL ERRSEA (514,6HGRPA21)
 1520 WRITE (6,GRDTA)
      CALL ERRSEA (520,6HGRPA22)
      RETURN
      END
