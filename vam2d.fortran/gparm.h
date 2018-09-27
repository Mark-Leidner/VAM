c!# CSU IDENTIFICATION : gparm.h
c!#     $Id: gparm.h,v 1.6 1997/07/07 16:17:56 leidner Exp $

c!## PURPOSE : COMMON BLOCK DESCRIBING GRID PARMS WITH IDIM=IBF, IDIMY= JBF MAY82

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: gparm.h,v $
c!## CHANGE LOG : Revision 1.6  1997/07/07 16:17:56  leidner
c!## CHANGE LOG : updated documentation
c!## CHANGE LOG :
c!#	Revision 1.5  1997/07/02  20:58:23  leidner
c!#	added new contraint on vorticity tendency; controlled by init_dtz0
c!#
c!#	Revision 1.4  1997/05/21  14:17:18  leidner
c!#	Added grid shifting, controlled by (gshift_dlon, gshift_dlat)
c!#
c!#	Revision 1.3  1997/04/09  15:14:42  rnh
c!#	Added 12 point bicubic interpolation,
c!#	controlled by parameter interp in /gparm/.
c!#
c!#	Revision 1.2  1997/04/08  19:41:51  leidner
c!#	added lognum
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision (filename originally gparm.com)
c!#

c!## GLOBAL AND SHARED DATA :

c!# lognum    number which identifies this experiment; today's date in
c!#           the format yymmdd.##; for example, the second experiment
c!#           performed on July 7, 1997, lognum = 970707.02
c!# interp    parameter which controls interpolation method
c!#           =(1,2) for (bilinear, 12-point bicubic) interpolation
c!# init_dtz0 parameter which controls constraint on vorticity tendency
c!#           =(0,1) to initialize DTZ0 to (zero,background values)
c!# gshift_dlon, gshift_dlat  parameters which control translation 
c!#           of the analysis grid

      LOGICAL LIPER,LNP,LSP
      real lognum
      COMMON /GPARM/ XS,DELX,XF,XMIN,XMAX,YS,DELY,YF,YMIN,YMAX,
     +               ALPHA(IBF),BETA(IBF),AUNC(JBF),BUNC(IBF),AC(JBF),
     +               AREA,APOLE,AJ2(JBF),BJ2(JBF),CJ2(JBF),DJ2(JBF),
     +               EJ2,ADIV(JBF),BDIV(JBF),CDIV(JBF),AZ(JBF),BZ(JBF),
     +               NGRDPT,NX,IMIN,IMAX,KMIN,KMAX,NI,IP,NIP,IPMAX,
     +               IPER,IPASS,NY,JMIN,JMAX,LMIN,LMAX,NJ,JP,NJP,JPMAX,
     +               JPER,LIPER,LSP,LNP,lognum,interp,init_dtz0,
     +               gshift_dlon,gshift_dlat
