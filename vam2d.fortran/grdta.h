c!# CSU IDENTIFICATION : grdta.h
c!#     $Id: grdta.h,v 1.3 1997/07/07 16:52:26 leidner Exp $

c!## PURPOSE : COMMON BLOCK DESCRIBING GRID DATA WITH IDIM=IBF,IDIMY= JBF - MAY 82

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: grdta.h,v $
c!## CHANGE LOG : Revision 1.3  1997/07/07 16:52:26  leidner
c!## CHANGE LOG : updated documentation
c!## CHANGE LOG :
c!#	Revision 1.2  1997/07/07  16:31:14  leidner
c!#	Added storage array for background vorticity tendency
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision (filename originally grdta.com)
c!#

c!## GLOBAL AND SHARED DATA :

c!# (U0,V0)   background wind field ("forecast")
c!# (U,V)     current wind analysis
c!# DELSSU    dJ/du gradients
c!# DELSSV    dJ/dv gradients
c!# VEL       speed of analysis winds
c!# CD        drag coeff
c!# DTZ0      background vorticity tendency
c!# IDIM      number of analysis gridpoints in x-direction
c!# IDIMY     number of analysis gridpoints in y-direction
c!# NAME0     ascii name of background field
c!# NAME      ascii name of vam analysis field

      CHARACTER*8 NAME0,NAME
      COMMON /GRDTA/ U0(IBF,JBF),V0(IBF,JBF),U(IBF,JBF),V(IBF,JBF),
     &               DELSSU(IBF,JBF),DELSSV(IBF,JBF),VEL(IBF,JBF),
     &               CD(IBF,JBF),DTZ0(IBF,JBF),IDIM,IDIMY,NAME0,NAME
