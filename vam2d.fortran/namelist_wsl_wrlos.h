c!# CSU IDENTIFICATION : namelist_wsl_wrlos.h
c!#     $Id: namelist_wsl_wrlos.h,v 1.1 1997/08/11 17:45:05 leidner Exp $

c!## PURPOSE : define input namelist for wsl_wrlos.F

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :
 
c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: namelist_wsl_wrlos.h,v $
c!## CHANGE LOG : Revision 1.1  1997/08/11 17:45:05  leidner
c!## CHANGE LOG : Initial revision
c!## CHANGE LOG :

c!## GLOBAL AND SHARED DATA :

c!# iu               unit number of output file
c!# data_fmt         format for writing out reals
c!# header_fmt       format for header writes
c!# header_fmt2      format for header write of VAM Log Number
      integer iu
      character*24 data_fmt, header_fmt, header_fmt2
c
      namelist /input/ iu, data_fmt, header_fmt, header_fmt2
