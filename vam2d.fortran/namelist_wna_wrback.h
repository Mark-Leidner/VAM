c!# CSU IDENTIFICATION : namelist_wna_wrback.h
c!#     $Id: namelist_wna_wrback.h,v 1.1 1997/06/23 17:56:29 leidner Exp $

c!## PURPOSE : define input namelist for wna_wrback.F

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :
 
c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: namelist_wna_wrback.h,v $
c!## CHANGE LOG : Revision 1.1  1997/06/23 17:56:29  leidner
c!## CHANGE LOG : Initial revision
c!## CHANGE LOG :

c!## GLOBAL AND SHARED DATA :

c!# iu               unit number of output file
c!# data_fmt         format for writing out reals
c!# data_fmt2        format for writing out integers
c!# header_fmt       format for header writes
c!# header_fmt2      format for header write of VAM Log Number
c!# header_fmt_revs  format for header write of rev numbers in this data
      integer iu
      character*24 data_fmt, data_fmt2, 
     &    header_fmt, header_fmt2, header_fmt_revs
c
      namelist /input/ iu, data_fmt, data_fmt2, 
     &    header_fmt, header_fmt2, header_fmt_revs
