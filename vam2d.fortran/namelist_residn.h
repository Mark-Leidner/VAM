c!# CSU IDENTIFICATION : namelist_residn.h
c!#     $Id: namelist_residn.h,v 1.1 1997/10/24 04:14:42 leidner Exp $

c!## PURPOSE : define input namelist for NSCAT sigma0 QC

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :
 
c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG : $Log: namelist_residn.h,v $
c!## CHANGE LOG : Revision 1.1  1997/10/24 04:14:42  leidner
c!## CHANGE LOG : Initial revision
c!## CHANGE LOG :

c!## GLOBAL AND SHARED DATA :

c!# iprint   controls frequency of diagnostic output from this
c!#          routine:
c!#            iprint = 0 (no output)
c!#                     1 (every ob failing QC is printed)
c!#                     2 (every other ob failing QC is printed)
c!# iver     specifies which field to use as a reference for
c!#          determining the quality of the NSCAT observations:
c!#            iver = 0 (background used)
c!#                   1 (current analysis used)
c!# gamma    threshold or maximum displacement that a given NSCAT obs.
c!#          may have from the reference field according to the 
c!#          following formula:
c!#            displacement = (s0obs - s0calc) (dB space)
c!#            if (-gamma > displacement > gamma) report fails QC
c!#
      integer iprint, iver
      real gamma
c
      namelist /input/ iprint, iver, gamma

