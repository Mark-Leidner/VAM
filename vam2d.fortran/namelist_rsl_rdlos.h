c!# CSU IDENTIFICATION : namelist_rsl_rdlos.h
c!#     $Id: namelist_rsl_rdlos.h,v 1.3 1998/03/25 16:58:45 leidner Exp $

c!## PURPOSE : input namelist definition for rsl_rdlos.F

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!## CHANGE LOG :
c!#	$Log: namelist_rsl_rdlos.h,v $
c!#	Revision 1.3  1998/03/25 16:58:45  leidner
c!#	added option to store ssmi wind speed as sass wind speed
c!#	controlled by namelist parameter store_sass
c!#	
c!#	Revision 1.2  1997/08/14 14:17:29  leidner
c!#	added namelist flag to control use of los directional info
c!#
c!#	Revision 1.1  1997/08/11  17:43:00  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!# iprint           print interval for SSMI-4 sig0 data
c!# iu               unit number for SSMI-4 data files
c!# nfiles           number of SSMI-4 files to read
c!# timwin           time window for data acceptance (+/- timwin)
c!# datawin_prints   flag for prints of data time/location info
c!# ssmiqual_prints  flag for prints of los qc info
c!# use_losm         flag for use of los direction information
c!# store_sass       store SSMI wind speed as sass velocity magnitude?
c!# filenames        file names of SSMI-4 geophysical data to be read
      integer iprint, iu, nfiles
      real timwin
      logical datawin_prints, ssmiqual_prints, use_losm, store_sass
      character*256 filenames(15)
 
      namelist /input/ iprint, iu, nfiles, timwin, 
     &    datawin_prints, ssmiqual_prints, use_losm, store_sass,
     &    filenames
