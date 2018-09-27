c!# CSU IDENTIFICATION : wna_wrback.F
c!#     $Id: wna_wrback.F,v 1.4 1998/03/04 14:19:12 leidner Exp $

c!## PURPOSE : writes out NSCAT and trajectory sig0 values to ascii file

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      function WNA_wrback                                               !#
     I      ( idate, itime )                                            !#

c!# Input:
c!# idate   date in yymmdd format
c!# itime   time in hhmmss format
      integer WNA_wrback, idate, itime                                  !#

c!# Output: Normally 0, otherwise see ERROR HANDLING

c!## DATA CONVERSION :

c!# Change longitude from VAM coords (0,360) to Splus map plotting coords
c!# (-180,180)

c!## ALGORITHMS :

c!## REFERENCES :

c!## LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: wna_wrback.F,v $
c!#   Revision 1.4  1998/03/04 14:19:12  leidner
c!#   changed write format to accomodate 4-digit row #'s (25km data)
c!#
c!#   Revision 1.3  1998/02/11 20:04:59  leidner
c!#   added wvc row and column to backscatter storage
c!#
c!#   Revision 1.2  1997/07/25 17:00:31  leidner
c!#   fixed some bugs and added s0sd and Kp eqn coeffs to ouput
c!#  
c!#	Revision 1.1  1997/06/23  17:54:42  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "grdta.h"
#include "ns_bscat.h"
#include "namelist_wna_wrback.h"

c!## LOCAL DATA ELEMENTS :

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   0.  Initialize function and namelist parameters
c
      WNA_wrback = 0
      data_fmt = '(f40.10)'
      data_fmt2 = '(10i5)'
      header_fmt = '(i10)'
      header_fmt2 = '(f10.2)'
      header_fmt_revs = '(15i6)'
c
c     ------------------------------------------------------------------
c
c!#   1.  Retrieve namelist parameters
c
      read(IUNML,input,end=902,err=902)
      write(*,input)
c
c     ------------------------------------------------------------------
c
c!#   2.  Establish I/O link
c
      if (ioopen(iu) .ne. 0) go to 901
c
c     ------------------------------------------------------------------
c
c!#   3.  Write header from gparm and ns_bscat.h
c
      write(iu,header_fmt) idate
      write(iu,header_fmt) itime
      write(iu,header_fmt2) lognum
      write(iu,header_fmt_revs) ( revs(i),i=1,NS_MAXREVS )
      write(iu,header_fmt_revs) ( ns_nWVC_rev(i),i=1,NS_MAXREVS )
      write(iu,header_fmt_revs) ns_nobs
c
c     ------------------------------------------------------------------
c
c!#   4.  Convert longitudes to (-180,180)lon coord
c
      do i=1,ns_nobs
        if ( ns_lon(i) .gt. 180 ) ns_lon(i) = ns_lon(i) - 360
      enddo
c
c     ------------------------------------------------------------------
c
c!#   5.  Write NSCAT and trajectory sig0's to ascii file
c
      write(iu,data_fmt) ( ns_lat(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_lon(n),n=1,ns_nobs )
      write(iu,data_fmt2) ( ns_pol(n),n=1,ns_nobs )
      write(iu,data_fmt2) ( ns_ant(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_theta(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_azim(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_s0obs(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_s05(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_s0sd(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_kpa(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_kpb(n),n=1,ns_nobs )
      write(iu,data_fmt) ( ns_kpc(n),n=1,ns_nobs )
      write(iu,data_fmt2) ( ns_row(n),n=1,ns_nobs )
      write(iu,data_fmt2) ( ns_col(n),n=1,ns_nobs )
c
      go to 999
c
c     ------------------------------------------------------------------
c
c!## ERROR HANDLING : return nonzero error codes
c!# WNA_wrback =  0 (normal)
c!#              -1 (error returned opening output file)
c!#              -2 (error reading namelist)
c
 901  continue
      WNA_wrback = -1
      go to 999
c
 902  continue
      WNA_wrback = -2
      print *, 'vam(WNA_wrback): error reading namelist'
      go to 999
c
 999  return
      end
