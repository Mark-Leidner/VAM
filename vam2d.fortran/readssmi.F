c!# CSU IDENTIFICATION : readssmi.F
c!#     $Id: readssmi.F,v 1.4 1998/03/25 16:58:45 leidner Exp $

c!## PURPOSE : Read ssmi data, apply quality control and fill in /ssmidta/.

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## INPUT/OUTPUT INTERFACE :

      subroutine readssmi
     c       ( idate, itime, timwin,                                    !#
     c       datawin_prints, ssmiqual_prints, use_losm, store_sass,     !#
     i       n, filename, iu )                                          !#

c!# Input:
c!# idate            desired date in yymmdd format
c!# itime            desired time in hhmmss format
c!# timwin           time window for data acceptance (+/- timwin)
c!# datawin_prints   flag for prints of data time/location info
c!# ssmiqual_prints  flag for prints of los qc info
c!# use_losm         flag for use of los direction information
c!# store_sass       store SSMI wind speed as sass velocity magnitude?
c!# n                nth SSMI-4 file to be read
c!# filename         file name of SSMI-4 geophysical data to be read
c!# iu               unit number for SSMI-4 data files
      integer idate, itime, iu, n                                       !#
      real timwin                                                       !#
      logical datawin_prints, ssmiqual_prints, store_sass, use_losm     !#
      character*256 filename                                            !#

c!## DATA CONVERSION :

c!# Line-of-sight wind direction signal (K) is converted to wind speed (m/s).

c!## ALGORITHMS :

c!# Code in all caps is unchanged from Remote Sensing Systems fortran program
c!# TESGEO4.F.

c!## REFERENCES :

c!# Wentz, F. J., 1997: "User's Manual for SSM/I-4 Geophysical Products",
c!# Remote Sensing Systems, Santa Rosa, CA.

c!#
c!# Note on empirical formulas used to process los data:
c!#
c!# From rnh Tue Apr  8 16:00:23 1997
c!# To: leidner, ardizone@nimbus.gsfc.nasa.gov
c!# Subject: directional info in SSM/I
c!#
c!# I talked with Frank and he told me the following:
c!#  
c!# 1. The signal is in degrees K.  You can approximately normalize into wind
c!# speed according to 1.2 K = 10 m/s (i.e. multiply by 100/12).
c!#  
c!# (Before he had told me that the LOS wind error is roughly 5 m/s.)
c!# 
c!# The relationship with LOS wind speed and the directional temperature
c!# signature is linear to first order.  It goes thru zero and increases
c!# (when the wind is blowing towards the satellite) with wind speed, but
c!# what happens at high wind speed is not known.
c!# 
c!# 2. The azimuth angle at the data location is approximated by
c!# 
c!#         (icell - 1)*1.6 + azm - 50.4
c!# 
c!# which gives azm when icell is 32.5.  Frank says this is good enough
c!# for latitudes equatorward of 60 and definitely 50 degrees.
c!#  

c!## LIMITATIONS :

c!## CHANGE LOG : 
c!#	$Log: readssmi.F,v $
c!#	Revision 1.4  1998/03/25 16:58:45  leidner
c!#	added option to store ssmi wind speed as sass wind speed
c!#	controlled by namelist parameter store_sass
c!#	
c!#	Revision 1.3  1997/10/23 20:36:43  leidner
c!#	added data to ssmidata buffer
c!#
c!#	Revision 1.2  1997/08/14  14:33:31  leidner
c!#	added argument to control use of los directional info
c!#	added documentation and reference material
c!#
c!#	Revision 1.1  1997/08/11  15:47:20  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

#include "vam.h"
#include "gparm.h"
#include "ssmidata.h"
#include "ptdta.h"

c!# See Wentz reference for description of /GEODAT/.

      integer*4 lyear, idayjl, imon, idaymo, isecdy, isat, iqual,
     &    isur, icewarn, iwinqc, irainadj
      real*4 tht, azm, xlat, xlon, sst, tice, oice, wind, dir, dtb19h,
     &    vapor, cloud, rain, tsky
      REAL*8 REV
      COMMON /GEODAT/ LYEAR,IDAYJL,IMON,IDAYMO,ISECDY,ISAT,
     1 REV,THT,AZM,IQUAL,XLAT(64),XLON(64),
     2 ISUR(64),ICEWARN(64),IWINQC(64),IRAINADJ(64),
     3 SST(64),TICE(64),OICE(64),WIND(64),DIR(64),DTB19H(64),
     4 VAPOR(64),CLOUD(64),RAIN(64),TSKY(64)

c!## LOCAL DATA ELEMENTS :

c!# iblock           counter for blocks
c!# ibytes           counter for bytes
c!# irecords         counter for records
c!# istat            file open status code
c!# lblk             buffer for 1 block of ssmi data
c!# lrec             buffer for 1 record of ssmi data
c!# ssmirow          scan (row) counter
      integer iblock, ibyte, irecord, istat, ssmirow
      logical connect
      CHARACTER*1 LBLK(994,30), LREC(994)

c!## LOCAL DATA STRUCTURES :

c!## DATA FILES :

      data pi/3.1415926535897932D0/

c!## LOGIC FLOW AND DETAILED ALGORITHM :

c
c     ------------------------------------------------------------------
c
c!#   1.  Open SSMI-4 data file.
c
      inquire(unit=iu,opened=connect)
      if (connect) goto 901
c
      open (unit=iu,file=filename,form='unformatted',access='direct',
     &    recl=29820,status='unknown',iostat=istat)
      if (istat .ne. 0) goto 902
      ssmirow = 0
c
c     ------------------------------------------------------------------
c
c!#   2.  Loop over all blocks of data in file.
c
      DO 300 IBLOCK=1,12345678
c
C     READBLK IS A USER SUPPLIED ROUTINE FOR READING ONE BLOCK OF DATA
C     FROM TAPE INTO ARRAY LBLK.  IF A FILEMARK IS ENCOUNTERED, READBLK
C     RETURNS IEOF=1, OTHERWISE IEOF=0.
c
         call readblk                                                !#
     c       ( iu,
     i       iblock,
     o       lblk, ieof )
c
         if (ieof .eq. 1) goto 301
c
c     ------------------------------------------------------------------
c
c!#   3.  Loop over all records in data block.
c
         DO 200 IRECORD=1,30
            ssmirow = ssmirow + 1
            DO 50 IBYTE=1,994
               LREC(IBYTE) = LBLK(IBYTE,IRECORD)
   50       CONTINUE
            CALL READGEO4                                               !#
     i          ( LREC, IEND )

c!#       If this is a zero-filled record, skip to next
            IF (IEND .EQ. 1) GO TO 300                                  !#

c!#       If there is no good data in this record, skip to next
            IF (IQUAL.NE.0) GO TO 200                                   !#
c
c     ------------------------------------------------------------------
c
c!#   3.1.  Check data date/time against analysis date/time.
c
            ssmi_rtime = float(isecdy)/3600.
            call yymmdd2julian (idate, julday)
            itime_hh = itime/10000
            itime_mm = (itime - itime_hh*10000)/100
            itime_ss = (itime - itime_hh*10000 - itime_mm*100)
            rtime    = float(itime_hh) + float(itime_mm)/60 +
     &          float(itime_ss)/3600
c
c     ------------------------------------------------------------------
c
c!#   3.1.1.  Is data date within +/-1 day of analysis date?
c
            if (idayjl .eq. julday) then
               dt = ssmi_rtime - rtime
            else if (idayjl .eq.(julday-1)) then
               dt = ssmi_rtime - rtime - 24.
            else if (idayjl .eq.(julday+1)) then
               dt = ssmi_rtime - rtime + 24.
            else 
               if (datawin_prints)
     &         write(*,885) iday, iblock, irecord, idate, julday,
     &                lyear, imon, idaymo, idayjl
               goto 200
            endif
c
c     ------------------------------------------------------------------
c
c!#   3.1.2.  Is data time within +/-timwin of analysis time?
c
            if (abs(dt).gt.timwin) then
               if (datawin_prints)
     &           write(*,887) timwin, iday, iblock, irecord,
     &                        idate, julday, rtime,
     &                        lyear, imon, idaymo, idayjl, ssmi_rtime,
     &                        dt
               goto 200
            endif
c
            if (datawin_prints)
     &        write(*,889) timwin, iday, iblock, irecord,
     &                     idate, julday, rtime,
     &                     lyear, imon, idaymo, idayjl, ssmi_rtime,
     &                     dt
c
  885 format(/'Observation outside analysis time window +/-1 julday',
     &       '; iday',i4,' iblock',i4,' irecord',i4,/
     &       '     background :',i9,i4,/ 
     &       '     observation:',i5,i2,i2,i4)
  887 format(/'Observation outside analysis time window +/-',f3.1,' h',
     &       '; day',i4,' block',i4,' record',i4,/
     &       '     background :',i9,i4,f8.4,/ 
     &       '     observation:',i5,i2,i2,i4,f8.4,/
     &       '     difference :',f8.4,' hours')
  889 format(/'Observation within analysis time window +/-',f3.1,' h',
     &       '; day',i4,' block',i4,' record',i4,/
     &       '     background :',i9,i4,f8.4,/ 
     &       '     observation:',i5,i2,i2,i4,f8.4,/
     &       '     difference :',f8.4,' hours')
c
c     ------------------------------------------------------------------
c
c!#   4.  Loop over all cells in record.
c
            DO 100 ICEL=1,64
c
c!#   4.1. Check QC flags.
c
               IF (ISUR(ICEL) .EQ. 2) GOTO 100     !#no info for land
               IF (ICEWARN(ICEL) .EQ. 2) GOTO 100  !#no info when there is sea ice
               if (icewarn(icel) .eq. 0 .and.      !#no chance of sea ice
     &             iwinqc(icel) .eq. 0 .and.       !#good quality for 10m wind speed
     &             irainadj(icel) .eq. 0) then     !#no rain adjacent to cell
c
                  if (wind(icel) .ne. -999) then
c
c     ------------------------------------------------------------------
c
c!#   4.2.  Convert (xlat,xlon) to (iyg,ixg).
c
                     rlat8 = xlat(icel)
                     rlon8 = xlon(icel)
c 
                     if (datawin_prints) then
                        write(*,900) ys,yf,xs,xf
                        write(*,910) icel,rlat8,rlon8
                     endif
c
                     call gridx(rlon8,xs,delx,iper,xmin,xmax,ic,xc,*100)
                     call gridx(rlat8,ys,dely,   0,ymin,ymax,jc,yc,*100)
c
                     if (datawin_prints) write(*,920) irecord,icel
c
  900 format('    Data window : lat[',f8.3,',',f8.3,']  ',
     &                         'lon[',f8.3,',',f8.3,']')
  910 format('     cell',i3,' loc :         ',f8.3,'                ',
     &        f8.3)
  920 format('                  irecord[',i4,']              icel[',
     &       i3,']',/
     &       '                                ...ssmi cell accepted...')
c
c     ------------------------------------------------------------------
c
c!#   4.3.  Add to data matrix.
c
                     nssmi = nssmi + 1
                     if (nssmi .gt. mssmi) goto 903
c
                     vssmi(nssmi,ixg) = float(ic) + xc
                     vssmi(nssmi,iyg) = float(jc) + yc
                     vssmi(nssmi,ivelm) = wind(icel)
c
c     ------------------------------------------------------------------
c
c!#   4.4.  Convert dir to equivalent los wind speed (see note above).
c
                     vssmi(nssmi,ilosm) = dir(icel)*(10/1.2)
                     if (.not.use_losm .or. dir(icel) .eq. -999)
     &                   vssmi(nssmi,ilosm) = mvi_ssmi
c
c     ------------------------------------------------------------------
c
c!#   4.5.  Calculate los vector in terms of azimuth direction
c!#         (measured clockwise from north; see note above).
c
                     psi = (azm + (icel - 32.5)*1.6)*(pi/180)
                     vssmi(nssmi,ipx) = sin(psi)
                     vssmi(nssmi,ipy) = cos(psi)
                     vssmi(nssmi,isrow) = float(ssmirow)
                     vssmi(nssmi,iscol) = float(icel)
                     vssmi(nssmi,isrev) = rev
                     vssmi(nssmi,isst) = sst(icel)
                     vssmi(nssmi,itice) = tice(icel)
                     vssmi(nssmi,iwv) = vapor(icel)
                     vssmi(nssmi,icw) = cloud(icel)
                     vssmi(nssmi,irain) = rain(icel)
c
c     ------------------------------------------------------------------
c
c!#   4.6.  Store ssmi wind speed as SASS data with no direction
c!#         (if requested)
c
                     if (store_sass) then
                        
                        NPTS = NPTS + 1

                        if (NPTS .gt. PTMAX) then
                           print *, 'not enough space; increase PTMAX'
                           stop 4000
                        endif
                        
                        RA                    = 0.
                        XCOORD(NPTS)          = xc
                        YCOORD(NPTS)          = yc
                        ILL(NPTS)             = ic
                        JLL(NPTS)             = jc
                        IOBS(NPTS)            = 0
                        pt_qc(NPTS)           = 0
                        NWINDS(NPTS)          = 1
                        UOBS(1,NPTS)          = 0.
                        VOBS(1,NPTS)          = 0.
                        VBAR(NPTS)            = vssmi(nssmi,ivelm)

c!#         Define data weight so ssvelm departures are indepenent of
c!#         SSMI wind speed (see [AtlHB+96] for discussion).

                        WGT(NPTS)             = wgt_velm

c!#         Set missing ambiguities to NS_FILL

                        do 2100 j=2,4
                           UOBS(j,NPTS)        = FILL
                           VOBS(j,NPTS)        = FILL
 2100                   continue
                     endif
                  endif
               endif

  100       CONTINUE
  200    CONTINUE
  300 CONTINUE
  301 continue
      nchar = iolen(filename)
      write(*,*) 'Finished processing SSMI LOS data file',n,': ',
     &    filename(1:nchar)
c
c     ------------------------------------------------------------------
c
c!#   5.  Close SSMI-4 data file.
c
      close (unit=iu)
      goto 999
c
c     ------------------------------------------------------------------
c
c!## ERROR HANDLING :
 901  continue
      print *, 'vam(readssmi): unit no.', iu, ' already in use.'
      print *, '               Stopping...'
      stop 901
c
 902  continue
      print *, 'vam(readssmi): error opening file. iostat =', istat
      print *, '               Stopping...'
      stop 902
c
 903  continue
      print *, 'vam(readssmi): out of storage space for ssmi ',
     &    'data: nssmi=', nssmi
      print *, '               Increase parameter mssmi. Stopping...'
      stop 903
c 
 999  continue
      return
      end
