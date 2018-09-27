	SUBROUTINE MGDR_READ(irec)
c
c       $Id: mgdr_read.F,v 1.3 1998/03/25 16:56:12 leidner Exp $
c       $Log: mgdr_read.F,v $
c       Revision 1.3  1998/03/25 16:56:12  leidner
c       added return mistakenly removed from r1.2
c
c       Revision 1.2  1998/03/10 16:07:21  leidner
c       changed file handling and some variable names which conflicted
c       with existing variables in the vam
c
c       Revision 1.1  1998/02/16 21:01:42  leidner
c       Initial revision
c
c	Record conversion routine for the NOAA/NSCAT MGDR files.
c
c      	Copyright (C) 1995, California Institute of Technology.  U.S.
c      	Government Sponsorship acknowledged.
c 
c
#include "l20param.h"
#include "mgdr_out.h"
#include "mgdr_com.h"
#include "l20files.h"
c
      	character*1 a(MGDRSIZE)
      	equivalence (a(1),Row_Time)
c       -----------------------------------------------------------------
c
      	if (irec .eq. 0) then         ! OPEN FILE IF IREC = 0
	   OPEN (unit=LUN20,name=MGDR_FILE,err=20,recl=MGDRSIZE,
     &       access='direct')
	   return
	endif
	if (irec .eq. 1) then
	   print *,'Attempt to read header record as data'
	   print *,'Resetting record number to 2'
	   irec = 2
	endif
c       
      	read(LUN20,rec=irec,end=40) a
c
      	TimeTag = Row_Time
      	Revnum  = Rev
      	WVC_I   = WVC_Rowm
c
	do i = 1,OUTW
		WVC_J(i) 	= WVC_Colm(i)
		WVCqual(i) 	= WVCqual_flag(i)
		No_ambigs(i) 	= Nambigs(i)
		Ambselect(i) 	= WVC_select(i)
		NumSig0(i) 	= NumSigma0(i)
		NumGood(i) 	= Num_Good_Sigma0(i)
		NumFORE(i) 	= NumBeam_FORE(i)
		NumMIDV(i) 	= NumBeam_MIDV(i)
		NumMIDH(i) 	= NumBeam_MIDH(i)
		NumAFT(i) 	= NumBeam_AFT(i)
c
		WVCLat(i) 	= WVC_latm(i)*0.01
		WVCLon(i) 	= WVC_lonm(i)*0.01
		if (WVCLon(i) .lt. 0.) 
     $              WVCLon(i)   = WVCLon(i) + 655.36
		MeanWind(i) 	= Mean_wind(i)*0.01

		if (i .le. 24) then
			iside = 1
		else
			iside = 2
		endif

	    	Lowspd_flags(i)  = btest(Low_wind_flag(iside),i-1)
            	Highspd_flags(i) = btest(High_wind_flag(iside),i-1)

	    do j = 1,AMBIGS
		Windspeed(j,i) 	= Windspd(j,i)*0.01
		Wind_dir(j,i) 	= Winddir(j,i)*0.01
		if (Wind_dir(j,i) .lt. 0.) 
     $ 		    Wind_dir(j,i) = Wind_dir(j,i) + 655.36
		Errspeed(j,i) 	= Errspd(j,i)*0.01
		Err_dir(j,i) 	= Errdir(j,i)*0.01
		Likelihood(j,i) = MLE_like(j,i)*0.1
            enddo

	    do j = 1,BUFD
		Polarization(j,i) = K_Polar(j,i)
		Sig0Qual(j,i) 	= Sigma0_Quality_Flag(j,i)
		SurfFlag(j,i) 	= Surface_Flag(j,i)

		Sig0Lat(j,i) 	= Cen_Lat(j,i)*0.01
		Sig0Lon(j,i) 	= Cen_Lon(j,i)*0.01
		if (Sig0Lon(j,i) .lt. 0.) 
     $		    Sig0Lon(j,i) = Sig0Lon(j,i) + 655.36
		Cell_Azi(j,i) 	= Cell_Azimuth(j,i)*0.01
		if (Cell_Azi(j,i) .lt. 0.) 
     $ 		    Cell_Azi(j,i) = Cell_Azi(j,i) + 655.36
		Inc_Angle(j,i) 	= Incidence_Angle(j,i)*0.01
		Sig0(j,i) 	= Sigma0(j,i)*0.01

		int4 		= Coeff_A(j,i)
		if (int4 .lt. 0) int4 = int4 + 65536
		Alpham(j,i) 	= int4*1.e-6
		int4 		= Coeff_B(j,i)
		if (int4 .lt. 0) int4 = int4 + 65536
		Betam(j,i) 	= int4*1.e-7
		int4 		= Coeff_C(j,i)
		if (int4 .lt. 0) int4 = int4 + 65536
		Gammam(j,i) 	= int4*1.e-9

		int4 		= Mean_Atm_Atten(j,i)
		if (int4 .lt. 0) int4 = int4 + 256
		Atm_Atten(j,i)  = int4*0.004

	    	ibyte = (j-1)/8 + 1
	    	jbit = mod((j-1),8)
	    	itest = Sigma0_Usable_Flag(ibyte,i)
	    	Sig0Use(j,i) = btest(itest,jbit)

	    enddo

	    do j = 1,4
		do k = 1,2
		     Beam_inx(k,j,i) = Beam_Ptr(k,j,i)
c         print 100, k, j, i, Beam_Ptr(k,j,i), Beam_inx(k,j,i)
		enddo
	    enddo
	enddo
c
      	return
 100	format('k=', i1, '  j=', i1, '  i=', i2, ' Beam_Ptr=', l2, 
     &       ' Beam_inx=', i2)

20    	print *,'Open error'
	irec = -1
	return

30    	Print *,'Read error'
	irec = -1
	return

40    	close(LUN20)
	print ('(a,a)'), 'reached end of file', MGDR_FILE
	irec = -1
	return
        end


