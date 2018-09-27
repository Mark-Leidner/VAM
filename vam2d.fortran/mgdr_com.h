c	$Id: mgdr_com.h,v 1.2 1998/03/10 16:12:37 leidner Exp $
c	$Log: mgdr_com.h,v $
c	Revision 1.2  1998/03/10 16:12:37  leidner
c	changed some variable names which conflicted with existing
c	variable names in the vam
c	
c	Revision 1.1  1998/02/16 21:01:42  leidner
c	Initial revision
c

c      Copyright (C) 1995, California Institute of Technology.  U.S.
c      Government Sponsorship acknowledged.
c 
c       ** MGDR_COM.INC **
c
c     Definition for NSCAT 25km Merged Geophysical Data Record
c     Converted output common block
c
c     
      	CHARACTER*24 TimeTag

      	INTEGER*4
     $             Revnum,              ! rev number
     $             WVC_I,               ! WV row index (along-track)
     $             WVC_J(OUTW),         ! WV column index (cross-track)
     $             WVCqual(OUTW),    	! 2-bit data quality flag
     $             No_ambigs(OUTW),     ! number of ambiguities
     $             Ambselect(OUTW)      ! selected ambiguity (checkmark)
c
      	LOGICAL
     $             Lowspd_flags(OUTW),
     $             Highspd_flags(OUTW),
     $             Sig0Use(BUFD,OUTW)
c
      	REAL*4
     $             WVCLat(OUTW),           ! WVC latitude
     $             WVCLon(OUTW),           ! WVC longitude
     $             MeanWind(OUTW),         ! mean speed of ambiguities
     $             Windspeed(AMBIGS,OUTW), ! wind speed sol'ns 1-AMBIGS
     $             Wind_dir(AMBIGS,OUTW),  ! wind direction 1-AMBIGS
     $             Errspeed(AMBIGS,OUTW),  ! wind speed error 1-AMBIGS
     $             Err_dir(AMBIGS,OUTW),   ! error in direction 1-AMBIGS
     $             Likelihood(AMBIGS,OUTW) ! rel. sol'n probability 1-AMBIGS
c
      	INTEGER*4
     $             NumSig0(OUTW),
     $             NumGood(OUTW),
     $             NumFORE(OUTW),
     $             NumMIDV(OUTW),
     $             NumMIDH(OUTW),
     $             NumAFT(OUTW),
     $             Beam_inx(2,4,OUTW),
     $             Polarization(BUFD,OUTW),
     $             Sig0Qual(BUFD,OUTW),    
     $             SurfFlag(BUFD,OUTW),
     $             Atm_Atten(BUFD,OUTW)
c
     	REAL*4
     $             Sig0Lat(BUFD,OUTW),     ! sigma0 cell latitudes
     $             Sig0Lon(BUFD,OUTW),     ! sigma0 cell longitudes
     $             Cell_Azi(BUFD,OUTW),    ! cell azimuths of sigma0's
     $             Inc_Angle(BUFD,OUTW),   ! incidence angles of sigma0's
     $             Sig0(BUFD,OUTW),        ! sigma0 values
     $             Alpham(BUFD,OUTW),      ! Kp "alpha" coefficient
     $             Betam(BUFD,OUTW),       ! Kp "beta" coefficient
     $             Gammam(BUFD,OUTW)       ! Kp "gamma" coefficient
c
      	COMMON /MGDR_COM/
     $             TimeTag, Revnum, WVC_I,
     $             WVCLat, WVCLon, WVC_J, WVCqual,
     $             MeanWind, No_ambigs, Ambselect,
     $             Windspeed, Errspeed, Wind_dir, Err_dir, Likelihood,
     $             Lowspd_flags, Highspd_flags,
     $             NumSig0, NumGood,
     $             NumFORE, NumMIDV, NumMIDH, NumAFT,
     $             Beam_inx,
     $             Sig0Lat, Sig0Lon,
     $             Cell_Azi, Inc_Angle,
     $             Sig0, Alpham, Betam, Gammam, Polarization,
     $             Atm_Atten, 
     $             Sig0Qual, Sig0Use,
     $             SurfFlag
