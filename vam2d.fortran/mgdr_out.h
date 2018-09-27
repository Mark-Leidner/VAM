c	$Id: mgdr_out.h,v 1.2 1998/03/10 16:10:59 leidner Exp $
c	$Log: mgdr_out.h,v $
c	Revision 1.2  1998/03/10 16:10:59  leidner
c	changed some variable names which conflicted with existing
c	variable names in the vam
c	
c	Revision 1.1  1998/02/16 21:01:42  leidner
c	Initial revision
c

c      Copyright (C) 1995, California Institute of Technology.  U.S.
c      Government Sponsorship acknowledged.
c 
c       ** MGDR_OUT.INC **
c
c     Definition for NSCAT 25km Merged Geophysical Data Record
c     Output common block
c
c     
      CHARACTER*24 Row_Time                     ! mean time tag for WVC row

      INTEGER*4
     $             Low_wind_flag(2),
     $             High_wind_flag(2)

      INTEGER*2
     $             Rev,                         ! rev number
     $             WVC_Rowm,                    ! WV row index (along-track)
     $             WVC_latm(OUTW),              ! WVC latitude
     $             WVC_lonm(OUTW),              ! WVC longitude
     $             Mean_wind(OUTW),             ! mean speed of ambiguities
     $             Windspd(AMBIGS,OUTW),        ! wind speed sol'ns 1-AMBIGS
     $             Winddir(AMBIGS,OUTW),        ! wind direction 1-AMBIGS
     $             Errspd(AMBIGS,OUTW),         ! wind speed error 1-AMBIGS
     $             Errdir(AMBIGS,OUTW),         ! error in direction 1-AMBIGS
     $             MLE_like(AMBIGS,OUTW),     ! rel. sol'n probability 1-AMBIGS
     $             Cen_Lat(BUFD,OUTW),          ! sigma0 cell latitudes
     $             Cen_Lon(BUFD,OUTW),          ! sigma0 cell longitudes
     $             Cell_Azimuth(BUFD,OUTW),     ! cell azimuths of sigma0's
     $             Incidence_Angle(BUFD,OUTW),  ! incidence angles of sigma0's
     $             Sigma0(BUFD,OUTW),           ! sigma0 values
     $             Coeff_A(BUFD,OUTW),          ! Kp "alpha" coefficient
     $             Coeff_B(BUFD,OUTW),          ! Kp "beta" coefficient
     $             Coeff_C(BUFD,OUTW),          ! Kp "gamma" coefficient
     $             Sigma0_Quality_Flag(BUFD,OUTW)    

      BYTE
     $             WVC_Colm(OUTW),        ! WV column index (cross-track)
     $             WVCqual_flag(OUTW),    ! 2-bit data quality flag
     $             Nambigs(OUTW),               ! number of ambiguities
     $             WVC_select(OUTW),      ! selected ambiguity (checkmark)
     $             NumSigma0(OUTW),
     $             Num_Good_Sigma0(OUTW),
     $             NumBeam_FORE(OUTW),
     $             NumBeam_MIDV(OUTW),
     $             NumBeam_MIDH(OUTW),
     $             NumBeam_AFT(OUTW),
     $             Beam_Ptr(2,4,OUTW),
     $             K_Polar(BUFD,OUTW),
     $             Sigma0_Usable_Flag(2,OUTW),
     $             Surface_Flag(BUFD,OUTW),
     $             Mean_Atm_Atten(BUFD,OUTW)  
c
      COMMON /MGDR_OUT/
     $             Row_Time, Rev, WVC_Rowm,
     $             WVC_Latm, WVC_Lonm, WVC_Colm, WVCqual_flag,
     $             Mean_Wind, Nambigs, WVC_select,
     $             Windspd, Errspd, Winddir, Errdir, Mle_like,
     $             Low_wind_flag, High_wind_flag,
     $             NumSigma0, Num_Good_Sigma0,
     $           NumBeam_FORE, NumBeam_MIDV, NumBeam_MIDH, NumBeam_AFT,
     $             Beam_Ptr,
     $             Cen_Lat, Cen_Lon,
     $             Cell_Azimuth, Incidence_Angle,
     $             Sigma0, Coeff_A, Coeff_B, Coeff_C, K_Polar,
     $             Mean_Atm_Atten, 
     $             Sigma0_Quality_Flag, Sigma0_Usable_Flag,
     $             Surface_Flag
