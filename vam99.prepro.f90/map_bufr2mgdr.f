      subroutine map_bufr2mgdr (values, nsubsets,
     o     WVC_Row_Time,                
     o     Rev_Number,                  
     o     WVC_Row,                     
     o     WVC_Lat,                     
     o     WVC_Lon,                     
     o     WVC_Quality_Flag,            
     o     Model_Speed,                 
     o     Model_Dir,                   
     o     Num_Ambigs,                  
     o     Wind_Speed,                  
     o     Wind_Dir,                    
     o     Wind_Speed_Err,              
     o     Wind_Dir_Err,                
     o     Max_Likelihood_Est,          
     o     WVC_Selection,               
     o     Sigma0_In_Cell,              
     o     Sigma0_Lat,                  
     o     Sigma0_Lon,                  
     o     Sigma0_Azimuth,              
     o     Sigma0_Incidence,            
     o     Sigma0,                      
     o     Kp_Alpha,                    
     o     Kp_Beta,                     
     o     Kp_Gamma,                    
     o     Sigma0_Atten_Value,          
     o     Sigma0_Qual_Flag,            
     o     Sigma0_Mode_Flag,            
     o     Sigma0_Surface_Flag,
     o     MP_Rain_Index,
     o     NOF_Rain_Index,
     o     Tb_Mean_H,
     o     Tb_Mean_V,
     o     Tb_StdDev_H,
     o     Tb_StdDev_V,
     o     Num_Tb_H,
     o     Num_Tb_V, 
     o     Tb_Rain_Rate,
     o     Tb_Attenuation,
     o     Satellite_ID,
     o     Sat_Motion,
     o     Instrument_ID,
     o     Cross_Track_Res,
     o     Along_Track_Res,
     o     WVC_Col,                              
     o     GMF_ID,
     o     Software_ID,
     o     Num_Fore_Inner,                       
     o     Num_Fore_Outer,                       
     o     Num_Aft_Inner,                        
     o     Num_Aft_Outer,
     o     K_Polar,
     o     Sigma0_Variance_QC,
     o     Time_to_Edge ) 
C
C**** *MAP_BUFR2MGDR*
C
C     $Id: map_bufr2mgdr.f,v 1.1 2001/08/10 17:58:20 mcc Exp $
C     $Log: map_bufr2mgdr.f,v $
C     Revision 1.1  2001/08/10 17:58:20  mcc
C     Initial revision.
C
C     Revision 1.17  2000/02/15 14:12:33  stl
C     changed order and encoding of time-to-edge parameter
C
C     Revision 1.16  2000/02/14 15:38:23  stl
C     fixed bug found by Frank Tveter
C
C     Revision 1.15  2000/02/07 15:56:27  stl
C     handle century events in BUFR
C
C     Revision 1.14  2000/01/31 17:52:41  stl
C     added new MGDR variables and time_to_edge
C
C     Revision 1.13  1999/09/13 12:08:02  stl
C     now a variable # of subsets is allowed
C
c Revision 1.12  1999/08/25  13:02:21  stl
c moved software id and gmf into header of message
c
c Revision 1.11  1999/07/14  11:25:54  stl
c xchanged ordering of sigma0 lat/lon and atmos_atten in BUFR template
c
c Revision 1.10  1999/06/22  13:36:54  stl
c made changes to satisfy Sun f90 compiler
c
c Revision 1.9  1999/05/25  10:21:59  stl
c added Model_Speed and _Dir
c
c Revision 1.8  1999/04/29  16:51:16  stl
c removed variables never used
c
c Revision 1.7  1999/04/29  15:12:36  stl
c added Model_Speed and Model_Dir
c
c Revision 1.6  1999/04/29  10:14:18  stl
c removed storage array init, since theses are overwritten
c removed iptr1
c
c Revision 1.5  1999/04/06  13:36:02  stl
c removed ms0
c
c Revision 1.4  1999/03/16  15:00:58  stl
c improved documentation and updated md2d algorithm
c
c Revision 1.3  1999/02/24  11:58:30  stl
c added new fields
c
c Revision 1.2  1999/02/22  18:02:54  stl
c updated documentation; updated for new QSCAT BUFR template
c
c Revision 1.1  1999/02/09  17:26:52  stl
c Initial revision
c
C
C     PURPOSE.
C     --------
C
C          Maps values from a decoded QSCAT BUFR message (i.e., one 25 km
C          WVC row) to storage arrays by data type.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *MAP_BUFR2MGDR* (VALUES,
C     o                    DATE_TIME,
C     o                    REV,
C     o                    ROW,
C     o                    WVC_LAT,
C     o                    .
C     o                    .
C     o                    .
C     o                    SURFACE_FLAG )
C
C          INPUT :
C               *VALUES*   - real array (expanded data values)
C
C          OUTPUT :
C               *WVC_Row_Time*  - Mean time for measurements in this WVC row
C               *Rev_Number*    - Orbit number
C               *WVC_Row*       - Along-track row number
C                .
C                .           - remaining storage arrays for QSCAT row data
C                .
C
C     METHOD.
C     -------
C
C     First, the input values array is transferred to a local work array
C     with missing values set to a printable MDI.  Then, all data in the
C     work array is copied to storage arrays by data type (i.e., WVC
C     latitudes, sigma0, wind speeds, etc.).  The ordering of sigma0's
C     in the MGDR file are not reproduced, so the order of the sigma0
C     data elements for a given WVC will be different from the original
C     MGDR data.  Filled storage arrays are passed back to the calling
C     routine.
C
C         1. First, store values in a local array.
C         2. Fill QSCAT MGDR arrays with data values.
C
C
C     EXTERNALS.
C     ----------
C
C          CALL MD2D
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. LEIDNER    *AER*       22/02/99.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
C     ------------------------------------------------------------------
      implicit none

c     Parameters for dimensioning storage arrays for scatterometer data
c     MaximumWINDs, MaximumTBs, NumberBeams, NumberWindVectorCells
      integer mwind, mtb, nb, nwvc
      parameter (mwind=4, mtb=2, nb=4, nwvc=76)

c     Parameters for filling the MGDR storage arrays
c     NumberParameters_IDentification
      integer npid
      parameter (npid=26)
c     NumberParameters_RAIN, NumberParameters_WIND
      integer nprain, npwind
c     NumberParametersTB, NumberParameters_Sigma0
      integer nptb, nps0
      parameter (nprain=4, npwind=5, nptb=4, nps0=14)
c     NumberParameters
      integer np
      parameter (np=npid+nprain+npwind*mwind+nptb*mtb+nb*(1+nps0))

c     Input/Output interface variables
      real  values(np,nsubsets)
      integer nsubsets
      character*24  WVC_Row_Time
      real
     $              WVC_Lat(nwvc),
     $              WVC_Lon(nwvc),
     $              Model_Speed(nwvc),
     $              Model_Dir(nwvc),
     $              Wind_Speed(mwind,nwvc),
     $              Wind_Dir(mwind,nwvc),
     $              Wind_Speed_Err(mwind,nwvc),
     $              Wind_Dir_Err(mwind,nwvc),
     $              Max_Likelihood_Est(mwind,nwvc),
     $              Sigma0_Lat(nb,nwvc),
     $              Sigma0_Lon(nb,nwvc),
     $              Sigma0_Azimuth(nb,nwvc),
     $              Sigma0_Incidence(nb,nwvc),
     $              Sigma0(nb,nwvc),
     $              Kp_Alpha(nb,nwvc),
     $              Kp_Beta(nb,nwvc),
     $              Kp_Gamma(nb,nwvc),
     $              Sigma0_Atten_Value(nb,nwvc),
     $              MP_Rain_Index(nwvc),
     $              Tb_Mean_H(nwvc),
     $              Tb_Mean_V(nwvc),
     $              Tb_StdDev_H(nwvc),
     $              Tb_StdDev_V(nwvc),
     $              Tb_Rain_Rate(nwvc),
     $              Tb_Attenuation(nwvc)
      integer
     $              Rev_Number,     
     $              WVC_Row,
     $              WVC_Quality_Flag(nwvc),
     $              Num_Ambigs(nwvc),
     $              WVC_Selection(nwvc),
     $              Sigma0_In_Cell(nwvc),
     $              Sigma0_Surface_Flag(nb,nwvc),
     $              Sigma0_Qual_Flag(nb,nwvc),
     $              Sigma0_Mode_Flag(nb,nwvc),
     $              NOF_Rain_Index(nwvc),
     $              Num_Tb_H(nwvc),
     $              Num_Tb_V(nwvc)

      integer Software_ID
      integer
     $              Satellite_ID, Instrument_ID, GMF_ID,
     $              WVC_Col(nwvc),
     $              Num_Fore_Inner(nwvc),
     $              Num_Fore_Outer(nwvc),
     $              Num_Aft_Inner(nwvc),
     $              Num_Aft_Outer(nwvc),
     $              K_Polar(nb,nwvc)
      real
     $              Cross_Track_Res, Along_Track_Res,
     $              Sigma0_Variance_QC(nb,nwvc),
     $              Sat_Motion, Time_to_Edge

c     Local variables for filling the values array
      integer iwvc, iwind, iptr, im, icel, jwvc
      integer i, j, kyear, kmonth, khour, kday, kdays, kminute, ksecond
      real    MISSING, RMISS, EPS

c     Local work space
      real    work(np,nwvc)

      data MISSING /-777.77/, RMISS /1.7E38/, EPS /10.E-10/

C     ------------------------------------------------------------------

c*    1. First, store values in a local array.
c        -------------------------------------

      do j=1,nsubsets          ! loop over subsets
         do i=1,np             ! loop over elements
            work(i,j) = values(i,j)
            if (abs(work(i,j)-RMISS).lt.EPS) work(i,j) = MISSING
         enddo
      enddo


c*    2. Fill QSCAT MGDR arrays with data values.
c        -----------------------------------------

c     Reconstruct the QSCAT Time Tag from BUFR elements

      kyear     = int(work( 9,1))
      if(kyear.eq.100) kyear=0   ! handle century events in BUFR
      kmonth    = int(work(10,1))
      kday      = int(work(11,1))
      khour     = int(work(12,1))
      kminute   = int(work(13,1))
      ksecond   = int(work(14,1))

      kdays = 0
      call md2d (kyear,kmonth,kday,kdays) 
      
      write(WVC_Row_Time,100) kyear, kdays, khour, kminute, ksecond, 0
  100 format(i4.4,'-',i3.3,'T',i2.2,':',i2.2,':',i2.2,'.',i3.3)

      Satellite_ID                           = int(work(1,1))
      Sat_Motion                             = work(2,1)
      Instrument_ID                          = int(work(3,1))
      GMF_ID                                 = int(work(4,1))
      Software_ID                            = int(work(5,1))
      Cross_Track_Res                        = int(work(6,1))
      Along_Track_Res                        = int(work(7,1))
      Rev_Number                             = int(work(8,1))
      Time_to_Edge                           = work(18,1)
      WVC_Row                                = int(work(19,1))

      icel = 1
      do 200 iwvc=1,nwvc


         WVC_Col(iwvc)                       = int(MISSING)
         WVC_Quality_Flag(iwvc)              = int(MISSING)
         WVC_Lat(iwvc)                       = MISSING
         WVC_Lon(iwvc)                       = MISSING
         Model_Dir(iwvc)                     = MISSING
         Model_Speed(iwvc)                   = MISSING
         Num_Ambigs(iwvc)                    = int(MISSING)
         WVC_Selection(iwvc)                 = int(MISSING)
         Sigma0_In_Cell(iwvc)                = int(MISSING)
         MP_rain_index(iwvc)                 = MISSING
         NOF_rain_index(iwvc)                = int(MISSING)
         Tb_rain_rate(iwvc)                  = MISSING
         Tb_attenuation(iwvc)                = MISSING

         do iwind=1,mwind                      
            Wind_Speed(iwind,iwvc)           = MISSING
            Wind_Speed_Err(iwind,iwvc)       = MISSING
            Wind_Dir(iwind,iwvc)             = MISSING
            Wind_Dir_Err(iwind,iwvc)         = MISSING
            Max_Likelihood_Est(iwind,iwvc)   = MISSING
         enddo

         Num_Tb_H(iwvc)                      = int(MISSING)
         Tb_Mean_H(iwvc)                     = MISSING
         Tb_StdDev_H(iwvc)                   = MISSING
         Num_Tb_V(iwvc)                      = int(MISSING)
         Tb_Mean_V(iwvc)                     = MISSING
         Tb_StdDev_V(iwvc)                   = MISSING

c        For each Sigma0 measurement slot
         do im = 1, nb

            if (im .eq. 1) then
               Num_Fore_Inner(iwvc)          = int(MISSING)
            else if (im .eq. 2) then           
               Num_Fore_Outer(iwvc)          = int(MISSING)
            else if (im .eq. 3) then           
               Num_Aft_Inner(iwvc)           = int(MISSING)
            else if (im .eq. 4) then           
               Num_Aft_Outer(iwvc)           = int(MISSING)
            endif

            Sigma0_Lat(im,iwvc)              = MISSING
            Sigma0_Lon(im,iwvc)              = MISSING
            Sigma0_Atten_Value(im,iwvc)      = MISSING
            Sigma0_Azimuth(im,iwvc)          = MISSING
            Sigma0_Incidence(im,iwvc)        = MISSING
            K_Polar(im,iwvc)                 = int(MISSING)
            Sigma0(im,iwvc)                  = MISSING
            Kp_Alpha(im,iwvc)                = MISSING
            Kp_Beta(im,iwvc)                 = MISSING
            Kp_Gamma(im,iwvc)                = MISSING
            Sigma0_Qual_Flag(im,iwvc)        = int(MISSING)
            Sigma0_Mode_Flag(im,iwvc)        = int(MISSING)
            Sigma0_Surface_Flag(im,iwvc)     = int(MISSING)
            Sigma0_Variance_QC(im,iwvc)      = MISSING
            
         enddo

         if(icel .gt. nsubsets) goto 200

         jwvc = int(work(20,icel))

         if (iwvc .eq. jwvc) then

         WVC_Col(iwvc)                       = jwvc
         WVC_Quality_Flag(iwvc)              = int(work(21,icel))
         WVC_Lat(iwvc)                       = work(15,icel)
         WVC_Lon(iwvc)                       = work(16,icel)
         Model_Dir(iwvc)                     = work(22,icel)
         Model_Speed(iwvc)                   = work(23,icel)
         Num_Ambigs(iwvc)                    = int(work(24,icel))
         WVC_Selection(iwvc)                 = int(work(25,icel))
         Sigma0_In_Cell(iwvc)                = int(work(26,icel))
         MP_rain_index(iwvc)                 = work(27,icel)
         NOF_rain_index(iwvc)                = int(work(28,icel))
         Tb_rain_rate(iwvc)                  = work(29,icel)
         Tb_attenuation(iwvc)                = work(30,icel)

         do iwind=1,mwind
            iptr=npid + nprain + (iwind-1)*npwind
            Wind_Speed(iwind,iwvc)           = work(1+iptr,icel)
            Wind_Speed_Err(iwind,iwvc)       = work(2+iptr,icel)
            Wind_Dir(iwind,iwvc)             = work(3+iptr,icel)
            Wind_Dir_Err(iwind,iwvc)         = work(4+iptr,icel)
            Max_Likelihood_Est(iwind,iwvc)   = work(5+iptr,icel)
         enddo
         
         iptr=npid + nprain + npwind*mwind
         Num_Tb_H(iwvc)                      = int(work(2+iptr,icel))
         Tb_Mean_H(iwvc)                     = work(3+iptr,icel)
         Tb_StdDev_H(iwvc)                   = work(4+iptr,icel)

         iptr=npid + nprain + npwind*mwind + nptb
         Num_Tb_V(iwvc)                      = int(work(2+iptr,icel))
         Tb_Mean_V(iwvc)                     = work(3+iptr,icel)
         Tb_StdDev_V(iwvc)                   = work(4+iptr,icel)

c        For each Sigma0 measurement slot
         do im = 1, nb

            iptr=npid + nprain + mwind*npwind + nptb*mtb +
     &       (im-1)*(1 + nps0) + 1

            if (im .eq. 1) then
               Num_Fore_Inner(iwvc)          = work(iptr,icel)
            else if (im .eq. 2) then
               Num_Fore_Outer(iwvc)          = work(iptr,icel)
            else if (im .eq. 3) then
               Num_Aft_Inner(iwvc)           = work(iptr,icel)
            else if (im .eq. 4) then
               Num_Aft_Outer(iwvc)           = work(iptr,icel)
            endif

            Sigma0_Lat(im,iwvc)              = work(1+iptr,icel)
            Sigma0_Lon(im,iwvc)              = work(2+iptr,icel)
            Sigma0_Atten_Value(im,iwvc)      = work(3+iptr,icel)
            Sigma0_Azimuth(im,iwvc)          = work(4+iptr,icel)
            Sigma0_Incidence(im,iwvc)        = work(5+iptr,icel)
            K_Polar(im,iwvc)                 = work(6+iptr,icel)
            Sigma0(im,iwvc)                  = work(7+iptr,icel)
            Kp_Alpha(im,iwvc)                = work(8+iptr,icel)
            Kp_Beta(im,iwvc)                 = work(9+iptr,icel)
            Kp_Gamma(im,iwvc)                = work(10+iptr,icel)
            Sigma0_Qual_Flag(im,iwvc)        = work(11+iptr,icel)
            Sigma0_Mode_Flag(im,iwvc)        = work(12+iptr,icel)
            Sigma0_Surface_Flag(im,iwvc)     = work(13+iptr,icel)
            Sigma0_Variance_QC(im,iwvc)      = work(14+iptr,icel)

         enddo
         icel = icel + 1

         endif
 200  continue

      return
      end
C
C     ------------------------------------------------------------------
C
      subroutine md2d (kyear,kmonth,kday,kdays)
c
      dimension imonth(12)
      save imonth
      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
c
c!!!! will not work in 2100
      if (mod(kyear,4).eq.0) then
         imonth(2)=29
      else
         imonth(2)=28
      endif

      kdays=0 ! value returned if kmonth or kday is out of range
c
      if (kmonth .gt. 0 .and. kmonth .le. 12) then
        if (kday .gt. 0 .and. kday .le. imonth(kmonth)) then
          kdays=kday
          do i=1,kmonth-1
            kdays = kdays + imonth(i)
          enddo
        endif
      endif

      return
      end
