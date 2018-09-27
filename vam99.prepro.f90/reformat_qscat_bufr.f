c!#
c!# CSU IDENTIFICATION : reformat_qscat_bufr
c!#     $Id: reformat_qscat_bufr.f,v 1.5 2004/03/19 20:47:33 rnh Exp $

c!# PURPOSE : 

c!# CSU SPECIFICATION AND CONSTRAINTS:

c!# REQUIREMENTS : 
c!#  % setenv BUFR_TABLES /foehn/nwp/leidner/p651/bufr/src/bufr.tables/

c!# CONSTRAINTS : set parcel=1 for SGI, parcel=4 for SUN.

c!# LANGUAGE : Fortran90

c!# CSU DESIGN :

c!# INPUT/OUTPUT INTERFACE :

      program reformat_qscat_bufr

c!# GLOBAL AND SHARED DATA : 

      implicit none

c!# DATA CONVERSION : None

c!# ALGORITHMS : 

c!# LIMITATIONS : 

c!# CHANGE LOG : 
c!#	$Log: reformat_qscat_bufr.f,v $
c!#	Revision 1.5  2004/03/19 20:47:33  rnh
c!#	FGAT: u_fgat=vfgat=0, alpha=1.  Still TBD: are times useful
c!#	
c!#	Revision 1.4  2002/05/22 17:36:03  mcc
c!#	Same as revision 1.3 just put this in different place within code.
c!#	
c!#	Revision 1.3  2002/05/22 12:21:46  mcc
c!#	Added coastlnd, rain, rainflguse flags to control wnds obs write.
c!#	This control must be hardwired by the user as needed.
c!#	
c!#	Revision 1.2  2001/10/04 13:31:44  mcc
c!#	Adding lat/lon windowing capability via vam99_qscat_bufr.csh for both
c!#	wnds and sig0 modes. Also added some additional commenting.
c!#	

c!# LOCAL DATA ELEMENTS : TBD

c!# LOCAL DATA STRUCTURES : None

c!# DATA FILES : None

c!# LOGIC FLOW AND DETAILED ALGORITHM: 

c!#     1. 

c!#     2. 

      character*132 :: fname1, hdrfname, binfname
      character*256 :: bufrfile
      character*4 :: outftype

      logical :: jplrank

      integer, parameter ::  lunin=90
      integer :: ierr, iostat
      integer :: imess   ! # of messages (rows) counted by scan_bufr

      real :: satidconst, modfnconst, kpm2const, dfacconst      
      real :: xmin, xmax, ymin, ymax

      external getarg

      namelist /qsfnwnds/ 
     &     bufrfile, outftype, hdrfname, binfname, jplrank,
     &     dfacconst, xmin, xmax, ymin, ymax

      namelist /qsfnsig0/ 
     &     bufrfile, outftype, hdrfname, binfname, 
     &     satidconst, modfnconst, kpm2const,
     &     xmin, xmax, ymin, ymax

c     Retrieve Input Arguments

      call getarg(1,fname1)

      print*,'fname1 = ',fname1
      open(unit=lunin,file=fname1,status='old',err=900)
      rewind lunin

c     Execute as directed by namelist.

      ierr = 0
      print*,'try reading wnds namelist'
      read (lunin,qsfnwnds,iostat=ierr) 
      if (ierr /= 0) then
         print*,'error reading wnds namelist: iostat=',ierr
         print*,' '
         print*,'try reading sig0 namelist'
         ierr = 0
         rewind lunin
         read (lunin,qsfnsig0,iostat=ierr) 
         if (ierr /= 0) then
            print*, 'error reading namelist: iostat=',ierr
            goto 999
         endif
      endif

c
      print*,' ' 
      print*,'namelist = '
      print*,'quikscat buffer filename =  ',bufrfile
      print*,'quikscat processing type =  ',outftype
      if ((outftype == 'wnds') .or. (outftype == 'WNDS')) then
         print*,'jplrank = ',jplrank
         print*,'dfacconst = ',dfacconst
         print*,'longitude min max range = ',xmin,' ',xmax
         print*,'latitude min max range = ',ymin,' ',ymax
      endif
      if ((outftype == 'sig0') .or. (outftype == 'SIG0')) then
         print*,'satidconst = ',satidconst
         print*,'modfnconst = ',modfnconst
         print*,'kpm2const = ',kpm2const
         print*,'longitude min max range = ',xmin,' ',xmax
         print*,'latitude min max range = ',ymin,' ',ymax
      endif
      print*,' ' 

c     scan BUFR file to determine # of messages

      imess = 0
      call scan_bufr (bufrfile, imess)

c     read in qscat data in buffer format
      call rd_bufr (bufrfile, hdrfname, binfname, imess, outftype, 
     &     satidconst, modfnconst, kpm2const, dfacconst, jplrank,
     &     xmin, xmax, ymin, ymax)

      go to 999

 900  continue
      print *, 'reformat_qscat_bufr: unable to access namelist file.'
      call exit(1)

 999  continue

      close(lunin)
      stop
      end
c
c====================================================================
c
      subroutine rd_bufr 
     &     (bufrfile, hdrfname, binfname, maxrows, outftype, 
     &     satidconst, modfnconst, kpm2const, dfacconst, jplrank,
     &     xmin, xmax, ymin, ymax)
c
c====================================================================
C
c     METHOD.
c     -------
C
c     All QSCAT BUFR messages are read, decoded and copied into
c     storage arrays.
c     Data not present in the 25 km QSCAT BUFR message or not present in
c     the 25 km QSCAT MGDR product are written to lines prefixed by '+'.
C
c     EXTERNALS.
c     ----------
C
c         CALL PBOPEN                 |  from ECMWF BUFR software
c         CALL PBBUFR                 |  from ECMWF BUFR software
c         CALL BUS012                 |  from ECMWF BUFR software
c         CALL BUFREX                 |  from ECMWF BUFR software
c         CALL BUPRS0                 |  from ECMWF BUFR software
c         CALL BUPRS1                 |  from ECMWF BUFR software
c         CALL BUUKEY                 |  from ECMWF BUFR software
c         CALL BUPRS2                 |  from ECMWF BUFR software
c         CALL BUSEL                  |  from ECMWF BUFR software
c         CALL BUPRS3                 |  from ECMWF BUFR software
c         CALL MAP_BUFR2MGDR          |  from AER 
C
c     REFERENCE.
c     ----------
C
c     NONE.
C
C
c     AUTHOR.
c     -------
C
c          M. LEIDNER       *AER*         12/11/98
C
C
c     MODIFICATIONS.
c     --------------
c
c          Made coding changes for quikscat buffer reformat 
c          for use as input to the vam.  -M. Cerniglia 2/2001.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)

C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=20000,JSUBS=400,JCVAL=150 ,JBUFL=20000,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT=1200,
     3          JKEY=46,JBYTE=80000)
C
      PARAMETER (KELEM=20000)
      PARAMETER (KVALS=80000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY)
C
      DIMENSION KTDLST(JELEM),KTDEXP(JELEM)
C

      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)

c
c     Parameters which define dimensions of the scat data
c     MaximumWINDs, MaximumTBs, NumberBeams, NumberWindVectorCells
      integer mwind, mtb, nb, nwvc
      parameter (mwind=4, mtb=2, nb=4, nwvc=76)

c     Parameters for filling the values array
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

      DIMENSION VALUES(np,nwvc)
      parameter(jwork=np*nwvc)

c  QSCAT MGDR Data Storage Arrays
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
      integer       
     $              Satellite_ID, Instrument_ID, GMF_ID,
     $              WVC_Col(nwvc),
     $              Num_Fore_Inner(nwvc),
     $              Num_Fore_Outer(nwvc),
     $              Num_Aft_Inner(nwvc),
     $              Num_Aft_Outer(nwvc),
     $              K_Polar(nb,nwvc)
                    
      real          
     $              Sigma0_Variance_QC(nb,nwvc),
     $              Sat_Motion, Cross_Track_Res, Along_Track_Res,
     $              Time_to_Edge

c     Local variables for command line interface
      character*256 bufrfile
      integer return_code
c
c------------------------------------------------------------------
      integer, parameter :: maxwvc=76, maxambig=4, maxnb=4 
      integer :: ierr
      integer, intent (in) :: maxrows
      character*132 :: hdrfname, binfname
      character*4 :: outftype
      character*21, dimension(:), allocatable :: timetags

      integer, dimension(:), allocatable :: revnum, wvcrow

      real :: xmin, xmax, ymin, ymax
cmcc      real, dimension(:,:), allocatable :: wvclat, wvclon, wvcqualflg,
cmcc     &     wvccol, numambigs, wvcselection, modelspeed, modeldir

      real, dimension(:,:), allocatable :: wvclat, wvclon, wvcqualflg,
     &     wvccol, numambigs, wvcselection, modelspeed, modeldir,
     &     mprain

      real, dimension(:,:,:), allocatable :: windspeed, winddir,
     & windspeederr, winddirerr, maxlikelihoodest, celllat, celllon,
     & cellazimuth, cellincidence, sig0, sigma0attnval, kpalpha,
     & kpbeta, kpgamma, kpolar, sigma0qualflag, sigma0modeflag,
     & surfaceflag, cellindex

c     allocate memory based on maxrows

      ierr = 0 
      allocate(timetags(maxrows),stat=ierr)
      if (ierr /= 0) stop 'rd_bufr: allocate failure'

      allocate(revnum(maxrows), wvcrow(maxrows), stat=ierr)
      if (ierr /= 0) stop 'rd_bufr: allocate failure'

cmcc      allocate(wvclat(maxwvc,maxrows), wvclon(maxwvc,maxrows),
cmcc     &     wvccol(maxwvc,maxrows), numambigs(maxwvc,maxrows), 
cmcc     &     wvcselection(maxwvc,maxrows), modelspeed(maxwvc,maxrows), 
cmcc     &     modeldir(maxwvc,maxrows), stat=ierr)

      allocate(wvclat(maxwvc,maxrows), wvclon(maxwvc,maxrows),
     &     wvccol(maxwvc,maxrows), numambigs(maxwvc,maxrows), 
     &     wvcselection(maxwvc,maxrows), modelspeed(maxwvc,maxrows), 
     &     modeldir(maxwvc,maxrows), wvcqualflg(maxwvc,maxrows),
     &     mprain(maxwvc,maxrows),stat=ierr)

      if (ierr /= 0) stop 'rd_bufr: allocate failure'

      allocate(windspeed(maxambig,maxwvc,maxrows), 
     & winddir(maxambig,maxwvc,maxrows),
     & windspeederr(maxambig,maxwvc,maxrows), 
     & winddirerr(maxambig,maxwvc,maxrows), 
     & maxlikelihoodest(maxambig,maxwvc,maxrows), 
     & celllat(maxnb,maxwvc,maxrows), 
     & celllon(maxnb,maxwvc,maxrows), 
     & cellazimuth(maxnb,maxwvc,maxrows), 
     & cellincidence(maxnb,maxwvc,maxrows), 
     & sig0(maxnb,maxwvc,maxrows), 
     & sigma0attnval(maxnb,maxwvc,maxrows), 
     & kpalpha(maxnb,maxwvc,maxrows),
     & kpbeta(maxnb,maxwvc,maxrows), 
     & kpgamma(maxnb,maxwvc,maxrows), 
     & kpolar(maxnb,maxwvc,maxrows), 
     & sigma0qualflag(maxnb,maxwvc,maxrows), 
     & sigma0modeflag(maxnb,maxwvc,maxrows),
     & surfaceflag(maxnb,maxwvc,maxrows), 
     & cellindex(maxnb,maxwvc,maxrows), stat=ierr)
      if (ierr .ne. 0) stop 'rd_bufr: allocate failure'

c------------------------------------------------------------------

      nbytpw=jbpw/8
      iobs=0
      eps=10.E-10
      n=0
      OSEC3=.FALSE.
C
C*          1.  OPEN FILE CONTAINING BUFR DATA.
c               -------------------------------

      return_code=0 
      call pbopen(iunit,bufrfile(1:index(bufrfile,' ')-1),
     &     'r',return_code)
      if (return_code.eq.-1) stop 'Open failed'
      if (return_code.eq.-2) stop 'Invalid input file name'
      if (return_code.eq.-3) stop 'Invalid open mode specified'
C
C*          2.  READ BUFR MESSAGE.
c               ------------------

c     loop over entire file and count number of messages (rows)

        imsg=1
        do while (.TRUE.)
C
         IERR=0
         KBUFL=0
C
         IRET=0
         CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET) 
         IF(IRET.EQ.-1) THEN
            print*,' '
            print*,'Number of subsets     ',iobs
            print*,'Number of messages    ',n
            print*,'read complete, end-of-file reached'
            print*,' '
            goto 801
         END IF
         IF(IRET.EQ.-2) STOP 'File handling problem' 
         IF(IRET.EQ.-3) STOP 'Array too small for product'
C
         KBUFL=KBUFL/nbytpw+1
         n = n + 1
C
C*          3.  EXPAND BUFR MESSAGE.
c               --------------------

            CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
            IF(KER.ne.0) THEN
               PRINT*,'Error in BUS012: ',KERR
               PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
               KERR=0
               stop 'Stopping.'
            END IF

            KEL=np

            CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2,
     1           KSEC3 ,KSEC4, KEL,CNAMES,CUNITS,KVALS,
     2           VALUES,CVALS,IERR)

            iobs=iobs+ksec3(3)

c     
C*          3.1 PRINT CONTENT OF EXPANDED DATA.
c               -------------------------------

            IF(.NOT.OSEC3) GO TO 450
C
C*          3.2 PRINT SECTION ZERO OF BUFR MESSAGE.
c               -----------------------------------

            CALL BUPRS0(KSEC0)
C
C*          3.3 PRINT SECTION ONE OF BUFR MESSAGE.
c               -----------------------------------

            CALL BUPRS1(KSEC1)
C
C*          3.4 PRINT SECTION TWO OF BUFR MESSAGE.
c               -----------------------------------
c              AT ECMWF SECTION 2 CONTAINS RDB KEY.
c              SO UNPACK KEY

            CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
c               PRINT KEY
C
            CALL BUPRS2(KSUP ,KEY)
C
C*          3.5 PRINT SECTION 3 OF BUFR MESSAGE.
c               -----------------------------------
 450        CONTINUE
C
c               FIRST GET DATA DESCRIPTORS
C
            CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
            IF(KERR.ne.0) THEN
               WRITE(*,*) 'KERR =', KERR
               STOP 'CALL TO BUSEL FAILED'
            ENDIF
C
c               PRINT  CONTENT
C
            IF(OSEC3) THEN
               CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
            ENDIF
C
c     -----------------------------------------------------------------
C*          4.   STORE BUFR DATA VALUES IN QSCAT MGDR STORAGE ARRAYS.
c                ---------------------------------------------------

c                Initialize Storage arrays

            nsub                      = ksec3(3) ! number of subsets in this message
            WVC_Row_Time              = ' '
            Rev_Number                = 0
            WVC_Row                   = 0

            do iwvc=1,nwvc

               WVC_Lat(iwvc)              = 0.
               WVC_Lon(iwvc)              = 0.
               Model_Speed(iwvc)          = 0.
               Model_Dir(iwvc)            = 0.
               Sigma0_In_Cell(iwvc)       = 0
               WVC_Quality_Flag(iwvc)     = 0
               Num_Ambigs(iwvc)           = 0
               WVC_Selection(iwvc)        = 0
               WVC_Col(iwvc)              = 0

               do is0=1,nb
                  Sigma0_Lat(is0,iwvc)           = 0.
                  Sigma0_Lon(is0,iwvc)           = 0.
                  Sigma0_Azimuth(is0,iwvc)       = 0.
                  Sigma0_Incidence(is0,iwvc)     = 0.
                  Sigma0(is0,iwvc)               = 0.
                  Kp_Alpha(is0,iwvc)             = 0.
                  Kp_Beta(is0,iwvc)              = 0.
                  Kp_Gamma(is0,iwvc)             = 0.
                  Sigma0_Qual_Flag(is0,iwvc)     = 0
                  Sigma0_Mode_Flag(is0,iwvc)     = 0
                  Sigma0_Surface_Flag(is0,iwvc)  = 0
                  Sigma0_Atten_Value(is0,iwvc)   = 0.
                  K_Polar(is0,iwvc)              = 0
                  Sigma0_Variance_QC(is0,iwvc)   = 0.
               end do

               do iwind=1,mwind
                  Wind_Speed(iwind,iwvc)           = 0.
                  Wind_Dir(iwind,iwvc)             = 0.
                  Wind_Speed_Err(iwind,iwvc)       = 0.
                  Wind_Dir_Err(iwind,iwvc)         = 0.
                  Max_Likelihood_Est(iwind,iwvc)   = 0.
               end do

            end do
C
c                Store BUFR values array in Storage Arrays
C
            call map_bufr2mgdr (values, nsub, 
     o           WVC_Row_Time,                      
     o           Rev_Number,              
     o           WVC_Row,                            
     o           WVC_Lat,                        
     o           WVC_Lon,                        
     o           WVC_Quality_Flag,               
     o           Model_Speed,
     o           Model_Dir,
     o           Num_Ambigs,                     
     o           Wind_Speed,                     
     o           Wind_Dir,                       
     o           Wind_Speed_Err,                    
     o           Wind_Dir_Err,                      
     o           Max_Likelihood_Est,                 
     o           WVC_Selection,                  
     o           Sigma0_In_Cell,                     
     o           Sigma0_Lat,                        
     o           Sigma0_Lon,                        
     o           Sigma0_Azimuth,                   
     o           Sigma0_Incidence,                
     o           Sigma0,                         
     o           Kp_Alpha,                        
     o           Kp_Beta,                        
     o           Kp_Gamma,                        
     o           Sigma0_Atten_Value,               
     o           Sigma0_Qual_Flag,            
     o           Sigma0_Mode_Flag,             
     o           Sigma0_Surface_Flag,
     o           MP_Rain_Index,
     o           NOF_Rain_Index,
     o           Tb_Mean_H,
     o           Tb_Mean_V,
     o           Tb_StdDev_H,
     o           Tb_StdDev_V,
     o           Num_Tb_H,
     o           Num_Tb_V, 
     o           Tb_Rain_Rate,
     o           Tb_Attenuation,
     o           Satellite_ID,
     o           Sat_Motion,
     o           Instrument_ID,
     o           Cross_Track_Res,
     o           Along_Track_Res,
     o           WVC_Col,
     o           GMF_ID,
     o           Software_ID,
     o           Num_Fore_Inner,
     o           Num_Fore_Outer,
     o           Num_Aft_Inner,
     o           Num_Aft_Outer,
     o           K_Polar,
     o           Sigma0_Variance_QC,
     o           Time_to_Edge )
C
C*          5.   LOAD  DATA
c                ----------
c------------------------------------------------------------------
            do iwvc=1,nwvc

               wvclat(iwvc,imsg) = wvc_lat(iwvc)
               wvclon(iwvc,imsg) = wvc_lon(iwvc)
               wvcqualflg(iwvc,imsg)=float(WVC_Quality_Flag(iwvc))
               wvccol(iwvc,imsg) = float(wvc_col(iwvc))
               modelspeed(iwvc,imsg) = model_speed(iwvc)
               modeldir(iwvc,imsg) = model_dir(iwvc)
               numambigs(iwvc,imsg) = num_ambigs(iwvc)
               wvcselection(iwvc,imsg) = wvc_selection(iwvc)
               mprain(iwvc,imsg) = MP_Rain_Index(iwvc)

               do is0=1,nb
                   celllat(is0,iwvc,imsg) = sigma0_lat(is0,iwvc)
                   celllon(is0,iwvc,imsg) = sigma0_lon(is0,iwvc)
                   cellazimuth(is0,iwvc,imsg) = 
     &                  sigma0_azimuth(is0,iwvc)
                   cellincidence(is0,iwvc,imsg) = 
     &                 sigma0_incidence(is0,iwvc)
                   sig0(is0,iwvc,imsg) = sigma0(is0,iwvc)
                   kpalpha(is0,iwvc,imsg) = kp_alpha(is0,iwvc)
                   kpbeta(is0,iwvc,imsg) = kp_beta(is0,iwvc)
                   kpgamma(is0,iwvc,imsg) = kp_gamma(is0,iwvc)
                   sigma0qualflag(is0,iwvc,imsg) =
     &                 sigma0_qual_flag(is0,iwvc)
                   sigma0modeflag(is0,iwvc,imsg) = 
     &                 sigma0_mode_flag(is0,iwvc)
                   surfaceflag(is0,iwvc,imsg) = 
     &                 sigma0_surface_flag(is0,iwvc)
                   sigma0attnval(is0,iwvc,imsg) =
     &                 sigma0_atten_value(is0,iwvc)
                   kpolar(is0,iwvc,imsg) = k_polar(is0,iwvc)
               end do
c
               do iwnd=1,mwind
                  windspeed(iwnd,iwvc,imsg) = wind_speed(iwnd,iwvc) 
                  winddir(iwnd,iwvc,imsg) = wind_dir(iwnd,iwvc)
                  windspeederr(iwnd,iwvc,imsg) = 
     &                 wind_speed_err(iwnd,iwvc)
                  winddirerr(iwnd,iwvc,imsg) = 
     &                 wind_dir_err(iwnd,iwvc)
                  maxlikelihoodest(iwnd,iwvc,imsg) =
     &                 max_likelihood_est(iwnd,iwvc)
               end do

            end do

c     revnum may change within qscat bufr file

         revnum(imsg) = rev_number
         wvcrow(imsg) = wvc_row
         timetags(imsg) = wvc_row_time
         imsg = imsg + 1

      end do
c         
 801  imsg = imsg - 1
      print*,'** IMESS, IMSG = ',maxrows,' ',imsg
      if (maxrows /= imsg) stop 'rd_bufr: maxrows .ne. imsg'

      if (outftype == 'wnds') call wt_bufr_wnds
     &     (hdrfname, binfname, jplrank, 
     &     dfacconst, maxrows, timetags, revnum, wvcrow, wvclat, wvclon,
     &     modelspeed, modeldir, numambigs, wvcselection, windspeed,
     &     winddir, windspeederr, winddirerr, maxlikelihoodest,
     &     wvcqualflg, mprain, xmin, xmax, ymin, ymax)
cmcc     &     xmin, xmax, ymin, ymax)


      if (outftype == 'sig0') call wt_bufr_sig0
     &     (hdrfname, binfname, satidconst, modfnconst, kpm2const, 
     &     maxrows, timetags, revnum, wvcrow, wvccol, celllat, celllon, 
     &     cellazimuth, cellincidence, sig0, sigma0attnval, kpalpha, 
     &     kpbeta, kpgamma, kpolar, sigma0qualflag, sigma0modeflag, 
     &     surfaceflag, cellindex, xmin, xmax, ymin, ymax)

      return
      end
c     
c====================================================================
c     
      subroutine  wt_bufr_wnds
     &     (hdrfname, binfname, jplrank, 
     &     dfacconst, maxrows, timetags, revnum, wvcrow, wvclat, wvclon,
     &     modelspeed, modeldir, numambigs, wvcselection, windspeed,
     &     winddir, windspeederr, winddirerr, maxlikelihoodest,
     &     wvcqualflg, mprain, xmin, xmax, ymin, ymax)
cmcc     &     xmin, xmax, ymin, ymax)
c     
c====================================================================
c
c   notes: 

c   timetags(maxrows) is in the form of
c   YYYY-DDDThh:mm:ss.sss - 21 byte character field
c   YYYY is the year, DDD is day of year (Julian),
c   hh is hour (UTC), mm is minutes and ss.sss is seconds

      implicit none
    
      integer, parameter :: lunout = 91, lunrev=92, nvar=5, maxwvc=76, 
     &     maxambig=4, nocc = maxambig, nconst=1, lname=10, parecl=4
c     Note, parecl=1 for SGI, parecl=4 for SUN.

      integer :: jpltim2secs, avetim, nnloc, iocc,
     &     i, j, k, ierr, nocc, nvar, namb, jamb, iamb,
     &     isel, rank(4,4), recl, jdb, jde, sb, se, sbhold

c
c     rank matrix (isel cols, namb rows)
c      1 2 3 4
c      2 1 1 1
c      3 3 2 2
c      4 4 4 3

      character*21 :: btime, etime, ytime
      character*(lname) :: cobsid, namcon(nconst), namvar(nvar)

      logical :: yrchng

      real :: vconst(nconst), dfacconst, wspd, wdir, uwnd, vwnd, pi
      real :: xmin, xmax, ymin, ymax, rain, rainflguse, coastlnd

c!#   local data structures :

      integer, intent (in) :: maxrows
      logical, intent (in) :: jplrank

      character*21,  intent(in) :: timetags(maxrows)
      character*132, intent(in) :: hdrfname, binfname

      integer, intent(in) :: revnum(maxrows), wvcrow(maxrows)

      real, intent(in) :: wvclat(maxwvc,maxrows),wvclon(maxwvc,maxrows),
     &     modelspeed(maxwvc,maxrows), modeldir(maxwvc,maxrows),
     &     numambigs(maxwvc,maxrows), wvcselection(maxwvc,maxrows),
     &     wvcqualflg(maxwvc,maxrows), mprain(maxwvc,maxrows)

      real, intent(in) :: windspeed(maxambig,maxwvc,maxrows), 
     & winddir(maxambig,maxwvc,maxrows),
     & windspeederr(maxambig,maxwvc,maxrows), 
     & winddirerr(maxambig,maxwvc,maxrows), 
     & maxlikelihoodest(maxambig,maxwvc,maxrows)

      logical, dimension(:,:), allocatable :: qcflag
c     qcflag = F for good retrieval
c     qcflag = T for bad retrieval

      integer, dimension(:), allocatable :: numocc, recid

      real, dimension(:), allocatable :: time,
     &     latdeg, londeg, uint, vint, ufgat, vfgat, alpha

      real, dimension(:,:,:), allocatable :: qsdata

      ierr=0
      allocate(qcflag(nocc,maxwvc*maxrows),stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_wnds: allocate failure'

      allocate(numocc(maxwvc*maxrows), recid(maxwvc*maxrows), stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_wnds: allocate failure'

      allocate(time(maxwvc*maxrows), 
     &     latdeg(maxwvc*maxrows), londeg(maxwvc*maxrows), 
     &     uint(maxwvc*maxrows), vint(maxwvc*maxrows),
     &     ufgat(maxwvc*maxrows), vfgat(maxwvc*maxrows),
     &     alpha(maxwvc*maxrows), stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_wnds: allocate failure'

      allocate(qsdata(nvar,nocc,maxwvc*maxrows), stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_wnds: allocate failure'
c
      data rank /1,2,3,4,2,1,1,1,3,3,2,2,4,4,4,3/
      data cobsid/'ambiguous'/
      data namcon/'dfac'/
      data namvar/'u_wind','v_wind','sd(spd)','sd(dir)','mle'/
c
      qsdata(:,:,:) = 0.
      pi = 4.0 * atan(1.e0)
      vconst(1) = dfacconst

c     get start and end times for this rev and write to file.
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field

      btime=timetags(1)
      etime=timetags(maxrows)

c     revnum may change within qscat bufr file,
c     only write initial rev number.

      open (unit=lunrev, file='rev_times.dat', form='formatted',
     &     iostat=ierr)
      if ( ierr /= 0 ) 
     &     stop 'wt_bufr_wnds: aborting on rev_times.dat open'
      write (lunrev,101)
      write (lunrev,102) revnum(1), btime, etime
      close (lunrev)
c
c     if end rev time crosses into next year adjust end time accordingly.

      sb = jpltim2secs(btime)
      se = jpltim2secs(etime)
      if ( se  <  sb ) then
         read (btime(06:08),'(i3.3)') jdb 
         read (etime(06:08),'(i3.3)') jde
         print*,' rev time crosses into next year ',jdb,' ',jde
         if ( jdb .le. jde ) stop 
     &       'begin julday le end julday at year change'
         se = se + jdb * 86400
      endif
      avetim = ( sb + se ) / 2

c     if rev time crosses into next year, set yrchng to .true. 

      yrchng = .false.
      sbhold = 0 
      nnloc = 0
      where (wvclon < 0.) wvclon = wvclon + 360.

      do i = 1, maxrows

         ytime=timetags(i)
         sb = jpltim2secs(ytime)
         if( sbhold > sb ) yrchng = .true.
         if( yrchng ) sb = sb + jdb * 86400
         if(yrchng) stop 'wnds: yrchng = .true.'
         sbhold = sb

         do j = 1, maxwvc

            if(numambigs(j,i) <= 0.) cycle
            if((wvclat(j,i) < -90.).or.(wvclon(j,i) < -180.)) cycle

            if((wvclat(j,i)>=ymin).and.(wvclat(j,i)<=ymax))then
               if((wvclon(j,i)>=xmin).and.(wvclon(j,i)<=xmax))then

c     coastlnd, rain, rainflguse can be used to control obs write
c     as determined by user.

                  coastlnd = 1.   ! assume %wvc over land
c     coastlnd = 1.               ! %wvc over land
c     coastlnd = 0.               ! no land mass

                  rain = 1.       ! assume rain and rain flag not useable
c     rain = 1.                   ! rain
c     rain = 0.                   ! no rain

                  rainflguse = 1. ! assume rain and rain flag not useable
c     rainflguse = 1              ! rain flag is not useable
c     rainflguse = 0              ! rain flag is useable

                  rainflguse=float(ibits(int(wvcqualflg(j,i)),3,1))
                  rain = float(ibits(int(wvcqualflg(j,i)),2,1))
                  coastlnd= float(ibits(int(wvcqualflg(j,i)),8,1))

                  nnloc = nnloc + 1
                  iamb = int(numambigs(j,i))
                  numocc(nnloc) = iamb
                  do iocc = 1, nocc
                     qcflag(iocc,nnloc) = iocc > iamb  
                  enddo
                  time(nnloc) = float(sb)-float(avetim)
                  latdeg(nnloc) = wvclat(j,i)
                  londeg(nnloc) = wvclon(j,i) 
                  recid(nnloc) = j+maxwvc*(i+maxrows*revnum(i))

c     get wind direction, need to convert from -180->180 to 0->360;
c     directions are in meteorological convention.
c     load u and v model wind components.

                  wspd = modelspeed(j,i)
                  wdir = modeldir(j,i) 
                  if (wdir < 0.) wdir = wdir + 360.
                  wdir = wdir * pi / 180.0
                  call winds(uwnd, vwnd, wspd, wdir, .true.)
                  uint(nnloc) = uwnd
                  vint(nnloc) = vwnd
                  ufgat(nnloc) = 0.
                  vfgat(nnloc) = 0.
                  alpha(nnloc) = 1.

c     isel selects proper row in rank array

                  isel = 1
                  if(jplrank) isel = int(wvcselection(j,i))

c     load u and v wind components then load errors in 
c     wind speed and direction. jamb controls ambiguity 
c     ordering of loading based on isel.

                  do namb=1,iamb
                    jamb = rank(isel,namb)
                    wspd = windspeed(jamb,j,i)
                    wdir = winddir(jamb,j,i)
                    if (wdir < 0.) wdir = wdir + 360.0
                    wdir = wdir * pi / 180.0
                    call winds(uwnd, vwnd, wspd, wdir, .true.)

                    wspd = windspeederr(jamb,j,i)
                    wdir = winddirerr(jamb,j,i)
                    if (wdir < 0.) wdir = wdir + 360.0

                    qsdata(1,namb,nnloc) = uwnd
                    qsdata(2,namb,nnloc) = vwnd
                    qsdata(3,namb,nnloc) = wspd
                    qsdata(4,namb,nnloc) = wdir
                    qsdata(5,namb,nnloc) = maxlikelihoodest(jamb,j,i)  
                  enddo

               endif
            endif

         enddo
      enddo

c     
c     write header/data to ascii/binary file.

      recl = nnloc * parecl 
      call wrtobs(lunout, hdrfname, binfname,
     *     cobsid, nconst, nnloc, nocc, nvar,
     *     namcon, vconst, namvar, recl, numocc(1:nnloc), 
     *     recid(1:nnloc), time(1:nnloc), latdeg(1:nnloc), 
     *     londeg(1:nnloc), uint(1:nnloc), vint(1:nnloc), 
     *     ufgat(1:nnloc), vfgat(1:nnloc), alpha(1:nnloc),
     *     qcflag(:,1:nnloc), qsdata(:,:,1:nnloc), ierr)

      print*,'hdrfname = ',hdrfname
      print*,'binfname = ',binfname
      print*,' '
      if (ierr == 0) then
         print *,'reformat_qscat_bufr: wrote wnds obs to ',
     *        hdrfname,binfname
      else
         print *,'reformat_qscat_bufr: wrtobs error ierr= ',ierr
         print *,'writing obs to ',hdrfname,binfname
      endif
c
 101  format('  Rev         UTC Start                UTC Stop')
 102  format(2x,i4,3x,a21,3x,a21)

      return
      end     
c    
c====================================================================
c     
      subroutine  wt_bufr_sig0
     &     (hdrfname, binfname, satidconst, modfnconst, kpm2const, 
     &     maxrows, timetags, revnum, wvcrow, wvccol, celllat, celllon, 
     &     cellazimuth, cellincidence, sig0, sigma0attnval, kpalpha, 
     &     kpbeta, kpgamma, kpolar, sigma0qualflag, sigma0modeflag, 
     &     surfaceflag, cellindex, xmin, xmax, ymin, ymax)
c     
c====================================================================
c
c     notes: 
c     timetags(maxrows) is in the form of
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field
c     YYYY is the year, DDD is day of year (Julian),
c     hh is hour (UTC), mm is minutes and ss.sss is seconds

      implicit none
    
      integer, parameter :: maxwvc = 76, maxnb = 4, nvar = 7, 
     &     lunout = 91, lunrev = 92, nocc = 1, nconst = 3, lname = 10, 
     &     parecl = 4
c     Note, parecl=1 for SGI, parecl=4 for SUN.

      integer :: jpltim2secs, avetim, i, j, k, nnloc, ierr, lierr,
     & jdb, jde, sb, se, sb_hold, recl, nloc

      character*21 :: btime, etime, ytime

      logical :: yrchng
      real :: pi, d2r, satidconst, modfnconst, kpm2const, vconst(nconst)
      real :: xmin, xmax, ymin, ymax

      character*(lname) :: cobsid, namcon(nconst), namvar(nvar)

      integer, intent (in) :: maxrows
      integer, intent (in) :: revnum(maxrows), wvcrow(maxrows)

      character*21, intent(in) :: timetags(maxrows)
      character*132, intent(in) ::  hdrfname, binfname

      real, intent(inout) :: sig0(maxnb,maxwvc,maxrows)

      real, intent(in) :: wvccol(maxwvc,maxrows)

      real, intent(in) :: celllat(maxnb,maxwvc,maxrows), 
     & celllon(maxnb,maxwvc,maxrows), 
     & cellazimuth(maxnb,maxwvc,maxrows), 
     & cellincidence(maxnb,maxwvc,maxrows), 
     & sigma0attnval(maxnb,maxwvc,maxrows), 
     & kpalpha(maxnb,maxwvc,maxrows),
     & kpbeta(maxnb,maxwvc,maxrows), 
     & kpgamma(maxnb,maxwvc,maxrows), 
     & kpolar(maxnb,maxwvc,maxrows), 
     & sigma0qualflag(maxnb,maxwvc,maxrows), 
     & sigma0modeflag(maxnb,maxwvc,maxrows),
     & surfaceflag(maxnb,maxwvc,maxrows), 
     & cellindex(maxnb,maxwvc,maxrows)
c
      integer, dimension(:), allocatable :: numocc, recid

      logical, dimension(:,:), allocatable :: qcflag

      real, dimension(:), allocatable :: time,
     &     latdeg, londeg, uint, vint, ufgat, vfgat, alpha

      real, dimension(:,:,:), allocatable :: qsdata

      ierr=0
      allocate(numocc(maxwvc*maxrows*maxnb),recid(maxwvc*maxrows*maxnb),
     &     stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_sig0: allocate failure'

      allocate(time(maxwvc*maxrows*maxnb), uint(maxwvc*maxrows*maxnb), 
     &     vint(maxwvc*maxrows*maxnb), ufgat(maxwvc*maxrows*maxnb), 
     &     vfgat(maxwvc*maxrows*maxnb), alpha(maxwvc*maxrows*maxnb),
     &     latdeg(maxwvc*maxrows*maxnb), londeg(maxwvc*maxrows*maxnb),
     &     stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_sig0: allocate failure'

      allocate(qcflag(nocc,maxwvc*maxrows*maxnb),stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_sig0: allocate failure'

      allocate(qsdata(nvar,nocc,maxwvc*maxrows*maxnb), stat=ierr)
      if (ierr /= 0) stop 'wt_bufr_sig0: allocate failure'

c     initialize 
      data cobsid/'sigma0'/
      data namcon/'satid', 'modfn', 'kpm2'/
      data namvar/'hv-polar','incidence','azimuth','sigma0',
     &  'kpa','kpb','kpc'/

      uint(:) = 0.
      vint(:) = 0.
      numocc(:) = 1
      ufgat(:) = 0.
      vfgat(:) = 0.
      alpha(:) = 1.
      qcflag(:,:) = .false.     ! all good obs
      qsdata(:,:,:) = 0.

      pi = 4.0 * atan(1.e0)
      d2r = pi / 180.0
      vconst(1) = satidconst
      vconst(2) = modfnconst
      vconst(3) = kpm2const

c     get start and end times for this rev and write to file.
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field

      btime=TimeTags(1)
      etime=TimeTags(maxrows)

c     revnum may change within qscat bufr file, only write initial rev number.

      open  (unit=lunrev, file='rev_times.dat', form='formatted',
     &     iostat=ierr)
      if ( ierr /= 0 ) 
     &     stop 'wt_bufr_sig0: aborting on rev_times.dat open'
      write (lunrev,201)
      write (lunrev,202) revnum(1), btime, etime
      close (lunrev)
c
c     if end rev time crosses into next year, adjust end time accordingly.

      sb = jpltim2secs(btime)
      se = jpltim2secs(etime)
      if ( se < sb ) then
         read (btime(06:08),'(i3.3)') jdb 
         read (etime(06:08),'(i3.3)') jde
         print*,' rev time crosses into next year ',jdb,' ',jde
         if ( jdb .le. jde ) stop 
     &       'begin julday le end julday at year change'
         se = se + jdb * 86400
      endif
      avetim = ( sb + se ) / 2

c     if rev time crosses into next year, set yrchng to TRUE.
c     loop over all rows and sigma0 cells.

      yrchng = .false.
      sb_hold = 0 
      nnloc = 0
      where (celllon < 0.) celllon = celllon + 360.

      do  i = 1, maxrows

         ytime=TimeTags(i)
         sb = jpltim2secs(ytime)
         if( sb_hold > sb ) yrchng = .true.
         if( yrchng ) sb = sb + jdb * 86400
         sb_hold = sb

         do  j = 1, maxwvc

            do k = 1, maxnb

c     check for land mass (= value of 0 -> no land is present)
               if(ibits(int(surfaceflag(k,j,i)),15,1)/=0) cycle

c     check for missing values (-777.77)
               if((celllat(k,j,i)<-90.).or.(celllon(k,j,i)<-180)) cycle

c     only process data with window described by namelist

               if((celllat(k,j,i)>=ymin).and.(celllat(k,j,i)<=ymax))then
                  if((celllon(k,j,i)>=xmin).and.(celllon(k,j,i)<=xmax))
     &                 then

                     nnloc = nnloc + 1
                     time(nnloc) = float(sb)-float(avetim)
                     recid(nnloc) = 
     &                    k+maxnb*(j+maxwvc*(i+maxrows*revnum(i)))
                     latdeg(nnloc) = celllat(k,j,i)
                     londeg(nnloc) = celllon(k,j,i)

c     perform quality control on each sigma0 cell(k,j,i).
c     check state of several parameters and load qcflag.
c     set sigma0 cells at polar latitudes to bad retrieval.

                     if((latdeg(nnloc) < -70.).or.
     &                    (latdeg(nnloc) > 70.)) then
                        qcflag(nocc,nnloc) = .true.

                     else if(kpalpha(k,j,i) == 0.) then
                        qcflag(nocc,nnloc) = .true.
                     else if(kpgamma(k,j,i) == 0.) then
                        qcflag(nocc,nnloc) = .true.
                     else if(kpbeta(k,j,i)  == 0. .and. 
     &                       kpalpha(k,j,i) == 0. .and. 
     &                       kpgamma(k,j,i) == 0.) then
                        qcflag(nocc,nnloc) = .true.

c     check quality flag to determine if data is usable
                     else if(sigma0qualflag(k,j,i) /= 0.) then
                        qcflag(nocc,nnloc) = .true.

c     check for land/ice and map availabilities
                     else if(surfaceflag(k,j,i) /= 0.) then
                        qcflag(nocc,nnloc) = .true.
                     else if(ibits(int(sigma0modeflag(k,j,i)),15,1)/=0)
     &                       then
                        qcflag(nocc,nnloc) = .true.
                     else if(ibits(int(sigma0modeflag(k,j,i)),14,1)/=0)
     &                       then
                        qcflag(nocc,nnloc) = .true.
                     else if(ibits(int(sigma0modeflag(k,j,i)),11,1)/=0)
     &                       then 
                        qcflag(nocc,nnloc) = .true.
                     else if(ibits(int(sigma0modeflag(k,j,i)),10,1)/=0)
     &                       then 
                        qcflag(nocc,nnloc) = .true.

                     endif

c     correct for atmospheric effect, sigma0 (surface) = 
c     sigma0 (toa) + attenuation correction * secant of incidence angle;
c     convert from db to linear space .

                     sig0(k,j,i) = sig0(k,j,i) + ! db space
     &                sigma0attnval(k,j,i)/cos(cellincidence(k,j,i)*d2r)

                     sig0(k,j,i) = 10.**(sig0(k,j,i)/10.) * ! linear space
     &                   (-1.)**(ibits(int(sigma0qualflag(k,j,i)),13,1))

c     polarization = 0. -> horz. pol. ; = 1. -> vert. pol.
c     kpgamma needs to be converted from [dB] to a ratio.
                     qsdata(1,nocc,nnloc) = kpolar(k,j,i)
                     qsdata(2,nocc,nnloc) = cellincidence(k,j,i) * d2r
                     qsdata(3,nocc,nnloc) = cellazimuth(k,j,i) * d2r
                     qsdata(4,nocc,nnloc) = sig0(k,j,i)
                     qsdata(5,nocc,nnloc) = kpalpha(k,j,i)
                     qsdata(6,nocc,nnloc) = kpbeta(k,j,i)
                     if(kpbeta(k,j,i) < 0.) qsdata(6,nocc,nnloc) = 1.e-6
                     qsdata(7,nocc,nnloc) = 10.**(kpgamma(k,j,i)/10.)

                  endif
               endif

            enddo
         enddo

      enddo

c     write header/data to ascii/binary file.

      recl = nnloc * parecl 
      call wrtobs(lunout, hdrfname, binfname,
     *     cobsid, nconst, nnloc, nocc, nvar,
     *     namcon, vconst, namvar, recl, numocc(1:nnloc), 
     *     recid(1:nnloc), time(1:nnloc), latdeg(1:nnloc), 
     *     londeg(1:nnloc), uint(1:nnloc), vint(1:nnloc), 
     *     ufgat(1:nnloc), vfgat(1:nnloc), alpha(1:nnloc),
     *     qcflag(:,1:nnloc), qsdata(:,:,1:nnloc), lierr)

      ierr = lierr
      if (ierr == 0) then
         print *,'reformat_qscat_bufr: wrote sig0 obs to ',
     *        hdrfname, binfname
      else
         print *,'reformat_qscat_bufr: wrtobs error ierr= ',ierr
         print *,'writing obs to ', hdrfname, binfname
      endif
c
 201  format('  Rev         UTC Start                UTC Stop')
 202  format(2x,i4,3x,a21,3x,a21)

      return
      end     

c====================================================================
c
      function jpltim2secs(jpltime)
c
c====================================================================
c
c     convert jpl time stamp to seconds from YYYY.

c     JPL timetags is in the form of
c     YYYY-DDDThh:mm:ss.sss - 21 byte character field
c     YYYY is the year, DDD is day of year (Julian),
c     hh is hour (UTC), mm is minutes and ss.sss is seconds

      implicit none
      integer ihrb, imnb, iscb, jdb
      integer secm, sech, secd, jpltim2secs

      parameter (secm=60,sech=60*secm,secd=24*sech)

      character*21 jpltime
c
      read (jpltime(06:08),'(i3.3)') jdb
      read (jpltime(10:11),'(i2.2)') ihrb
      read (jpltime(13:14),'(i2.2)') imnb
      read (jpltime(16:17),'(i2.2)') iscb

      jpltim2secs = (jdb-1)*secd + ihrb*sech + imnb*secm + iscb

      return
      end
c
c====================================================================
c
