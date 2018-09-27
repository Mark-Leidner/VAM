c!# CSU IDENTIFICATION : ns_bscat.h
c!#   $Id: ns_bscat.h,v 1.6 1998/02/11 19:58:13 leidner Exp $

c!## PURPOSE : data structure for NSCAT backscatter measurements

c!# CSU SPECIFICATION AND CONSTRAINTS :

c!## REQUIREMENTS :

c!## CONSTRAINTS :

c!## LANGUAGE : Fortran

c!# CSU DESIGN :

c!## REFERENCES :

c!## LIMITATIONS :

c!##  CHANGE LOG :
c!#   $Log: ns_bscat.h,v $
c!#   Revision 1.6  1998/02/11 19:58:13  leidner
c!#   added wvc row and column to backscatter storage
c!#
c!#   Revision 1.5  1997/06/23 14:55:10  leidner
c!#   updated documentation style
c!#

c!#	Revision 1.4  1997/06/23  14:20:34  leidner
c!#	Added data version # and removed ns_kp (now carried as kpa, kpb, kpc)
c!#
c!#	Revision 1.3  1997/04/11  17:17:37  leidner
c!#	added ns_nsig0 to data structure and filled in some missing documentation
c!#
c!#	Revision 1.2  1997/04/08  19:49:16  leidner
c!#	added coeffs for sig0 variance equation
c!#
c!#	Revision 1.1  1997/02/21  23:45:35  leidner
c!#	Initial revision
c!#

c!## GLOBAL AND SHARED DATA :

c!# Parameters defined in vam.h
c!# NS_MWVC   Maximum number of Wind Vector Cells (WVC)
c!# NS_MOBS   Maximum number of sigma0 measurements

c!# Integers
c!# ns_nWVC   Number of WVCs
c!# ns_nobs   Number of sigma0 values
c!# ns_nsig0  Number of sigma0 values by WVC
c!# ns_ant    Antenna number
c!# ns_pol    Polarization (0=Hpol, 1=Vpol)
c!# WVC_ic    Integer part of grid coordinate ic+xc for WVC
c!# WVC_jc    Integer part of grid coordinate jc+yc for WVC
c!# ns_ic     Integer part of grid coordinate ic+xc for sigma0 observation
c!# ns_jc     Integer part of grid coordinate jc+yc for sigma0 observation
c!# ns_qc     Q.C. flag for sigma0 observation
c!# ns_tag    Arbitrary data identifier tag useful for isolating
c!#              data types
c!# revs      Rev numbers for this NSCAT data
c!# ns_nWVC_rev Number of WVCs by rev
c!# WVC_row   I-index (along track) of WVC (1-820)
c!# WVC_col   J-index (across track) of WVC (1-24)
c!# ns_row    I-index (along track) of WVC (1-820)
c!# ns_col    J-index (across track) of WVC (1-24)
c!# Ver       JPL version ID of Science Data Processing System (SDPS)
c!#           software used to process L17 data in the form 
c!#           <revision #>.<level #>.<branch #>
c!#           Ver(1) - revision #
c!#           Ver(2) - level #
c!#           Ver(3) - branch #
      integer ns_nWVC                , ns_nobs
      integer ns_nsig0(NS_MWVC)
      integer ns_ant(NS_MOBS)        , ns_pol(NS_MOBS)
      integer WVC_ic(NS_MWVC)        , WVC_jc(NS_MWVC)
      integer ns_ic (NS_MOBS)        , ns_jc (NS_MOBS)
      integer ns_qc (NS_MOBS)        , ns_tag(NS_MOBS)
      integer revs(NS_MAXREVS)       , ns_nWVC_rev(NS_MAXREVS)
      integer WVC_row(NS_MWVC)       , WVC_col(NS_MWVC)
      integer ns_row(NS_MOBS)        , ns_col(NS_MOBS)
      integer Ver(3)

c!# Reals
c!# ns_time   The time tag (from Vdata variable 'Mean_Time') as a
c!#              deviation, in minutes, from the synoptic time.
c!# WVC_Lat   Latitude of 50 km Wind Vector Cell
c!# WVC_Lon   Longitude of 50 km Wind Vector Cell
c!# WVC_xc    Real part of grid coordinate ic+xc for sigma0 observation
c!# WVC_yc    Real part of grid coordinate jc+yc for sigma0 observation
c!# ns_lat    Latitude of observation
c!# ns_lon    Longitude of observation
c!# ns_xc     Real part of grid coordinate ic+xc for sigma0 observation
c!# ns_yc     Real part of grid coordinate jc+yc for sigma0 observation
c!# ns_S0obs  Sigma0
c!# ns_theta  Beam incidence angle
c!# ns_azim   Radar pointing direction
c!# ns_kpa    Coefficient alpha in the JPL sigma0 variance (kp) eqn
c!# ns_kpb    Coefficient beta in the JPL sigma0 variance (kp) eqn
c!# ns_kpc    Coefficient gamma in the JPL sigma0 variance (kp) eqn
c!# ns_s0sd   Sigma0 std. dev.
c!# ns_s05    Trajectory sigma0
c!# ns_s0bg   Background sigma0
c!# ns_vel5   Trajectory wind speed
c!# ns_dir5   Trajectory wind direction
c!# ns_velbg  Background wind speed
c!# ns_dirbg  Background wind direction
      real ns_time (NS_MOBS)
      real WVC_Lat (NS_MWVC)        , WVC_Lon (NS_MWVC)
      real WVC_xc  (NS_MWVC)        , WVC_yc  (NS_MWVC)
      real ns_lat  (NS_MOBS)        , ns_lon  (NS_MOBS)
      real ns_xc   (NS_MOBS)        , ns_yc   (NS_MOBS)
      real ns_S0obs(NS_MOBS)
      real ns_theta(NS_MOBS)        , ns_azim (NS_MOBS)
      real ns_kpa  (NS_MOBS)        , ns_kpb  (NS_MOBS)
      real ns_kpc  (NS_MOBS) 
      real ns_s0sd (NS_MOBS)
      real ns_s05  (NS_MOBS)        , ns_s0bg (NS_MOBS)
      real ns_vel5 (NS_MOBS)        , ns_dir5 (NS_MOBS) 
      real ns_velbg(NS_MOBS)        , ns_dirbg(NS_MOBS)
c 
      common /NS_BSCAT/ ns_time , 
     &                  WVC_Lat , WVC_Lon , WVC_xc  , WVC_yc  ,
     &                  ns_lat  , ns_lon  , ns_xc   , ns_yc   , 
     &                  ns_s0obs, ns_theta, ns_azim , ns_kpa  , 
     &                  ns_kpb  , ns_kpc  , ns_s0sd ,
     &                  ns_s05  , ns_vel5 , ns_dir5 ,
     &                  ns_s0bg , ns_velbg, ns_dirbg, 
     &                  ns_nWVC , ns_nobs , ns_ant  , ns_pol  ,
     &                  WVC_ic  , WVC_jc  , WVC_row , WVC_col ,
     &                  ns_row  , ns_col  ,
     &                  ns_ic   , ns_jc   , ns_qc   , ns_tag  , 
     &                  revs    , ns_nsig0, ns_nWVC_rev,  Ver
