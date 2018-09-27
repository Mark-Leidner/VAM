c!#   $Id: ns_winds.h,v 1.3 1998/05/20 14:28:23 leidner Exp $
c!#   $Log: ns_winds.h,v $
c!#   Revision 1.3  1998/05/20 14:28:23  leidner
c!#   added storage for position of selected ambiguity
c!#
c!#   Revision 1.2  1998/03/18 16:29:56  leidner
c!#   added rain flagging QC option for NSCAT data
c!#
c!#   Revision 1.1  1997/04/17 13:39:25  leidner
c!#   Initial revision
c!#
c
c* Parameters defined in vam.h
c     NS_MWVC      Maximum number of Wind Vector Cells (WVC)
c     NS_MAXREVS   Maximum number of revs of data
c
c* Integers
c     nsw_col      column number (cell) of a given wvc
c     nsw_ic       Integer part of grid coordinate ic+xc for NSCAT winds
c     nsw_jc       Integer part of grid coordinate jc+yc for NSCAT winds
c     nsw_nambigs  Number of wind vector ambiguities for a given WVC
c     nsw_nwvc     Number of NSCAT WVCs
c     nsw_nwvc_rev Number of NSCAT WVCs by rev
c     nsw_qc       Q.C. flag for NSCAT winds
c     nsw_revs     Rev numbers of NSCAT winds data
c     nsw_row      row number of a given wvc
c     nsw_tag      Arbitrary data identifier tag useful for isolating
c                      data types
c     Sel          the position of the selected ambiguity
c
      integer nsw_col(NS_MWVC)        , nsw_ic(NS_MWVC), nsw_jc(NS_MWVC)
      integer nsw_nambigs(NS_MWVC)    , nsw_nwvc
      integer nsw_nwvc_rev(NS_MAXREVS), nsw_qc(NS_MWVC)
      integer nsw_revs(NS_MAXREVS)    , nsw_row(NS_MWVC)
      integer nsw_tag(NS_MWVC)        , Sel(NS_MWVC)
c
c* Reals
c     nsw_al       Collocated GOES albedo
c     nsw_dt       Time difference (GOES-NSCAT) in seconds
c     nsw_lat      Geodetic latitude of the center of the wvc.
c     nsw_lon      Geodetic longitude of the center of the wvc.
c     nsw_mle      Relative likelihood that a given wind vector
c                      solution is correct.
c     nsw_tb       Collocated GOES brightness temperature
c     nsw_time     The time tag (from Vdata variable 'Mean_Time') as a
c                      deviation, in minutes, from the synoptic time.
c     nsw_u        u-component of vector wind solutions for a given wvc.
c     nsw_v        v-component of vector wind solutions for a given wvc.
c     nsw_xc       Real part of grid coordinate ic+xc for NSCAT winds.
c     nsw_yc       Real part of grid coordinate jc+yc for NSCAT winds.
c
      real    nsw_al(NS_MWVC)         , nsw_dt(NS_MWVC)
      real    nsw_lat(NS_MWVC)        , nsw_lon(NS_MWVC)
      real    nsw_mle(NS_MWVC,4)      , nsw_tb(NS_MWVC)
      real    nsw_time(NS_MWVC)
      real    nsw_u(NS_MWVC,4)        , nsw_v(NS_MWVC,4)
      real    nsw_xc(NS_MWVC)         , nsw_yc(NS_MWVC)
c 
      common /NS_WINDS/ nsw_col     , nsw_ic      , nsw_jc      ,
     &                  nsw_nambigs , nsw_nwvc    , nsw_nwvc_rev,
     &                  nsw_qc      , nsw_revs    , nsw_row     ,
     &                  nsw_tag     , nsw_lat     , nsw_lon     ,
     &                  nsw_mle     , nsw_time    , nsw_u       ,
     &                  nsw_v       , nsw_xc      , nsw_yc      ,
     &                  nsw_tb      , nsw_al      , nsw_dt      ,
     &                  Sel
c
c**   Notes:
c
c     Comparison of common blocks for CONV data and NSCAT winds
c
c     COMMON /SLDTA/                        common/ns_winds/
c    1) XCONV(SLMAX)                       1) nsw_xc(NS_MWVC)
c    2) YCONV(SLMAX)                       2) nsw_yc(NS_MWVC)
c    3) WCONV(SLMAX)                          n/a
c    4) UCONV(SLMAX)                       3) nsw_u(NS_MWVC)
c    5) VCONV(SLMAX)                       4) nsw_v(NS_MWVC)
c    6) IDIMC                              5) NS_MWVC (declared in vam.h)
c    7) NPTC                               6) nsw_nobs
c    8) ILC(SLMAX)                         7) nsw_ic(NS_MWVC)
c    9) JLC(SLMAX)                         8) nsw_jc(NS_MWVC)
c   10) ICONV(SLMAX)                       9) nsw_tag(NS_MWVC)
c   11) sl_qc                             10) nsw_qc(NS_MWVC)
c
c     Additional fields stored to help use NSCAT winds
c
c                                         11) nsw_nambigs(NS_MWVC)
c                                         12) nsw_mle(NS_MWVC,4)
c                                         13) nsw_time(NS_MWVC)
c                                         14) nsw_lat(NS_MWVC)
c                                         15) nsw_lon(NS_MWVC)
c                                         16) nsw_row(NS_MWVC)
c                                         17) nsw_col(NS_MWVC)
c                                         18) nsw_nobs_rev(NS_MAXREVS)
c                                         19) nsw_revs(NS_MAXREVS)
c                                         20) NS_MAXREVS (declared in vam.h)
