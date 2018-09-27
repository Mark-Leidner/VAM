c!#   $Id: sdata.F,v 1.8 1998/02/11 20:03:27 leidner Exp $
c!#   $Log: sdata.F,v $
c!#   Revision 1.8  1998/02/11 20:03:27  leidner
c!#   added wvc row and column to backscatter storage
c!#
c!#   Revision 1.7  1997/12/05 16:00:35  leidner
c!#   added sorting of polerization for nscat data
c!#
c!#   Revision 1.6  1997/11/21 21:12:01  leidner
c!#   fixed bug in ssmi data processing, and removed unused array
c!#
c!#   Revision 1.5  1997/08/11 17:49:28  leidner
c!#   added ssmi los data sorting
c!#
c!#	Revision 1.4  1997/06/23  17:51:47  leidner
c!#	changed ns_kp to 3-parameter kp
c!#
c!#	Revision 1.3  1997/02/21  22:03:40  leidner
c!#	added sort code for nscat data
c!#
c!#	Revision 1.2  1997/02/12  18:26:09  leidner
c!#	all include files changed to *.h
c!#
c!#	Revision 1.1  1997/02/10  16:39:08  leidner
c!#	Initial revision
c!#
      SUBROUTINE SDATA                                                  
#include "vam.h"
C***********************************************************************
C*****SDATA SORTS THE POINT DATA                                        
C-----XREFS:                                                            
C-----USER : ERRSEA ...... ...... ...... ...... ...... ...... ......    
C-----IMSL : .VSRTR ...... ...... ...... ...... ...... ...... ......    
C-----FORT : IBCOM# ...... ...... ...... ...... ...... ...... ......    
#include "er_bscat.h"
#include "ns_bscat.h"
#include "gparm.h"
#include "ptdta.h"
#include "sldta.h"
#include "ssmidata.h"
      COMMON /WORK/ IPERM(MAXREP),RLOC(MAXREP)
C-----USE SINGLE PRECISION IMSL SORT IN ALL CASES
      REAL TLOC(MAXREP)
      EQUIVALENCE (RLOC(1),TLOC(1))
      DATA NDIM0/MAXREP/
      WRITE(6,234)                                                      
 234  FORMAT(2X,'-> -> -> BEGIN  SDATA  <- <- <-')                      
      IF (NPTS.GT.NDIM0 .OR. NPTC.GT.NDIM0) CALL ERRSEA (521,6HSDATA )  
      IF (NGRDPT.LE.0) CALL ERRSEA (513,6HSDATA )                       
C*****SORTS LOCATIONS OF SASS AND CONVENTIONAL                          
C*****DATA POINTS IN GRID COORDINATE SYSTEM:                            
      IF (NPTS.LE.0) GO TO 500                                          
C-----SORT SASS POINTS                                                  
      DO 400 N=1,NPTS                                                   
      TLOC(N)=JLL(N)*NX+ILL(N)                                          
      IPERM(N)=N                                                        
  400 CONTINUE                                                          
      CALL VSRTR (TLOC,NPTS,IPERM)                                      
      CALL JPERM (   ILL,RLOC,IPERM,1,NPTS)                             
      CALL JPERM (   JLL,RLOC,IPERM,1,NPTS)                             
      CALL JPERM (  IOBS,RLOC,IPERM,1,NPTS)                             
      CALL JPERM ( PT_QC,RLOC,IPERM,1,NPTS)                             
      CALL PERMUT(XCOORD,RLOC,IPERM,1,NPTS)                             
      CALL PERMUT(YCOORD,RLOC,IPERM,1,NPTS)                             
      CALL PERMUT(   WGT,RLOC,IPERM,1,NPTS)                             
      CALL JPERM (NWINDS,RLOC,IPERM,1,NPTS)                             
C                                                                       
      CALL PERMUT(UOBS(1,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(UOBS(2,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(UOBS(3,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(UOBS(4,1),RLOC,IPERM,4,NPTS)                          
C                                                                       
      CALL PERMUT(VOBS(1,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(VOBS(2,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(VOBS(3,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(VOBS(4,1),RLOC,IPERM,4,NPTS)                          
      CALL PERMUT(  VBAR,RLOC,IPERM,1,NPTS)                             
C                                                                       
      WRITE (6,401)                                                     
  401 FORMAT ('0SASS DATA POINTS SORTED IN CORE.')                      
C-----                                                                  
  500 CONTINUE                                                          
      IF (NPTC.LE.0) GO TO 899
C-----SORT CONVENTIONAL POINTS                                          
      DO 800 N=1,NPTC                                                   
      TLOC(N)=JLC(N)*NX+ILC(N)                                          
      IPERM(N)=N                                                        
  800 CONTINUE                                                          
      CALL VSRTR (TLOC,NPTC,IPERM)                                      
      CALL JPERM (   ILC,RLOC,IPERM,1,NPTC)                             
      CALL JPERM (   JLC,RLOC,IPERM,1,NPTC)                             
      CALL JPERM ( SL_QC,RLOC,IPERM,1,NPTC)                             
      CALL PERMUT( XCONV,RLOC,IPERM,1,NPTC)                             
      CALL PERMUT( YCONV,RLOC,IPERM,1,NPTC)                             
      CALL PERMUT( WCONV,RLOC,IPERM,1,NPTC)                             
      CALL JPERM ( ICONV,RLOC,IPERM,1,NPTC)                             
      CALL PERMUT( UCONV,RLOC,IPERM,1,NPTC)                             
      CALL PERMUT( VCONV,RLOC,IPERM,1,NPTC)                             
      WRITE (6,801)                                                     
  801 FORMAT ('0CONVENTIONAL DATA POINTS SORTED IN CORE.')              
c
c     Sort ERS1 Data
c     ==============
c
 899  continue
      
      if (er_nobs .le. 0) go to 1899
      if (er_nobs .gt. MAXREP) call errsea(521,'SDATA ')

      do 900 n = 1,er_nobs
         tloc(n) = er_jc(n)*nx + er_ic(n)
         iperm(n) = n
 900  continue

      call vsrtr (tloc,er_nobs,iperm)
      call jperm (er_qc    ,rloc,iperm,1,er_nobs)
      call jperm (er_ic    ,rloc,iperm,1,er_nobs)
      call jperm (er_jc    ,rloc,iperm,1,er_nobs)
      call jperm (er_tag   ,rloc,iperm,1,er_nobs)
      call permut(er_xc    ,rloc,iperm,1,er_nobs)
      call permut(er_yc    ,rloc,iperm,1,er_nobs)
      call permut(er_lat   ,rloc,iperm,1,er_nobs)
      call permut(er_lon   ,rloc,iperm,1,er_nobs)
      call permut(er_velobs,rloc,iperm,1,er_nobs)
      call permut(er_dirobs,rloc,iperm,1,er_nobs)
      call permut(er_time  ,rloc,iperm,1,er_nobs)
      call permut(er_vel5  ,rloc,iperm,1,er_nobs)
      call permut(er_dir5  ,rloc,iperm,1,er_nobs)
      call permut(er_velbg ,rloc,iperm,1,er_nobs)
      call permut(er_dirbg ,rloc,iperm,1,er_nobs)

      do 901 i = 1,ER_MBEAM
         call jperm (er_ant  (1,i),rloc,iperm,1,er_nobs)
         call permut(er_theta(1,i),rloc,iperm,1,er_nobs)
         call permut(er_azim (1,i),rloc,iperm,1,er_nobs)
         call permut(er_S0obs(1,i),rloc,iperm,1,er_nobs)
         call permut(er_kp   (1,i),rloc,iperm,1,er_nobs)
         call permut(er_s0sd (1,i),rloc,iperm,1,er_nobs)
         call permut(er_s05  (1,i),rloc,iperm,1,er_nobs)
         call permut(er_s0bg (1,i),rloc,iperm,1,er_nobs)
 901  continue

      WRITE (6,902)                                                     
 902  FORMAT ('0ERS1 DATA POINTS SORTED IN CORE.')              

C-----                                                                  
c
c     Sort NSCAT Data
c     =+=============
c
1899  continue
      
      print *, 'in sdata_ ns_nobs=',ns_nobs
      if (ns_nobs .le. 0) go to 2000
      if (ns_nobs .gt. MAXREP) call errsea(521,'SDATA ')

      do 1900 n = 1,ns_nobs
         tloc(n) = ns_jc(n)*nx + ns_ic(n)
         iperm(n) = n
1900  continue

      call vsrtr (tloc,ns_nobs,iperm)
      call jperm (ns_qc    ,rloc,iperm,1,ns_nobs)
      call jperm (ns_ic    ,rloc,iperm,1,ns_nobs)
      call jperm (ns_jc    ,rloc,iperm,1,ns_nobs)
      call jperm (ns_tag   ,rloc,iperm,1,ns_nobs)
      call permut(ns_xc    ,rloc,iperm,1,ns_nobs)
      call permut(ns_yc    ,rloc,iperm,1,ns_nobs)
      call permut(ns_lat   ,rloc,iperm,1,ns_nobs)
      call permut(ns_lon   ,rloc,iperm,1,ns_nobs)
      call permut(ns_time  ,rloc,iperm,1,ns_nobs)
      call permut(ns_vel5  ,rloc,iperm,1,ns_nobs)
      call permut(ns_dir5  ,rloc,iperm,1,ns_nobs)
      call permut(ns_velbg ,rloc,iperm,1,ns_nobs)
      call permut(ns_dirbg ,rloc,iperm,1,ns_nobs)

         call jperm (ns_ant  ,rloc,iperm,1,ns_nobs)
         call jperm (ns_pol  ,rloc,iperm,1,ns_nobs)
         call jperm (ns_col  ,rloc,iperm,1,ns_nobs)
         call jperm (ns_row  ,rloc,iperm,1,ns_nobs)
         call permut(ns_theta,rloc,iperm,1,ns_nobs)
         call permut(ns_azim ,rloc,iperm,1,ns_nobs)
         call permut(ns_S0obs,rloc,iperm,1,ns_nobs)
         call permut(ns_kpa  ,rloc,iperm,1,ns_nobs)
         call permut(ns_kpb  ,rloc,iperm,1,ns_nobs)
         call permut(ns_kpc  ,rloc,iperm,1,ns_nobs)
         call permut(ns_s0sd ,rloc,iperm,1,ns_nobs)
         call permut(ns_s05  ,rloc,iperm,1,ns_nobs)
         call permut(ns_s0bg ,rloc,iperm,1,ns_nobs)

      WRITE (6,1902)                                                     
1902  FORMAT ('0NSCAT DATA POINTS SORTED IN CORE.')              
c
c     Sort SSMI line of sight data
c     =============================
c
2000  continue
      
      print *, 'in sdata_ nssmi=',nssmi
      if (nssmi .le. 0) go to 3000
      if (nssmi .gt. mssmi) call errsea(521,'SDATA ')

      do 2001 n = 1,nssmi
         tloc(n) = int(vssmi(n,iyg))*nx + int(vssmi(n,ixg))
         iperm(n) = n
2001  continue

      call vsrtr (tloc,nssmi,iperm)
      do 2002 i = 1,mssmivar
         call permut(vssmi(1,i),rloc,iperm,1,nssmi)
2002  continue
      WRITE (6,2003)                                                     
2003  FORMAT ('0ssmi DATA POINTS SORTED IN CORE.')              


 3000 RETURN                                                            
      END                                                               
