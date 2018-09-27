      subroutine write_qscat25km_row
     i     ( outunit, 
     i     WVC_Row_Time,           
     i     Rev_Number,
     i     WVC_Row,
     i     WVC_Lat,
     i     WVC_Lon,
     i     WVC_Quality_Flag,
     i     Model_Speed,
     i     Model_Dir,
     i     Num_Ambigs,
     i     Wind_Speed,
     i     Wind_Dir,
     i     Wind_Speed_Err,
     i     Wind_Dir_Err,
     i     Max_Likelihood_Est,
     i     WVC_Selection,
     i     Sigma0_In_Cell,
     i     Sigma0_Lat,
     i     Sigma0_Lon,
     i     Sigma0_Azimuth,
     i     Sigma0_Incidence,
     i     Sigma0,
     i     Kp_Alpha,
     i     Kp_Beta,
     i     Kp_Gamma,
     i     Sigma0_Atten_Value,
     i     Sigma0_Qual_Flag,
     i     Sigma0_Mode_Flag,
     i     Sigma0_Surface_Flag,
     i     MP_Rain_Index,
     i     NOF_Rain_Index,
     i     Tb_Mean_H,
     i     Tb_Mean_V,
     i     Tb_StdDev_H,
     i     Tb_StdDev_V,
     i     Num_Tb_H,
     i     Num_Tb_V, 
     i     Tb_Rain_Rate,
     i     Tb_Attenuation )

C
C**** *WRITE_QSCAT25KM_ROW*
C
C $Id: write_qscat25km_row.f,v 1.1 2001/08/10 17:58:40 mcc Exp $
C $Log: write_qscat25km_row.f,v $
C Revision 1.1  2001/08/10 17:58:40  mcc
C Initial revision.
C
C Revision 1.5  2000/01/27 13:32:51  stl
C added arrays for writing  new MGDR rain info
C
c Revision 1.4  1999/04/06  13:44:21  stl
c removed ms0
c
c Revision 1.3  1999/02/22  17:55:32  stl
c updated documentation, changed format for MLE print
c
c Revision 1.2  1999/02/09  18:10:10  stl
c changed Kp_Gamma format
c
c Revision 1.1  1999/02/09  17:26:52  stl
c Initial revision
c
C
C     PURPOSE.
C     --------
C          Writes QSCAT data from one 25 km WVC row to a formatted
C          ASCII file.
C
C     INTERFACE.
C     ----------
C
C          *CALL* *WRITE_QSCAT25KM_ROW
C     i                  ( OUTUNIT,
C     i                  WVC_Row_Time,
C     i                  Rev_Number,
C     i                    .
C     i                    .
C     i                    .
C     i                  Sigma0_Surface_Flag )
C
C          INPUT :
C               *OUTUNIT*       - Unit number of output device
C               *WVC_Row_Time*  - Mean time for measurements in this WVC row
C               *Rev_Number*    -  Orbit number
C                .
C                .              - remaining storage arrays for QSCAT row data
C                .
C
C          OUTPUT :
C               NONE.
C
C
C     METHOD.
C     -------
C
C          Each variable is written out using its own write and format
C          statement.
C     
C
C     EXTERNALS.
C     ----------
C
C          NONE
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. LEIDNER    *AER*       11/11/98.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
C     -----------------------------------------------------------------
      implicit none

c     Parameters for dimensioning storage arrays for scatterometer data
c     MaximumWINDs, NumberBeams, NumberWindVectorCells
      integer mwind, nb, nwvc
      parameter (mwind=4, nb=4, nwvc=76)

c     Input/Output interface variables
      integer outunit
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

C     ----------------------------------------------------------------- 
C
C*    1. WRITE OUT ONE 25 KM ROW OF QSCAT DATA.
C        --------------------------------------

      write (outunit, '("Mean_Time: ", a24)') WVC_Row_Time
      write (outunit, '("Rev: ", i6)') Rev_Number
      write (outunit, '("WVC_Row: ", i6)') WVC_Row
      write (outunit, '("WVC_Lat: ",/ 7(12f8.2/))') WVC_Lat
      write (outunit, '("WVC_Lon: ",/ 7(12f8.2/))') WVC_Lon
      write (outunit, '("WVC_Quality_Flag: ",/ 7(12i8/))')
     &     WVC_Quality_Flag
      write (outunit, '("Model_Speed: ",/ 7(12f8.2/))') Model_Speed
      write (outunit, '("Model_Dir: ",/ 7(12f8.2/))') Model_Dir
      write (outunit, '("Num_Ambigs: ",/ 7(12i8/))') Num_Ambigs
      write (outunit, '("Wind_Speed: ",/ 26(12f8.2/))') Wind_Speed
      write (outunit, '("Wind_Dir: ",/ 26(12f8.2/))') Wind_Dir
      write (outunit, '("Wind_Speed_Err: ",/ 26(12f8.2/))')
     &     Wind_Speed_Err
      write (outunit, '("Wind_Dir_Err: ",/ 26(12f8.2/))') Wind_Dir_Err
      write (outunit, '("MLE_Likelihood_Est: ",/ 26(12f8.3/))')
     &     Max_Likelihood_Est
      write (outunit, '("WVC_Selection: ",/ 7(12i8/))') WVC_Selection
      write (outunit, '("Sigma0_In_Cell: ",/ 7(12i8/))')
     &     Sigma0_In_Cell
      write (outunit, '("Sigma0_Lat: ",/ 26(12f8.2/))') Sigma0_Lat
      write (outunit, '("Sigma0_Lon: ",/ 26(12f8.2/))') Sigma0_Lon
      write (outunit, '("Sigma0_Azimuth: ",/ 26(12f8.2/))')
     &     Sigma0_Azimuth
      write (outunit, '("Sigma0_Incidence: ",/ 26(12f8.2/))')
     &     Sigma0_Incidence
      write (outunit, '("Sigma0: ",/ 26(12f8.2/))') Sigma0
      write (outunit, '("Kp_Alpha: ",/ 76(4e14.5/))') Kp_Alpha
      write (outunit, '("Kp_Beta: ",/ 76(4e14.5/))') Kp_Beta
      write (outunit, '("Kp_Gamma: ",/ 76(4e14.5/))') Kp_Gamma
      write (outunit, '("Sigma0_Atten_Value: ",/ 26(12f8.3/))')
     &     Sigma0_Atten_Value
      write (outunit, '("Sigma0_Qual_Flag: ",/ 26(12i8/))')
     &     Sigma0_Qual_Flag
      write (outunit, '("Sigma0_Mode_Flag: ",/26(12i8/))')
     &     Sigma0_Mode_Flag
      write (outunit, '("Sigma0_Surface_Flag: ",/ 26(12i8/))')
     &     Sigma0_Surface_Flag
      write (outunit, '("MP_Rain_Index: ",/ 7(12f8.3/))') 
     &     MP_Rain_Index
      write (outunit, '("NOF_Rain_index: ",/ 7(12i8/))')
     &     NOF_Rain_index
      write (outunit, '("Tb_Mean_H: ",/ 7(12f8.2/))') 
     &     Tb_Mean_H
      write (outunit, '("Tb_Mean_V: ",/ 7(12f8.2/))') 
     &     Tb_Mean_V
      write (outunit, '("Tb_StdDev_H: ",/ 7(12f8.2/))') 
     &     Tb_StdDev_H
      write (outunit, '("Tb_StdDev_V: ",/ 7(12f8.2/))') 
     &     Tb_StdDev_V
      write (outunit, '("Num_Tb_H: ",/ 7(12i8/))')
     &     Num_Tb_H
      write (outunit, '("Num_Tb_V: ",/ 7(12i8/))')
     &     Num_Tb_V
      write (outunit, '("Tb_Rain_Rate: ",/ 7(12f8.2/))') 
     &     Tb_Rain_Rate
      write (outunit, '("Tb_Attenuation: ",/ 7(12f8.2/))') 
     &     Tb_Attenuation

      return
      end



