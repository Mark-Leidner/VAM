      subroutine write_derived_data
     i     ( outunit, 
     i     Satellite_ID,
     i     Sat_Motion,
     i     Instrument_ID,
     i     Cross_Track_Res,
     i     Along_Track_Res,
     i     WVC_Col,
     i     GMF_ID,
     i     Software_ID,
     i     Num_Fore_Inner,
     i     Num_Fore_Outer,
     i     Num_Aft_Inner,
     i     Num_Aft_Outer,
     i     K_Polar,
     i     Sigma0_Variance_QC,
     i     Time_to_Edge )

C
C**** *WRITE_DERIVED_DATA*
C
C $Id: write_derived_data.f,v 1.1 2001/08/10 17:58:37 mcc Exp $
C $Log: write_derived_data.f,v $
C Revision 1.1  2001/08/10 17:58:37  mcc
C Initial revision.
C
C Revision 1.5  2000/01/31 17:52:41  stl
C added new MGDR variables and time_to_edge
C
C Revision 1.4  1999/04/06 13:53:49  stl
C removed ms0
C
c Revision 1.3  1999/02/24  12:21:22  stl
c updated documentation
c
c Revision 1.2  1999/02/24  11:58:30  stl
c added new fields
c
c Revision 1.1  1999/02/22  17:54:20  stl
c Initial revision
c
C
C     PURPOSE.
C     --------
C            Writes derived QSCAT data from one 25 km WVC row to a
C            formatted ASCII file.
C
C     ----------------------------------------------------------------- 

c     Parameters which define dimensions of the scat data
c     NumberBeams, NumberBeams
      integer nwvc, nb
      parameter (nwvc=76, nb=4)

C     Input/Output Interface Variables
      integer outunit
c     derived scatterometer variables
      real          
     $              Cross_Track_Res, Along_Track_Res,
     $              Sigma0_Variance_QC(nb,nwvc),
     $              Sat_Motion, Time_to_Edge
      integer
     $              Satellite_ID, Instrument_ID, GMF_ID,
     $              WVC_Col(nwvc),
     $              Num_Fore_Inner(nwvc),
     $              Num_Fore_Outer(nwvc),
     $              Num_Aft_Inner(nwvc),
     $              Num_Aft_Outer(nwvc),
     $              K_Polar(nb,nwvc)
C
C     ----------------------------------------------------------------- 
C
C*          1. WRITE DERIVED QSCAT DATA
C              ------------------------

      write (outunit,'("+ Additional BUFR fields -")')
      write (outunit,'("+ Satellite ID: ", i6)') Satellite_ID
      write (outunit,'("+ Direction of Motion: ", f8.2)')
     &     Sat_Motion
      write (outunit,'("+ Time to Edge of Processing Segment: ", f8.2)')
     &     Time_to_Edge
      write (outunit,'("+ Instrument ID: ", i6)') Instrument_ID
      write (outunit,'("+ Cross Track_Resolution: ", e13.6)')
     &     Cross_Track_Res
      write (outunit,'("+ Along Track_Resolution: ", e13.6)')
     &     Along_Track_Res
      write (outunit,'("+ WVC_Col: ",/ 7("+",i7,11i8/),"+")')
     &     WVC_Col
      write (outunit,'("+ Geophysical Model Function ID: ", i6)')
     &     GMF_ID
      write (outunit,'("+ JPL Software Build Number: ", i6)')
     &     Software_ID
      write (outunit,'("+ Num_Fore_Inner: ",/ 7("+",i7,11i8/),"+")')
     &     Num_Fore_Inner
      write (outunit,'("+ Num_Fore_Outer: ",/ 7("+",i7,11i8/),"+")')
     &     Num_Fore_Outer
      write (outunit,'("+ Num_Aft_Inner: ",/ 7("+",i7,11i8/),"+")')
     &     Num_Aft_Inner
      write (outunit,'("+ Num_Aft_Outer: ",/ 7("+",i7,11i8/),"+")')
     &     Num_Aft_Outer
      write (outunit,'("+ K_Polar: ",/ 26("+",i7,11i8/),"+")')
     &     K_Polar
      write (outunit,'("+ Sigma0_Variance_QC: ",/
     &     26("+",f7.2,11f8.2/),"+")') Sigma0_Variance_QC

      return
      end



