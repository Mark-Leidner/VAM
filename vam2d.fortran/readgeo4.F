c!#   $Id: readgeo4.F,v 1.3 1997/08/11 15:34:05 leidner Exp $
c!#   $Log: readgeo4.F,v $
c!#   Revision 1.3  1997/08/11 15:34:05  leidner
c!#   added 4-byte declarations for reals and ints in /GEODAT/.
c!#
c!#	Revision 1.2  1997/04/16  15:42:16  leidner
c!#	removed mail text
c!#
c!#	Revision 1.1  1997/04/16  14:53:58  leidner
c!#	Initial revision
c!#
      SUBROUTINE READGEO4(LREC, IEND)

C     This routine decodes the data on the ssmi4 geophysical tapes.
C     Argument IEND=1 denotes that a zero-filled record was encountered.
C     Zero-filled records are used to pad the last block in a file.

      CHARACTER*1 LREC(994),AADATE(24)
      CHARACTER*24 ADATE
      INTEGER*2 IBUF(994)
      REAL*8 REV
      real*4 THT,AZM,XLAT,XLON,
     1 SST,TICE,OICE,WIND,DIR,DTB19H,VAPOR,CLOUD,RAIN,TSKY
      integer*4 LYEAR,IDAYJL,IMON,IDAYMO,ISECDY,ISAT,IQUAL,
     1 ISUR,ICEWARN,IWINQC,IRAINADJ
      COMMON /GEODAT/ LYEAR,IDAYJL,IMON,IDAYMO,ISECDY,ISAT,
     1 REV,THT,AZM,IQUAL,XLAT(64),XLON(64),
     2 ISUR(64),ICEWARN(64),IWINQC(64),IRAINADJ(64),
     3 SST(64),TICE(64),OICE(64),WIND(64),DIR(64),DTB19H(64),
     4 VAPOR(64),CLOUD(64),RAIN(64),TSKY(64)

      EQUIVALENCE (AADATE(1),ADATE)

      DATA N1,N2,N3/256,65536,16777216/

C     CONVERT TAPE BYTES TO INTEGERS (0-255)

      DO 100 I=1,994
      IBUF(I)=ICHAR(LREC(I))
  100 CONTINUE

C     CHECK IF ZERO FILLED RECORD (PAD AT END OF FILE)

      IF(IBUF(1)+IBUF(2)+IBUF(3)+IBUF(4).EQ.0) THEN
      IEND=1
      RETURN
      ELSE
      IEND=0
      ENDIF

C     FIND DATE-TIME-SATELLITE NO., REV, INC. ANG., AND AZIMUTH

      DO 110 I=1,24
      AADATE(I)=LREC(I)
  110 CONTINUE

      READ(ADATE,9001) LYEAR,IDAYJL,IMON,IDAYMO,ISECDY,ISAT
 9001 FORMAT(I4,I4,I3,I3,I6,I4)

      REV=1.D-4*(N3*IBUF(28)+N2*IBUF(27)+N1*IBUF(26)+IBUF(25))
      THT= 0.01*(N1*IBUF(30) + IBUF(29))
      AZM=  0.1*(N1*IBUF(32) + IBUF(31))
      IQUAL=     N1*IBUF(34) + IBUF(33) 

C     LOOP THROUGH 64 CELLS IN SCAN AND FIND LATITUDE, LONGITUDE,
C     AND GEOPHYSICAL PARAMETERS

      DO 200 ICEL=1,64

      N=2*ICEL-1
      XLAT(ICEL)=0.01*(N1*IBUF(N + 35) + IBUF(N + 34) -9000)
      XLON(ICEL)=0.01*(N1*IBUF(N +163) + IBUF(N +162))
      IF(XLON(ICEL).GE.360) XLON(ICEL)=XLON(ICEL)-360

      IRES=IBUF(ICEL + 290)
      IRAINADJ(ICEL)=IRES/128
      IRES=IRES-128*IRAINADJ(ICEL)
      IVAPQC=IRES/64
      IRES=IRES-64*IVAPQC
      IWINQC(ICEL)=IRES/32
      IRES=IRES-32*IWINQC(ICEL)
      ICEWARN(ICEL)=IRES/8
      IRES=IRES-8*ICEWARN(ICEL)
      IOOB=IRES/4
      ISUR(ICEL)=IRES-4*IOOB

      SST(ICEL) =  -999.
      TICE(ICEL)=  -999.
      OICE(ICEL)=  -999.
      WIND(ICEL)=  -999.
      DIR(ICEL)=   -999.
      DTB19H(ICEL)=-999.
      VAPOR(ICEL)= -999.
      CLOUD(ICEL)= -999.
      RAIN(ICEL)=  -999.
      TSKY(ICEL)=  -999.

      IF(ISUR(ICEL).EQ.2)  GO TO 200  !no data for land observations
      IF(IOOB.EQ.1)  GO TO 200        !no data for out of bounds TA'S

      SST(ICEL)=0.15*(IBUF(ICEL+354) - 20)                              !climate sst (C)
      IF(IBUF(ICEL+418).NE.255) TICE(ICEL)=0.5*IBUF(ICEL+418)           !total ice (%)
      IF(IBUF(ICEL+482).NE.255) OICE(ICEL)=0.5*IBUF(ICEL+482)           !multiyear ice (%)

      IF(ICEWARN(ICEL).EQ.2) GO TO 200  !ice present, other parameters not available

      IF(IBUF(ICEL+546).NE.255) WIND(ICEL)=0.15*IBUF(ICEL+546)          !wind (m/s)
      IF(IBUF(ICEL+610).NE.255) DIR(ICEL)=0.1*IBUF(ICEL+610) - 12.5     !direction signal (K)
      IF(IBUF(ICEL+674).NE.255) DTB19H(ICEL)=0.1*IBUF(ICEL+674) - 12.5  !19H residual (K)
      IF(IBUF(ICEL+738).NE.255) VAPOR(ICEL)=0.3*IBUF(ICEL+738)          !water vapor (mm)
      IF(IBUF(ICEL+802).NE.255) CLOUD(ICEL)=0.01*IBUF(ICEL+802)         !cloud water (mm)
      IF(IBUF(ICEL+866).NE.255) RAIN(ICEL)=0.1*IBUF(ICEL+866)           !rain rate (mm/h)
      IF(IBUF(ICEL+930).NE.255) TSKY(ICEL)=0.2*IBUF(ICEL+930) -10.      !scattering signal (K)

      IF(IVAPQC.EQ.1) VAPOR(ICEL)=-999. !rain too heavy. Tape contains climate value.

  200 CONTINUE
C
      RETURN
      END
