      SUBROUTINE BBUPRS0(KNT,KSEC0)
C
C**** *BUPRS0*
C
C
C     PURPOSE.
C     --------
C           Print section 0 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS0(KNT,KSEC0)*
C
C        INPUT :
C               *KNT*   -  unit number for io
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSEC0(JSEC0)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 0.
C                ----------------
 100  CONTINUE
C
      WRITE(KNT,'(1H1)')
C
      WRITE(KNT,'(1H ,A)')    '         BUFR SECTION 0    '
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A,I5)') 'Length of section 0 (bytes)         ',
     1                       KSEC0(1)
      WRITE(KNT,'(1H ,A,I5)') 'Total length of Bufr message (bytes)',
     1                       KSEC0(2)
      WRITE(KNT,'(1H ,A,I5)') 'Bufr Edition number                 ',
     1                       KSEC0(3)
C
      RETURN
      END
      SUBROUTINE BBUPRS1(KNT,KSEC1)
C
C**** *BUPRS1*
C
C
C     PURPOSE.
C     --------
C           Print section 1 of bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS1(KNT,KSEC1)*
C
C        INPUT :
C               *KNT*   -  unit number for io
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C
C                            For Bufr Edition >= 3
C
C                            KSEC1(16) - Originating sub-centre
C                            KSEC1(18) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C                            
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSEC1(JSEC1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 1.
C                ----------------
 100  CONTINUE
C
      WRITE(KNT,'(1H1)')
C
      WRITE(KNT,'(1H ,A)')    '        BUFR SECTION 1    '
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A,I5)') 'Length of section 1 (bytes)    ',
     1       KSEC1( 1)
      WRITE(KNT,'(1H ,A,I5)') 'Bufr Edition number            ', 
     1       KSEC1( 2)
      if(ksec1(2).ge.3) then
      WRITE(KNT,'(1H ,A,I5)') 'Originating sub-centre         ', 
     1       KSEC1(16)
      end if
      WRITE(KNT,'(1H ,A,I5)') 'Originating centre             ', 
     1       KSEC1( 3)
      WRITE(KNT,'(1H ,A,I5)') 'Update sequence number         ',
     1       KSEC1( 4)
      WRITE(KNT,'(1H ,A,I5)') 'Flag (presence of section 2)   ', 
     1       KSEC1( 5)
      WRITE(KNT,'(1H ,A,I5)') 'Bufr message type              ', 
     1       KSEC1( 6)
      WRITE(KNT,'(1H ,A,I5)') 'Bufr message subtype           ', 
     1       KSEC1( 7)
      WRITE(KNT,'(1H ,A,I5)') 'Version number of local table  ', 
     1       KSEC1( 8)
      WRITE(KNT,'(1H ,A,I5)') 'Year                           ', 
     1       KSEC1( 9)
      WRITE(KNT,'(1H ,A,I5)') 'Month                          ', 
     1       KSEC1(10)
      WRITE(KNT,'(1H ,A,I5)') 'Day                            ', 
     1       KSEC1(11)
      WRITE(KNT,'(1H ,A,I5)') 'Hour                           ', 
     1       KSEC1(12)
      WRITE(KNT,'(1H ,A,I5)') 'Minute                         ',
     1       KSEC1(13)
      WRITE(KNT,'(1H ,A,I5)') 'Version number of Master table ', 
     1       KSEC1(15)
      WRITE(KNT,'(1H ,A,I5)') 'Bufr Master table              ', 
     1       KSEC1(14)
C
      RETURN
      END
      SUBROUTINE BBUPRS2(KNT,KSUP,KEY)
C
C**** *BUPRS2*
C
C
C     PURPOSE.
C     --------
C           Print section 2 of bufr message (expanded RDB key).
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS2(KNT,KSUP,KEY)*
C
C        INPUT :
C               *KNT*   -  unit number for io
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KEY*     -  array containing section 2 information
C                            KEY( 1)-- length of section 2 (bytes)
C                            KEY( 2)-- RDB type
C                            KEY( 3)-- RDB subtype
C                            KEY( 4)-- year
C                            KEY( 5)-- month
C                            KEY( 6)-- day
C                            KEY( 7)-- hour
C                            KEY( 8)-- minute
C                            KEY( 9)-- second
C                            KEY(10)-- longitude1
C                            KEY(11)-- latitude1
C                            KEY(12)-- longitude2
C                            KEY(13)-- latitude2
C                            KEY(14)-- number of subsets
C                            KEY(15)-- ident (numeric)
C                            KEY(16)-- ident ( CCITTIA5) one character
C                            KEY(17)-- ident ( CCITTIA5) one character
C                            KEY(18)-- ident ( CCITTIA5) one character
C                            KEY(19)-- ident ( CCITTIA5) one character
C                            KEY(20)-- ident ( CCITTIA5) one character
C                            KEY(21)-- ident ( CCITTIA5) one character
C                            KEY(22)-- ident ( CCITTIA5) one character
C                            KEY(23)-- ident ( CCITTIA5) one character
C                            KEY(24)-- ident ( CCITTIA5) one character
C                            KEY(25)-- total Bufr message length
C                            KEY(26)-- day    (RDB insertion)
C                            KEY(27)-- hour   (RDB insertion)
C                            KEY(28)-- minute (RDB insertion)
C                            KEY(29)-- second (RDB insertion)
C                            KEY(30)-- day    (MDB insertion)
C                            KEY(31)-- hour   (MDB insertion)
C                            KEY(32)-- minute (MDB insertion)
C                            KEY(33)-- second (MDB insertion)
C                            KEY(34)-- correction number
C                            KEY(35)-- part
C                            KEY(36)-- 0
C                            KEY(37)-- correction number
C                            KEY(38)-- part
C                            KEY(39)-- 0
C                            KEY(40)-- correction number
C                            KEY(41)-- part
C                            KEY(42)-- 0
C                            KEY(43)-- correction number
C                            KEY(44)-- part
C                            KEY(45)-- 0
C                            KEY(46)-- the lowest Q/C % confidence
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSUP(JSUP),KEY(JKEY)
C
      CHARACTER*9 CIDENT
      CHARACTER*13 YFM
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 2.
C                ----------------
 100  CONTINUE
C
      YFM='(1H ,A,TR0,A)'
C
      IF(KSUP(2).LE.1) THEN
         PRINT*,'Prtkey : RDB key not defined in section 2.'
         RETURN
      END IF
C
      WRITE(KNT,'(1H1)')
C
      WRITE(KNT,'(1H ,A)')       '        BUFR SECTION 2    '
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A,I9)')    'Length of section 2       ', KEY(1)
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A)')       '      Report Data Base Key  '
      WRITE(KNT,'(1H )')
C
      IKTYPE=0
      IF(KEY(2).EQ.2) IKTYPE=2
      IF(KEY(2).EQ.3) IKTYPE=2
      IF(KEY(2).EQ.12)IKTYPE=2
      IF(KEY(2).EQ.08)IKTYPE=2
      IF(IKTYPE.EQ.0.AND.KSUP(6).GT.1) IKTYPE=2
C
      IF(IKTYPE.EQ.2) THEN
C      IF(KEY(2).EQ.2.OR.KEY(2).EQ.3.OR.KEY(2).EQ.12) THEN
C
         WRITE(KNT,'(1H ,A,I9)') 'RDB data type             ', KEY(2)
         WRITE(KNT,'(1H ,A,I9)') 'RDB data subtype          ', KEY(3)
         WRITE(KNT,'(1H ,A,I9)') 'Year                      ', KEY(4)
         WRITE(KNT,'(1H ,A,I9)') 'Month                     ', KEY(5)
         WRITE(KNT,'(1H ,A,I9)') 'Day                       ', KEY(6)
         WRITE(KNT,'(1H ,A,I9)') 'Hour                      ', KEY(7)
         WRITE(KNT,'(1H ,A,I9)') 'Minute                    ', KEY(8)
         WRITE(KNT,'(1H ,A,I9)') 'Second                    ', KEY(9)
         RLAT1=(KEY(11)-9000000)/100000.
         RLON1=(KEY(10)-18000000)/100000.
         WRITE(KNT,'(1H ,A,F9.2)')'Latitude  1               ', RLAT1
         WRITE(KNT,'(1H ,A,F9.2)')'Longitude 1               ', RLON1
         RLAT2=(KEY(13)-9000000)/100000.
         RLON2=(KEY(12)-18000000)/100000.
         WRITE(KNT,'(1H ,A,F9.2)')'Latitude  2               ', RLAT2
         WRITE(KNT,'(1H ,A,F9.2)')'Longitude 2               ', RLON2
         WRITE(KNT,'(1H ,A,I9)') 'Number of observations    ', KEY(14)
         WRITE(KNT,'(1H ,A,I9)') 'Identifier                ', KEY(15)
         WRITE(KNT,'(1H ,A,I9)') 'Total Bufr message length ', KEY(25)
         WRITE(KNT,'(1H ,A,I9)') 'Day    (RDB insertion)    ', KEY(26)
         WRITE(KNT,'(1H ,A,I9)') 'Hour   (RDB insertion)    ', KEY(27)
         WRITE(KNT,'(1H ,A,I9)') 'Minute( (RDB insertion)   ', KEY(28)
         WRITE(KNT,'(1H ,A,I9)') 'Second (RDB insertion)    ', KEY(29)
         WRITE(KNT,'(1H ,A,I9)') 'Day    (MDB arrival)      ', KEY(30)
         WRITE(KNT,'(1H ,A,I9)') 'Hour   (MDB arrival)      ', KEY(31)
         WRITE(KNT,'(1H ,A,I9)') 'Minute (MDB arrival)      ', KEY(32)
         WRITE(KNT,'(1H ,A,I9)') 'Second (MDB arrival       ', KEY(33)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(34)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(35)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(37)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(38)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(40)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(41)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(43)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(44)
         WRITE(KNT,'(1H ,A,I9)') 'Quality control % conf    ', KEY(46)
      ELSE
         WRITE(KNT,'(1H ,A,I9)') 'RDB data type             ', KEY(2)
         WRITE(KNT,'(1H ,A,I9)') 'RDB data subtype          ', KEY(3)
         WRITE(KNT,'(1H ,A,I9)') 'Year                      ', KEY(4)
         WRITE(KNT,'(1H ,A,I9)') 'Month                     ', KEY(5)
         WRITE(KNT,'(1H ,A,I9)') 'Day                       ', KEY(6)
         WRITE(KNT,'(1H ,A,I9)') 'Hour                      ', KEY(7)
         WRITE(KNT,'(1H ,A,I9)') 'Minute                    ', KEY(8)
         WRITE(KNT,'(1H ,A,I9)') 'Second                    ', KEY(9)
         RLAT1=(KEY(11)-9000000)/100000.
         RLON1=(KEY(10)-18000000)/100000.
         WRITE(KNT,'(1H ,A,F9.2)')'Latitude  1               ', RLAT1
         WRITE(KNT,'(1H ,A,F9.2)')'Longitude 1               ', RLON1
         IDD=0
         CIDENT=' '
         DO 201 ID=16,24
         IDD=IDD+1
         CIDENT(IDD:IDD)=CHAR(KEY(ID))
 201     CONTINUE
         IDD=INDEX(CIDENT,' ')
         if(idd.eq.0) idd=10
         IDD=10-IDD
         WRITE(YFM(10:10),'(I1)',ERR=202) IDD
         GO TO 203
 202     YFM(10:10)='9'
 203     WRITE(KNT,FMT=YFM)      'Identifer                 ', CIDENT
         WRITE(KNT,'(1H ,A,I9)') 'Total Bufr message length ', KEY(25)
         WRITE(KNT,'(1H ,A,I9)') 'Day    (RDB insertion)    ', KEY(26)
         WRITE(KNT,'(1H ,A,I9)') 'Hour   (RDB insertion)    ', KEY(27)
         WRITE(KNT,'(1H ,A,I9)') 'Minute (RDB insertion)    ', KEY(28)
         WRITE(KNT,'(1H ,A,I9)') 'Second (RDB insertion)    ', KEY(29)
         WRITE(KNT,'(1H ,A,I9)') 'Day    (MDB arrival)      ', KEY(30)
         WRITE(KNT,'(1H ,A,I9)') 'Hour   (MDB arrival)      ', KEY(31)
         WRITE(KNT,'(1H ,A,I9)') 'Minute (MDB arrival)      ', KEY(32)
         WRITE(KNT,'(1H ,A,I9)') 'Second (MDB arrival       ', KEY(33)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(34)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(35)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(37)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(38)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(40)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(41)
         WRITE(KNT,'(1H ,A,I9)') 'Correction number         ', KEY(43)
         WRITE(KNT,'(1H ,A,I9)') 'Part of message           ', KEY(44)
         WRITE(KNT,'(1H ,A,I9)') 'Quality control % conf    ', KEY(46)
      END IF
C
      RETURN
      END
      SUBROUTINE BBUPRS3(KNT,KSEC3,KTDLEN,KTDLST,KTDEXL,
     1                  KTDEXP,KELEM,CNAMES)
C
C**** *BUPRS3*
C
C
C     PURPOSE.
C     --------
C           Print section 3 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS3(KNT,KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,
C                          KELEM,CNAMES)*
C
C        INPUT :
C               *KNT*     -  unit number io
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KTDLEN*  -  number of data descriptors in section 3
C               *KTDLST*  -  array containing data descriptors in section 3
C               *KTDEXL*  -  number of entries in list of expanded data
C                            descriptors
C               *KTDEXP*  -  array containig expanded data descriptors
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KTDLST(KTDLEN),KTDEXP(KTDEXL)
C
      CHARACTER*64 CNAMES(KELEM)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 3.
C                ----------------
 100  CONTINUE
C
      WRITE(KNT,'(1H1)')
C
      WRITE(KNT,'(1H ,A)')    '         BUFR SECTION 3    '
      WRITE(KNT,'(1H )')
      WRITE(KNT,'(1H ,A,I5)') 'Length of section 3 (bytes)         ',
     1                       KSEC3(1)
      WRITE(KNT,'(1H ,A,I5)') 'Reserved                            ',
     1                       KSEC3(2)
      WRITE(KNT,'(1H ,A,I5)') 'Number of data subsets              ',
     1                       KSEC3(3)
      WRITE(KNT,'(1H ,A,I5)') 'Flag (data type/data compression)   ',
     1                       KSEC3(4)
C
      WRITE(KNT,'(1H ,//)')
      WRITE(KNT,'(1H ,A)')    '       Data descriptors (unexpanded)'
C
      WRITE(KNT,'(1H )')
      DO 110 I=1,KTDLEN
       WRITE(KNT,'(1H ,I4,2X,I6.6)') I,KTDLST(I)
 110  CONTINUE
C
      WRITE(KNT,'(1H ,/)')
      WRITE(KNT,'(1H ,A)')    '       Data descriptors (expanded)'
      WRITE(KNT,'(1H )')
      DO 120 I=1,KTDEXL
       WRITE(KNT,'(1H ,I4,2X,I6.6,2X,A)') I,KTDEXP(I),CNAMES(I)
 120  CONTINUE

      RETURN
      END
      SUBROUTINE BBUPRT(KNT,K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
     1                 CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)
C
C**** *BUPRT*
C
C
C     PURPOSE.
C     --------
C           Print expanded Bufr messag.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRT(KNT,K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
C                         CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)*
C
C        INPUT :
C               *KNT*     -  unit number for io
C               *K*       -  switch to print with/witout content of code tables
C                            0  - no  code table content
C                            1  - yes code table content
C               *KSUB1*   -  starting subset
C               *KSUB2*   -  ending subset
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *CVALS*   -  character array containing bufr code table
C                            entries
C               *KVALS*   -  dimension of VALUES array
C               *VALUES*  -  real array (expanded data values)
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C        OUTPUT:
C               *KERR*    -  returned error code
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER YCHAR*30,YLONG*320
C
      DIMENSION KSUP(JSUP),KSEC1(JSEC1)
      DIMENSION VALUES(KVALS)
C
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BUFR MESSAGE.
C                -------------------
 100  CONTINUE
C
      KERR=0
C
      ISUB1=KSUB1
      ISUB2=KSUB2
      IF(ISUB1.LE.0.OR.ISUB2.LE.0) THEN
         WRITE(KNT,'(A)')    ' Warning - NEGATIVE KSUB1 OR KSUB2.'
         WRITE(KNT,'(A,I5)') ' Warning - number of subsets is ',KSUP(6)
         RETURN
      END IF
      IF(ISUB1.GT.KSUP(6)) THEN
         WRITE(KNT,'(A,I5)') ' Warning - number of subsets is ',KSUP(6)
         RETURN
      END IF
      IF(ISUB2.GT.KSUP(6)) THEN
         ISUB2=KSUP(6)
         WRITE(KNT,'(A,I5)') ' Warning - KSUB2 replaced by ',KSUP(6)
      END IF
C
      JQCP1= 0
C
      IF(K.EQ.0) THEN
         JQPR=0
         JQUA=0
         JQC=0
c          DO 171 J171=1,KSUP(5)
c          IF(CNAMES(J171)(1:8).EQ.'DATA PRE') THEN
c            JQPR=J171
c            GO TO 172
c          END IF
c  171     CONTINUE
c  172     DO 173 J173=1,KSUP(5)
c          IF(CNAMES(J173)(1:9).EQ.'QUALITY I') JQUA=J173
c  173     CONTINUE
c          DO 174 J174=1,KSUP(5)
c          IF(CNAMES(J174)(1:3).EQ.'% C') THEN
c            JQC =J174
c            GO TO 175
c          END IF
c  174     CONTINUE
c 
c  175     CONTINUE
C
c         WRITE(KNT,'(1H1)')
C
c         WRITE(KNT,'(1H ,A)')    'EXPANDED BUFR MESSAGE  '
c         WRITE(KNT,'(1H ,//)')
c         WRITE(KNT,'(1H ,A,I6)') 'BUFR MESSAGE  DATA TYPE   ',KSEC1(6)
c         WRITE(KNT,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
c         WRITE(KNT,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
         NTYPE=KSEC1(7)
         IF(JQUA.EQ.0) THEN      !if(jqua.NE.0) then
            JQUA=KSUP(5)
C
            DO 103 JB=ISUB1,ISUB2
C
            iln=0
            WRITE(KNT,'(1H )')
C
            DO 104 JA=1,JQUA
C
            iln=iln+1
            JAJB=JA+(JB-1)*KELEM
C
            if(VALUES(JAJB).ge.(rvind-eps).and.
     1         VALUES(JAJB).le.(rvind+eps)) then
               WRITE(KNT,9918) iln,CNAMES(JA),CUNITS(JA)
            ELSE
               IF(CUNITS(JA)(1:4).EQ.'CCIT') THEN
                  I=NINT(VALUES(JAJB)/1000)
                  NCHAR=VALUES(JAJB)-I*1000
                  NW=NCHAR/80
                  NWOFF=NCHAR-NW*80
                  IF(NWOFF.NE.0) NW=NW+1
C
                  YLONG=' '
                  YLONG(1:80)=CVALS(I)
C
                  II=I
                  DO 125 JC=1,NW-1
                  II=II+1
                  KF=JC*80+1
                  KL=(JC+1)*80
                  YLONG(KF:KL)=CVALS(II)
 125              CONTINUE
C
                  NLINE=NCHAR/30
                  IDIF =NCHAR-NLINE*30
                  IF(IDIF.NE.0) NLINE=NLINE+1
                  YCHAR=' '
                  YCHAR=YLONG(1:30)
C
                  WRITE(KNT,9919)iln,CNAMES(JA),VALUES(JAJB),
     1                         CUNITS(JA),YCHAR
C
                  IF(NLINE.GT.1) THEN
                     DO 130 JJ=1,NLINE-1
C
                     K2=JJ*30+1
                     K1=(JJ+1)*30
                     YCHAR=' '
                     YCHAR=YLONG(K2:K1)
C
                     WRITE(KNT,9920) YCHAR
 130                 CONTINUE
C
                   END IF
               ELSE
                  WRITE(KNT,9917) iln,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
               END IF
            END IF
C
 104        CONTINUE
 103        CONTINUE
C
         ELSE
            JQPRM1=JQPR-1
            JQC=JQC-1
C
            DO 101 JB=ISUB1,ISUB2
C
            iln=0
            JQCP1=0
C
            WRITE(KNT,'(1H )')
C
            DO 102 JA=1,JQUA-1
C
            iln=iln+1
            JAJB=JA+(JB-1)*KELEM
            JQPJB=JQPRM1+JA+(JB-1)*KELEM
C
            IF(VALUES(JQPJB).EQ.0.0) THEN
               JQCP1=JQCP1+1
               JQCPP1=JQC+JQCP1+(JB-1)*KELEM
               if(VALUES(JAJB).ge.(rvind-eps).and.
     1             VALUES(JAJB).le.(rvind+eps)) then
                  WRITE(KNT,9918) iln,CNAMES(JA),CUNITS(JA)
               ELSE
                  WRITE(KNT,9916) iln,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA),
     1            CNAMES(JQC+JQCP1),VALUES(JQCPP1),
     1            CUNITS(JQC+JQCP1)
               END IF
            ELSE
               if(VALUES(JAJB).ge.(rvind-eps).and.
     1            VALUES(JAJB).le.(rvind+eps)) then
                  WRITE(KNT,9918) iln,CNAMES(JA),CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               ELSE
                  WRITE(KNT,9917) iln,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               END IF
            END IF
C
C
 102        CONTINUE
 101        CONTINUE
C
         END IF
      END IF
C
      IF(K.EQ.1) THEN
C
C---------------------------------------------------------------------
          WRITE(KNT,'(1H1)')
C
          WRITE(KNT,'(1H ,A)') 'WARNING : Printing content of code'//
     1   ' tables not yet implemented.'
          RETURN
C---------------------------------------------------------------------
C
C          WRITE(KNT,'(1H ,A)')    'EXPANDED BUFR MESSAGE  '
C          WRITE(KNT,'(1H ,//)')
C          WRITE(KNT,'(1H ,A,I6)') 'RDB DATA TYPE             ',KSEC1(6)
C          WRITE(KNT,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
C          WRITE(KNT,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
C          DO 150 JB=1,KSUP(6)
C
C          WRITE(KNT,'(1H )')
C
C          DO 160 JA=1,KSUP(5)
C
C          JAJB=JA+(JB-1)*KELEM
C
C          if(VALUES(JAJB).ge.(rvind-eps).and.
C     1       VALUES(JAJB).le.(rvind+eps)) then
C             WRITE(KNT,9903) CNAMES(JA)(1:32),CUNITS(JA)
C             WRITE(KNT,9903) CNAMES(JA)(33:64)
C          ELSE
C             IF(CUNITS(JA)(1:10).EQ.'CODE TABLE'.OR.
C     1         CUNITS(JA)(1:9) .EQ.'CCITTIA5'     ) THEN
C                I=NINT(VALUES(JAJB)/1000)
C                NCHAR=VALUES(JAJB)-I*1000
C                NW=NCHAR/80
C                NWOFF=NCHAR-NW*80
C                IF(NWOFF.NE.0) NW=NW+1
Cc
C                YLONG(1:80)=CVALS(I)
C
C                II=I
C                DO 165 JC=1,NW-1
C                II=II+1
C                KF=JC*80+1
C                KL=(JC+1)*80
C                YLONG(KF:KL)=CVALS(II)
C  165           CONTINUE
C
C                NLINE=NCHAR/30
C                IDIF =NCHAR-NLINE*30
C                IF(IDIF.NE.0) NLINE=NLINE+1
C                YCHAR=YLONG(1:30)
C
C                WRITE(KNT,9904)CNAMES(JA)(1:32),VALUES(JAJB),
C     1         CUNITS(JA),YCHAR
C                WRITE(KNT,9904)CNAMES(JA)(33:64)
C
C                IF(NLINE.GT.1) THEN
C                   DO 170 JJ=1,NLINE-1
C
C                   K2=JJ*30+1
C                   K1=(JJ+1)*30
C                   YCHAR=YLONG(K2:K1)
C
C                   WRITE(KNT,9905) YCHAR
C  170              CONTINUE
C
C                END IF
C             ELSE
C                WRITE(KNT,9906) CNAMES(JA)(1:32),VALUES(JAJB),
C     1                       CUNITS(JA)
C                WRITE(KNT,9906) CNAMES(JA)(33:64)
C             END IF
C          END IF
C
C  160     CONTINUE
C  150     CONTINUE
C
        END IF
C
C
C       RETURN
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
C     ------------------------------------------------------------------
 9903 FORMAT(1H ,A,'     MISSING',2X,A)
 9904 FORMAT(1H ,A,F20.4,2X,A,2X,A)
 9905 FORMAT(1H ,100X,A)
 9906 FORMAT(1H ,i4,1x,A,F14.4,2X,A)
 9916 FORMAT(1H ,i4,1x,A15,1X,F20.4,1X,A20,1X,A15,1X,F3.0,1X,A15)
 9917 FORMAT(1H ,i4,1x,A15,1X,F20.4,1X,A24)
 9918 FORMAT(1H ,i4,1x,A15,1X,'             MISSING',1X,A24)
 9919 FORMAT(1H ,i4,1x,A15,1X,F20.4,1X,A24,1X,A)
 9920 FORMAT(1H ,62X,A)
      END
      SUBROUTINE BBUPRTBOX(KNT,KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
C
C**** *BUPRTBOX*
C
C
C     PURPOSE.
C     --------
C
C
C
C**   INTERFACE.
C     ----------
C
C               *call* *buprtbox(knt,kbox,kapp,klen,kboxr,vals,cboxn,cboxu)*
C
C        INPUT :
C               *knt*     -  unit number for io
C               *kbox*    -  number of rows      
C               *kapp*    -  number of columns
C               *klen*    -  offset for start of next column
C               *kboxr*   -  array containing Bufr table B reference numbers
C               *vals*    -  array containing unpacked values
C               *cboxn*   -  array containing element names
C               *cboxu*   -  array containing element units
C
C     METHOD.
C     -------
C
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/94.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      parameter(JELEM=80000)
      dimension ioper(200),rprint(60)
      dimension kboxr(360000),vals(360000),ibval(jelem),ibprint(60)
      character*64 cboxn(40000)
      character*24 cboxu(40000)
C
C
C     ------------------------------------------------------------------
C*                 1. Print boxed expanded bufr message
C                     ---------------------------------
 100  continue
C
      if(kbox.le.6) then
         write(knt,'(a)') 'There is no usefull data to be printed.'
         kbox=0
        return
      end if
c
      if(kapp.gt.60) then
         write(knt,'(a)') 'There is more than 60 applications in 
     1         the data'
         write(knt,'(a)') 'Only first 60 will be processed'
         kapp=60
      end if
c
      if(kapp.gt.1) then
         irep=(kapp-1)/10
         ioff=(kapp-1)-irep*10
         if(ioff.ne.0) irep=irep+1
      else
         irep=1
         ioff=0
      end if
c
      ist=2
      iend=11
c      if(irep.eq.1.and.kapp.eq.1) iend=ioff+1
      if(irep.eq.1) iend=ioff+1
c
      do 2005 j=1,irep
c
      write(knt,'(a)')' '
      do 2002 i=1,kbox
      iiii=1
      rprint(iiii)=vals(i)
c
      do 2003 ii=ist,iend
      iiii=iiii+1
      iii=i+(ii-1)*klen      
      rprint(iiii)=vals(iii)
      ibprint(iiii)=kboxr(iii)
 2003 continue
c     write(knt,'(1h ,i4,1x,a32,1x,15(1x,i6,1x,f8.1))') 
c    1        i,cboxn(i),(ibprint(nn),rprint(nn),nn=1,kapp)
      write(knt,'(1h ,i4,1x,a32,1x,f14.1,30(1x,f8.1))')
     1        i,cboxn(i),(rprint(nn),nn=1,iiii)
 2002 continue
c
      if(ioff.ne.0.and.j.eq.(irep-1)) then
         ist=iend+1
         iend=iend+ioff
      else
         ist=iend+1
         iend=iend+10
      end if
c
 2005 continue
c
c
      return
      end
      SUBROUTINE BUAUG(KPT,KDLEN,KDATA,KJ,KY,KSTACK,KERR)
C
C**** *BUAUG*
C
C
C     PURPOSE.
C     --------
C          Update augmented Bufr table B.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUAUG(KPT,KDLEN,KDATA,KJ,KY,KSTACK,KER)
C
C        INPUT :
C               *KPT*      - pointer too kdata array
C               *KDLEN*    -  dimension of KDATA array
C               *KDATA*    -  array containing data needed for data descriptor
C                            expansion
C               *KJ*       - pointer to kstack array
C               *KY*       - operand of the data descriptor operator
C
C        OUTPUT:
C               *KSTACK* - list of elements
C               *KERR*   - return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          GBYTE        - pack bit pathern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCMATBC/ CATBEN(JTAB),CATBU (JTAB)
C
C             CATBEN      - augmented table B element name
C             CATBU       - augmented table B units
C
C
      COMMON /BCMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24
C
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
C     ------------------------------------------------------------------
C
C*          1.   UPDATE AUGMENTED TABLE B .
C                --------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1 Y = 0 ?
C               -------
 110  CONTINUE
C
      IF( KY.EQ.0) THEN
C
C*          1.1.1 CLEAR AUGMENTED TABLE B.
C                 ------------------------
         NJA= 0
C
         DO 111 J=1,JTAB
C
         NATBTR(J)= 0
         NATBS (J)= 0
         NATBRV(J)= 0
         NATBDW(J)= 0
         CATBEN(J)=' '
         CATBU (J)=' '
C
 111     CONTINUE
C
         GO TO 300
      END IF
C
C*          1.2  GET NEXT DESCRIPTOR FROM STACK.
C                -------------------------------
 120  CONTINUE
C
      KJ=KJ + 1
      KDD = KSTACK(KJ)
C
C*          1.3  ELEMENT DESCRIPTOR  ?
C                ---------------------
 130  CONTINUE
C
      IF  = KDD /100000
      IDIF= KDD -IF*100000
      IX  = IDIF/1000
      IY  = IDIF-IX*1000
C
      IF(IF.EQ.0) THEN
C
C*          1.3.1 ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
         NWT = NWT + 1
         M   = M   + 1
         CWTEN(NWT)='REFERENCE VALUE'
         CWTU (NWT)='  '
         NWTDW(NWT)= KY
         NWTS (NWT)=0
         NWTRV(NWT)=0
         NWTR (NWT)=0
C
C*          1.3.2 ADD ENTRY TO AUGMENTED TABLE B .
C                 --------------------------------
         DO 131 J=1,JTAB
C
         IF(NTABBTR(J).EQ.KDD) THEN
            I=J
            GO TO 133
         END IF
C
 131     CONTINUE
C
         KERR= 23
         PRINT*,' BUAUG :'
         CALL BUERR(KERR)
         GO TO 300
C
 133  CONTINUE
C
         NJA=NJA + 1
C
         NATBTR(NJA)=NTABBTR(I)
         NATBS (NJA)=NTABBS (I)
         NATBRV(NJA)=NTABBRV(I)
         NATBDW(NJA)=NTABBDW(I)
         CATBEN(NJA)=CTABBEN(I)
         CATBU (NJA)=CTABBU (I)
C
C*          1.3.3 COMPLITE ENTRY WITH NEW REFERENCE VALUE
C                 ----------------------------------------
C                 FROM DATA SECTION.
C                 ------------------
C
         KPT=KPT+1
         NATBRV(NJA)=KDATA(KPT)
C
C           1.3.4 UPDATE WORKING TABLES
C                 ---------------------
 135     CONTINUE
C
         CALL BUEPWT(KDD,KERR)
         IF(KERR.GT.0) RETURN
C
         GO TO 120
C
      END IF
C
C     ------------------------------------------------------------------
C
C*           1.4   CHANGE REFERENCE VALUE ?
C                  ------------------------
 140  CONTINUE
C
      IF( IF.EQ.2.AND.IX.EQ.3) THEN
         IF(IY.EQ.255) GO TO 300
      END IF
C     ------------------------------------------------------------------
 200  CONTINUE
C
      KERR=23
      PRINT*,' BUAUG :'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      RETURN
C
      END
      SUBROUTINE BUBOX(KSUB,KSUP,KELEM,KWTR,CNAMES,CUNITS,KVALS,VALUES,
     1                 KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,KERR)
C
C**** *BUBOX*
C
C
C     PURPOSE.
C     --------
C
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL BUBOX(KSUB,KSUP,KELEM,KWTR,CNAMES,CUNITS,KVALS,VALUES,
C                           KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,KERR)*
C
C        INPUT :
C               *ksub*    -  subset number
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KELEM*   -  expected number of expanded table B elements
C               *Kwtr*    -  array containing Bufr table B reference numbers
C               *cnames*  -  array containing element names
C               *cunits*  -  array containing element units
C               *kvals*   -  dimension of values array
C               *values*  -  array containing unpacked values
C               
C        OUTPUT :
C
C               *kbox*    -  number of rows      
C               *kapp*    -  number of columns
C               *klen*    -  offset for start of next column
C               *kboxr*   -  array containing Bufr table B reference numbers
C               *vals*    -  array containing unpacked values
C               *cboxn*   -  array containing element names
C               *cboxu*   -  array containing element units
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/94.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      character*64 cnames(*)
      character*24 cunits(*)
      dimension    values(*)
C
C
      DIMENSION KSUP(9),kwtr(*)
C
      DIMENSION IOPER(200),rprint(60)
C
      dimension kboxr(360000),vals(360000),ibval(40000),ibprint(60)
      character*64 cboxn(40000)
      character*24 cboxu(40000)
C
C     ------------------------------------------------------------------
C
C*          1.  Get all elements until first operator appear.
C               ---------------------------------------------
 100  CONTINUE
C

      IF(KERR.GT.0) RETURN
c
      rvind=1.7E38
      eps=10.E-10
      nvind=2147483647
c
      if(kelem.gt.360000/3) then
         kerr=47
         call buerr(kerr)
         return
      end if
c
c     just to keep program for future multy subset boxing
c
      ksub1=ksub
      ksub2=ksub
c
      if(ksub1.lt.1.or.ksub1.gt.ksup(6)) then
         kerr=44
         call buerr(kerr)
         return
      end if
c
      if(ksub1.ne.ksub2) then
         print*,'Only one subset at a time will be boxed.'
         ksub2=ksub1
      end if
c
      kboxr(1)=2147483647
      cboxn(1)='OPERATOR'
      cboxu(1)=' '
      kboxr(2)=2147483647
      cboxn(2)='GENERATING CENTRE( CODE TABLE 001031)'
      cboxu(2)='CODE TABLE'
      kboxr(3)=2147483647
      cboxn(3)='GENERATING APPLICATION (CODE TABLE 001032)'
      cboxu(3)='CODE TABLE'
      kboxr(4)=2147483647
      cboxn(4)='STATISTICS (008024/008023) '
      cboxu(4)='CODE TABLE'
      kboxr(5)=2147483647
      cboxn(5)='INCREMENTAL UPDATE NUMBER'
      cboxu(5)='NUMERIC'
      kboxr(6)=2147483647
      cboxn(6)='MINIMISATION SIMULATION NUMBER'
      cboxu(6)='NUMERIC'
C
      mrel=0
      j=0
      do 103 i=1,ksup(5)
      if(kwtr(i).eq.222000.or.
     1   kwtr(i).eq.223000.or.
     1   kwtr(i).eq.224000.or.
     1   kwtr(i).eq.225000.or.
     1   kwtr(i).eq.235000.or.
     1   kwtr(i).eq.237255.or.
     1   kwtr(i).eq.232000) then
         j=j+1
         ioper(j)=i
      end if
 103  continue
c
      noper=j
c
      ioper(j+1)=ksup(5)
c
      nopp1=noper+1
      ini=ioper(1)*8+20
      if(ini*nopp1.gt.360000) then
         kerr=47
         print*,'Too many data for boxing.'
         return
      end if
c
c     do 105 i=1,ini*nopp1
c     vals(i)=rvind
c     kboxr(i)=nvind
c105  continue
c
      if(noper.eq.0) then
         kapp =1                            ! number of applications
         kappl=1
         klen=kelem
         j=6
         do 101 i=1,ksup(5)
         j=j+1
         kboxr(j)=kwtr(i)
         cboxn(j)=cnames(i)
         cboxu(j)=cunits(i)
         do 191 kk=ksub1,ksub2
         jj=i+(kk-1)*kelem                     ! pointer to values array
         ip=j  !+(kapp-1)*klen  ! +(kk-1)*kappl*klen ! pointer to vals array
         vals (ip)=values(jj)
 191     continue
 101     continue
c
         kbox=j                             ! total number of elements
         mrel=j
         go to 2000
      end if
c
      o236=.false.
c
      lfirst=1
      j=6
c
c
      last=ioper(1)-1
      if(mrel.eq.0) mrel=last+6
      klen=last*8+20
c
 102  continue
c
c*          1.1 Move elements from lfirst to last into box.
c               -------------------------------------------
 110  continue
c
      kapp =1                            ! number of applications
      kappl=60
      do 111 i=lfirst,last
      j=j+1
      kboxr(j)=kwtr(i)
      cboxn(j)=cnames(i)
      cboxu(j)=cunits(i)
      do 192 kk=ksub1,ksub2
      jj=i+(kk-1)*kelem
      ip=j ! +(kapp-1)*klen   ! +(kk-1)*kappl*klen
      vals (ip)=values(jj)
 192  continue
 111  continue
c
      kbox=j                             ! total number of elements
c
      do 1000 ll=1,noper
c
      k=ioper(ll)
c
      if(kwtr(k).eq.235000) then
         mrel=0
         k=k+1
c
         kl=ioper(ll+1)-1
         do 112 i=k,kl
         j=j+1
         kboxr(j)=kwtr(i)
         cboxn(j)=cnames(i)
         cboxu(j)=cunits(i)
         do 190 kk=ksub1,ksub2
         jj=i+(kk-1)*kelem
         ip=j  !+(kapp-1)*klen              ! +(kk-1)*kappl*klen
         vals (ip)=values(jj)
 190     continue
 112     continue
         go to 1000
      end if
      if(kwtr(k).eq.222000) then
         k=k+1
         imark=222000
         if(mrel.eq.0) mrel=j
         go to 119
      end if
      if(kwtr(k).eq.223000) then
         k=k+1
         imark=223000
         if(mrel.eq.0) mrel=j
         go to 119
      end if
      if(kwtr(k).eq.224000) then
         k=k+1
         imark=224000
         if(mrel.eq.0) mrel=j
         go to 119
      end if
      if(kwtr(k).eq.225000) then
         k=k+1
         imark=225000
         if(mrel.eq.0) mrel=j
         go to 119
      end if
      if(kwtr(k).eq.232000) then
         k=k+1
         imark=232000
         if(mrel.eq.0) mrel=j
      end if
c
 119  continue
c
c     Check if next element is operator
c
      if(kwtr(k).eq.236000) then
         o236=.true.
         k=k+1
      end if
c
      if(kwtr(k).eq.237000) then
         k=k+1
         go to 120
      end if
c
      if(kwtr(k).eq.237255) then
         o236=.false.
         k=k+1
         go to 1000
      end if
c
c     skip if delayed replication factor follow
c
      if(kwtr(k).eq.31002.or.kwtr(k).eq.31001.or.
     1   kwtr(k).eq.31000) k=k+1
c
c     Next element must be data present indicator
c
      if(kwtr(k).ne.31031.and.kwtr(k).ne.31192) then
         kerr=42
         call buerr(kerr)
         print*,'Element ',k,' must be data present indicator.'
         return
      end if
c
c     Count number of data present indicators
c
c     ibits - pointer to the first data present indicator
c     idpi  - number of data present indicators
c
      ibits=k
      idpi=0
      do 113 i=k,ioper(ll+1)
      if(kwtr(i).eq.31031) then
         idpi=idpi+1 
      else
         go to 114
      end if
 113  continue
c
      kerr=43
      call buerr(kerr)
      return
c
 114  continue
c
c     reset current pointer
c
      k=i
c
c     Get bit map from values
c
      ibp=ibits-1
      do 115 i=1,idpi
      ibp=ibp+1
      if(abs(values(ibp)-rvind).lt.eps) then
         ibval(i)=nvind
      else 
         ibval(i)=values(ibp)
      end if
 115  continue
c
c
 120  continue
c
c     Next 3 elements must be generating centre and application 
c     and class 8 element.
c
      kapp=kapp+1
      if(kapp.gt.60) then
         print*,'There is more than 60 applications in the data.'
         return
      end if
      if(kwtr(k).eq.1031) then
         kp=(kapp-1)*klen
         kp1=kp+1
         kboxr(kp1)=imark
c         cboxn(kp1)=' '
c         cboxu(kp1)=' '
         do 194 kk=ksub1,ksub2
         ip=1+(kapp-1)*klen             ! +(kk-1)*kappl*klen
         vals(ip)=imark
 194     continue
         kp2=kp+2
         kboxr(kp2)=kwtr(k)
c         cboxn(kp2)=cnames(k)
c         cboxu(kp2)=cunits(k)
         do 195 kk=ksub1,ksub2
         jj=k+(kk-1)*kelem
         ip=2+(kapp-1)*klen             ! +(kk-1)*kappl*klen
         vals(ip)=values(jj)  
 195     continue
         k=k+1
      end if
      if(kwtr(k).eq.1032.or.kwtr(k).eq.1201.or.kwtr(k).eq.63191) then
         kp=(kapp-1)*klen
         kp3=kp+3
         kboxr(kp3)=kwtr(k)
c         cboxn(kp3)=cnames(k)
c         cboxu(kp3)=cunits(k)
         do 196 kk=ksub1,ksub2
         jj=k+(kk-1)*kelem
         ip=3+(kapp-1)*klen             ! +(kk-1)*kappl*klen
         vals(ip)=values(jj)
 196     continue
         k=k+1
      end if
      if(kwtr(k)/1000.eq.8) then
         kp=(kapp-1)*klen
         kp4=kp+4
         kboxr(kp4)=kwtr(k)
c         cboxn(kp4)=cnames(k)
c         cboxu(kp4)=cunits(k)
         do 197 kk=ksub1,ksub2
         jj=k+(kk-1)*kelem
         ip=4+(kapp-1)*klen             !  +(kk-1)*kappl*klen
         vals(ip)=values(jj)
 197     continue
         k=k+1
      end if
      if(kwtr(k).eq.33210) then
         kp=(kapp-1)*klen
         kp5=kp+5
         kboxr(kp5)=kwtr(k)
c         cboxn(kp5)=cnames(k)
c         cboxu(kp5)=cunits(k)
         do 201 kk=ksub1,ksub2
         jj=k+(kk-1)*kelem
         ip=5+(kapp-1)*klen             !  +(kk-1)*kappl*klen
         vals(ip)=values(jj)
 201     continue
         k=k+1
      end if
      if(kwtr(k).eq.33211) then
         kp=(kapp-1)*klen
         kp6=kp+6
         kboxr(kp6)=kwtr(k)
c         cboxn(kp6)=cnames(k)
c         cboxu(kp6)=cunits(k)
         do 202 kk=ksub1,ksub2
         jj=k+(kk-1)*kelem
         ip=6+(kapp-1)*klen             !  +(kk-1)*kappl*klen
         vals(ip)=values(jj)
 202     continue
         k=k+1
      end if
c
c     next element can be delayed/extended delayed replication
c     skip it
c
      if(kwtr(k).eq.31002.or.kwtr(k).eq.31001.or.
     1   kwtr(k).eq.31000) k=k+1
c
c
c     move coresponding quality control into box
c
      kq=mrel-idpi+1
      kappk=(kapp-1)*klen  
      do 121 i=1,idpi
      if(ibval(i).ne.0) then
         kkq=kq+kappk
         vals(kkq)=rvind 
         kq=kq+1
      else
         kkq=kq+kappk
         kboxr(kkq)=kwtr(k)
c         cboxn(kkq)=cnames(k)
c         cboxu(kkq)=cunits(k)
         do 198 kk=ksub1,ksub2
         jj=k+(kk-1)*kelem                ! +(kk-1)*kappl*klen
         vals(kkq)=values(jj)
 198     continue
         k=k+1 
         kq=kq+1
      end if
 121  continue
c
c     check if some new data follow.
c
      if(k.lt.ioper(ll+1)-1) then
c
c        move these elements into box
c
         if(kwtr(ioper(ll+1)).gt.JELEM.and.
     1      kwtr(ioper(ll+1)).ne.999999) then
            ill=ioper(ll+1)-1
         else
            ill=ioper(ll+1)
         end if
         do 122 i=k,ill
         j=j+1
         kboxr(j)=kwtr(i)
         cboxn(j)=cnames(i)
         cboxu(j)=cunits(i)
         do 199 kk=ksub1,ksub2
         jj=i+(kk-1)*kelem
         ip=j   !+(kapp-1)*klen              !  +(kk-1)*kappl*klen
         vals(ip)=values(jj)
 199     continue
 122     continue
         kbox=j
      end if
c
 1000 continue
c
 2000 continue
c
      kbox=j
      if(kbox.le.6) kbox=0
c
      return
      end
      SUBROUTINE BUEDD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1                 KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
C**** *BUEDD*
C
C
C     PURPOSE.
C     --------
C
C          Expand section 3 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEDD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
C                        KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
C        INPUT :
C               *KPT*     -  pointer to kdata array
C               *KTDLEN*  -  number of data descriptors in section 3
C               *KTDLST*  -  array containing data descriptors in section 3
C               *KDLEN*   -  dimension of KDATA array
C               *KDATA*   -  array containing data needed for data descriptor
C                            expansion
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C        OUTPUT :
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C           Data descriptor taken from KTDLST array are fully
C        expanded using data from KDATA array if needed.
C        ( delayed replication factors etc.)
C
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - set word and bit pointers at the begining of
C                          next section
C          BUNPCK        - unpacks bit pathern
C          BUREP         - solves replication problem
C          BUETDR        - solves table D reference
C          BUOPER        - process operator
C          BUEPWT        - updates working table
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMEL/ NTDLEN,NTDLST(JELEM),NTDEXL,NTDEXP(JELEM)
C
C             NTDLEN - number of Data descriptors in section 3
C             NTDLST - list of Data descriptors
C             NTDEXL - number of expanded Data Descriptors
C             NTDEXP - list of expanded Data descriptors
C
C
      CHARACTER CWTEN*64,CWTU*24
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION VALUES(KVALS)
C
      DIMENSION ISTACK(JELEM),IISTACK(JELEM)
      DIMENSION IMASK(8)
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KDATA(KDLEN),KTDLST(KTDLEN)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
C     ------------------------------------------------------------------
C
C*          1.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      N = KSEC3(3)
C
C
C*          2.   EXPAND DATA DESCRIPTORS.
C                ------------------------
 200  CONTINUE
C
C
C*          2.1  SET EXPECTED NUMBER OF DATA DESCRIPTORS.
C                ----------------------------------------
C                AND INITIALIZE NUMBER OF DATA VALUES PER SUB-SET.
C                -------------------------------------------------
 210  CONTINUE
C
      J      = 0
      KPT    = 0
      NWT    = 0
      JMAX   = KTDLEN
      JMAXNEW=JMAX
C
      IF(KTDLEN.GT.JELEM) THEN
         PRINT*,' BUETD :'
         KERR=29
         CALL BUERR(KERR)
         RETURN
      END IF
C
C*          2.2  PUT DATA DESCRIPTORS IN STACK.
C                ------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,JMAX
C
      ISTACK(JJ)=KTDLST(JJ)
      IISTACK(JJ)=ISTACK(JJ)
C
 221  CONTINUE
C
C*          2.2.1 CHECK IF IT IS SAME DATA DESCRIPTOR DESCRIOPTION.
C                 -------------------------------------------------
C                 TO MAKE MORE EFFICIENT DATA DESCRIPTOR DESCRIPTION
C                 EXPANSION, IN CASE THAT DELAYED REPLICATION FACTOR
C                 IS NOT PRESENT AND DATA DESCRIPTORS ARE THE SAME,
C                 PREVIOUS WORKING TABLE SHOULD BE USED. IT IS POSIBLE
C                 AT THIS PLACE IN THE FUTURE TO MAKE MORE SOPHISTICATED
C                 CONTROL.
C
C
      DO 222 JC=1,JMAX
C
      IF(ISTACK(JC).NE.NSTACK(JC)) THEN
C
C
         ODREPF=.FALSE.
C
C        SWAP CONTENT OF THE STACKS.
C
         DO 223 JJC=1,JMAX
         NSTACK(JJC)=ISTACK(JJC)
 223     CONTINUE
C
         NTDLEN = JMAX
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         MBMP=0
         MBMPL=0
C
         GO TO 230
C
      END IF
C
 222  CONTINUE
C
C
C*    IF MARKER OPERATOR PRESENT EXPAND DESCRIPTORS AGAIN
C
      IF(OMARKER) THEN
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    RETURN IF DELAYED REPLICATION FACTOR IS NOT PRESENT.
C
      IF(JMAX.NE.NTDLEN) THEN
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
      OB=.FALSE.
      if(iand(KSEC3(4),imask(7)).ne.0)  OB=.TRUE.
C
      IF(ODREPF) GO TO 229
C
      IF(.NOT.ODREPF) THEN
         IF(N.GT.NOLD) NOLD=N
         IF(OB.AND.NFCM.EQ.1)      GO TO 300
         IF(.NOT.OB.AND.NFCM.EQ.0) GO TO 300
      END IF
C
 229  CONTINUE
C
      M=0
      NOLD=N
      NFCM=0
      MREL=0
      OMARKER=.FALSE.
      NTDLEN=JMAX
      MBMP=0
      MBMPL=0
C
C     ------------------------------------------------------------------
C*          2.3  GET NEXT DESCRIPTOR FROM THE STACK.
C                -----------------------------------
 230  CONTINUE
C
      J   = J + 1
      IF(J.GT.JMAX) GO TO 270
C
      IDD = ISTACK(J)
      IF(IDD.EQ.0)  GO TO 230
C
      IF = IDD/100000
C
C     ------------------------------------------------------------------
C*          2.4  CHECK IF IT IS REPLICATION DESCRIPTOR.
C                --------------------------------------
 240  CONTINUE
C
      if(if.eq.0) then
C
C*          2.6  ELEMENT DESCRIPTOR, SO UPDATE WORKING TABLE.
C                --------------------------------------------
 260     CONTINUE
C
         if(idd.eq.31031.or.idd.eq.31192) then
            nwt=nwt+1
            nwtr(nwt)=idd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=1
            cwten(nwt)='DATA PRESENT INDICATOR'
            cwtu (nwt)='NUMERIC'
            m=m+1
         elseif(idd.eq.33007.or.idd.eq.63192) then
            nwt=nwt+1
            nwtr(nwt)=idd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=7
            cwten(nwt)='% CONFIDENCE'
            cwtu (nwt)='NUMERIC'
            m=m+1
         else
            CALL BUEPWTC(IDD,KERR)
            IF(KERR.GT.0) RETURN
         end if
      elseif(if.eq.1) then
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
         CALL BUREPC(KPT,KDLEN,KDATA,J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
c
      elseif(if.eq.2) then
C
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
         CALL BUOPERC(KPT,KDLEN,KDATA,J,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
c
      elseif(if.eq.3) then
c
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
         CALL BUETDR(J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) THEN
            DO 252 IQ=1,JELEM
            NSTACK(IQ)=0.
 252        CONTINUE
            RETURN
         END IF
      else
         kerr=37
         call buerr(kerr)
         return
      end if
c
      go to 230
c
C     ------------------------------------------------------------------
C*          2.7 RESOLVE MARKER OPERATOR.
C               ------------------------
 270  CONTINUE
C
c      if(omarker) then
C         CALL BUEPMRKC(KSEC3,KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C         IF(KERR.GT.0) RETURN
c      end if
C
C     ------------------------------------------------------------------
C
C*          3. COLLECT  SUPPLEMENTARY ITEMS.
C              -----------------------------
 300  CONTINUE
C
      NTDEXL =M
      DO 301 I=1,NTDEXL
      NTDEXP(I)=NWTR(I)
 301  CONTINUE
C
      NTDLEN=JMAXNEW
      DO 302 I=1,NTDLEN
      NTDLST(I)=IISTACK(I)
 302  CONTINUE
C
      DO 303 I=1,M
      CNAMES(I)=CWTEN(I)
      CUNITS(I)=CWTU (I)
 303  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUENS0( KSEC0,KBUFL,KBUFF,KERR)
C
C**** *BUENS0*
C
C
C     PURPOSE.
C     --------
C          Pack section 0 of Bufr message.
C     Packing Bufr Edition 2.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS0( KSEC0,KBUFL,KBUFF,KERR)*
C
C
C        INPUT:
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C        OUTPUT:
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUPKS         - pack bit pathern in repeated way
C          BUPCK         - pack bit pathern
C          BUOCTN        - set length of section
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSEC0(JSEC0)
      DIMENSION IBUFR(4)
C
      DATA IBUFR/66,85,70,82/
C
C     B IS CCITT.5 DECIMAL 66
C     U IS CCITT.5 DECIMAL 85
C     F IS CCITT.5 DECIMAL 70
C     R IS CCITT.5 DECIMAL 82
C
C
C     ------------------------------------------------------------------
C*          1. PACK SECTION 0.
C              ---------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C
C*          1.1  INITIALIZE WORKING POINTERS NWPT AND NBPT.
C                ------------------------------------------
      NWPT = 1
      NBPT = 0
C
C*          1.2  PACK FIRST FOUR OCTETS CONTAINING *BUFR*.
C                -------------------------------------------
C
      CALL BUPKS(NBPW,KBUFF(NWPT),IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing first four octets of bufr message.'
         RETURN
      END IF
C
      IF(KSEC0(3).LE.1) GO TO 200
C
C*          1.2 PACK TOTAL LENGTH OF BUFR MESSAGE.
C               ------------------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing total length of bufr message'
         PRINT*,'in section 0.'
         RETURN
      END IF
C
C*          1.3 PACK BUFR EDITION NUMBER (IT IS 8TH BYTE ).
C               ---------------------------------------------
 130  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC0(3),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing Bufr Edition number in section 0.'
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUENS1( KSEC0,KSEC1,KBUFL,KBUFF,KERR)
C
C**** *BUENS1*
C
C
C     PURPOSE.
C     --------
C          Pack section 1 of Bufr message.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS1( KSEC0,KSEC1,KBUFL,KBUFF,KERR)*
C
C
C        INPUT :
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) to ksec1(JSEC1) - local ADP centre
C                                        information(PACKED FORM)
C
C                            For Bufr Edition 3 onward
C                            
C                            KSEC1(16)-- Originating Sub-centre
C                            KSEC1(17)-- Not used
C                            KSEC1(18) to ksec1(JSEC1) - local ADP centre
C                                        information(PACKED FORM)
C
C        OUTPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C*     METHOD.
C      -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK         -  pack bit pathern
C          BUOCTN        - set length of section
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       16/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION ISEC1(JSEC1)
C
C     ------------------------------------------------------------------
C*          1.  PACK SECTION 1.
C               ---------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 101 I=1,JSEC1
      ISEC1(I)=0
 101  CONTINUE
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C
C*          1.2 PACK LENGTH OF SECTION 1.
C               -------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing length of section 1.'
         RETURN
      END IF
C
C
C*          1.3  PACK BUFR EDITION NUMBER/MASTER TABLE USED.
C                -------------------------------------------
 130  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(2),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing Bufr Edition number'
            PRINT*,'in section 1.'
            RETURN
         END IF
      ELSE
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(14),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing Bufr Master Table number'
            PRINT*,'in section 1.'
            RETURN
         END IF
      END IF
C
C*          1.4  PACK ORIGINATING CENTRE.
C                ------------------------
 140  CONTINUE
C
      if(ksec0(3).lt.3) then
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(3),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing Originating centre in section 1.'
            RETURN
         END IF
      else
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(16),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing Originating centre in section 1.'
            RETURN
         END IF
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(3),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing Originating centre in section 1.'
            RETURN
         END IF
      end if
C
C*          1.5  PACK UPDATE SEQUENCE NUMBER.
C                ----------------------------
 150  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(4),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing update sequence number in section 1.'
         RETURN
      END IF
C
C*          1.6  PACK INTEGER VALUE OF THE OCTET CONTAINING
C                ------------------------------------------
C                FLAG BITS(ZERO IF SECTION TWO IS NOT PRESENT).
C                ----------------------------------------------
 160  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(5),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing flag in section 1.'
         RETURN
      END IF
C
C*          1.7  PACK *BUFR* MESSAGE TYPE.
C                -------------------------
 170  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(6),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing Bufr message type in section 1.'
         RETURN
      END IF
C
C*          1.8  PACK *BUFR* MESSAGE SUB-TYPE.
C                -----------------------------
 180  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(7),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing Bufr message subtype.'
         RETURN
      END IF
C
C*          1.9  PACK LOCAL TABLE VERSION NUMBER OR
C                ----------------------------------
C                VERSION NUMBER OF MASTER TABLE USED.
C                ------------------------------------
 190  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(8),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing version number of local table used.'
            RETURN
         END IF
      ELSE
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(15),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing version number of master table used.'
            RETURN
         END IF
         CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1( 8),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing version number of local table used.'
            RETURN
         END IF
      END IF
C
C*          2.0  PACK YEAR.
C                ----------
 200  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(9),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec1(9) in section 1.'
         RETURN
      END IF
C
C*          2.1  PACK MONTH.
C                -----------
 210  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(10),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec1(10) in section 1.'
         RETURN
      END IF
C
C*          2.2 PACK DAY.
C               -----------
 220  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(11),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec1(11) in section 1.'
         RETURN
      END IF
C
C*          2.3 PACK HOUR.
C               ------------
 230  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(12),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec1(12) in section 1.'
         RETURN
      END IF
C
C*          2.4 PACK MINUTE.
C               --------------
 240  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC1(13),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec1(13) in section 1.'
         RETURN
      END IF
C
C          2.5 PACK LOCAL ADP CENTRE INFORMATION.
C              ----------------------------------
 250  CONTINUE
C
      if(ksec0(3).lt.3) then
         IOFF=KSEC1(1)-17
         IW=16
         ib=0
      else
         IOFF=KSEC1(1)-17
         iw=18
         ib=0
      end if
      IF(IOFF.NE.0) THEN
c         IW=16
c         IB=0
         CALL BUNPKS(NBPW,KSEC1,ISEC1,IW,IB,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing local ADP centre information'
            PRINT*,'in section 1.'
            RETURN
         END IF
C
         CALL BUPKS(NBPW,KBUFF(NWPT),ISEC1,NWPT,NBPT,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing local ADP centre information'
            print*,'in section 1.'
            RETURN
         END IF
      END IF
C     ------------------------------------------------------------------
C*          2.6  SET UP LENGTH OF THE SECTION 1.
C                --------------------------------
 260  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
      RETURN
      END
      SUBROUTINE BUENS2( KSEC1,KSEC2,KBUFL,KBUFF,KERR )
C
C**** *BUENS2*
C
C
C     PURPOSE.
C     --------
C          Pack section 2 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS2(KSEC1,KSEC2,KBUFL,KBUFF,KERR)*
C
C
C        INPUT :
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) to ksec1(JSEC1) - local ADP centre
C                                        information(PACKED FORM)
C               *KSEC2*   -  array containing section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(JSEC2) local ADP centre
C                                         information (PACKED FORM)
C        OUTPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK          - pack bit pathern
C          BUOCTN         - set length of section
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSEC1(JSEC1),KSEC2(JSEC2)
      DIMENSION IDUM(8),     ISEC2(JSEC2)
C
      DATA IDUM/8*0/
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 2.
C               -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF( KSEC1(5).NE.128) RETURN
C
      DO 101 I=1,JSEC2
      ISEC2(I)=0
 101  CONTINUE
C
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C*          1.2  PACK LENGTH OF SECTION 2.
C                -------------------------
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing length of section 2.'
         RETURN
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) RETURN
C
C
C*          1.3  LOCAL ADP CENTRE INFORMATION.
C                -----------------------------
C
      IOFF=KSEC2(1)-4
      IF(IOFF.GT.0) THEN
         IW=2
         IB=0
         CALL BUNPKS(NBPW,KSEC2,ISEC2,IW,IB,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing local ADP centre information'
            PRINT*,'in section 2.'
            RETURN
         END IF
C
         CALL BUPKS(NBPW,KBUFF(NWPT),ISEC2,NWPT,NBPT,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing local ADP centre information'
            PRINT*,'in section 2.'
            RETURN
         END IF
      END IF
C
C
C*          1.5  SET UP LENGTH OF THE SECTION 2.
C                --------------------------------
 150  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
      RETURN
      END
      SUBROUTINE BUENS3(KSEC3,KTDLEN,KTDLST,KBUFL,KBUFF,KERR)
C
C**** *BUENS3*
C
C
C     PURPOSE.
C     --------
C
C          Pack section 3 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS3( KSEC3,KTDLEN,KTDLST,KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KTDLEN*  -  number of data descriptors in section 3
C               *KTDLST*  -  array containing data descriptors in section 3
C
C        OUTPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C     METHOD.
C      -------
C
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK        - packs bit pathern
C          BUOCTN       - set length of section
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KTDLST(KTDLEN)
C
      DIMENSION KSEC3(JSEC3)
C
C     ------------------------------------------------------------------
C*          1.   PACK PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C
C*          1.2   PACK LENGTH OF SECTION 3.
C                 -------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing length of section 3.'
         RETURN
      END IF
C
C
C*          1.3    PACK ZERO BYTE AND PUT IT IN KSEC3(2).
C                  --------------------------------------
 130  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing reserved byte in section 3.'
         RETURN
      END IF
C
C*          1.4    PACK NUMBER OF DATA SUB-SETS.
C                  -----------------------------
 140  CONTINUE
C
      IF(KSEC3(3).GT.65535) THEN
         KERR=27
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC3(3),NWPT,NBPT,16,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec3(3) in section 3.'
         RETURN
      END IF
C
C
C*          1.5    PACK INTEGER VALUE OF THE OCTET
C                  ---------------------------------
C                  CONTAINIG FLAG BITS.
C                  --------------------
 150  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),KSEC3(4),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ksec3(4) in section 3.'
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C*          1.6  PACK DATA DESCRIPTORS.
C                ----------------------
 160  CONTINUE
C
C
      DO 161 I=1,KTDLEN
      IFIXIY=KTDLST(I)
C
      II=IFIXIY/1000
      IY=IFIXIY-II*1000
      IF=II/100
      IX=II-IF*100
C
      CALL BUPCK(NBPW,KBUFF(NWPT),IF,NWPT,NBPT,2,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ',i,' descriptor in section 3.'
         RETURN
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),IX,NWPT,NBPT,6,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ',i,' descriptor in section 3.'
         RETURN
      END IF
      CALL BUPCK(NBPW,KBUFF(NWPT),IY,NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing ',i,' descriptor in section 3.'
         RETURN
      END IF
C
C
 161  CONTINUE
C
C*          1.7  SET UP LENGTH OF THE SECTION 1.
C               --------------------------------
 170  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
      RETURN
      END
      SUBROUTINE BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
     1                  KBUFL,KBUFF,KERR)
C
C**** *BUENS4*
C
C
C     PURPOSE.
C     --------
C          Pack preliminary items and data of section 4 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
C                         KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KSEC4*   -  array containing section 4 information
C                            KSEC4( 1)-- length of section 4 (bytes)
C                            KSEC4( 2)-- reserved
C               *KELEM*   -  number of elements in bufr template
C               *KVALS*   -  dimension of VALUES array
C               *VALUES*  -  real array (expanded data values)
C
C        OUTPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUPCK          -  pack bit pathern
C          BUPKS          -  pack bit pathern in repeated way,
C                            pointer adjustment
C          BUOCTN         -  set length of section
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
      COMMON /BCPRQ/ NPMISS,NPRUS,NOKEY
C
      CHARACTER*64 CWTEN
      CHARACTER*24 CWTU
C
      DIMENSION KBUFF(KBUFL)
C
      DIMENSION VALUES(KVALS)
      DIMENSION KSEC3(JSEC3),KSEC4(JSEC4)
C
      DIMENSION IVALS(JWORK),INC(JELEM),ilocval(jelem)
C
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 YVAL
C
      DIMENSION IMASK(8),IMAXV(31)
      DATA IMASK/1,2,4,8,16,32,64,128/
      data ilocval/jelem*0/
C
      DATA IMAXV/1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,
     1  16383,32767,65535,131071,262143,524287,1048575,2097151,
     2  4194305,8388607,16777215,33554431,671108863,134217727,
     3  268435455,536870911,1073741823,2147483647/
C
C     ------------------------------------------------------------------
C*          1.  PACK PRELIMINARY ITEMS OF SECTION 4.
C               ------------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
C
C*          1.1  KEEP POINTERS TO THE BEGINING OF THE SECTION.
C                ---------------------------------------------
 110  CONTINUE
C
      IWPTB = NWPT
      IBPTB = NBPT
C
C*          1.2  PACK LENGTH OF SECTION 4.
C                -------------------------
 120  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing length of section 4.'
         RETURN
      END IF
C
C*          1.4  PACK RESERVED BYTE.
C                -------------------
 140  CONTINUE
C
      CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing reserved byte in section 4.'
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C*          2. PACK DATA.
C              ----------
 200  CONTINUE
C
c      IKK=KELEM*KSEC3(3)
c      IF(IKK.GT.JWORK) THEN
c         KERR=17
c         CALL BUERR(KERR)
c         PRINT*,'Check values of KELEM and number of subsets ksec3(3).'
c         RETURN
c      END IF
c      IF(IKK.GT.KVALS) THEN
c         KERR=14
c         CALL BUERR(KERR)
c         PRINT*,'KVALS must be greater than KELEM*KSEC3(3).'
c         RETURN
c      END IF
C
C*          2.1  CHECK IF DATA HAS TO BE COMRESSED.
C                ----------------------------------
 210  CONTINUE
C
      IB=0
      IF(IAND(ksec3(4),IMASK(7)).NE.0) IB=1
C
C
C     ------------------------------------------------------------------
C
C*          3.  UNCOMPRESSED DATA.
C               ------------------
 300  CONTINUE
C
      IF(IB.EQ.0) THEN
C
         DO 301 I=1,KSEC3(3)
C
         IM1K=(I-1)*KELEM
C
         DO 302 J=1,M
C
         IF(NWTDW(J).EQ.0) GO TO 302
         IREF  =NWTRV(J)
         ISCALE=NWTS (J)
         IBDW  =NWTDW(J)
C
         JI=J+IM1K
C
         VAL=VALUES(JI)
         IF(NWTEN(J).EQ.658367) THEN
            IST=NINT(VAL)/1000
            YVAL=CVALS(IST)
            NCHAR=NINT(VAL)-IST*1000
            DO 303 II=1,NCHAR
            IPACK=ICHAR(YVAL(II:II))
            IF(IPACK.GT.IMAXV(8)) IPACK=IMAXV(8)
            CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error packing ',ji,' value for ',j,' element.'
               RETURN
            END IF
 303        CONTINUE
            GO TO 302
         END IF
C
         IF(ABS(VAL-RVIND).LE.EPS) THEN
            CALL BUPCK(NBPW,KBUFF(NWPT),NMASK(IBDW),NWPT,NBPT,IBDW,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error packing ',ji,' value for ',j,' element.'
               RETURN
            END IF
         ELSE
            IF(ISCALE.LT.0) THEN
               ISCALE=IABS(ISCALE)
               IPACK=NINT(VAL/10.**ISCALE) - IREF
            ELSE
               IPACK=NINT(VAL*10.**ISCALE) - IREF
            END IF
C
C           CHECK IF VALUE TO BE PACKED NEGATIVE
C
            IF(IPACK.LT.0) THEN
               KERR=-33
               PRINT*,'Buens4:'
               print*,'Value ',ipack,' is negative'
               PRINT*,'Probably reference value too big.'
               PRINT*,j,' element = ',nwtr(j),' reference value = ',iref
               IPACK=0
               print*,'Element value packed as',iref
            END IF
C
C*          CHECK IF VALUE TO BE PACKED TOO BIG.
C
            IF(IPACK.GT.IMAXV(IBDW)) THEN
c
               if(npmiss.eq.0) then
                  kerr=-28
                  print*,'Value ',ipack,' too big.'
                  print*,'Value for ',j,' element and ',i,' subset'
                  print*,'packed as missing value for data width -1.'
                  IPACK=IMAXV(IBDW)-1
               else
c
c                 all elements in class 1 to 9 must be correct
c
                  if(nwtr(j).lt.9000.or.
     1               nwtr(j).ge.31000.and.nwtr(j).le.31012) then
                     print*,'Value ',ipack,' too big.'
                     print*,'Value for ',j,' element and ',i,' subset'
                     kerr=28
                     call buerr(kerr)
                     return
                  end if
c
                  kerr=-28
                  print*,'Value ',ipack,' too big.'
                  print*,'Value for ',j,' element and ',i,' subset'
                  print*,'packed as missing value for data width.'
                  IPACK=IMAXV(IBDW)
               end if
            END IF
            CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,IBDW,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error packing ',ji,' value for ',j,' element.'
               PRINT*,'Value ',ipack,' data width ',ibdw,' bits'
               RETURN
            END IF
         END IF
C
 302     CONTINUE
 301     CONTINUE
C
      END IF
C
C*          4.  COMPRESS DATA.
C               --------------
 400  CONTINUE
C
      IF(IB.EQ.1) THEN
         DO 402 J=1,M
C
         IF(NWTDW(J).EQ.0) GO TO 402
         IREF  =NWTRV(J)
         ISCALE=NWTS (J)
         IBDW  =NWTDW(J)
C
         DO 401 I=1,KSEC3(3)
C
         JI=J+(I-1)*KELEM
C
         IF(NWTEN(J).EQ.658367) THEN
            IVALS(JI)=VALUES(JI)
            GO TO 401
         END IF
C
         IF(ABS(VALUES(JI)-RVIND).LE.EPS) THEN
            IVALS(JI)=NMASK(IBDW)
         ELSE
            IF(ISCALE.LT.0) THEN
               ISCAL=IABS(ISCALE)
               IPACK=NINT(VALUES(JI)/10.**ISCAL ) - IREF
            ELSE
               IPACK=NINT(VALUES(JI)*10.**ISCALE) - IREF
            END IF
C
            IF(IPACK.LT.0) THEN
               PRINT*,'Buens4 :'
               KERR=-33
               print*,'Value ',ipack,' is negative'
               PRINT*,'Probably reference value too big.'
               PRINT*,j,'element = ',nwtr(j),' reference value = ',iref
               IPACK=0
               print*,'Element packed as',iref
            END IF
C
C           REPLACE IPACK VALUE WITH MISSING VALUE FOR IBDW -1 
C           IF GREATER THEN MAXIMUM ALLOWED.
C
            IF(IPACK.GT.IMAXV(IBDW)) then
c
               if(npmiss.eq.0) then
                  kerr=-28
                  print*,'Value ',ipack,' too big.'
                  print*,'Value for ',j,' element and ',i,' subset'
                  print*,'packed as missing value for data width -1.'
                  IPACK=IMAXV(IBDW)-1
               else
c
c                 all elements in class 1 to 9 must be correct
c
                  IF(NWTR(J).LT.9000.OR.
     1               NWTR(J).GE.31000.AND.NWTR(J).LE.31012) THEN
                     kerr=28
                     call buerr(kerr)
                     print*,'Value ',ipack,' too big.'
                     print*,'Value for ',j,' element and ',i,' subset'
                     return
                  end if
c
                  kerr=-28
                  print*,'Value ',ipack,' too big.'
                  print*,'Value for ',j,' element and ',i,' subset'
                  print*,'packed as missing value.'
                  IPACK=IMAXV(IBDW)
               END IF
            END IF
            IVALS(JI)=IPACK
C
         END IF
C
 401     CONTINUE
 402     CONTINUE
C
C*          4.1  CHECK IF ALL VALUES ARE MISSING.
C
 410  CONTINUE
C
         DO 411 I=1,M
C
         IF(NWTDW(I).EQ.0) GO TO 411
         IBDW  =NWTDW(I)
C
         OMIS=.TRUE.
         IF(NWTEN(I).EQ.658367) then
            OMIS=.FALSE.
         ELSE
            DO 412 J=1,KSEC3(3)
            IJ=I+(J-1)*KELEM
            IF(IVALS(IJ).NE.NMASK(IBDW)) THEN
               OMIS=.FALSE.
            END IF
 412        CONTINUE
         END IF
C
         IF(.NOT.OMIS) THEN
C
            IF(NWTEN(I).NE.658367) THEN
C
C              FIND MINIMUM VALUE FOR ELEMENT
C
               MIN=IVALS(I)
               DO 413 J=1,KSEC3(3)
               IJ=I+(J-1)*KELEM
               IF(IVALS(IJ).LT.MIN) MIN=IVALS(IJ)
 413           CONTINUE
C
C              FIND INCREMENTS
C
               DO 414 J=1,KSEC3(3)
               IJ=I+(J-1)*KELEM
               INC(J)=IVALS(IJ)-MIN
               IF(IVALS(IJ).EQ.NMASK(IBDW)) INC(J)=NVIND
 414           CONTINUE
C
C              FIND NUMBER OF BITS NEEDED FOR MAX VALUE OF INCREMENT
C
               MAX=0
               DO 415 J=1,KSEC3(3)
               IF(INC(J).NE.NVIND.AND.INC(J).GT.MAX) MAX=INC(J)
 415           CONTINUE
C
C              CHECK IF ALL INCREMENTS ARE ZERO
C
               INC0=0
               DO 419 J=1,KSEC3(3)
               IF(INC(J).NE.0) INC0=1
 419           CONTINUE
C
C              FIND NUMBER OF BITS NEEDED
C
               IF(INC0.NE.0) THEN
                  MAX=MAX+1
                  DO 416 J=1,32
                  IR=MAX/2
                  IF(IR.EQ.0) GO TO 417
                  MAX=IR
 416              CONTINUE
C
               END IF
C
 417           CONTINUE
C
               INCBIT=0
               IF(INC0.NE.0) INCBIT=J
C
C              REPLACE MISSING VALUES FOR INCREMENT BY ALL BITS SET TO 1.
C
               DO 418 J=1,KSEC3(3)
               IF(INC(J).EQ.NVIND) INC(J)=NMASK(INCBIT)
 418           CONTINUE
            END IF
         END IF
C
C*          4.2  PACK DATA IN COMPRESSED FORM.
C                -----------------------------
 420  CONTINUE
C
         IF(NWTEN(I).EQ.658367) THEN
C
C
C           PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
            INCHAR=NWTDW(I)/8
            ISKIP=0
            CALL BUPKS(NBPW,KBUFF(NWPT),ILOCVAL,NWPT,NBPT,8,
     1                ISKIP,INCHAR,KERR)
            IF(KERR.GT.0) then
               print*,'BUENS4 :'
               print*,'Error packing local reference value'
               call buerr(kerr)
               RETURN
            end if
C
C           PACK NUMBER OF BITS FOR INCREMENTS/NUMBER OF CHARACTERS
C
            CALL BUPCK(NBPW,KBUFF(NWPT),INCHAR,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) then
              print*,'BUENS4 :'
              print*,'Error packing number of bits for increments'
              call buerr(kerr)
              RETURN
            end if
C
C           PACK INCREMENTS
C
            DO 421 J=1,KSEC3(3)
C
            JI=I+(J-1)*KELEM
C
            IST=IVALS(JI)/1000
            YVAL=CVALS(IST)
            INCHAR=IVALS(JI)-IST*1000
C
            DO 423 II=1,INCHAR
            IPACK=ICHAR(YVAL(II:II))
            CALL BUPCK(NBPW,KBUFF(NWPT),IPACK,NWPT,NBPT,8,KERR)
            IF(KERR.GT.0) then
               print*,'BUENS4 :'
               call buerr(kerr)
               RETURN
            end if
 423        CONTINUE
C
 421        CONTINUE
C
         ELSE
             IF(OMIS) THEN
C
C               PACK LOCAL REFERENCE VALUE FOR ELEMENT SET TO MISSING VALUE.
C
                CALL BUPCK(NBPW,KBUFF(NWPT),NMASK(IBDW),NWPT,NBPT,
     1                     IBDW,KERR)
                IF(KERR.GT.0) THEN
                   print*,'BUENS4 :'
                   PRINT*,'Error packing local reference value '
                   PRINT*,I,' element.'
                   RETURN
                END IF
C
C               PACK NUMBER OF BITS FOR INCREMENTS (SET TO ZERO)
C
                CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,6,KERR)
                IF(KERR.GT.0) THEN
                   print*,'BUENS4 :'
                   PRINT*,'Error packing number of bits for'
                   PRINT*,'increments for ',i,' element.'
                   RETURN
                END IF
C
             ELSE
C
                IF(INCBIT.EQ.0) THEN
C
C
C                  PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
                   CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW,KERR)
                   IF(KERR.GT.0) THEN
                      print*,'BUENS4 :'
                      PRINT*,'Error packing local reference value'
                      PRINT*,'for ',i,' element in ',ibdw,' bits.'
                      RETURN
                   END IF
C
C                  PACK NUMBER OF BITS FOR INCREMENTS
C
                   CALL BUPCK(NBPW,KBUFF(NWPT),INCBIT,NWPT,NBPT,6,KERR)
                   IF(KERR.GT.0) THEN
                      print*,'BUENS4 :'
                      PRINT*,'Error packing number of bits for'
                      PRINT*,'increments for ',i,' element.'
                      RETURN
                   END IF
C
                ELSE
C
C                  PACK LOCAL REFERENCE VALUE FOR ELEMENT
C
                   CALL BUPCK(NBPW,KBUFF(NWPT),MIN,NWPT,NBPT,IBDW,KERR)
                   IF(KERR.GT.0) THEN
                      print*,'BUENS4 :'
                      PRINT*,'Error packing local reference value'
                      PRINT*,'for ',i,' element.'
                      RETURN
                   END IF
C
C                  PACK NUMBER OF BITS FOR INCREMENTS
C
                   CALL BUPCK(NBPW,KBUFF(NWPT),INCBIT,NWPT,NBPT,6,KERR)
                   IF(KERR.GT.0) THEN
                      print*,'BUENS4 :'
                      PRINT*,'Error packing number of bits for'
                      PRINT*,'increments for ',i,' element.'
                      RETURN
                   END IF
C
C                  PACK INCREMENTS
C
                   CALL BUPKS(NBPW,KBUFF(NWPT),INC,NWPT,NBPT,
     1                        INCBIT,0,KSEC3(3),KERR)
                   IF(KERR.GT.0) THEN
                      print*,'BUENS4 :'
                      PRINT*,'Error packing increments for',i,' element'
                      RETURN
                   END IF
C
                END IF
             END IF
          END IF
C
 411     CONTINUE
C
      END IF
C
C*          5.  SET UP LENGTH OF THE SECTION 4.
C               --------------------------------
 500  CONTINUE
C
      CALL BUOCTN(IWPTB,IBPTB,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) then
         call buerr(kerr)
         RETURN
      end if
C
C     ------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE BUENS5( KSEC0,KSEC1,KBUFL,KBUFF,KERR)
C
C**** *BUENS5*
C
C
C     PURPOSE.
C     --------
C          Pack section 5 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUENS5( KSEC0,KSEC1,KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing Bufr section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(40) -- local ADP centre
C                                        information(BYTE by BYTE)
C        OUTPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUPKS         - pack bit pathern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
      COMMON /BCPRQ/ NPMISS,NPRUS,NOKEY 
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION IBUFR(4)
      DIMENSION KSEC0(JSEC0),KSEC1(JSEC1)
C
      DATA IBUFR/55,55,55,55/
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 5.
C                -----------------
 100  CONTINUE
C
      IF( KERR.GT.0 ) RETURN
C
C*          1.2  PACK LAST FOUR OCTETS CONTAINING '7777'.
C                ----------------------------------------
C
      CALL BUPKS(NBPW,KBUFF(NWPT),IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.GT.0) RETURN
C
C*          1.3  SET TOTAL LENGTH OF BUFR MESSAGE.
C                ---------------------------------
 130  CONTINUE
C
      IBYTES=(NWPT-1)*NBPW/8+NBPT/8
      IWORDS=IBYTES*8/NBPW
      IF(IWORDS*NBPW.LT.IBYTES*8) IWORDS=IWORDS+1
      KBUFL =IWORDS
C
C*          1.4  SET TOTAL LENGTH OF BUFR MESSAGE IN SECTION 0.
C                ----------------------------------------------
 140  CONTINUE
C
      IF(KSEC0(3).GT.1) THEN
C
C        FOR BUFR EDITION 2 LENGTH OF MESSAGE STARTS AT 5 TH BYTE.
C
         IWPT= 32/NBPW + 1
         IBPT= 32 - (IWPT-1)* NBPW
C
         CALL BUPCK(NBPW,KBUFF(IWPT),IBYTES,IWPT,IBPT,24,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing total length of bufr message
     1 in section 0.'
         end if
      END IF
C
C*          1.5 SET TOTAL BUFR LENGTH IN SECTION 2
C
 150  CONTINUE
C
C     RDB KEY PACKED ONLY FOR ECMWF DATA AND SECTION 2 PRESENT.
C
      IF(NOKEY.EQ.0.and.ksec1(5).NE.0) THEN
         IBTS=4
         IF(KSEC0(3).GT.1) IBTS=8
C
C        SIZE OF SECTION 0 AND 1 PLUS 36 BYTES FROM BEGINING
C        OF SECTION 2.
C
         IBTS=IBTS+KSEC1(1)+36
         IBIT=IBTS*8
C
         IWPT= IBIT/NBPW + 1
         IBPT= IBIT - (IWPT-1)* NBPW
C
         CALL BUPCK(NBPW,KBUFF(IWPT),IBYTES,IWPT,IBPT,16,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing total length of bufr message
     1 in section 2.'
         END IF
      END IF
C
      RETURN
      END
      SUBROUTINE BUEPMRK(KSEC3,KVALS,VALUES,KELEM,KERR)
C
C**** *BUEPMRK*
C
C
C     PURPOSE.
C     --------
C
C          Process marker operator, relacing it with corresponding
C     table B element descriptor.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEPMRK(KSEC3,KVALS,VALUES,KELEM,KERR)*
C
C        INPUT :
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data
C                                        compression)
C               *KVALS*   -  dimension of VALUES array
C               *VALUES*  -  aray containing data
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpacks bit pattern
C          BUNPKS         - unpacks bit pattern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. Dragosavac 1/10/92 :
C
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             NFUCM    -  first uncompressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      DIMENSION    KSEC3(JSEC3)
      DIMENSION    VALUES(KVALS)
C
      DIMENSION IMASK(8)
      DATA IMASK/1,2,4,8,16,32,64,128/
C
C     ------------------------------------------------------------------
C*          1.  FINED OPERATOR 223000 TO 237255.
C               --------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
C
C           1.1 CHECK IF DATA ARE COMRESSED.
C               ----------------------------
 110  CONTINUE
C
      IB=0
      if(iand(KSEC3(4),imask(7)).ne.0) IB=1
C
C*          1.2 FIND POSITION OF OPREATORS.
C               ---------------------------
 120  CONTINUE
C
      DO 121 J=1,M
C
      NR223=0
      NR224=0
      NR225=0
      NR232=0
C
C
      IF(NWTR(J).EQ.222000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
c
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 122 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 121
            END IF
 122        CONTINUE
         END IF
      END IF
      IF(NWTR(J).EQ.223000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP223000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 123 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 200
            END IF
 123        CONTINUE
         END IF
         GO TO 200
      END IF
      IF(NWTR(J).EQ.224000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP224000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 124 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 300
            END IF
 124        CONTINUE
         END IF
         GO TO 300
      END IF
      IF(NWTR(J).EQ.225000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP225000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 125 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 400
            END IF
 125        CONTINUE
         END IF
         GO TO 400
      END IF
      IF(NWTR(J).EQ.232000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP232000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 126 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 500
            END IF
 126        CONTINUE
         END IF
         GO TO 500
      END IF
C
      IF(NWTR(J).EQ.235000) THEN
C
C        RESET POINTER  POINTER TO THE LAST DATA ELEMENT
C
         MREL=0
         MBMP=0
         MBMPL=0
      END IF
C
      IF(NWTR(J).EQ.237255) THEN
C
C        RESET POINTERS TO THE BIT MAP
C
         MBMP=0
         MBMPL=0
      END IF
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          2.  PROCESS SUBSTITUTED VALUES OPERATOR.
C               ------------------------------------
C
 200  CONTINUE
C
C*          2.1 FIND FIRST ACCURANCE OD 223255.
C               -------------------------------
 210  CONTINUE
C
      NP223255=0
      DO 211 I=NP223000,M
      IF(NWTR(I).EQ.223255) THEN
         NP223255=I
         GO TO 220
      END IF
 211  CONTINUE
C
      go to 121
C
C*          2.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 220  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I223=0
         OF223=.TRUE.
         DO 221 I=NP223000,NP223255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF223) THEN
               I223=I
               OF223=.FALSE.
            END IF
            NR223=NR223+1
         END IF
 221     CONTINUE
      ELSE
         I223=MBMP
         NR223=MBMPL-MBMP+1
      END IF
C
C
C*          2.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 250  CONTINUE
C
      ISUBST=MREL-NR223+1
C
C*          2.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 260  CONTINUE
C
      DO 261 I=I223,NR223+I223
      IF(VALUES(I).EQ.0) THEN
c
 262     continue
c
         if(nwtr(NP223255).eq.223255) then
            NWTDW (NP223255)=NWTDW(ISUBST)
            NWTR  (NP223255)=NWTR (ISUBST)
            NWTRV (NP223255)=NWTRV(ISUBST)
            NWTS  (NP223255)=NWTS (ISUBST)
c            CWTEN (NP223255)=CWTEN(ISUBST)
C            CWTU  (NP223255)=CWTU (ISUBST)
            nwten(NP223255)=nwten(ISUBST)
            ISUBST=ISUBST+1
            NP223255=NP223255+1
         else
            NP223255=NP223255+1
            go to 262
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 261  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          3.  PROCESS FIRST ORDER STATISTICS OPERATOR.
C               ----------------------------------------
 300  CONTINUE
C
C*          3.1 FIND FIRST ACCURANCE OD 224255.
C               -------------------------------
 310  CONTINUE
C
      NP224255=0
      DO 311 I=NP224000,M
      IF(NWTR(I).EQ.224255) THEN
         NP224255=I
         GO TO 320
      END IF
 311  CONTINUE
C
      go to 121
C
C*          3.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 320  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I224=0
         OF224=.TRUE.
         DO 321 I=NP224000,NP224255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF224) THEN
               I224=I
               OF224=.FALSE.
            END IF
            NR224=NR224+1
         END IF
 321     CONTINUE
      ELSE
         I224=MBMP
         NR224=MBMPL-MBMP+1
      END IF
C
C
C*          3.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 350  CONTINUE
C
      ISUBST=MREL-NR224+1
C
C*          3.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 360  CONTINUE
C
      DO 361 I=I224,NR224+I224
      IF(VALUES(I).EQ.0) THEN
c
 362     continue
c
         if(nwtr(NP224255).eq.224255) then
            NWTDW (NP224255)=NWTDW(ISUBST)
            NWTR  (NP224255)=NWTR (ISUBST)
            NWTRV (NP224255)=NWTRV(ISUBST)
            NWTS  (NP224255)=NWTS (ISUBST)
c            CWTEN (NP224255)=CWTEN(ISUBST)
c            CWTU  (NP224255)=CWTU (ISUBST)
            nwten (NP224255)=nwten(ISUBST)
            ISUBST=ISUBST+1
            NP224255=NP224255+1
         else
            NP224255=NP224255+1
            go to 362
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 361  CONTINUE
C
      GO TO 121
C
C
C     -----------------------------------------------------------------
C*          4.  PROCESS DIFFERENCE STATISTICS OPERATOR.
C               ---------------------------------------
 400  CONTINUE
C
C
C*          4.1 FIND FIRST ACCURANCE OD 225255.
C               -------------------------------
 410  CONTINUE
C
      NP225255=0
      DO 411 I=NP225000,M
      IF(NWTR(I).EQ.225255) THEN
         NP225255=I
         GO TO 420
      END IF
 411  CONTINUE
C
      go to 121
C
C*          4.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 420  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I225=0
         OF225=.TRUE.
         DO 421 I=NP225000,NP225255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF225) THEN
               I225=I
               OF225=.FALSE.
            END IF
            NR225=NR225+1
         END IF
 421     CONTINUE
      ELSE
         I225=MBMP
         NR225=MBMPL-MBMP+1
      END IF
C
C
C*          4.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 450  CONTINUE
C
      ISUBST=MREL-NR225+1
C
C*          4.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 460  CONTINUE
C
      DO 461 I=I225,NR225+I225
      IF(VALUES(I).EQ.0) THEN
c
 462     continue
c
         if(nwtr(NP225255).eq.225255) then
            NWTR  (NP225255)=NWTR (ISUBST)
C
C           CHANGE REFERENCE VALUE TO BE CENTRED AROUND ZERO
C           AND INCREASE DATA WIDTH BY 1
C
            NWTRV (NP225255)=-2**NWTDW(ISUBST)
            NWTDW (NP225255)=NWTDW(ISUBST)+1
C
            NWTS  (NP225255)=NWTS (ISUBST)
c           CWTEN (NP225255)=CWTEN(ISUBST)
c           CWTU  (NP225255)=CWTU (ISUBST)
            nwten(NP225255)=nwten(ISUBST)
            ISUBST=ISUBST+1
            NP225255=NP225255+1
         else
            NP225255=NP225255+1
            go to 462
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 461  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          5.  PROCESS REPLACE/RETAINED OPERATOR.
C               ----------------------------------
 500  CONTINUE
C
C
C*          5.1 FIND FIRST ACCURANCE OF 232255.
C               -------------------------------
 510  CONTINUE
C
      NP232255=0
      DO 511 I=NP232000,M
      IF(NWTR(I).EQ.232255) THEN
         NP232255=I
         GO TO 520
      END IF
 511  CONTINUE
C
      go to 121
C
C*          5.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 520  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I232=0
         OF232=.TRUE.
         DO 521 I=NP232000,NP232255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF232) THEN
               I232=I
               OF232=.FALSE.
            END IF
            NR232=NR232+1
         END IF
 521     CONTINUE
      ELSE
         I232=MBMP
         NR232=MBMPL-MBMP+1
      END IF
C
C
C*          5.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 550  CONTINUE
C
      ISUBST=MREL-NR232+1
C
C*          5.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 560  CONTINUE
C
      DO 561 I=I232,NR232+I232
      IF(VALUES(I).EQ.0) THEN
c
 562     continue
c
         if(nwtr(NP232255).eq.232255) then
            NWTDW (NP232255)=NWTDW(ISUBST)
            NWTR  (NP232255)=NWTR (ISUBST)
            NWTRV (NP232255)=NWTRV(ISUBST)
            NWTS  (NP232255)=NWTS (ISUBST)
c            CWTEN (NP232255)=CWTEN(ISUBST)
c            CWTU  (NP232255)=CWTU (ISUBST)
            nwten(NP232255)=nwten(ISUBST)
            ISUBST=ISUBST+1
            NP232255=NP232255+1
         else
            NP232255=NP232255+1
            go to 562
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 561  CONTINUE
C
C     -----------------------------------------------------------------
 121  CONTINUE
C
      RETURN
C
C
      END
      SUBROUTINE BUEPMRKC(KSEC3,KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
C**** *BUEPMRKC*
C
C
C     PURPOSE.
C     --------
C
C          Process marker operator, relacing it with corresponding
C     table B element descriptor.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEPMRKC(KSEC3,KVALS,VALUES,KELEM,CNAMES,
C                           CUNITS,KERR)*
C
C        INPUT :
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data
C                                        compression)
C               *KVALS*   -  dimension of VALUES array
C               *VALUES*  -  aray containing data
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C        OUTPUT:
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpacks bit pattern
C          BUNPKS         - unpacks bit pattern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          M. Dragosavac 1/10/92 :
C
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             NFUCM    -  first uncompressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      DIMENSION    KSEC3(JSEC3)
      DIMENSION    VALUES(KVALS)
C
      DIMENSION IMASK(8)
      DATA IMASK/1,2,4,8,16,32,64,128/
C
C     ------------------------------------------------------------------
C*          1.  FINED OPERATOR 223000 TO 237255.
C               --------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
C
C           1.1 CHECK IF DATA ARE COMRESSED.
C               ----------------------------
 110  CONTINUE
C
      IB=0
      if(iand(KSEC3(4),imask(7)).ne.0) IB=1
C
C*          1.2 FIND POSITION OF OPREATORS.
C               ---------------------------
 120  CONTINUE
C
      DO 121 J=1,M
C
      NR223=0
      NR224=0
      NR225=0
      NR232=0
C
C
      IF(NWTR(J).EQ.222000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 122 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 121
            END IF
 122        CONTINUE
         END IF
      END IF
      IF(NWTR(J).EQ.223000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP223000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 123 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 200
            END IF
 123        CONTINUE
         END IF
         GO TO 200
      END IF
      IF(NWTR(J).EQ.224000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP224000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 124 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 300
            END IF
 124        CONTINUE
         END IF
         GO TO 300
      END IF
      IF(NWTR(J).EQ.225000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP225000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 125 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 400
            END IF
 125        CONTINUE
         END IF
         GO TO 400
      END IF
      IF(NWTR(J).EQ.232000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         NP232000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 126 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               GO TO 500
            END IF
 126        CONTINUE
         END IF
         GO TO 500
      END IF
C
      IF(NWTR(J).EQ.235000) THEN
C
C        RESET POINTER  POINTER TO THE LAST DATA ELEMENT
C
         MREL=0
         MBMP=0
         MBMPL=0
      END IF
C
      IF(NWTR(J).EQ.237255) THEN
C
C        RESET POINTERS TO THE BIT MAP
C
         MBMP=0
         MBMPL=0
      END IF
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          2.  PROCESS SUBSTITUTED VALUES OPERATOR.
C               ------------------------------------
C
 200  CONTINUE
C
C*          2.1 FIND FIRST ACCURANCE OD 223255.
C               -------------------------------
 210  CONTINUE
C
      NP223255=0
      DO 211 I=NP223000,M
      IF(NWTR(I).EQ.223255) THEN
         NP223255=I
         GO TO 220
      END IF
 211  CONTINUE
C
      go to 121
C
C*          2.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 220  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I223=0
         OF223=.TRUE.
         DO 221 I=NP223000,NP223255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF223) THEN
               I223=I
               OF223=.FALSE.
            END IF
            NR223=NR223+1
         END IF
 221     CONTINUE
      ELSE
         I223=MBMP
         NR223=MBMPL-MBMP+1
      END IF
C
C
C*          2.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 250  CONTINUE
C
      ISUBST=MREL-NR223+1
C
C*          2.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 260  CONTINUE
C
      DO 261 I=I223,NR223+I223
      IF(VALUES(I).EQ.0) THEN
c
 262     continue
c
         if(nwtr(NP223255).eq.223255) then
            NWTDW (NP223255)=NWTDW(ISUBST)
            NWTR  (NP223255)=NWTR (ISUBST)
            NWTRV (NP223255)=NWTRV(ISUBST)
            NWTS  (NP223255)=NWTS (ISUBST)
            CWTEN (NP223255)=CWTEN(ISUBST)
            CWTU  (NP223255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP223255=NP223255+1
         else
            NP223255=NP223255+1
            go to 262
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 261  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          3.  PROCESS FIRST ORDER STATISTICS OPERATOR.
C               ----------------------------------------
 300  CONTINUE
C
C*          3.1 FIND FIRST ACCURANCE OD 224255.
C               -------------------------------
 310  CONTINUE
C
      NP224255=0
      DO 311 I=NP224000,M
      IF(NWTR(I).EQ.224255) THEN
         NP224255=I
         GO TO 320
      END IF
 311  CONTINUE
C
      go to 121
C
C*          3.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 320  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I224=0
         OF224=.TRUE.
         DO 321 I=NP224000,NP224255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF224) THEN
               I224=I
               OF224=.FALSE.
            END IF
            NR224=NR224+1
         END IF
 321     CONTINUE
      ELSE
         I224=MBMP
         NR224=MBMPL-MBMP+1
      END IF
C
C
C*          3.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 350  CONTINUE
C
      ISUBST=MREL-NR224+1
C
C*          3.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 360  CONTINUE
C
      DO 361 I=I224,NR224+I224
      IF(VALUES(I).EQ.0) THEN
c
 362     continue
c
         if(nwtr(NP224255).eq.224255) then
            NWTDW (NP224255)=NWTDW(ISUBST)
            NWTR  (NP224255)=NWTR (ISUBST)
            NWTRV (NP224255)=NWTRV(ISUBST)
            NWTS  (NP224255)=NWTS (ISUBST)
            CWTEN (NP224255)=CWTEN(ISUBST)
            CWTU  (NP224255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP224255=NP224255+1
         else
            NP224255=NP224255+1
            go to 362
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 361  CONTINUE
C
      GO TO 121
C
C
C     -----------------------------------------------------------------
C*          4.  PROCESS DIFFERENCE STATISTICS OPERATOR.
C               ---------------------------------------
 400  CONTINUE
C
C
C*          4.1 FIND FIRST ACCURANCE OD 225255.
C               -------------------------------
 410  CONTINUE
C
      NP225255=0
      DO 411 I=NP225000,M
      IF(NWTR(I).EQ.225255) THEN
         NP225255=I
         GO TO 420
      END IF
 411  CONTINUE
C
      go to 121
C
C*          4.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 420  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I225=0
         OF225=.TRUE.
         DO 421 I=NP225000,NP225255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF225) THEN
               I225=I
               OF225=.FALSE.
            END IF
            NR225=NR225+1
         END IF
 421     CONTINUE
      ELSE
         I225=MBMP
         NR225=MBMPL-MBMP+1
      END IF
C
C
C*          4.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 450  CONTINUE
C
      ISUBST=MREL-NR225+1
C
C*          4.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 460  CONTINUE
C
      DO 461 I=I225,NR225+I225
      IF(VALUES(I).EQ.0) THEN
c
 462     continue
c
         if(nwtr(NP225255).eq.225255) then
            NWTR  (NP225255)=NWTR (ISUBST)
C
C           CHANGE REFERENCE VALUE TO BE CENTRED AROUND ZERO
C           AND INCREASE DATA WIDTH BY 1
C
            NWTRV (NP225255)=-2**NWTDW(ISUBST)
            NWTDW (NP225255)=NWTDW(ISUBST)+1
C
            NWTS  (NP225255)=NWTS (ISUBST)
            CWTEN (NP225255)=CWTEN(ISUBST)
            CWTU  (NP225255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP225255=NP225255+1
         else
            NP225255=NP225255+1
            go to 462
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 461  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          5.  PROCESS REPLACE/RETAINED OPERATOR.
C               ----------------------------------
 500  CONTINUE
C
C
C*          5.1 FIND FIRST ACCURANCE OF 232255.
C               -------------------------------
 510  CONTINUE
C
      NP232255=0
      DO 511 I=NP232000,M
      IF(NWTR(I).EQ.232255) THEN
         NP232255=I
         GO TO 520
      END IF
 511  CONTINUE
C
      go to 121
C
C*          5.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 520  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I232=0
         OF232=.TRUE.
         DO 521 I=NP232000,NP232255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF232) THEN
               I232=I
               OF232=.FALSE.
            END IF
            NR232=NR232+1
         END IF
 521     CONTINUE
      ELSE
         I232=MBMP
         NR232=MBMPL-MBMP+1
      END IF
C
C
C*          5.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 550  CONTINUE
C
      ISUBST=MREL-NR232+1
C
C*          5.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 560  CONTINUE
C
      DO 561 I=I232,NR232+I232
      IF(VALUES(I).EQ.0) THEN
c
 562     continue
c
         if(nwtr(NP232255).eq.232255) then
            NWTDW (NP232255)=NWTDW(ISUBST)
            NWTR  (NP232255)=NWTR (ISUBST)
            NWTRV (NP232255)=NWTRV(ISUBST)
            NWTS  (NP232255)=NWTS (ISUBST)
            CWTEN (NP232255)=CWTEN(ISUBST)
            CWTU  (NP232255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP232255=NP232255+1
         else
            NP232255=NP232255+1
            go to 562
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 561  CONTINUE
C
C     -----------------------------------------------------------------
 121  CONTINUE
C
      RETURN
C
C
      END
      SUBROUTINE BUEPWT(KDD,KERR)
C
C**** *BUEPWT*
C
C
C     PURPOSE.
C     --------
C          Updates working tables setting element name,unit,scale,
C     reference value and data width.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEPWT(KDD,KERR)*
C
C        INPUT :
C               *KDD*     -  data descriptor
C        OUTPUT:
C               *KERR*    -  return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      COMMON /BCMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCMATBC/ CATBEN(JTAB),CATBU (JTAB)
C
C             CATBEN      - augmented table B element name
C             CATBU       - augmented table B units
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64,YWTEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24,YWTU*24
C
C     ------------------------------------------------------------------
C*          1.   UPDATE WORKING TABLE.
C                ---------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      ICLASS=KDD/1000
      iyyy  =kdd-iclass*1000+1
      iclass=iclass+1
C
C*          1.1  ASSOCIATED FIELD ?
C                ------------------
 110  CONTINUE
C
      IF(NAFDW.EQ.0) GO TO 140
C
C*          1.2  UNITS ELEMENT DESCRIPTOR ?
C                --------------------------
 120  CONTINUE
C
      i=ntabp(iclass,iyyy)
      if(i.eq.0) then
         KERR=23
         PRINT*,'BUEPWT : ',KDD
         CALL BUERR(KERR)
         DO 1 IQ=1,JELEM
         NSTACK(IQ)=0.
 1       CONTINUE
         RETURN
      end if
C
      IF(CTABBU(I)(1:4).EQ.'CODE') GO TO 140
      IF(CTABBU(I)(1:4).EQ.'FLAG') GO TO 140
      IF(CTABBU(I)(1:3).EQ.'NUM' ) GO TO 140
C
C*          1.3   ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
 130  CONTINUE
C
      NWT=NWT+1
c      CWTEN(NWT)='ASSOCIATED FIELD'
c      CWTU (NWT)=' '
      NWTDW(NWT)=NAFDW
      NWTR (NWT)= 999999
      NWTEN(NWT)= 0
      NWTS (NWT)= 0
      NWTRV(NWT)= 0
C
      M=NWT
C
C     ------------------------------------------------------------------
C*          1.4   SEARCH AUGMENTED TABLE *B ENTRIES .
C                 -----------------------------------
 140  CONTINUE
C
      DO 141 J=1,NJA
C
      IF(NATBTR(J).EQ.KDD) THEN
         II=J
C
C*             MODIFY ENTRY FOR OPERATOR IN FORCE.
C              -----------------------------------
C
C*             ADD ENTRY TO WORKING TABLE.
C              ---------------------------
C
         NWT=NWT+1
         NWTR (NWT) = KDD
         NWTS (NWT) = NATBS (II) + NSCAM
         NWTRV(NWT) = NATBRV(II)
         NWTDW(NWT) = NATBDW(II) + NDWINC
C
C        CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
         IF(N221.NE.0) THEN
            IX=KDD/1000
            IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
            N221=N221-1
         END IF
c
         if(nwtr(nwt).eq.31011.or.nwtr(nwt).eq.31012) then
            if(nwtr(nwt-1).eq.31011.or.nwtr(nwt-1).eq.31012) then
               NWTDW(NWT)=0
            end if
         end if
C
C*            UPDATE M, CNAMES, CUNITS.
C             -------------------------
C
c         CWTEN(NWT) = CATBEN(II)
c         CWTU (NWT) = CATBU (II)
         NWTEN(NWT) = 0
         IF(CATBU(II)(1:3).EQ.'CCI') NWTEN(NWT)=658367
C
         M = NWT
         RETURN
      END IF
C
 141  CONTINUE
C
C
C*          1.5  GET TABLE *B ENTRY .
C                ---------------------
 150  CONTINUE
C
      i=ntabp(iclass,iyyy)
      if(i.eq.0) then
         KERR=23
         PRINT*,'BUEPWT : ',KDD
         CALL BUERR(KERR)
         DO 2 IQ=1,JELEM
         NSTACK(IQ)=0.
 2       CONTINUE
         RETURN
      end if
C
 155  continue
C
C     -----------------------------------------------------------------
C*          1.6   MODIFY ENTRY FOR OPERATOR IN FORCE.
C                 -----------------------------------
 160  CONTINUE
C
C*                ADD ENTRY TO WORKING TABLE.
C                 ---------------------------
      NWT=NWT+1
      NWTR (NWT) = KDD
      NWTS (NWT) = NTABBS (I) + NSCAM
      NWTRV(NWT) = NTABBRV(I)
      NWTDW(NWT) = NTABBDW(I) + NDWINC
C
C     CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
      IF(N221.NE.0) THEN
         IX=KDD/1000
         IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
         N221=N221-1
      END IF
c
      if(nwtr(nwt).eq.31011.or.nwtr(nwt).eq.31012) then
         if(nwtr(nwt-1).eq.31011.or.nwtr(nwt-1).eq.31012) then
            NWTDW(NWT)=0
         end if
      end if
C
 175  CONTINUE
C
C*          1.8 UPDATE M.
C               ---------
 180  CONTINUE
C
c      CWTEN(NWT)=  CTABBEN(I)
c      CWTU (NWT)=  CTABBU(I)
      NWTEN(NWT)= 0
      IF(CTABBU(I)(1:3).EQ.'CCI') NWTEN(NWT)=658367
C
      M=NWT
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
C
      END
      SUBROUTINE BUEPWTC(KDD,KERR)
C
C**** *BUEPWTC*
C
C
C     PURPOSE.
C     --------
C          Updates working tables setting element name,unit,scale,
C     reference value and data width.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEPWTC(KDD,KERR)*
C
C        INPUT :
C               *KDD*     -  data descriptor
C        OUTPUT:
C               *KERR*    -  return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      COMMON /BCMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCMATBC/ CATBEN(JTAB),CATBU (JTAB)
C
C             CATBEN      - augmented table B element name
C             CATBU       - augmented table B units
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64,YWTEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24,YWTU*24
C
C     ------------------------------------------------------------------
C*          1.   UPDATE WORKING TABLE.
C                ---------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      ICLASS=KDD/1000
      iyyy  =kdd-iclass*1000+1
      iclass=iclass+1
C
C*          1.1  ASSOCIATED FIELD ?
C                ------------------
 110  CONTINUE
C
      IF(NAFDW.EQ.0) GO TO 140
C
C*          1.2  UNITS ELEMENT DESCRIPTOR ?
C                --------------------------
 120  CONTINUE
C
      i=ntabp(iclass,iyyy)
      if(i.eq.0) then
         KERR=23
         PRINT*,'BUEPWT : ',KDD
         CALL BUERR(KERR)
         DO 1 IQ=1,JELEM
         NSTACK(IQ)=0.
 1       CONTINUE
         RETURN
      end if
C
      IF(CTABBU(I)(1:4).EQ.'CODE') GO TO 140
      IF(CTABBU(I)(1:4).EQ.'FLAG') GO TO 140
      IF(CTABBU(I)(1:3).EQ.'NUM' ) GO TO 140
C
C*          1.3   ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
 130  CONTINUE
C
      NWT=NWT+1
      CWTEN(NWT)='ASSOCIATED FIELD'
      CWTU (NWT)=' '
      NWTDW(NWT)=NAFDW
      NWTR (NWT)= 999999
      NWTEN(NWT)= 0
      NWTS (NWT)= 0
      NWTRV(NWT)= 0
C
      M=NWT
C
C     ------------------------------------------------------------------
C*          1.4   SEARCH AUGMENTED TABLE *B ENTRIES .
C                 -----------------------------------
 140  CONTINUE
C
      DO 141 J=1,NJA
C
      IF(NATBTR(J).EQ.KDD) THEN
         II=J
C
C*             MODIFY ENTRY FOR OPERATOR IN FORCE.
C              -----------------------------------
C
C*             ADD ENTRY TO WORKING TABLE.
C              ---------------------------
C
         NWT=NWT+1
         NWTR (NWT) = KDD
         NWTS (NWT) = NATBS (II) + NSCAM
         NWTRV(NWT) = NATBRV(II)
         NWTDW(NWT) = NATBDW(II) + NDWINC
C
C        CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
         IF(N221.NE.0) THEN
            IX=KDD/1000
            IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
            N221=N221-1
         END IF
c
         if(nwtr(nwt).eq.31011.or.nwtr(nwt).eq.31012) then
            if(nwtr(nwt-1).eq.31011.or.nwtr(nwt-1).eq.31012) then
               NWTDW(NWT)=0
            end if
         end if
C
C*            UPDATE M, CNAMES, CUNITS.
C             -------------------------
C
         CWTEN(NWT) = CATBEN(II)
         CWTU (NWT) = CATBU (II)
         NWTEN(NWT) = 0
         IF(CATBU(II)(1:3).EQ.'CCI') NWTEN(NWT)=658367
C
         M = NWT
         RETURN
      END IF
C
 141  CONTINUE
C
C
C*          1.5  GET TABLE *B ENTRY .
C                ---------------------
 150  CONTINUE
C
      i=ntabp(iclass,iyyy)
      if(i.eq.0) then
         KERR=23
         PRINT*,'BUEPWT : ',KDD
         CALL BUERR(KERR)
         DO 2 IQ=1,JELEM
         NSTACK(IQ)=0.
 2       CONTINUE
         RETURN
      end if
C
 155  continue
C
C     -----------------------------------------------------------------
C*          1.6   MODIFY ENTRY FOR OPERATOR IN FORCE.
C                 -----------------------------------
 160  CONTINUE
C
C*                ADD ENTRY TO WORKING TABLE.
C                 ---------------------------
      NWT=NWT+1
      NWTR (NWT) = KDD
      NWTS (NWT) = NTABBS (I) + NSCAM
      NWTRV(NWT) = NTABBRV(I)
      NWTDW(NWT) = NTABBDW(I) + NDWINC
C
C     CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
      IF(N221.NE.0) THEN
         IX=KDD/1000
         IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
         N221=N221-1
      END IF
c
      if(nwtr(nwt).eq.31011.or.nwtr(nwt).eq.31012) then
         if(nwtr(nwt-1).eq.31011.or.nwtr(nwt-1).eq.31012) then
            NWTDW(NWT)=0
         end if
      end if
C
 175  CONTINUE
C
C*          1.8 UPDATE M.
C               ---------
 180  CONTINUE
C
      CWTEN(NWT)=  CTABBEN(I)
      CWTU (NWT)=  CTABBU(I)
      NWTEN(NWT)= 0
      IF(CTABBU(I)(1:3).EQ.'CCI') NWTEN(NWT)=658367
C
      M=NWT
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
C
      END
      SUBROUTINE BUERR(KERR)
C**** *BUERR*
C
C
C     PURPOSE.
C     --------
C         PRINT ERROR CODE.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BUERR(KERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/02/92.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      CHARACTER*80 CERROR(100)
C
      DATA CERROR/100*' '/
C
C     ------------------------------------------------------------------
C
C*          1.   INITIALIZE MESSAGES.
C                --------------------
 100  CONTINUE
C
      CERROR( 1)=' Start of BUFR message not found.'
      CERROR( 2)=' End of BUFR message not found.'
      CERROR( 3)=' Array to receive BUFR message too small.'
      CERROR( 4)=' JSEC1 parameter too smal. Local ADP center'//
     1           ' information skipped.'
      CERROR( 5)=' JSEC2 parameter too small. Local ADP centre'//
     2           ' information skipped.'
      CERROR( 6)=' Error during read BUFR table B.'
      CERROR( 7)=' Error during read BUFR table C.'
      CERROR( 8)=' Error during read BUFR table D.'
      CERROR( 9)=' Open error.'
      CERROR(10)=' Error during close BUFR table B.'
      CERROR(11)=' Error during close BUFR table C.'
      CERROR(12)=' Error during close BUFR table D.'
      CERROR(13)=' Number of bits to be extracted greater than'//
     1           ' number of bits per computer word.'
      CERROR(14)=' Argument KVALS too small.'
      CERROR(15)=' Increment value for compressed data too big.'
      CERROR(16)=' JSUBS parameter too small.'
      CERROR(17)=' JWORK parameter too small.'
      CERROR(18)=' Replication factor equal to zero.'
      CERROR(19)=' Delayed replication factor too big.'
      CERROR(20)=' Table D reference not found.'
      CERROR(21)=' Data descriptor operator not found.'
      CERROR(22)=' BUFR Opeartor name not found.'
      CERROR(23)=' Table B reference not found.'
      CERROR(24)=' Augmented table B reference not found.'
      CERROR(25)=' KELEM argument too small.'
      CERROR(26)=' Word pointer out of range.'
      CERROR(27)=' Too many subsets to be packed.'
      CERROR(28)=' Number to be packed too big.'
      CERROR(29)=' Number of descriptors KTDLEN too big.'
      CERROR(30)=' Number of elements greater than JELEM.'
      CERROR(31)=' Too few elements in KDATA array.'
      CERROR(32)=' Number of subsets equal to zero.'
      CERROR(33)=' Negative value to be packed.'
      CERROR(34)=' Number of bits to be packed greater than'//
     1           ' number of bits per computer word.'
      CERROR(35)=' '
      CERROR(36)=' Bad order of data descriptors.'
      CERROR(37)=' Wrong data descriptor.'
      CERROR(38)=' Partial expansion on total message not supported.'
      CERROR(39)=' Can not recognize feedback data in this message.'
      CERROR(40)=' Request flag illegal.'
      CERROR(41)=' Bit map not set.'
      CERROR(42)=' This element must be data present indicator.'
      CERROR(43)=' Table B element must follow bit map.'
      CERROR(44)=' Requested subset does not exist.'
      CERROR(45)=' There is no one requested element in the data.'
      CERROR(46)=' Input array is too small to receive information.'
      CERROR(47)=' KELEM argument is too big. Maximum allowed is 40000'
      CERROR(48)=' '
      CERROR(49)=' Too many data for boxing '
C
      IF(KERR.GE.0.AND.KERR.LE.100) THEN
        WRITE(*,'(1h ,A)') CERROR(KERR)
      ELSE
        WRITE(*,'(1h ,A)') ' Error number out of range !'
      END IF
C
      RETURN
      END
      SUBROUTINE BUETAB( KSEC1,KERR )
C
C**** *BUETAB*
C
C
C     PURPOSE.
C     --------
C          Load Bufr table B, D and C according to Edition and version
C     of Bufr code.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUETAB(KSEC1,KERR)*
C
C        OUTPUT:
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
C      COMMON /BCMCT/ NREF(JCTAB)   ,NSTART(JCTAB) ,NLEN(JCTAB),
C     1               NCODNUM(JCTST),NSTARTC(JCTST),
C     2               NLENC(JCTST)
C
C             NREF      - table C reference
C             NSTART    - starting pointers to array NCODNUM
C             NLEN      - lengths
C             NCODNUM   - code/flag table number
C             NSTARTC   - starting pointers to array CTEXT
C             NLENC     - lengths
C
C
C      COMMON /BCMCTC/ CTEXT(JCTEXT)
C
C             CTEXT     - text in code/flag tables
C
C
      COMMON /BCMROOT/ CROOT
C
C            CROOT    -  path for Bufr tables
C
C
      CHARACTER*256 YTAB ,YTAC ,YTAD
      CHARACTER*11  YTABB,YTABC,YTABD
      CHARACTER    CTABBEN*64,CTABBU*24,CTEXT*64
      CHARACTER*256 CROOT
C
      DIMENSION KSEC1(JSEC1)
C
      SAVE OFIRST
C
C     ------------------------------------------------------------------
C
C*          1.   GET BUFR TABLES/LOCAL BUFR TABLES.
C                ----------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0) RETURN
C
C*          2.   SET UP BUFR TABLE FILE NAME.
C                ----------------------------
 200  CONTINUE
C
C
C             BUFR EDITION 2 NAMING CONVENTION
C
C             BXXXXXYYZZ , CXXXXXYYZZ , DXXXXXYYZZ
C
C             B      - BUFR TABLE 'B'
C             C      - BUFR TABLE 'C'
C             D      - BUFR TABLE 'D'
C             XXXXX  - ORIGINATING CENTRE
C             YY     - VERSION NUMBER OF MASTER
C                      TABLE USED( CURRENTLY 2 )
C             ZZ     - VERSION NUMBER OF LOCAL TABLE USED
C
C
         IXX=KSEC1(3)
         IYY=KSEC1(15)
         IZZ=KSEC1(08)
         IF(KSEC1(2).GE.3) THEN
            IWW=KSEC1(16)
         ELSE
            IWW=0
         END IF
C
C        IF STANDARD TABLES USED, USE ECMWF ORIGINATING CENTRE ID
C
         IF(KSEC1(8).EQ.0) IXX=98
C
         IF(OFIRST) THEN
            IF(IWW.EQ.NWWP.AND.IXX.EQ.NXXP.AND.IYY.EQ.NYYP.AND.
     1         IZZ.EQ.NZZP) RETURN
         END IF
C
         OFIRST=.TRUE.
C
         NWWP=IWW
         NXXP=IXX
         NYYP=IYY
         NZZP=IZZ
C
         if(ksec1(2).ge.3) then
            WRITE(YTABB,'(A1,I3.3,I3.3,I2.2,I2.2)') 'B',IWW,IXX,IYY,IZZ
C            WRITE(YTABC,'(A1,I3.3,I3.3,I2.2,I2.2)') 'C',IWW,IXX,IYY,IZZ
            WRITE(YTABD,'(A1,I3.3,I3.3,I2.2,I2.2)') 'D',IWW,IXX,IYY,IZZ
         else
            WRITE(YTABB,'(A1,I5.5,I2.2,I2.2)') 'B',IXX,IYY,IZZ
C            WRITE(YTABC,'(A1,I5.5,I2.2,I2.2)') 'C',IXX,IYY,IZZ
            WRITE(YTABD,'(A1,I5.5,I2.2,I2.2)') 'D',IXX,IYY,IZZ
         end if
C
C         PRINT*,'BUFR Tables to be loaded ',YTABB,',',YTABC,',',YTABD
ccc          PRINT*,'BUFR Tables to be loaded ',YTABB,',',YTABD
C
C
C     ----------------------------------------------------------------
C*          3. OPEN AND READ FILES CONTAINING BUFR TABLES.
C              -------------------------------------------
 300  CONTINUE
C
      I=INDEX(CROOT,' ')
      IF(I.NE.0) I=I-1
C
C*          3.1 READ BUFR TABLE B.
C               ------------------
 310  CONTINUE
C
      YTAB=CROOT(1:I)//YTABB
      II=I+11
C
      OPEN(UNIT=38,IOSTAT=IOS,ERR=311,FILE=YTAB(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
      GO TO 312
C
 311  CONTINUE
C
      CLOSE(38)
      print*,'Open error on ',YTAB(1:II)
      print*,'Try on /home/ma/emos/data/mrfs/tables/bufr/ directory'
C
      YTAB=' '
      YTAB='/home/ma/emos/data/mrfs/tables/bufr/'//YTABB
C
      II=INDEX(YTAB,' ')
      IF(II.NE.0) II=II-1
C
      OPEN(UNIT=38,IOSTAT=IOS,ERR=410,FILE=YTAB(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
 312  CONTINUE
C
      READ(38,ERR=400,IOSTAT=IOS) NTABBTR,CTABBEN,CTABBU,NTABBS,
     1                            NTABBRV,NTABBDW,NTABP
C
      CLOSE(UNIT=38,IOSTAT=IOS,ERR=420)
C
C*          3.2 READ BUFR TABLE C.
C               ------------------
 320  CONTINUE
C
C      YTAC=CROOT(1:I)//YTABC
C
C      OPEN(UNIT=39,IOSTAT=IOS,ERR=510,FILE=YTAC(1:II),
C     1     FORM='UNFORMATTED',
C     2     ACCESS='SEQUENTIAL',
C     5     STATUS='OLD')
C
C      READ(39,ERR=500,IOSTAT=IOS)NREF   ,NSTART,NLEN,NCODNUM,
C     1                           NSTARTC,NLENC ,CTEXT
C
C      CLOSE(UNIT=39,IOSTAT=IOS,ERR=520)
C
C
C*          3.3 READ BUFR TABLE D.
C               ------------------
 330  CONTINUE
C
      YTAD=CROOT(1:I)//YTABD
C
      OPEN(UNIT=40,IOSTAT=IOS,ERR=331,FILE=YTAD(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
      GO TO 332
C
 331  CONTINUE
C
      CLOSE(40)
      print*,'Open error on ',YTAD(1:II)
      print*,'Try on /home/ma/emos/data/mrfs/tables/bufr/ directory'
C
      YTAD='/home/ma/emos/data/mrfs/tables/bufr/'//YTABD
C
      OPEN(UNIT=40,IOSTAT=IOS,ERR=610,FILE=YTAD(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
 332  CONTINUE
C
      READ(40,ERR=600,IOSTAT=IOS) NTABDTR,NTABDL,NTABDST,NTABDSQ
C
      CLOSE(UNIT=40,IOSTAT=IOS,ERR=620)
C
C
      RETURN
C     ----------------------------------------------------------------
 400  CONTINUE
C
      KERR=6
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
 410  CONTINUE
C
      KERR=9
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
 420  CONTINUE
C
      KERR=10
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C     ----------------------------------------------------------------
 500  CONTINUE
C
      KERR=7
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
 510  CONTINUE
C
      KERR=9
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
 520  CONTINUE
C
      KERR=11
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C     -----------------------------------------------------------------
 600  CONTINUE
C
      KERR=8
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
 610  CONTINUE
C
      KERR=9
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
 620  CONTINUE
C
      KERR=12
      PRINT*,'BUETAB: IOS ',IOS
      CALL BUERR(KERR)
      RETURN
C
      END
      SUBROUTINE BUETD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1                 KVALS,VALUES,KELEM,KERR)
C
C**** *BUETD*
C
C
C     PURPOSE.
C     --------
C
C          Expand section 3 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUETD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
C                        KVALS,VALUES,KELEM,KERR)
C
C        INPUT :
C               *KPT*     -  pointer to kdata array
C               *KTDLEN*  -  number of data descriptors in section 3
C               *KTDLST*  -  array containing data descriptors in section 3
C               *KDLEN*   -  dimension of KDATA array
C               *KDATA*   -  array containing data needed for data descriptor
C                            expansion
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C        OUTPUT :
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C           Data descriptor taken from KTDLST array are fully
C        expanded using data from KDATA array if needed.
C        ( delayed replication factors etc.)
C
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - set word and bit pointers at the begining of
C                          next section
C          BUNPCK        - unpacks bit pathern
C          BUREP         - solves replication problem
C          BUETDR        - solves table D reference
C          BUOPER        - process operator
C          BUEPWT        - updates working table
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMEL/ NTDLEN,NTDLST(JELEM),NTDEXL,NTDEXP(JELEM)
C
C             NTDLEN - number of Data descriptors in section 3
C             NTDLST - list of Data descriptors
C             NTDEXL - number of expanded Data Descriptors
C             NTDEXP - list of expanded Data descriptors
C
C
      CHARACTER CWTEN*64,CWTU*24
C
      DIMENSION VALUES(KVALS)
C
      DIMENSION ISTACK(JELEM),IISTACK(JELEM)
      DIMENSION IMASK(8)
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KDATA(KDLEN),KTDLST(KTDLEN)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
C     ------------------------------------------------------------------
C
C*          1.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      N = KSEC3(3)
C
C
C*          2.   EXPAND DATA DESCRIPTORS.
C                ------------------------
 200  CONTINUE
C
C
C*          2.1  SET EXPECTED NUMBER OF DATA DESCRIPTORS.
C                ----------------------------------------
C                AND INITIALIZE NUMBER OF DATA VALUES PER SUB-SET.
C                -------------------------------------------------
 210  CONTINUE
C
      J      = 0
      KPT    = 0
      NWT    = 0
      JMAX   = KTDLEN
      JMAXNEW=JMAX
C
      IF(KTDLEN.GT.JELEM) THEN
         PRINT*,' BUETD :'
         KERR=29
         CALL BUERR(KERR)
         RETURN
      END IF
C
C*          2.2  PUT DATA DESCRIPTORS IN STACK.
C                ------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,JMAX
C
      ISTACK(JJ)=KTDLST(JJ)
      IISTACK(JJ)=ISTACK(JJ)
C
 221  CONTINUE
C
C*          2.2.1 CHECK IF IT IS SAME DATA DESCRIPTOR DESCRIOPTION.
C                 -------------------------------------------------
C                 TO MAKE MORE EFFICIENT DATA DESCRIPTOR DESCRIPTION
C                 EXPANSION, IN CASE THAT DELAYED REPLICATION FACTOR
C                 IS NOT PRESENT AND DATA DESCRIPTORS ARE THE SAME,
C                 PREVIOUS WORKING TABLE SHOULD BE USED. IT IS POSIBLE
C                 AT THIS PLACE IN THE FUTURE TO MAKE MORE SOPHISTICATED
C                 CONTROL.
C
C
      DO 222 JC=1,JMAX
C
      IF(ISTACK(JC).NE.NSTACK(JC)) THEN
C
C
         ODREPF=.FALSE.
C
C        SWAP CONTENT OF THE STACKS.
C
         DO 223 JJC=1,JMAX
         NSTACK(JJC)=ISTACK(JJC)
 223     CONTINUE
C
         NTDLEN = JMAX
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         MBMP=0
         MBMPL=0
C
         GO TO 230
C
      END IF
C
 222  CONTINUE
C
C
C*    IF MARKER OPERATOR PRESENT EXPAND DESCRIPTORS AGAIN
C
      IF(OMARKER) THEN
         NTDLEN = JMAX
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    RETURN IF DELAYED REPLICATION FACTOR IS NOT PRESENT.
C
      IF(JMAX.NE.NTDLEN) THEN
         M=0
         NOLD=N
         NFCM=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
      OB=.FALSE.
      if(iand(KSEC3(4),imask(7)).ne.0) OB=.TRUE.
      IF(.NOT.ODREPF) THEN
         IF(N.GT.NOLD) NOLD=N
         IF(OB.AND.NFCM.EQ.1)      GO TO 300
         IF(.NOT.OB.AND.NFCM.EQ.0) GO TO 300
      END IF
C
      M=0
      NOLD=N
      NFCM=0
      MREL=0
      OMARKER=.FALSE.
      NTDLEN=JMAX
      MBMP=0
      MBMPL=0
C
C     ------------------------------------------------------------------
C*          2.3  GET NEXT DESCRIPTOR FROM THE STACK.
C                -----------------------------------
 230  CONTINUE
C
      J   = J + 1
      IF(J.GT.JMAX) GO TO 270
C
      IDD = ISTACK(J)
      IF(IDD.EQ.0)  GO TO 230
C
      IF = IDD/100000
C
C     ------------------------------------------------------------------
C*          2.4  CHECK IF IT IS REPLICATION DESCRIPTOR.
C                --------------------------------------
 240  CONTINUE
C
      if(if.eq.0) then
C
C*          2.6  ELEMENT DESCRIPTOR, SO UPDATE WORKING TABLE.
C                --------------------------------------------
 260     CONTINUE
C
         if(idd.eq.31031.or.idd.eq.31192) then
            nwt=nwt+1
            nwtr(nwt)=idd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=1
            m=m+1
         elseif(idd.eq.33007.or.idd.eq.63192) then
            nwt=nwt+1
            nwtr(nwt)=idd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=7
            m=m+1
         else
            CALL BUEPWT(IDD,KERR)
            IF(KERR.GT.0) RETURN
         end if
c
      elseif(if.eq.1) then
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
         CALL BUREP(KPT,KDLEN,KDATA,J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
         GO TO 230
c
      elseif(if.eq.2) then
C
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
         CALL BUOPER(KPT,KDLEN,KDATA,J,IDD,ISTACK,KERR)
         IF(KERR.GT.0) RETURN
c
      elseif(if.eq.3) then
c
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
         CALL BUETDR(J,JMAX,IDD,ISTACK,KERR)
         IF(KERR.GT.0) THEN
            DO 252 IQ=1,JELEM
            NSTACK(IQ)=0.
 252        CONTINUE
            RETURN
         END IF
      else
         kerr=37
         call buerr(kerr)
         return
      end if
c
      go to 230
c
C     ------------------------------------------------------------------
C*          2.7 RESOLVE MARKER OPERATOR.
C               ------------------------
 270  CONTINUE
C
      if(omarker) then
         CALL BUEPMRK(KSEC3,KVALS,VALUES,KELEM,KERR)
         IF(KERR.GT.0) RETURN
      end if
C
C     ------------------------------------------------------------------
C
C*          3. COLLECT  SUPPLEMENTARY ITEMS.
C              -----------------------------
 300  CONTINUE
C
      nfcm=0
      if(ob) nfcm=1
c
      RETURN
      END
      SUBROUTINE BUETDR(KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *BUETDR*
C
C
C     PURPOSE.
C     --------
C          Solve Bufr table D reference.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUETDR(KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C                 *KDD*      - data descriptor
C        OUTPUT:
C                 *KJ*       - pointer to kstack array
C                 *KJ1*      - pointer to last element in kstack
C                 *KSTACK*   - list of data descriptors
C                 *KERR*     - return error code
C
C
C     METHOD.
C     -------
C
C           NONE.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      CHARACTER CTABBEN*64,CTABBU*24
C
      DIMENSION ILIST(JELEM),KSTACK(*)
C
C     ------------------------------------------------------------------
C
C*          1.   OBTAIN LIST OF DESCRIPTORS FROM BUFR TABLE D.
C                ---------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 110 J=1,JTAB
C
      IF(KDD.EQ.NTABDTR(J)) THEN
         I=J
         GO TO 120
      END IF
C
 110  CONTINUE
C
      KERR=20
      PRINT*,'BUETDR : ',KDD
      CALL BUERR(KERR)
      RETURN
C
 120  CONTINUE
C
      J1=NTABDST(I)
      J2=NTABDL (I)
      J3=0
C
      DO 121 J=J1,J1+J2-1
C
      J3 = J3 +1
      ILIST(J3) = NTABDSQ(J)
C
 121  CONTINUE
C
C     ------------------------------------------------------------------
C*          2.  PUSH DOWN DATA DESCRIPTION DESCRIPTORS
C               --------------------------------------
C               TO MAKE ROOM FOR LIST.
C               ----------------------
 200  CONTINUE
C
      J2M1=J2-1
C
      DO 210 J=KJ1,KJ+1,-1
C
      KSTACK(J+J2M1) = KSTACK(J)
C
 210  CONTINUE
C
C     ------------------------------------------------------------------
C*          3.  INSERT LIST IN PLACE OF SEQUENCE DESCRIPTORS.
C               ---------------------------------------------
 300  CONTINUE
C
      KJM1=KJ-1
C
      DO 310 J=1,J3
C
      KSTACK(KJM1+J)= ILIST(J)
C
 310  CONTINUE
C
C     ------------------------------------------------------------------
C*          4.  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C               ----------------------------------------
 400  CONTINUE
C
      KJ  = KJ  - 1
      KJ1 = KJ1 +J3 -1
C     ------------------------------------------------------------------
C*          4.1  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 610  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C     -----------------------------------------------------------------
 500  CONTINUE
C
      RETURN
C
      END
      SUBROUTINE BUEVAR(KERR)
C
C**** *BUEVAR*
C
C
C     PURPOSE.
C     --------
C         INITIALIZE CONSTANTS AND VARIABLES.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BUEVAR(KERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/03/92.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMROOT/ CROOT
C
C            CROOT    -  path for Bufr tables
C
      COMMON /BCPRQ/ NPMISS,NPRUS,NOKEY
C
      CHARACTER*256 CROOT
C
      EXTERNAL GETENV
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES AND CONSTANTS.
C                -----------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      NJA= 0
      M  =0
      MM =0
      N  =0
      JCV=0
      NBPW=JBPW
      NWPT=0
      NBPT=0
      NWPTB=0
      NBPTB=0
      NVIND=2147483647
      RVIND=1.7E38
      EPS=10.E-10
      NBENP=0
      NLTVNP=0
      NWWP=0
      NXXP=0
      NYYP=0
      NZZP=0
      NDWINC=0
      NSCAM=0
      NAFDW=0
      NWT=0
      ODREPF=.FALSE.
      N221=0
      MREL=0
      NFCM=0
      EPS=10.E-11
      MBMP=0
      MBMPL=0
      OMARKER=.FALSE.
      CROOT=' '
C
C
C     vax bufr tables path
C
C     CROOT='BUFR_TABLES:'
C
C     unicos bufr tables path
C
C     CROOT=' '
C     CALL GETENV('BUFR_TABLES',CROOT)
C     ILNG=INDEX(CROOT,' ')
C     IF(ILNG.EQ.1) CROOT='/tmp/emos_sms/tables/temp/'
C
C     SGi/HP/Sun  bufr tables path
C
      CROOT=' '
      CALL GETENV('BUFR_TABLES',CROOT)
      ILNG=INDEX(CROOT,' ')
      IF(ILNG.EQ.1) CROOT='/home/ma/emos/tables/bufr/'
C
C     VPP700 bufr tables path
C
C     CROOT=' '
C     CALL GETENV('BUFR_TABLES',CROOT)
C     ILNG=INDEX(CROOT,' ')
C     IF(ILNG.EQ.1) CROOT='/vpp700/mrfs/tables/bufr/'
C
C     VPP300 bufr tables path
C
C     CROOT=' '
C     CALL GETENV('BUFR_TABLES',CROOT)
C     ILNG=INDEX(CROOT,' ')
C     IF(ILNG.EQ.1) CROOT='/vpp300/mrfs/tables/bufr/'
C
C
      WRITE(*,'(1H ,A)') '                  ECMWF '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A)') '     BUFR DECODING SOFTWARE VERSION -  3.6 '
      WRITE(*,'(1H ,A)') '            14 APR 1998. '
      WRITE(*,'(1H )')
      WRITE(*,'(1H )')
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A)') 'Your path for Bufr tables is :'
      WRITE(*,'(1H ,A)')  CROOT(1:63)
C
      DO 101 I=1,JBPW-1
      NMASK(I)=2**I-1
 101  CONTINUE
C
C     for vax machine
C
C     DO 101 I=1,JBPW-2
C     for vax machine
C     NMASK(I)=2**I-1
C101  CONTINUE
C     NMASK(31)=2147483647
C
      IF(NPMISS.LT.0.AND.NPMISS.GT.1) NPMISS=0
      IF(NPRUS.LT.0.AND.NPRUS.GT.1) NPRUS=0
      IF(NOKEY.LT.0.AND.NOKEY.GT.1) NOKEY=0
C
      RETURN
      END
      SUBROUTINE BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR)
C
C**** *BUEXS0*
C
C
C     PURPOSE.
C     --------
C          Expands section 0 of Bufr message. Saves Bufr Edition number
C     and total length of Bufr message (bytes).
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C               *KERR*    -  returned error code
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPKS          - unpack bit pattern in repeated way
C          BUNPCK          - unpack bit pattern
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL),KSUP(*),KSEC0(*)
      DIMENSION IBUFR(4)
C
      CHARACTER*4 YBUFR
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 0.
C                -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C
C*          1.1  INITIALIZE WORKING POINTERS NWPT AND NBPT.
C                ------------------------------------------
      IWPT = 0
      IBPT = 0
      NWPT = 1
      NBPT = 0
C
C*          1.2  UNPACK FIRST FOUR OCTETS CONTAINING *BUFR*.
C                -------------------------------------------
C
      CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking first four octets of bufr message.'
         RETURN
      END IF
C
C*          1.3  CHECK IF FIRST FOUR OCTETS ARE 'BUFR'.
C                --------------------------------------
      YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1      CHAR(IBUFR(3))//CHAR(IBUFR(4))
      IF(YBUFR.NE.'BUFR') THEN
         KERR = 1
         CALL BUERR(KERR)
         RETURN
      END IF
C
C*          1.4 UNPACK BUFR EDITION NUMBER (IT IS 8TH BYTE ).
C               ---------------------------------------------
 140  CONTINUE
C
      IWPT=56/NBPW+1
      IBPT=56-(IWPT-1)*NBPW
C
      CALL BUNPCK(NBPW,KBUFF,KSEC0(3),IWPT,IBPT,8,KERR)
      IF(KERR.GT.0) THEN
      	     PRINT*,'Error unpacking KSEC0(3).'
         RETURN
      END IF
C
      IF(KSEC0(3).LE.1) GO TO 170
C
C*          1.5 UNPACK TOTAL LENGTH OF BUFR MESSAGE.
C               ------------------------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC0(2),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC0(2).'
         RETURN
      END IF
C
C*          1.6 UNPACK BUFR EDITION NUMBER.
C               ---------------------------
 160  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC0(3),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC0(3).'
         RETURN
      END IF
C
C
C*          1.7 SET LENGTH OF SECTION 0.
C               ------------------------
 170  CONTINUE
C
      KSEC0(1)= 4
      IF(KSEC0(3).GT.1) KSEC0(1)= 8
C
C*          1.8 SET SUPPLEMENTARY INFORMATION.
C               ------------------------------
 180  CONTINUE
C
      KSUP (9)= 3
C
      RETURN
C     -----------------------------------------------------------------
C
      END
      SUBROUTINE BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR)
C
C**** *BUEXS1*
C
C
C     PURPOSE.
C     --------
C          Expands section 1 of Bufr message. Saves expanded information
C     in the array KSEC1.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(PACKED FORM)
C
C                            For Bufr Edition 3 onward
C
C                            KSEC1(16)-- Originating Sub-centre
C                            KSEC1(17)-- Not used
C                            KSEC1(18) to ksec1(JSEC1) - local ADP centre
C                                        information(PACKED FORM)
C
C
C     *METHOD.
C      -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          -  unpack bit pattern
C          BUNEXS         -  set word and bit pointers at the begining of
C                            next section
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       16/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL),KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION ISEC1(JSEC1)
c
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 1.
C               ------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C     SAVE WORD AND BIT POINTERS OF THR BRGINING OF SECTION 1.
C
      IWPTB=NWPT
      IBPTB=NBPT
C
C*          1.1 UNPACK LENGTH OF SECTION 1.
C               ----------------------------
 110  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(1).'
         RETURN
      END IF
      if(ksec1(1).gt.18) then
         print*,'Local ADP Centre information present in the Section 1.'
      end if
C
C*          1.1.1 SET THE POINTERS NWPTB AND NBPTB.
C                 ---------------------------------
C                 TO BEGINING OF THE NEXT SECTION.
C                 --------------------------------
 1110 CONTINUE
C
      CALL BUNEXS(KSEC1(1))
C
C
C*          1.2  UNPACK BUFR EDITION NUMBER/MASTER TABLE USED.
C                ---------------------------------------------
 120  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(2),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(2).'
            RETURN
         END IF
      ELSE
         CALL BUNPCK(NBPW,KBUFF,KSEC1(14),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(14).'
            RETURN
         END IF
C
         KSEC1(2)=KSEC0(3)
      END IF
C
C*          1.3  UNPACK ORIGINATING CENTRE.
C                --------------------------
 130  CONTINUE
C
      if(ksec0(3).lt.3) then
         CALL BUNPCK(NBPW,KBUFF,KSEC1(3),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(3).'
            RETURN
         END IF
      else
         CALL BUNPCK(NBPW,KBUFF,KSEC1(16),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(3).'
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,KSEC1(3),NWPT,NBPT,8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(3).'
            RETURN
         END IF
      end if
C
C*          1.4  UNPACK UPDATE SEQUENCE NUMBER.
C                ------------------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(4),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(4).'
         RETURN
      END IF
C
C*          1.5  UNPACK INTEGER VALUE OF THE OCTET CONTAINING
C                --------------------------------------------
C                 FLAG BITS(ZERO IF SECTION TWO IS NOT PRESENT).
C                 ----------------------------------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(5),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(5).'
         RETURN
      END IF
C
C*          1.6  UNPACK *BUFR* MESSAGE TYPE.
C                ---------------------------
 160  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(6),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(6).'
         RETURN
      END IF
C
C*          1.7  UNPACK *BUFR* MESSAGE SUB-TYPE.
C                -------------------------------
 170  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(7),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(7).'
         RETURN
      END IF
C
C*          1.8  UNPACK LOCAL TABLE VERSION NUMBER OR
C                ------------------------------------
C                VERSION NUMBER OF MASTER TABLE USED.
C                 -----------------------------------
 180  CONTINUE
C
      IF(KSEC0(3).LE.1) THEN
         CALL BUNPCK(NBPW,KBUFF,KSEC1(8),NWPT,NBPT,16,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(8).'
            RETURN
         END IF
      ELSE
         CALL BUNPCK(NBPW,KBUFF,KSEC1(15),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(15).'
            RETURN
         END IF
         CALL BUNPCK(NBPW,KBUFF,KSEC1( 8),NWPT,NBPT, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking KSEC1(8).'
            RETURN
         END IF
      END IF
C
C*          1.9  UNPACK YEAR.
C                ------------
 190  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(9),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking ksec1(9).'
         RETURN
      END IF
C
C*          2. UNPACK MONTH.
C              -------------
 200  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(10),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking ksec1(10).'
         RETURN
      END IF
C
C*          2.1 UNPACK DAY.
C               -----------
 210  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(11),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(11).'
         RETURN
      END IF
C
C*          2.2 UNPACK HOUR.
C               ------------
 220  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(12),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(12).'
         RETURN
      END IF
C
C*          2.3 UNPACK MINUTE.
C               --------------
 230  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC1(13),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC1(13).'
         RETURN
      END IF
C
C*          2.4 UNPACK LOCAL ADP CENTRE INFORMATION IF ANY.
C               -------------------------------------------
 240  CONTINUE
C
      if(ksec0(3).lt.3) then
         IOFF=KSEC1(1)-17
         iw=16
         ibt=0
      else
         IOFF=KSEC1(1)-17
         iw=18
         ibt=0
      end if
      IF(IOFF.LE.JSEC1) THEN
         CALL BUNPKS(NBPW,KBUFF,ISEC1(1),NWPT,NBPT,
     1   8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error unpacking local ADP centre information'
            PRINT*,'in section 1.'
            RETURN
         END IF
C
C                PACK LOCAL ADP CENTRE INFORMATION INTO KSE1(16) ONWARD.
C
         CALL BUPKS(NBPW,KSEC1(IW),ISEC1,IW,IBT,8,0,IOFF,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing local adp centre information'
            RETURN
         END IF
      ELSE
         IOFF=0
         KERR=4
         CALL BUERR(KERR)
         GO TO 300
      END IF
C
C
C*          3.  SET SUPPLEMENTARY INFORMATION.
C               ------------------------------
 300  CONTINUE
C
      KSUP(1)=15+IOFF/4+1
C
C     ------------------------------------------------------------------
C
      RETURN
      END
      SUBROUTINE BUEXS2( KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR )
C
C**** *BUEXS2*
C
C
C     PURPOSE.
C     --------
C          Expands section 2 of Bufr message. Expanded data are
C     stored in the array KSEC2.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS2(KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(PACKED FORM)
C               *KSEC2*   -  array containing section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(JSEC2) local ADP centre
C                                         information(PACKED FORM)
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          The length of section 2 and pointers to the begining
C     of section 3 are set. RDB key is then unpacked and all information
C     stored into array KSEC2.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpack bit pattern
C          BUNEXS         - set word and bit pointers at the begining
C                           of next Bufr section.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
      COMMON /BCPRQ/ NPMISS,NPRUS,NOKEY
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(JSUP),KSEC1(JSEC1),KSEC2(JSEC2)
      DIMENSION ISEC2(JSEC2),iisec2(jsec2)
C
C     ------------------------------------------------------------------
C*          1.  EXPAND SECTION 2.
C               -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 101 I=1,JSEC2
      KSEC2(I)=0
 101  CONTINUE
C
      IF( KSEC1(5).EQ.0) THEN
          KSUP(2) = 1
          KSEC2(1)= 0
          RETURN
      END IF
C
C*          1.1 SET THE POINTERS NWPT AND NBPT AT THE BEGINING OF SECTION.
C              -----------------------------------------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C*          1.2  UNPACK LENGTH OF SECTION 2.
C                ---------------------------
      CALL BUNPCK(NBPW,KBUFF,KSEC2(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC2(1).'
         RETURN
      END IF
C
C*          1.2.1  SET POINTERS NWPTB AND NBPTB TO THE
C                  -----------------------------------
C                  BEGINING OF THE NEXT SECTION.
C                  -----------------------------
      CALL BUNEXS(KSEC2(1))
C
      CALL BUNPCK(NBPW,KBUFF,IDUMMY,NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking dummy octet in section 2.'
         RETURN
      END IF
C
C*          1.3  UNPACK LOCAL ADP CENTRE INFORMATION.
C                ------------------------------------
C
      IOFF=KSEC2(1)-4
C
      IF(IOFF.GT.JSEC2) THEN
         KERR=5
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL BUNPKS(NBPW,KBUFF,ISEC2(1),NWPT,NBPT,
     1            8,0,IOFF,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking local ADP centre information'
         PRINT*,'in section2.'
         RETURN
      END IF
c
      if(nokey.eq.0) then
         if(isec2(2).ne.ksec1(7)) then
c
c           bytes in the key in reversed order.
c
            j=0
            do 131 i=1,jsec2,4
            iisec2(i)=isec2(i+3)
            iisec2(i+1)=isec2(i+2)
            iisec2(i+2)=isec2(i+1)
            iisec2(i+3)=isec2(i)
 131        continue
            do 132 i=1,jsec2
            isec2(i)=iisec2(i)
 132        continue
         end if
      end if
C
C                PACK LOCAL ADP CENTRE INFORMATION INTO KSE2(2) ONWARD.
C
      IW=2
      IBT=0
      CALL BUPKS(NBPW,KSEC2(IW),ISEC2,IW,IBT,8,0,IOFF,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error packing local ADP centre information into'
         PRINT*,'ksec2(2) onward.'
         RETURN
      END IF
C
C                SET LENGHT OF KSEC2
C
      KSUP(2)=IW
C     ------------------------------------------------------------------
 200  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUEXS3(KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)
C
C**** *BUEXS3*
C
C
C     PURPOSE.
C     --------
C
C          Expand section 3 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS3( KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *KERR*    -  returned error code
C
C     METHOD.
C      -------
C
C          Expands list of data descriptors packed in section 3
C     of Bufr message. Working tables for further data decoding are set,
C     list of packed Bufr data descriptors and list of Bufr data descriptors
C     expanded according to table D reference are returned respectively.
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - set word and bit pointers at the begining of
C                          next section
C          BUNPCK        - unpacks bit pattern
C          BUSRP         - solves replication problem
C          BUSTDR        - solves table D reference
C          BUPRCO        - process operator
C          BUUPWT        - updates working table
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
      COMMON /BCOMP/ INWTEN(JELEM),INWTR (JELEM),INWTS (JELEM),
     1                INWTRV (JELEM),INWTDW(JELEM),
     2                INWORDP(JWORK),INBITP(JWORK)
C             INWTEN   -  woking table
C             INWTR     -  working table reference
C             INWTS     -  working scale
C             INWTRV    -  working reference value
C             INWTDW    -  working data width
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      COMMON /BCOMREQ/ NREQ(2),NRQL,NRQ(JELEM),RQVAL(JELEM)
C
C             *NREQ*    -  flag
C                          bit number     meaning
C
C                              1        - 0 no bit map delivered to user
C                                         1    bit map delivered to user
C                              2        - 0 no partial expansion
C                                         1    partial expansion
C                              3        - 0 no Q/C required
C                                       - 1    Q/C required
C                              4        - 0 no statistics required
C                                       - 1    statistics
C                              5        - 0 no diffrence statistics
C                                       - 1    difference statistics
C                              6        - 0 no substituted values
C                                       - 1    substituted values
C             *NRQL*    -  number of requested elements
C             *NRQ*     -  list of requested table B reference
C             *RQVAL*   -  list of values signifying requested element
C                          (say pressure  at 50000 Pa)
C
C
      COMMON /BCOMEL/ NTDLEN,NTDLST(JELEM),NTDEXL,NTDEXP(JELEM)
C
C             NTDLEN - number of Data descriptors in section 3
C             NTDLST - list of Data descriptors
C             NTDEXL - number of expanded Data Descriptors
C             NTDEXP - list of expanded Data descriptors
C
      COMMON /BCPRQ/ NPMISS,NPRUS,NOKEY
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION ISTACK(JELEM),IISTACK(JELEM)
      DIMENSION KBUFF(KBUFL)
      DIMENSION IMASK(8)
C
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
c
      SAVE NOLD,KELEMOLD
C     ------------------------------------------------------------------

!       print *, 'top of BUEXS3'
C
C*          1.   EXPAND PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1   SET THE POINTERS NWPT AND NBPT TO THE
C                 -------------------------------------
C                 BEGINING OF THE SECTION 3.
C                 --------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C*          1.2   UNPACK LENGTH OF SECTION 3.
C                 ---------------------------
 120  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(1),NWPT,NBPT,24,KERR)
!      print *, ' ksec3 =', ksec3
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC3(1).'
         RETURN
      END IF
C
C*          1.2.1  SET THE POINTERS NWPTB AND NBPTB TO
C                  -----------------------------------
C                  THE BEGINNING OF THE NEXT SECTION.
C                  ----------------------------------
      CALL BUNEXS(KSEC3(1))
C
C*          1.3    UNPACK ZERO BYTE AND PUT IT IN KSEC3(2).
C                  ----------------------------------------
 130  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(2),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC3(2).'
         RETURN
      END IF
C
C*          1.4    UNPACK NUMBER OF DATA SUB-SETS.
C                  -------------------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(3),NWPT,NBPT,16,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC3(3).'
         RETURN
      END IF
      IF(KSEC3(3).LE.0) THEN
         KERR=32
         PRINT*,' BUEXS3 :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
      N = KSEC3(3)
C
C
C*          1.5    UNPACK INTEGER VALUE OF THE OCTET
C                  ---------------------------------
C                  CONTAINING FLAG BITS.
C                  --------------------
 150  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC3(4),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC3(4).'
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C
C*          2.   EXPAND DATA DESCRIPTORS.
C                ------------------------
 200  CONTINUE
C
C
C*          2.1  CALCULATE EXPECTED NUMBER OF DATA DESCRIPTORS.
C                ----------------------------------------------
C                AND INITIALIZE NUMBER OF DATA VALUES PER SUB-SET.
C                -------------------------------------------------
 210  CONTINUE
C
      J      = 0
      NWT    = 0
      JMAX   = ( KSEC3(1) - 7)/2
      JMAXNEW=JMAX
C
      IF(JMAX.GT.JELEM) THEN
         print*,'Number of elements in section3 too big.'
         print*,'Program can not handle',JMAX
         print*,'data descriptors in section3.'
         kerr=200
         return
      END IF
C
C*          2.2  UNPACK AND PUT DATA DESCRIPTORS IN STACK.
C                -----------------------------------------
 220  CONTINUE
C
      DO 221 JJ=1,JMAX
C
      CALL BUNPCK(NBPW,KBUFF,IF,NWPT,NBPT,2,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking F part of descriptor.'
         RETURN
      END IF
      CALL BUNPCK(NBPW,KBUFF,IX,NWPT,NBPT,6,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking X part of descriptor.'
         RETURN
      END IF
      CALL BUNPCK(NBPW,KBUFF,IY,NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking Y part of descriptor.'
         RETURN
      END IF
C
      ISTACK(JJ)=IF*100000+IX*1000+IY
      IISTACK(JJ)=ISTACK(JJ)
C
 221  CONTINUE
C
C*          2.2.1 CHECK IF IT IS SAME DATA DESCRIPTOR DESCRIPTION.
C                 ------------------------------------------------
C                 TO MAKE MORE EFFICIENT DATA DESCRIPTOR DESCRIPTION
C                 EXPANSION, IN CASE THAT DELAYED REPLICATION FACTOR
C                 IS NOT PRESENT AND DATA DESCRIPTORS ARE THE SAME,
C                 PREVIOUS WORKING TABLE SHOULD BE USED. IT IS POSIBLE
C                 AT THIS PLACE IN THE FUTURE TO MAKE MORE SOPHISTICATED
C                 CONTROL.
C
C
      DO 222 JC=1,JMAX
C
      IF(ISTACK(JC).NE.NSTACK(JC)) THEN
C
         ODREPF=.FALSE.
C
C        SWAP CONTENT OF THE STACKS.
C
         DO 223 JJC=1,JMAX
         NSTACK(JJC)=ISTACK(JJC)
 223     CONTINUE
C
         NTDLEN = JMAX
         M=0
         M0=1
         NOLD=N
         kelemold=kelem
         NFCM=0
         nfucm=0
         MREL=0
         OMARKER=.FALSE.
         MBMP=0
         MBMPL=0
C
         GO TO 230
C
      END IF
C
 222  CONTINUE
C
C*    IF MARKER OPERATOR PRESENT EXPAND DESCRIPTORS AGAIN
C
      IF(OMARKER) THEN
         M=0
         M0=1
         NOLD=N
         kelemold=kelem
         NFCM=0
         nfucm=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    CHECK IF THE SAME NUMBER OF DESCRIPTORS
C     AS IN A PREVIOUS MESSAGE
C
      IF(JMAX.NE.NTDLEN) THEN
         M=0
         M0=1
         NOLD=N
         kelemold=kelem
         NFCM=0
         nfucm=0
         MREL=0
         OMARKER=.FALSE.
         NTDLEN=JMAX
         MBMP=0
         MBMPL=0
         GO TO 230
      END IF
C
C*    RETURN IF DELAYED REPLICATION FACTOR IS NOT PRESENT.
C
      IF(NPRUS.EQ.1) GO TO 229
C
      OB=.FALSE.
      if(iand(ksec3(4),imask(7)).ne.0) ob=.true.
C
C     Check for delayed replication factor
C
      if(odrepf) go to 229
c
c     Check for compression
c
      if(ob) then
c
c        data compressed =/ previous  --> expand again
c
         go to 229
      else
        If(nfucm.eq.1.and.n.le.nold.and.kelem.eq.kelemold) then
c
c         uncompressed data n<= nold ---> use previous pointers
c
          go to 300
        elseif(nfucm.eq.1.and.n.eq.nold.and.kelem.ne.kelemold) then
c
c          uncompressed data n=/ nold ---> recalculate pointers only
          kelemold=kelem
          go to 280
c        elseif(nfucm.eq.1.and.n.ne.nold) then
c
c           uncompressed data n=/ nold ---> recalculate pointers only
c
c           nold=n
c           kelemold=kelem
c           go to 280
        else
c
c         expand descriptors again
c
          go to 229
        end if
      end if
c
 229  CONTINUE
C
      M=0
      M0=1
      NOLD=N
      kelemold=kelem
      NFCM=0
      nfucm=0
      MREL=0
      OMARKER=.FALSE.
      NTDLEN=JMAX
      MBMP=0
      MBMPL=0
C
C     ------------------------------------------------------------------
C*          2.3  GET NEXT DESCRIPTOR FROM THE STACK.
C                -----------------------------------
 230  CONTINUE
C
      J   = J + 1
      IF(J.GT.JMAX) GO TO 270
C
      IDD = ISTACK(J)
      IF(IDD.EQ.0)  GO TO 230
C
      IF = IDD/100000
C
C     ------------------------------------------------------------------
C*          2.4  CHECK IF IT IS REPLICATION DESCRIPTOR.
C                --------------------------------------
 240  CONTINUE
C
      if( if.eq.0) then
C
C*          2.6  ELEMENT DESCRIPTOR, SO UPDATE WORKING TABLE.
C                --------------------------------------------
 260     CONTINUE
C
         if(idd.eq.31031.or.idd.eq.31192) then
            nwt=nwt+1
            nwtr(nwt)=idd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=1
            cwten(nwt)='DATA PRESENT INDICATOR'
            cwtu (nwt)='NUMERIC'
            m=m+1
         elseif(idd.eq.33007.or.idd.eq.63192) then
            nwt=nwt+1
            nwtr(nwt)=idd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=7
            cwten(nwt)='% CONFIDENCE'
            cwtu (nwt)='NUMERIC'
            m=m+1
         else
            CALL BUUPWT(IDD,KELEM,KERR)
            IF(KERR.GT.0) RETURN
         end if
C       
      elseif( if.eq.1) then
C
C*          2.4.1     SOLVE REPLICATION PROBLEM.
C                     --------------------------
C
C
         CALL BUSRP(KBUFL,KBUFF,KSEC3,J,JMAX,IDD,ISTACK,KELEM,KERR)
         IF(KERR.GT.0) RETURN
C
      elseif( if.eq.2) then
C
C*          2.5.3 PROCESS OPERATOR.
C                 -----------------
            CALL BUPRCO(KBUFL,KBUFF,J,IDD,ISTACK,KELEM,KERR)
            IF(KERR.GT.0) RETURN
C
      elseif( if.eq.3) then
C
C*          2.5.2 REPLACE BY LIST OF DESCRIPTORS FROM TABLE *D.
C                 ---------------------------------------------
            CALL BUSTDR(J,JMAX,IDD,ISTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
      else
         kerr=37
         call buerr(kerr)
         return
      end if
c
      go to 230
C
C     ------------------------------------------------------------------
C*          2.7 RESOLVE MARKER OPERATOR.
C               ------------------------
 270  CONTINUE
C
      if(omarker) then
         CALL BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)
         IF(KERR.GT.0) RETURN
      end if
C
C*          2.8 CHECK IF IT IS CORRESPONDING DATA.
C               ----------------------------------
 280  CONTINUE
c
C     CHECK FOR WORKING SPACE.
C
      IF(JWORK/N.LT.KELEM) THEN
         KERR=17
         PRINT*,'BUEXS3:'
         CALL BUERR(KERR)
         MN=KELEM*N
         PRINT*,' Suggested value for JWORK ',MN
         PRINT*,' Check if too big KELEM used.'
         RETURN
      END IF
C
      if(iand(KSEC3(4),imask(7)).ne.0) then
C
C        COMPRESSED DATA
C
         CALL BURQC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP ,KSEC3,KERR)
         IF(KERR.GT.0) RETURN
      ELSE
C
C        UNCOMPRESSED DATA
C
         CALL BURQUC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP ,KSEC3,KERR)
         IF(KERR.GT.0) RETURN
C
      END IF
C
C     ------------------------------------------------------------------
C
C*          3. COLLECT  SUPPLEMENTARY ITEMS.
C              -----------------------------
 300  CONTINUE
C
      NTDEXL =M
      DO 301 I=1,NTDEXL
      NTDEXP(I)=INWTR(I)
 301  CONTINUE
C
      NTDLEN=JMAXNEW
      DO 302 I=1,NTDLEN
      NTDLST (I)=IISTACK(I)
 302  CONTINUE
C
      KSUP(3)= 4
      KSUP(5)= M
      KSUP(6)= KSEC3(3)
C
      RETURN
      END
      SUBROUTINE BUEXS3P(KBUFL,KBUFF,KSUP,KERR)
C
C**** *BUEXS3P*
C
C
C     PURPOSE.
C     --------
C
C          Expand section 3 of Bufr message.
C               (preliminary items)
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS3( KBUFL,KBUFF,KSUP,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KERR*    -  returned error code
C
C     METHOD.
C      -------
C
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNEXS        - set word and bit pointers at the begining of
C                          next section
C          BUNPCK        - unpacks bit pattern
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(JSUP),ISEC3(JSEC3)
C
C     ------------------------------------------------------------------
C
C*          1.   EXPAND PRELIMINARY ITEMS OF SECTION 3.
C                --------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1   SET THE POINTERS NWPT AND NBPT TO THE
C                 -------------------------------------
C                 BEGINING OF THE SECTION 3.
C                 --------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C*          1.2   UNPACK LENGTH OF SECTION 3.
C                 ---------------------------
 120  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,ISEC3(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) RETURN
C
C*          1.2.1  SET THE POINTERS NWPTB AND NBPTB TO
C                  -----------------------------------
C                  THE BEGINNING OF THE NEXT SECTION.
C                  ----------------------------------
      CALL BUNEXS(ISEC3(1))
C
C*          1.3    UNPACK ZERO BYTE AND PUT IT IN KSEC3(2).
C                  ----------------------------------------
 130  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,ISEC3(2),NWPT,NBPT,8,KERR)
      IF(KERR.GT.0) RETURN
C
C*          1.4    UNPACK NUMBER OF DATA SUB-SETS.
C                  -------------------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,ISUBS,NWPT,NBPT,16,KERR)
!      print *, ' BUEXS3P: ISEC3(1) =', ISEC3(1)
!      print *, '          ISEC3(2) =', ISEC3(2)
!      print *, '          ISUBS    =', ISUBS
      IF(KERR.GT.0) RETURN
      IF(ISUBS.LE.0) THEN
         KERR=32
         PRINT*,' BUEXS3 :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C
      KSUP(3)= 4
      KSUP(5)= 0
      KSUP(6)= ISUBS
C
      RETURN
      END
      SUBROUTINE BUEXS4(KBUFL,KBUFF,KSUP,KSEC3,KSEC4,KELEM,CNAMES,
     1                  CUNITS,KVALS,VALUES,CVALS,KERR)
C
C**** *BUEXS4*
C
C
C     PURPOSE.
C     --------
C          Expand preliminary items and data of section 4 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS4(KBUFL,KBUFF,KSUP,KSEC3,KSEC4,KELEM,CNAMES,
C                         CUNITS,KVALS,VALUES,CVALS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *KVALS*   -  dimension of VALUES array
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KSEC4*   -  array containing section 4 information
C                            KSEC4( 1)-- length of section 4 (bytes)
C                            KSEC4( 2)-- reserved
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *VALUES*  -  real array (expanded data values)
C               *CVALS*   -  character array containing text
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C           NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          -  unpack bit pattern
C          BUNPKS          -  unpack bit pattern in repeated way,
C                             pointer adjustment
C          BUUNPS          -  unpack bit pattern in repeated way,
C                             no pointer adjustment
C          BUUNP           -  unpack bit pattern, no pointer adjustment
C          BUNEXS          -  set word and bit pointers at the begining of
C                             next section
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
      COMMON /BCOMP/ INWTEN(JELEM),INWTR (JELEM),INWTS (JELEM),
     1                INWTRV (JELEM),INWTDW(JELEM),
     2                INWORDP(JWORK),INBITP(JWORK)
C             INWTEN   -  woking table
C             INWTR     -  working table reference
C             INWTS     -  working scale
C             INWTRV    -  working reference value
C             INWTDW    -  working data width
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
C      COMMON /BCOMCT/ NREF(JCTAB)   ,NSTART(JCTAB) ,NLEN(JCTAB),
C     1               NCODNUM(JCTST),NSTARTC(JCTST),
C     2               NLENC(JCTST)
C
C             NREF      - table C reference
C             NSTART    - starting pointers to array NCODNUM
C             NLEN      - lengths
C             NCODNUM   - code/flag table number
C             NSTARTC   - starting pointers to array CTEXT
C             NLENC     - lengths
C
C
C      COMMON /BCOMCTC/ CTEXT(JCTEXT)
C
C             CTEXT     - text in code/flag tables
C
C
C      COMMON /BCOMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      CHARACTER CTEXT*64
      CHARACTER CNAMR*64,CUNIR*24
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION ICH(255),ILIST(JELEM),IVALUES(JELEM) 
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
C
      DIMENSION VALUES(KVALS)
      DIMENSION KSUP(JSUP),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION IMASK(8),IRO(80)
      DATA IMASK/1,2,4,8,16,32,64,128/
C
C     ------------------------------------------------------------------
C*          1.  EXPAND PRELIMINARY ITEMS FROM SECTION4.
C               ---------------------------------------
 100  CONTINUE
C
C                JCV - POINTER TO CVALS ARRAY
C                JWT - POINTER TO WORKING TABLE ARRAY
C                JNS - POINTER TO VALUES ARRAY FOR SUB-SETS.
C
      IF(KERR.GT.0) RETURN
C
      JCV = 0
      OREPF =.FALSE.
C
      IF(KELEM*N.GT.KVALS) THEN
         KERR=14
         CALL BUERR(KERR)
         PRINT*,' BUEXS4: Number of elements ',M
         PRINT*,' BUEXS4: Number of subsets  ',N
         MN=M*N
         PRINT*,' BUEXS4: Suggested value for KVALS ',MN
         PRINT*,' BUEXS4: Suggested value for KELEM ',M
         RETURN
      END IF
C
C*          1.1  SET THE POINTERS NWPT AND NBPT
C                ------------------------------
C                TO THE BEGINING OF THE SECTION.
C                -------------------------------
 110  CONTINUE
C
      NWPT = NWPTB
      NBPT = NBPTB
      NWPTB4 = NWPTB
      NBPTB4 = NBPTB
C
C*          1.2  UNPACK LENGTH OF SECTION 4.
C                ---------------------------
 120  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC4(1),NWPT,NBPT,24,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC4(1).'
         RETURN
      END IF
C
C*          1.3  SET THE POINTERS NWPTB AND NBPTB.
C                ---------------------------------
C                TO BEGINING OF THE NEXT SECTION.
C                --------------------------------
 130  CONTINUE
C
      CALL BUNEXS(KSEC4(1))
C
C
C*          1.4  EXPAND RESERVED BYTE.
C                ---------------------
 140  CONTINUE
C
      CALL BUNPCK(NBPW,KBUFF,KSEC4(2),NWPT,NBPT, 8,KERR)
      IF(KERR.GT.0) THEN
         PRINT*,'Error unpacking KSEC4(2).'
         RETURN
      END IF
C
C     -----------------------------------------------------------------
C*          2. EXPAND DATA.
C              ------------
 200  CONTINUE
C
C
C*          2.1  CHECK IF DATA ARE COMRESSED.
C                ----------------------------
 210  CONTINUE
C
      IB=0
      if(iand(KSEC3(4),imask(7)).ne.0) ib=1
C
      IF(IB.EQ.0) THEN
C
C     ------------------------------------------------------------------
C
C*          3.  UNCOMPRESSED DATA.
C               ------------------
 300  CONTINUE
C
         DO 301 JNS=1,N
C
         JNSK=(JNS-1)*KELEM
C
         DO 302 JWT=1,M
C
         JWTJNS=JWT+JNSK
C
         IF(INWTDW(JWT).EQ.0) THEN
            VALUES(JWTJNS)=0.0
            GO TO 302
         END IF
C
         NWPT=INWORDP(JWTJNS)
         NBPT=INBITP (JWTJNS)
C
         IF(INWTR(JWT).EQ.31011.OR.INWTR(JWT).EQ.31012) OREPF=.TRUE.
C
C
C     ------------------------------------------------------------------
C*          3.1 CHARACTER DATA ?
C               ----------------
 310  CONTINUE
C
         IF(INWTEN(JWT).EQ.658367) THEN
C
C*          3.2  OBTAIN CHARACTER DATA FROM DATA SECTION.
C                ----------------------------------------
 320  CONTINUE
C
            IY=INWTDW(JWT)/8
C
            CALL  BUUNPS(NBPW,KBUFF,ICH,NWPT,NBPT,8,0,IY,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error unpacking CHARACTER data.'
               RETURN
            END IF
C
C*          3.3  MOVE CHARACTER DATA TO "CVALS".
C                -------------------------------
 330  CONTINUE
C
            IYLINE=IY/80
            IYOFF =IY-IYLINE*80
            JCVINC=IYLINE
C
            JCV = JCV + 1
            JCVW= JCV
C
            IF(IYLINE.EQ.0) THEN
               DO 331 J=1,IY
               CVALS(JCVW)(J:J)=CHAR(ICH(J))
 331           CONTINUE
            ELSE
               DO 332 J=1,IYLINE
               DO 333 JJ=1,80
               CVALS(JCVW)(JJ:JJ)= CHAR(ICH(JJ))
 333           CONTINUE
C
               JCVW=JCVW+1
C
 332           CONTINUE
C
               DO 334 J=1,IYOFF
               CVALS(JCVW)(J:J)= CHAR(ICH(J))
 334           CONTINUE
C
            END IF
C
C*          3.4  COMPUTE POINTER VALUES TO BE STORED IN "VALUES".
C                ------------------------------------------------
 340  CONTINUE
C
            VALUES(JWTJNS) = JCV*1000 + IY
            JCV            = JCV + JCVINC
C
            GO TO 302
C
         END IF
C
C*          3.5  OBTAIN VALUE FROM DATA SECTION.
C                -------------------------------
 350  CONTINUE
C
         CALL GBYTE(KBUFF(NWPT),IVAL,NBPT,INWTDW(JWT))
C
C
C     -----------------------------------------------------------------
C*          3.6  UPDATE THE ARRAY "VALUES".
C                --------------------------
 360  CONTINUE
C
C
C        CHECK IF DATA IS MISSING
C
         IF(IVAL.EQ.NMASK(INWTDW(JWT))) then
            VALUES(JWTJNS)=RVIND
         else
            IVAL=IVAL+INWTRV(JWT)
            IF(INWTS(JWT).GT.0) THEN
               VALUES(JWTJNS)= IVAL/10.**INWTS(JWT)
            ELSE
               IIWTS=IABS(INWTS(JWT))
               VALUES(JWTJNS)= IVAL*10.**IIWTS
            END IF
         end if
C
 302     CONTINUE
C
         KSUP(7)=JCV
C
 301     CONTINUE
C
      ELSE 
C     ------------------------------------------------------------------
C
C*          4.  COMPRESSED DATA.
C               ----------------
 400  CONTINUE
C
C*          4.1 OBTAIN N VALUES BY EXPANSION.
C               -----------------------------
 410  CONTINUE
C
         DO 411 JWT=1,M
C
         NWPT=INWORDP(JWT)
         NBPT=INBITP (JWT)
C
         IF(INWTDW(JWT).EQ.0) THEN
            DO 412 J=1,N
            JWTJ=JWT+(J-1)*KELEM
            VALUES(JWTJ)=0.0
 412        CONTINUE
C
            GO TO 411
         END IF
C
C        CHECK IF CHARACTER DATA
C
         IF(INWTEN(JWT).EQ.658367) THEN
            IICH=INWTDW(JWT)/8
            CALL BUNPKS(NBPW,KBUFF,IRO,NWPT,NBPT,8,0,IICH,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error unpacking Reference values for'
               PRINT*,JWT,' element, of ',J,' subset.'
               RETURN
            END IF
            CALL BUNPCK(NBPW,KBUFF,IDWINC,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error unpacking number of bits for increments'
               PRINT*,'for ',JWT,' element, of ',J,' subset'
               RETURN
            END IF
C
            ITOTAL=IDWINC*KSEC3(3)
            DO 413 J=1,ITOTAL
            ILIST(J)=0
 413        CONTINUE
C
            CALL BUNPKS(NBPW,KBUFF,ILIST,NWPT,NBPT,
     1                  8,0,ITOTAL,KERR)
            IF(KERR.GT.0) RETURN
C
C           MOVE CHARACTER DATA TO "CVALS" ARRAY.
C
            IY=IDWINC
            IYLINE=IY/80
            IYOFF=IY-IYLINE*80
C
            JCVINC=IYLINE
            JJC=0
C
            DO 414 I=1,KSEC3(3)
C
            JCV=JCV+1
            JCVW=JCV
C
            IF(IYLINE.EQ.0) THEN
               DO 415 J=1,IY
               JJC=JJC+1
               CVALS(JCVW)(J:J)=CHAR(ILIST(JJC))
 415           CONTINUE
C
            ELSE
C
               DO 416 J=1,IYLINE
               DO 417 JJJ=1,80
               JJC=JJC+1
               CVALS(JCVW)(JJJ:JJJ)=CHAR(ILIST(JJC))
 417           CONTINUE
C
               JCVW=JCVW+1
 416           CONTINUE
C
               DO 418 J=1,IYOFF
               JJC=JJC+1
               CVALS(JCVW)(J:J)=CHAR(ILIST(JJC))
 418           CONTINUE
C
            END IF
C
C           COMPUTE POINTERS TO VALUES ARRAY
C
            JWTI=JWT+(I-1)*KELEM
            VALUES(JWTI)=JCV*1000+IY
            JCV         =JCV+JCVINC
C
 414        CONTINUE
C
            GO TO 411
C
         ELSE
            CALL BUNPCK(NBPW,KBUFF,IR0,NWPT,NBPT,INWTDW(JWT),KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error unpacking Reference values for'
               PRINT*,JWT,' element.'
               RETURN
            END IF
            CALL BUNPCK(NBPW,KBUFF,IDWINC,NWPT,NBPT,6,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error unpacking number of bits for'
               PRINT*,'increments, for ',JWT,' element.'
               RETURN
            END IF
            IF(IDWINC.GT.JBPW) THEN
               KERR=15
               PRINT*,' BUEXS4 :'
               CALL BUERR(KERR)
               RETURN
            END IF
         END IF
C
         DO 423 J=1,N
         ILIST(J)=0
 423     CONTINUE
C
         IF(IDWINC.NE.0) THEN
            CALL BUNPKS(NBPW,KBUFF,ILIST,NWPT,NBPT,
     1                  IDWINC,0,N,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error unpacking increments for ',jwt,' element.'
               RETURN
            END IF
         END IF
C
         IF(INWTEN(JWT).NE.658367) THEN
            IF(IR0.EQ.NMASK(INWTDW(JWT))) THEN
               DO 425 J=1,N
               IVALUES(J)=NMASK(INWTDW(JWT))
 425           CONTINUE
            ELSE
               IWTPR0=INWTRV(JWT)+IR0
C
               IF(IDWINC.EQ.0) THEN
                  DO 426 J=1,N
                  IVALUES(J)=IWTPR0
 426              CONTINUE
C
               ELSE
                  DO 424 J=1,N
                  IF(ILIST(J).EQ.NMASK(IDWINC)) THEN
                     IVALUES(J)=NMASK(INWTDW(JWT))
                  ELSE
                     IVALUES(J)= IWTPR0 + ILIST(J)
                  END IF
 424              CONTINUE
C
               END IF
            END IF
         END IF
C
         DO 427 J=1,N
C
         JWTJ=JWT+(J-1)*KELEM
C
         VALUES(JWTJ)=RVIND
         IF(IVALUES(J).NE.NMASK(INWTDW(JWT))) THEN
            IF(INWTS(JWT).GT.0) THEN
               VALUES(JWTJ)= IVALUES(J)/10.**INWTS(JWT)
            ELSE
               IIWTS=IABS(INWTS(JWT))
               VALUES(JWTJ)= IVALUES(J)*10.**IIWTS
            END IF
         END IF
 427     CONTINUE
C
 411     CONTINUE
C
         KSUP(7)=JCV
C
      END IF
C
C     ------------------------------------------------------------------
C*             5.  REPEAT ENTRIES IN CNAMES,CUNITS AND VALUES IF NEEDED.
C                  -----------------------------------------------------
 500  CONTINUE
C
      IF(.NOT.OREPF) GO TO 600
      IST=1
C
C*             5.1 SEARCH CNAMES FOR DELAYED REPETITION FACTOR.
C                  --------------------------------------------
 510  CONTINUE
C
c      DO 511 J=IST,M
c      IF(INWTR(J).EQ.31001.OR.INWTR(J).EQ.31002
c     1   .or.INWTR(J).EQ.31000) GO TO 520
c 511  CONTINUE
C
c      GO TO 600
C
C*             5.2 GET REPETITION FACTOR FROM ARRAY "VALUES".
C                  -------------------------------------------
 520  CONTINUE
C
c      IREPF=VALUES(J)
c      CNAMR=CNAMES(J+1)
c      CUNIR=CUNITS(J+1)
c      VALUR=VALUES(J+1)
C
C*             5.3 PUSH DOWN ENTRIES IN CNAMES,CUNITS,VALUES.
C                  ------------------------------------------
 530  CONTINUE
C
c      IREPM2=IREPF-2
C
c      DO 531 JA=M,J+2,-1
C
c      CNAMES(JA+IREPM2)=CNAMES(JA)
c      CUNITS(JA+IREPM2)=CUNITS(JA)
C
c      DO 932 JB=1,N
C
c      JAJB=JA+(JB-1)*KELEM
C
c      VALUES(JAJB+IREPM2)=VALUES(JAJB)
c 932  CONTINUE
c
c 531  CONTINUE
C
C*             5.4 REPETITION.
C                  -----------
 540  CONTINUE
C
c      DO 541 JA=1,IREPF
c      CNAMES(J+JA)=CNAMR
c      CUNITS(J+JA)=CUNIR
C
c      DO 542 JB=1,N
C
c      JAJB=JA+(JB-1)*KELEM
C
c      VALUES(J+JAJB)=VALUR
c 542  CONTINUE
C
c 541  CONTINUE
C
C*             5.5 UPDATE M AND POINTER TO CONTINUE SEARCH.
C                  ----------------------------------------
c 550  CONTINUE
C
c      M=M+IREPF-2
c      IST=J+IREPF
C
c      GO TO 510
C
C     -----------------------------------------------------------------
C*            6.  SET SUPPLEMENTARY INFORMATION.
C                 ------------------------------
C
 600  CONTINUE
C
      KSUP(4)= 2
C
C     ------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE BUEXS5( KBUFL,KBUFF,KERR)
C
C**** *BUEXS5*
C
C
C     PURPOSE.
C     --------
C          Expands section 5 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUEXS5( KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C        OUTPUT:
C               *KERR*    -  returned error code
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPKS         - unpack bit pattern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION IBUFR(4)
C
      CHARACTER*4 YBUFR
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 5.
C                -----------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      NWPT = NWPTB
      NBPT = NBPTB
C
C
C*          1.2  UNPACK LAST FOUR OCTETS CONTAINING '7777'.
C                ------------------------------------------
C
      CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
C
C*          1.3  CHECK IF THE LAST FOUR OCTETS ARE '7777'.
C                --------------------------------------
      YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1      CHAR(IBUFR(3))//CHAR(IBUFR(4))
      IF(YBUFR.NE.'7777') THEN
         KERR = 2
         WRITE(*,'(1H ,A)') 'BUEXS5 :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     -----------------------------------------------------------------
      RETURN
      END
      SUBROUTINE BUFREN(KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1                  KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS,
     2                  VALUES,CVALS,KBUFL,KBUFF,KERR)
C
C**** *BUFREN*
C
C
C     PURPOSE.
C     --------
C          Encode Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL BUFREN(KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
C                            KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS,
C                            VALUES,CVALS,KBUFL,KBUFF,KERR)
C
C        INPUT :
C               *KSEC0*   -  Integer array of 3 words containing
C                            Bufr section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing Bufr section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) to KSEC1(40) - local ADP centre
C                                        information(PACKED FORM)
C
C               *KSEC2*   -  Integer array of at least 64 words
C                            containing Bufr section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(64) local ADP centre
C                                        information(PACKED FORM)
C
C               *KSEC3*   -  Integer array of 4 words containing
C                            Bufr section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C
C               *KSEC4*   -  Integer array of 2 words containing
C                            Bufr section 4 information
C                            KSEC4( 1)-- length of section 4 (bytes)
C                            KSEC4( 2)-- reserved
C
C               *KTDLEN*  -  Integer number of data descriptors in section 3
C               *KTDLST*  -  Integer array of at least KTDLEN words
C                            containing data descriptors for Bufr section 3
C               *KDLEN*   -  Integer (dimension of KDATA array)
C               *KDATA*   -  Integer array containing data needed for data
C                            descriptor expansion (delayed replication factors)
C                            which appear in the values array
C
C               *KELEM*   -  Integer number of elements in Bufr template.
C               *KVALS*   -  Integer (dimension of VALUES array)
C               *VALUES*  -  Real array of KVALS words (expanded data )
C               *CVALS*   -  character*80  array of KVALS
C
C        OUTPUT:
C               *KBUFL*   -  Length of Bufr message (words)
C               *KBUFF*   -  Integer array containing Bufr message
C               *KERR*    -  Return error code
C
C
C     METHOD.
C     -------
C
C          Bufr message sections contenet, and data are passed for
C     for packing into FM-94 Bufr data.During unpacking a bit pathern
C     GBYTE and GBYTES routines (VMS version) are used.
C
C
C     EXTERNALS.
C     ----------
C
C          BUENS0   -  pack section 0 of Bufr message
C          BUENS1   -  pack section 1 of Bufr message
C          BUENS2   -  pack section 2 of Bufr message
C          BUENS3   -  pack section 3 of Bufr message
C          BUETAB    -  load required Bufr tables
C          BUENS4   -  pack section 4 of Bufr message
C          BUENS5   -  pack section 5 of Bufr message
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSEC0(JSEC0),KSEC1(JSEC1),KSEC2(JSEC2)
     1,         KSEC3(JSEC3),KSEC4(JSEC4)
C
      DIMENSION  VALUES(KVALS)
      DIMENSION  KTDLST(KTDLEN)
      DIMENSION  KDATA(KDLEN)
C
      CHARACTER*4   CECMWF,CUSER
      CHARACTER*80  CVALS(KVALS)
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUIVAR(KERR)
         KPT   = 0
         CECMWF='ECMF'
      END IF
C
C     -----------------------------------------------------------------
C*          2.  PACK SECTION 0.
C               ---------------
 200  CONTINUE
C
      CALL BUENS0( KSEC0,KBUFL,KBUFF,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          3.   PACK SECTION 1.
C                ---------------
 300  CONTINUE
C
      CALL BUENS1( KSEC0,KSEC1,KBUFL,KBUFF,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          4.  PACK SECTION 2.
C               ---------------
 400  CONTINUE
C
      CALL BUENS2( KSEC1,KSEC2,KBUFL,KBUFF,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          5.   LOAD BUFR TABLES.
C                -----------------
 500  CONTINUE
C
      CALL BUETAB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C          6.  EXPAND DATA DESCRIPTORS.
C              ------------------------
 600  CONTINUE
C
      CALL BUETD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1           KVALS,VALUES,KELEM,KERR)
C
      IF(KERR.GT.0) RETURN
C
C*          6.1  PACK SECTION 3.
C                ---------------
 610  CONTINUE
      CALL BUENS3( KSEC3,KTDLEN,KTDLST,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
C     ------------------------------------------------------------------
C
C*          7.   PACK SECTION 4.
C                ---------------
 700  CONTINUE
C
      CALL BUENS4(KSEC3,KSEC4,KELEM,KVALS,VALUES,CVALS,
     1            KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C
C     ------------------------------------------------------------------
C
C*          8.   PACK SECTION 5.
C                ---------------
 800  CONTINUE
C
      CALL BUENS5(KSEC0,KSEC1,KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C     -----------------------------------------------------------------
C
C*          9.   SET TOTAL BUFR MESSAGE LENGTH.
C                ------------------------------
 900  CONTINUE
C
C
C     -----------------------------------------------------------------
      RETURN
C
      END
      SUBROUTINE BUFREX(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     2                  KELEM,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
C
C**** *BUFREX*
C
C
C     PURPOSE.
C     --------
C          Decode Bufr message into fully expanded form; returning
C     information relevant for all bufr sections, expanded values,
C     their names and units.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUFREX(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
C     1                   KELEM,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)*
C
C        INPUT :
C               *KBUFL*   -  Length of bufr message (words)
C               *KBUFF*   -  Integer array containing Bufr message
C               *KELEM*   -  Integer (expected number of expanded elements)
C               *KVALS*   -  Integer (expected number of data values)
C        OUTPUT:
C               *KSUP*    -  Integer array of 9 words containing
C                            suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values
C                                           array, first index)
C                         -  KSUP( 6) -- N (number of subsets,second index
C                                           of values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C
C               *KSEC0*   -  Integer array of 3 words containing
C                            Bufr section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C
C               *KSEC1*   -  Integer array of at least 40 words
C                            containing Bufr section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(40) -- local ADP centre
C                                        information(BYTE by BYTE)
C
C               *KSEC2*   -  Integer array of at least 64 words
C                            containing Bufr section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(47) RDB key
C
C               *KSEC3*   -  Integer array of 4 words containing
C                            Bufr section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C
C               *KSEC4*   -  Integer array of 2 words containing
C                            Bufr section 4 information
C                            KSEC4( 1)-- length of section 4 (bytes)
C                            KSEC4( 2)-- reserved
C
C               *CNAMES*  -  Character*64  array of KELEM containing
C                            Bufr Table B element names
C               *CUNITS*  -  Character*24 array of KELEM containig
C                            Bufr Table B units
C               *VALUES*  -  Real array of KVALS containing expanded
C                            data values
C               *CVALS*   -  Character*80 array of KVALS containing
C                            Bufr code table or CCITTIA5 Bufr elements
C                            entries
C               *KERR*    -  returned error code
C
C
C     METHOD.
C     -------
C
C          Bufr message passed as argument to this routine is decoded
C     section by section. Suplementary information and expanded data
C     are returned as well as error code. During bit pattern unpacking
C     GBYTE and GBYTES routines (VMS version) are used.
C
C
C     EXTERNALS.
C     ----------
C
C          BUEXS0   -  expands section 0 of Bufr message
C          BUEXS1   -  expands section 1 of Bufr message
C          BUEXS2   -  expands section 2 of Bufr message
C          BUEXS3   -  expands section 3 of Bufr message
C          BUGBTS   -  load required Bufr tables
C          BUEXS4   -  expands section 4 of Bufr message
C          BUEXS5   -  expands section 5 of Bufr message
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
C
      COMMON /BCOMREQ/ NREQ(2),NRQL,NRQ(JELEM),RQVAL(JELEM)
C
C             *NREQ*    -  flag
C                          bit number     meaning
C
C                              1        - 0 no bit map delivered to user
C                                         1    bit map delivered to user
C                              2        - 0 no partial expansion
C                                         1    partial expansion
C                              3        - 0 no Q/C required
C                                       - 1    Q/C required
C                              4        - 0 no statistics required
C                                       - 1    statistics
C                              5        - 0 no diffrence statistics
C                                       - 1    difference statistics
C                              6        - 0 no substituted values
C                                       - 1    substituted values
C             *NRQL*    -  number of requested elements
C             *NRQ*     -  list of requested table B reference
C             *RQVAL*   -  list of values signifying requested element
C                          (say pressure  at 50000 Pa)
C
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(JSUP),KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
C
      DIMENSION  VALUES(KVALS)
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*4 CECMWF,CUSER
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
C
      M0=1
C
      DO 101 I=1,JSEC0
      KSEC0(I)=0
 101  CONTINUE
C
      DO 102 I=1,JSEC1
      KSEC1(I)=0
 102  CONTINUE
C
      DO 103 I=1,JSEC3
      KSEC3(I)=0
 103  CONTINUE
C
      DO 104 I=1,JSUP
      KSUP(I)=0
 104  CONTINUE
C
      DO 105 I=1,JSEC4
      KSEC4(I)=0
 105  CONTINUE
C
      DO 106 I=1,JSEC2
      KSEC2(I)=0
 106  CONTINUE
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUEVAR(KERR)
         CECMWF='ECMF'
      END IF
C
      IF(CUSER.NE.'USER') THEN
         NREQ(1)=0
         nreq(2)=0
         NRQL=0
      END IF
C     -----------------------------------------------------------------
C*          2.  EXPAND SECTION 0.
C               -----------------
 200  CONTINUE
C
      CALL BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          3.   EXPAND SECTION 1.
C                ------------------
 300  CONTINUE
C
      CALL BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          4.  EXPAND SECTION 2.
C               -----------------
 400  CONTINUE
C
      CALL BUEXS2( KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          5.   LOAD BUFR TABLES.
C                -----------------
 500  CONTINUE
C
      CALL BUGBTS(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          6.  EXPAND SECTION 3.
C               -----------------
 600  CONTINUE
C
      CALL BUEXS3(KBUFL,KBUFF,KSUP,KSEC3,KELEM,CNAMES,CUNITS,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C*          7.   EXPAND SECTION 4.
C                -----------------
 700  CONTINUE
C
      CALL BUEXS4(KBUFL ,KBUFF ,KSUP  ,KSEC3,KSEC4,
     1            KELEM ,CNAMES,CUNITS,KVALS,VALUES,CVALS,KERR)
      IF(KERR.GT.0) RETURN
C
C
C     ------------------------------------------------------------------
C*          8.   EXPAND SECTION 5.
C                -----------------
 800  CONTINUE
C
      CALL BUEXS5(KBUFL,KBUFF,KERR)
      IF(KERR.GT.0) RETURN
C
C     -----------------------------------------------------------------
C*          9.   SET TOTAL BUFR MESSAGE LENGTH.
C                ------------------------------
 900  CONTINUE
C
      KSUP(8)=KSEC0(1)+KSEC1(1)+KSEC2(1)+KSEC3(1)+KSEC4(1)+4
C
C     -----------------------------------------------------------------
      RETURN
C
      END
      SUBROUTINE BUGBTS( KSEC1,KERR )
C
C**** *BUGBTS*
C
C
C     PURPOSE.
C     --------
C          Load Bufr table B, D and C according to Edition and version
C     of Bufr code.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUGBTS(KSEC1,KERR)*
C
C        OUTPUT:
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCOMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
C      COMMON /BCOMCT/ NREF(JCTAB)   ,NSTART(JCTAB) ,NLEN(JCTAB),
C     1               NCODNUM(JCTST),NSTARTC(JCTST),
C     2               NLENC(JCTST)
C
C             NREF      - table C reference
C             NSTART    - starting pointers to array NCODNUM
C             NLEN      - lengths
C             NCODNUM   - code/flag table number
C             NSTARTC   - starting pointers to array CTEXT
C             NLENC     - lengths
C
C
C      COMMON /BCOMCTC/ CTEXT(JCTEXT)
C
C             CTEXT     - text in code/flag tables
C
C
      COMMON /BCOMROOT/ CROOT
C
C            CROOT    -  path for Bufr tables
C
C
      CHARACTER*256 YTAB ,YTAC ,YTAD
      CHARACTER*11  YTABB,YTABC,YTABD
      CHARACTER    CTABBEN*64,CTABBU*24,CTEXT*64
      CHARACTER*256 CROOT
C
      DIMENSION KSEC1(JSEC1)
C
      SAVE OFIRST
C
C     ------------------------------------------------------------------
C
C*          1.   GET BUFR TABLES/LOCAL BUFR TABLES.
C                ----------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0) RETURN
C
C*          2.   SET UP BUFR TABLE FILE NAME.
C                ----------------------------
 200  CONTINUE
C
C
C             BUFR EDITION 2 NAMING CONVENTION
C
C             BXXXXXYYZZ , CXXXXXYYZZ , DXXXXXYYZZ
C
C             B      - BUFR TABLE 'B'
C             C      - BUFR TABLE 'C'
C             D      - BUFR TABLE 'D'
C             XXXXX  - ORIGINATING CENTRE
C             YY     - VERSION NUMBER OF MASTER TABLE
C                      USED( CURRENTLY 2 )
C             ZZ     - VERSION NUMBER OF LOCAL TABLE USED
C
C             BUFR EDITION 3 NAMING CONVENTION
C
C             BWWWXXXYYZZ , CWWWXXXYYZZ , DWWWXXXYYZZ
C
C             B      - BUFR TABLE 'B'
C             C      - BUFR TABLE 'C'
C             D      - BUFR TABLE 'D'
C             WWW    - ORIGINATING SUB-CENTRE
C             XXX    - ORIGINATING CENTRE
C             YY     - VERSION NUMBER OF MASTER TABLE
C                      USED( CURRENTLY 2 )
C             ZZ     - VERSION NUMBER OF LOCAL TABLE USED
C
C
            IXX=KSEC1(3)
            IYY=KSEC1(15)
            IZZ=KSEC1(08)
            IF(KSEC1(2).GE.3) THEN
               IWW=KSEC1(16)
            ELSE
               IWW=0
            END IF
C
C        IF STANDARD TABLES USED, USE ECMWF ORIGINATING CENTRE ID
C
         IF(KSEC1(8).EQ.0) IXX=98
C
         IF(OFIRST) THEN
            IF(IWW.EQ.NWWP.AND.IXX.EQ.NXXP.AND.IYY.EQ.NYYP.AND.
     1         IZZ.EQ.NZZP) RETURN
         END IF
C
         OFIRST=.TRUE.
C
         NXXP=IXX
         NYYP=IYY
         NZZP=IZZ
         NWWP=IWW
C
         if(ksec1(2).ge.3) then
            WRITE(YTABB,'(A1,I3.3,I3.3,I2.2,I2.2)') 'B',IWW,IXX,IYY,IZZ
C            WRITE(YTABC,'(A1,I3.3,I3.3,I2.2,I2.2)') 'C',IWW,IXX,IYY,IZZ
            WRITE(YTABD,'(A1,I3.3,I3.3,I2.2,I2.2)') 'D',IWW,IXX,IYY,IZZ
         else
            WRITE(YTABB,'(A1,I5.5,I2.2,I2.2)') 'B',IXX,IYY,IZZ
C            WRITE(YTABC,'(A1,I5.5,I2.2,I2.2)') 'C',IXX,IYY,IZZ
            WRITE(YTABD,'(A1,I5.5,I2.2,I2.2)') 'D',IXX,IYY,IZZ
         end if
C
c         PRINT*,'BUFR Tables to be loaded ',YTABB,',',YTABC,',',YTABD
ccc          PRINT*,'BUFR Tables to be loaded ',YTABB,',',YTABD
C
C     ----------------------------------------------------------------
C*          3. OPEN AND READ FILES CONTAINING BUFR TABLES.
C              -------------------------------------------
 300  CONTINUE
C
      I=INDEX(CROOT,' ')
      IF(I.NE.0) I=I-1
C
C*          3.1 READ BUFR TABLE B.
C               ------------------
 310  CONTINUE
C
      YTAB=CROOT(1:I)//YTABB
      II=I+11
C
      OPEN(UNIT=38,IOSTAT=IOS,ERR=311,FILE=YTAB(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
      GO TO 312
C
 311  CONTINUE
C
      CLOSE(38)
C
      print*,'Open error on ',YTAB(1:II)
      print*,'Try on /home/ma/emos/data/mrfs/tables/bufr/ directory'
C
C     try to read from :
C
      YTAB=' '
      YTAB='/home/ma/emos/data/mrfs/tables/bufr/'//YTABB
      II=INDEX(YTAB,' ')
      IF(II.NE.0) II=II-1
C
      OPEN(UNIT=38,IOSTAT=IOS,ERR=410,FILE=YTAB(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')

C
 312  CONTINUE
C
      READ(38,ERR=400,IOSTAT=IOS) NTABBTR,CTABBEN,CTABBU,NTABBS,
     1                            NTABBRV,NTABBDW,NTABP
C
      CLOSE(UNIT=38,IOSTAT=IOS,ERR=420)
C
C*          3.2 READ BUFR TABLE C.
C               ------------------
 320  CONTINUE
C
C      YTAC=CROOT(1:I)//YTABC
C
C      OPEN(UNIT=39,IOSTAT=IOS,ERR=510,FILE=YTAC(1:II),
C     1     FORM='UNFORMATTED',
C     2     ACCESS='SEQUENTIAL',
C     5     STATUS='OLD')
C
C      READ(39,ERR=500,IOSTAT=IOS)NREF   ,NSTART,NLEN,NCODNUM,
C     1                           NSTARTC,NLENC ,CTEXT
C
C      CLOSE(UNIT=39,IOSTAT=IOS,ERR=520)
C
C
C*          3.3 READ BUFR TABLE D.
C               ------------------
 330  CONTINUE
C
      YTAD=CROOT(1:I)//YTABD
C
      OPEN(UNIT=40,IOSTAT=IOS,ERR=331,FILE=YTAD(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
      GO TO 332
C
 331  CONTINUE
C
      CLOSE(40)
C
      print*,'Open error on ',YTAD
      print*,'Try on /home/ma/emos/data/mrfs/tables/bufr/ directory'
C
      YTAD='/home/ma/emos/data/mrfs/tables/bufr/'//YTABD
C
      OPEN(UNIT=40,IOSTAT=IOS,ERR=610,FILE=YTAD(1:II),
     1     FORM='UNFORMATTED',
     2     ACCESS='SEQUENTIAL',
     5     STATUS='OLD')
C
 332  CONTINUE
C
      READ(40,ERR=600,IOSTAT=IOS) NTABDTR,NTABDL,NTABDST,NTABDSQ
C
      CLOSE(UNIT=40,IOSTAT=IOS,ERR=620)
C
C
      RETURN
C     ----------------------------------------------------------------
 400  CONTINUE
C
      KERR=6
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 410  CONTINUE
C
      KERR=9
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 420  CONTINUE
C
      KERR=10
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C     ----------------------------------------------------------------
 500  CONTINUE
C
      KERR=7
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 510  CONTINUE
C
      KERR=9
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 520  CONTINUE
C
      KERR=11
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C     -----------------------------------------------------------------
 600  CONTINUE
C
      KERR=8
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 610  CONTINUE
C
      KERR=9
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C
 620  CONTINUE
C
      KERR=12
      PRINT*,'BUGBTS: IOS ',IOS
      CALL BUERR(KERR)
      OFIRST=.FALSE.
      RETURN
C     -----------------------------------------------------------------
C
      END
       SUBROUTINE BUGETBM(KBUFL,KBUFF,KSEC3,KBMP,KBMPL,KBV,KERR)
C
C**** *BUGETBM*
C
C
C     PURPOSE.
C     --------
C
C          Create bit map to resolve marker operators.
C
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUGETBM(KBUFL,KBUFF,KSEC3,KBMP,KBMPL,KBV,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KBMP     -  bit map pointer to the first data present
C                            indicator
C               *KBMPL    -  number of  data present indicators
C
C        OUTPUT:
C               *KBV*     -  bit map array
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpacks bit pattern
C          BUNPKS         - unpacks bit pattern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C             CWTU     -  working table units
C
C
      COMMON /BCOMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      CHARACTER CWTEN*64,CWTU*24
      DIMENSION KBUFF(KBUFL),KBV(*)
      DIMENSION KSEC3(JSEC3)
C
      DIMENSION IMASK(8),ilist(jelem)
C
      DATA IMASK /1,2,4,8,16,32,64,128/
C
C
C     ------------------------------------------------------------------
C*          1.  CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 100  CONTINUE
C
C
      IF(KERR.GT.0) RETURN
C
      IB=0
      IF(IAND(ksec3(4),IMASK(7)).NE.0) IB=1
C
      IF(IB.EQ.0) THEN
C
C        FOR UNCOMPRESSED DATA
C
         IBIT=NWPTB*NBPW+32+NBPTB
         DO 101 I=1,KBMP-1
         IBIT=IBIT+NWTDW(I)
 101     CONTINUE
C
         IWPT=IBIT/NBPW
         IBPT=IBIT-IWPT*NBPW
      ELSE
C
C        FOR COMPRESSED DATA
C
         IBIT=32+NBPTB
         IWORD=IBIT/NBPW
C
         NWORDP(1)=NWPTB+IWORD
         NBITP (1)=IBIT-IWORD*NBPW
C
         DO 102 I=2,KBMP
C
         IF(NWTDW(I-1).EQ.0) THEN
            NBITP(I)=NBITP(I-1)
            NWORDP(I)=NWORDP(I-1)
            GO TO 102
         END IF
C
         IWRD=NWORDP(I-1)
         IBTP=NBITP (I-1)
c
         IBTP=IBTP+NWTDW(I-1)
         IF(IBTP.GE.NBPW) THEN
            IW=IBTP/NBPW
            IBTP=IBTP-IW*NBPW
            IWRD=IWRD+IW
         END IF
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWRD,IBTP,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            PRINT*,'BUPMRK :'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         IF(CWTU(I-1).EQ.'CCITTIA5') THEN
            NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC*8
         ELSE
            NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC
         END IF
C
         IBIT = NBITP(I-1) + NWTIWS(I-1)
         IWORD= IBIT/NBPW
C
         NBITP (I)= IBIT - IWORD*NBPW
         NWORDP(I)= NWORDP(I-1) + IWORD
 102     CONTINUE
C
         IBPT=NBITP (KBMP)
         IWPT=NWORDP(KBMP)
      END IF
C
C*          2.  GET BIT MAP FROM DATA SECTION.
C               ------------------------------
 200  CONTINUE
C
      IF(IB.EQ.0) THEN
         CALL GBYTES(KBUFF(IWPT),KBV,IBPT,1,0,KBMPL)
      ELSE
         DO 201 I=1,KBMPL
         CALL BUNPCK(NBPW,KBUFF,IR0,IWPT,IBPT,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWPT,IBPT,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            PRINT*,'BUPMRK:'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         DO 202 K=1,KSEC3(3)
         ILIST(K)=0
 202     CONTINUE
C
         IF(IDWINC.NE.0) THEN
C
C           UNPACK INCREMENTS
C
            CALL BUNPKS(NBPW,KBUFF,ILIST,IWPT,IBPT,
     1                  IDWINC,0,KSEC3(3),KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
         KBV(I)=IR0
         IF(IDWINC.NE.0) KBV(I)=IR0+ILIST(1)
 201     CONTINUE
C
      END IF
C
      RETURN
      END
      SUBROUTINE BUIVAR(KERR)
C
C**** *BUIVAR*
C
C
C     PURPOSE.
C     --------
C         INITIALIZE CONSTANTS AND VARIABLES.
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *BUIVAR(KERR)*
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/03/92.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMROOT/ CROOT
C
C            CROOT    -  path for Bufr tables
C
C
      CHARACTER*256 CROOT
C
      EXTERNAL GETENV
C
C     ------------------------------------------------------------------
C*          1.   INITIALIZE VARIABLES AND CONSTANTS.
C                -----------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      NJA= 0
      M  =0
      MM =0
      N  =0
      JCV=0
      NBPW=JBPW
      NWPT=0
      NBPT=0
      NWPTB=0
      NBPTB=0
      NVIND=2147483647
      RVIND=1.7E38
      EPS=10.E-10
      NBENP=0
      NLTVNP=0
      NWWP=0
      NXXP=0
      NYYP=0
      NZZP=0
      NDWINC=0
      NSCAM=0
      NAFDW=0
      NWT=0
      ODREPF=.FALSE.
      N221=0
      MREL=0
      NFCM=0
      EPS=10.E-11
      MBMP=0
      MBMPL=0
      OMARKER=.FALSE.
      CROOT=' '
C
C     vax bufr tables path
C
C     CROOT='BUFR_TABLES:'
C
C     unicos bufr tables path
C
C     CROOT=' '
C     CALL GETENV('BUFR_TABLES',CROOT)
C     ILNG=INDEX(CROOT,' ')
C     IF(ILNG.eq.1) CROOT='/tmp/emos_sms/tables/temp/'
C
C
C     SGi/HP/SUN bufr tables path 
C
      CROOT=' '
      CALL GETENV('BUFR_TABLES',CROOT)
      ILNG=INDEX(CROOT,' ')
      IF(ILNG.eq.1) CROOT='/home/ma/emos/tables/bufr/'
C
C     VPP700  bufr tables path (memory resident)
C
C     CROOT=' '
C     CALL GETENV('BUFR_TABLES',CROOT)
C     ILNG=INDEX(CROOT,' ')
C     IF(ILNG.eq.1) CROOT='/vpp700/mrfs/tables/bufr/'
C
C     VPP300  bufr tables path (memory resident)
C
C     CROOT=' '
C     CALL GETENV('BUFR_TABLES',CROOT)
C     ILNG=INDEX(CROOT,' ')
C     IF(ILNG.eq.1) CROOT='/vpp300/mrfs/tables/bufr/'
C
C
C
      WRITE(*,'(1H ,A)') '                  ECMWF '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A)') '     BUFR ENCODING SOFTWARE VERSION -  3.6 '
      WRITE(*,'(1H ,A)') '            14 APR 1998. '
      WRITE(*,'(1H )')
      WRITE(*,'(1H )')
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A)') 'Your path for Bufr tables is :'
      WRITE(*,'(1H ,A)')  CROOT(1:63)
C
      DO 101 I=1,JBPW-1
      NMASK(I)=2**I-1
 101  CONTINUE
C
C     for vax machine
C
C     DO 101 I=1,JBPW-2
C     NMASK(I)=2**I-1
C 101 CONTINUE
C     NMASK(31)=2147483647
C
      RETURN
      END
      SUBROUTINE BUNEXS( KLEN)
C
C**** *BUNEXS*
C
C
C     PURPOSE.
C     --------
C           Sets word (NWPT) and bit(NBPT) pointers at the begining
C     of next section of Bufr message.
C
C**   INTERFACE.
C     ----------
C
C            *CALL* *BUNEXS( KLEN)*
C
C        INPUT :
C                 *KLEN*     - length of section in bytes
C
C     METHOD.
C     -------
C
C          Length of each section is added up. Word and bit pointers
C     pointing to the begining of next section are calculated.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       16/01/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
C
C     ------------------------------------------------------------------
C
C*          1.   SET UP POINTERS AT THE BEGINING OF THE NEXT SECTION.
C                -----------------------------------------------------
 100  CONTINUE
C
      IBIT = (NWPT - 1)*NBPW +NBPT +KLEN * 8 - 24
C
      NWPTB = IBIT/NBPW + 1
      NBPTB = IBIT - (NWPTB - 1)*NBPW
C
      RETURN
      END
      SUBROUTINE BUNPCK(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KERR)
C
C**** *BUNPCK*
C
C
C     PURPOSE.
C     --------
C          Purpose of this routine is to unpack bit string of
C     KSIZE bits, started at word KWPT of array KSOURC after
C     skipping KBPT bits. Result is put into KDEST. At the end
C     pointers KWPT and KBPT are adjusted.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUNPCK(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KERR)*
C
C        INPUT :
C              *KBPW*      - number of bits in computer word
C              *KSOURC*    - source (continuous bit string of
C                            arbitrary length)
C              *KWPT*      - word pointer
C              *KBPT*      - bit pointer
C              *KSIZE*     - number of bits to be extracted
C        OUTPUT:
C              *KDEST*     - destination
C              *KERR*      - return error code
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          GBYTE     - unpack bit pattern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KSOURC(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KSIZE.GT.KBPW) THEN
         KERR=13
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL GBYTE(KSOURC(KWPT),KDEST,KBPT,KSIZE)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + KSIZE
C
      IF(KBPT.GE.KBPW) THEN
         KBPT= KBPT -  KBPW
         KWPT= KWPT +1
      END IF
C
      RETURN
      END
      SUBROUTINE BUNPKS(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)
C
C**** *BUNPKS*
C
C
C     PURPOSE.
C     --------
C          Purpose of this routine is to unpack bit string of
C     KSIZE bits, started at word KWPT of array KSOURC after
C     skipping KBPT bits. Result is put into KDEST. At the end
C     pointers KWPT and KBPT are adjusted.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUNPKS(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)*
C
C        INPUT :
C            *KBPW*      - number of bits per computer word
C            *KSOURC*    - source (continuous bit string of
C                          arbitrary length)
C            *KWPT*      - word pointer
C            *KBPT*      - bit pointer
C            *KSIZE*     - number of bits to be extracted
C            *KSKIPB*    - number of bits to skip between elements
C            *K*         - iteration
C        OUTPUT:
C            *KDEST*     - destination
C            *KERR*      - return error code
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          GBYTES     - unpack bit pattern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KSOURC(*),KDEST(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KSIZE.GT.KBPW) THEN
         KERR=13
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL GBYTES(KSOURC(KWPT),KDEST,KBPT,KSIZE,KSKIPB,K)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + K*(KSIZE+KSKIPB)
C
      IF(KBPT.GE.KBPW) THEN
         IW  = KBPT/ KBPW
         KBPT= KBPT - IW * KBPW
         KWPT= KWPT +IW
      END IF
C
      RETURN
      END
      SUBROUTINE BUOCTN(KWPTB,KBPTB,KBUFL,KBUFF,KERR)
C
C**** *BUOCTN*
C
C
C     PURPOSE.
C     --------
C             CALCULATE NUMBER OF OCTETS FROM BIT POSITION DEFINED BY
C     KWPT,KBPT  AND KWPTB,KBPTB; NUMBER OF OCTETS MUST BE EVEN. IF IT IS
C     NEEDED PADING WITH 0 BIT PERFORMS. NUMBER OF OCTETS IS WRITTEN AT
C     BEGINIG OF THE SECTION.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUOCTN(KWPTB,KBPTB,KBUFL,KBUFF,KERR)*
C
C        INPUT :
C               *KWPTB*    - word pointer to the begining of section
C               *KBPTB*    - bit  pointer to the begining of section
C        OUTPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KERR*    -  returned error code
C
C
C     *METHOD.
C      -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUPCK       - pack bit pathern and resets pointers
C          SBYTE       - pack bit pathern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       07/10/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KBUFF(KBUFL)
C
C     ------------------------------------------------------------------
C*          1.    CALCULATE NUMBER OF OCTETS.
C                 ---------------------------
 100  CONTINUE
C
      IB   =(NWPT-1) * NBPW + NBPT
      IBB  =(KWPTB-1)* NBPW + KBPTB
C
      IDIFB= IB - IBB
C
      NOCT = IDIFB/8
      IBDW = IDIFB - NOCT*8
C
      IF(IBDW .NE.0)THEN
         NOCT=NOCT+1
         IBDW=8-IBDW
         CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,IBDW,KERR)
         IF(KERR.GT.0) then
            call buerr(kerr)
            RETURN
         end if
      END IF
C
C     ------------------------------------------------------------------
C*          2.  CHECK IF THERE ARE EVEN NUMBER OF OCTETS IN BLOCK.
C               --------------------------------------------------
 200  CONTINUE
C
      IF(MOD(NOCT,2).NE.0) THEN
          IBDW = 8
          CALL BUPCK(NBPW,KBUFF(NWPT),0,NWPT,NBPT,IBDW,KERR)
          IF(KERR.GT.0) then
            call buerr(kerr)
            RETURN
          end if
          NOCT = NOCT+1
      END IF
C     ------------------------------------------------------------------
C*          3.  WRITE NUMBER OF OCTETS AT BEGINING OF BLOCK.
C               --------------------------------------------
 300  CONTINUE
C
      IBDW  = 24
      CALL SBYTE(KBUFF(KWPTB),NOCT,KBPTB,IBDW)
C
      RETURN
      END
      SUBROUTINE BUOPER(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)
C
C
C**** *BUOPER*
C
C
C     PURPOSE.
C     --------
C
C          Process Bufr operator.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUOPER(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)*
C
C        INPUT :
C               *KPT*    - pointer to kdata array
C               *KDLEN*  - dimension of KDATA array
C               *KDATA*  - array containing data needed for descriptor
C                          expansion
C               *KJ*     - pointer t array kstack
C               *KDD*    - data descriptor
C        OUTPUT:
C               *KSTACK* - list of descriptors
C               *KERR*   - return error code
C
C     *METHOD.
C      -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUUATB           - update augmented table b
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CWTEN*64,CWTU*24
C
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
C
C     ------------------------------------------------------------------
C
C*          1.   DETERMINE *F *X AND *Y.
C                -----------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
C
      IF( IF.NE.2 ) THEN
         KERR=21
         PRINT*,' BUOPER :'
         CALL BUERR(KERR)
         GO TO 400
      END IF
C
C*          1.1   CHANGE DATA WIDTH ?
C                 -------------------
 110  CONTINUE
C
      IF(IX.EQ.1) THEN
         NDWINC= IY-128
         IF(IY.EQ.0) NDWINC=0
         GO TO 400
      END IF
C
C*          1.2   CHANGE SCALE ?
C                 --------------
 120  CONTINUE
C
      IF(IX.EQ.2) THEN
C
C*          1.2.1  UPDATE SCALE MULTIPLIER.
C                  ------------------------
C
         NSCAM=IY-128
         IF(IY.EQ.0) NSCAM=0
         GO TO 400
      END IF
C
C*          1.3  CHANGE REFERENCE VALUE ?
C                ------------------------
 130  CONTINUE
C
      IF(IX.EQ.3) THEN
C
C*          1.3.1  UPDATE AUGMENTED TABLE B.
C                  -------------------------
         CALL BUAUG(KPT,KDLEN,KDATA,KJ,IY,KSTACK,KERR)
         GO TO 400
      END IF
C
C*          1.4   ADD ASSOCIATED FIELD ?
C                 ----------------------
 140  CONTINUE
C
      IF(IX.EQ.4) THEN
C
C*          1.4.1   UPDATE ASSOCIATED FIELD WIDTH.
C                   ------------------------------
         NAFDW= IY
         IF(IY.EQ.0) NAFDW=0
         GO TO 400
      END IF
C
C*          1.5   SIGNIFY CHARACTER ?
C                 -------------------
 150  CONTINUE
C
      IF(IX.EQ.5) THEN
C
C*          1.5.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
c         CWTEN(NWT)='CHARACTERS'
c         CWTU (NWT)=' '
         NWTR (NWT)= 0
         NWTDW(NWT)= IY * 8
         NWTEN(NWT)=658367
         GO TO 400
      END IF
C
C*          1.5.2 SIGNIFY DATA WIDTH FOR IMMEDISTELY
C                 FOLLOWED LOCAL DESCRIPTOR
C
 152  CONTINUE
C
      IF(IX.EQ.6) THEN
         NWT = NWT + 1
         KJ=KJ+1
c         CWTEN(NWT)='UNKNOWN'
c         CWTU (NWT)='UNKNOWN'
         NWTR (NWT)= KSTACK(KJ)
         NWTDW(NWT)= IY
         NWTS (NWT)= 0
         NWTRV(NWT)= 0
         NWTEN(NWT)= NVIND
         M=NWT
C
C        CHECK IF LOCAL TABLE ENTRY KNOWN
C        --------------------------------
C
C         DO 153 I=1,JTAB
C         IF(NWTR(NWT).EQ.NTABBTR(I)) THEN
C            CWTEN(NWT)=CTABBEN(I)
C            CWTU (NWT)=CTABBU (I)
C            IF(CWTU(NWT)(1:3).EQ.'CCI') NWTEN(NWT)=65367
C            NWTS (NWT)=NTABBS (I)
C            NWTRV(NWT)=NTABBRV(I)
C            NWTDW(NWT)=NTABBDW(I)
C            NWTEN(NWT)=NTABBTR(I)
C            GO TO 400
C         END IF
C  153    CONTINUE
C
         GO TO 400
      END IF
C
C*          1.6   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 160  CONTINUE
C
      IF(IX.EQ.62) THEN
C
         IF(IY.EQ.0) GO TO 400
C
C*          1.7.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
c         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
c         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= KDD
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.  PROCESSING NEW OPERATORS.
C               -------------------------
 200  CONTINUE
C
C
C*          2.1   DATA NOT PRESENT.
C                 -----------------
 210  CONTINUE
C
      IF(IX.EQ.21) THEN
         N221=IY
         GO TO 400
      END IF
C
C
C*          2.2   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 220  CONTINUE
C
      IF(IX.EQ.22) THEN
C
C*          1.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
c         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
c         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 222000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.3   SUBSTITUTED VALUES FOLLOWS.
C                 ---------------------------
 230  CONTINUE
C
      IF(IX.EQ.23) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
c            CWTEN(NWT)='SUBSTITUTED VALUES FOLLOW'
c            CWTU (NWT)=' '
            NWTDW(NWT)=0
            NWTR (NWT)=223000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
c               CWTEN(NWT)='ASSOCIATED FIELD'
c               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='SUBSTITUTED VALUE MARKER'
c            CWTU (NWT)=' '
c            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.4   FIRST ORDER STATISTICS FOLLOWS.
C                 -------------------------------
 240  CONTINUE
C
      IF(IX.EQ.24) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
c            CWTEN(NWT)='FIRST ORDER STATISTICS FOLLOW'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=224000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            IF(NAFDW.NE.0) THEN
               NWT=NWT+1
c               CWTEN(NWT)='ASSOCIATED FIELD'
c               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='FIRST ORDER STATISTICS VALUE MARKER'
c            CWTU (NWT)=' '
c            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.5   DIFFERENCE STATISTICAL VALUES FOLLOW.
C                 -------------------------------------
 250  CONTINUE
C
      IF(IX.EQ.25) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
c            CWTEN(NWT)='DIFFERENCE STATISTICAL VALUES FOLLOW'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=225000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
c               CWTEN(NWT)='ASSOCIATED FIELD'
c               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='DIFFERENCE STATISTICS VALUE MARKER'
c            CWTU (NWT)=' '
c            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.6   REPLACED/RETAINED VALUES FOLLOWS.
C                 ---------------------------------
 260  CONTINUE
C
      IF(IX.EQ.32) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
c            CWTEN(NWT)='REPLACE/RETAINED VALUES FOLLOW'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=232000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
c               CWTEN(NWT)='ASSOCIATED FIELD'
c               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='REPLACE/RETAINED VALUE MARKER'
c            CWTU (NWT)=' '
c            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.7   CANCEL BACKWARD DATA REFERENCE.
C                 -------------------------------
 270  CONTINUE
C
      IF(IX.EQ.35) THEN
C
C*          2.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
c         CWTEN(NWT)='CANCEL BACKWARD DATA REFERENCE'
c         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 235000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.8   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 280  CONTINUE
C
      IF(IX.EQ.36) THEN
C
C*          2.8.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
c         CWTEN(NWT)='BACKWARD REFERENCE BIT MAP'
c         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 236000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.9   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 290  CONTINUE
C
      IF(IX.EQ.37) THEN
         IF(IY.EQ.0) THEN
C
C*          2.8.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
c            CWTEN(NWT)='USE PREVIOUSLY DEFINED BIT MAP'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 237000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          2.8.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
c            CWTEN(NWT)='CANCEL PREDEFINED BIT MAP'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400

         END IF
      END IF
C
C     ------------------------------------------------------------------
C
 300  CONTINUE
C
      KERR=22
      PRINT*,'BUOPER:'
      CALL BUERR(KERR)
      print *, ' IF IX IY =', IF,IX,IY
C
C     ------------------------------------------------------------------
C
 400  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUOPERC(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)
C
C
C**** *BUOPERC*
C
C
C     PURPOSE.
C     --------
C
C          Process Bufr operator.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUOPERC(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)*
C
C        INPUT :
C               *KPT*    - pointer to kdata array
C               *KDLEN*  - dimension of KDATA array
C               *KDATA*  - array containing data needed for descriptor
C                          expansion
C               *KJ*     - pointer t array kstack
C               *KDD*    - data descriptor
C        OUTPUT:
C               *KSTACK* - list of descriptors
C               *KERR*   - return error code
C
C     *METHOD.
C      -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUUATB           - update augmented table b
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CWTEN*64,CWTU*24
C
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
C
C     ------------------------------------------------------------------
C
C*          1.   DETERMINE *F *X AND *Y.
C                -----------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
C
      IF( IF.NE.2 ) THEN
         KERR=21
         PRINT*,' BUOPER :'
         CALL BUERR(KERR)
         GO TO 400
      END IF
C
C*          1.1   CHANGE DATA WIDTH ?
C                 -------------------
 110  CONTINUE
C
      IF(IX.EQ.1) THEN
         NDWINC= IY-128
         IF(IY.EQ.0) NDWINC=0
         GO TO 400
      END IF
C
C*          1.2   CHANGE SCALE ?
C                 --------------
 120  CONTINUE
C
      IF(IX.EQ.2) THEN
C
C*          1.2.1  UPDATE SCALE MULTIPLIER.
C                  ------------------------
C
         NSCAM=IY-128
         IF(IY.EQ.0) NSCAM=0
         GO TO 400
      END IF
C
C*          1.3  CHANGE REFERENCE VALUE ?
C                ------------------------
 130  CONTINUE
C
      IF(IX.EQ.3) THEN
C
C*          1.3.1  UPDATE AUGMENTED TABLE B.
C                  -------------------------
         CALL BUAUG(KPT,KDLEN,KDATA,KJ,IY,KSTACK,KERR)
         GO TO 400
      END IF
C
C*          1.4   ADD ASSOCIATED FIELD ?
C                 ----------------------
 140  CONTINUE
C
      IF(IX.EQ.4) THEN
C
C*          1.4.1   UPDATE ASSOCIATED FIELD WIDTH.
C                   ------------------------------
         NAFDW= IY
         IF(IY.EQ.0) NAFDW=0
         GO TO 400
      END IF
C
C*          1.5   SIGNIFY CHARACTER ?
C                 -------------------
 150  CONTINUE
C
      IF(IX.EQ.5) THEN
C
C*          1.5.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='CHARACTERS'
         CWTU (NWT)=' '
         NWTR (NWT)= 0
         NWTDW(NWT)= IY * 8
         NWTEN(NWT)=658367
         GO TO 400
      END IF
C
C*          1.5.2 SIGNIFY DATA WIDTH FOR IMMEDISTELY
C                 FOLLOWED LOCAL DESCRIPTOR
C
 152  CONTINUE
C
      IF(IX.EQ.6) THEN
         NWT = NWT + 1
         KJ=KJ+1
         CWTEN(NWT)='UNKNOWN'
         CWTU (NWT)='UNKNOWN'
         NWTR (NWT)= KSTACK(KJ)
         NWTDW(NWT)= IY
         NWTS (NWT)= 0
         NWTRV(NWT)= 0
         NWTEN(NWT)= NVIND
         M=NWT
C
C        CHECK IF LOCAL TABLE ENTRY KNOWN
C        --------------------------------
C
C         DO 153 I=1,JTAB
C         IF(NWTR(NWT).EQ.NTABBTR(I)) THEN
C            CWTEN(NWT)=CTABBEN(I)
C            CWTU (NWT)=CTABBU (I)
C            IF(CWTU(NWT)(1:3).EQ.'CCI') NWTEN(NWT)=65367
C            NWTS (NWT)=NTABBS (I)
C            NWTRV(NWT)=NTABBRV(I)
C            NWTDW(NWT)=NTABBDW(I)
C            NWTEN(NWT)=NTABBTR(I)
C            GO TO 400
C         END IF
C  153    CONTINUE
C
         GO TO 400
      END IF
C
C*          1.6   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 160  CONTINUE
C
      IF(IX.EQ.62) THEN
C
         IF(IY.EQ.0) GO TO 400
C
C*          1.7.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= KDD
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.  PROCESSING NEW OPERATORS.
C               -------------------------
 200  CONTINUE
C
C
C*          2.1   DATA NOT PRESENT.
C                 -----------------
 210  CONTINUE
C
      IF(IX.EQ.21) THEN
         N221=IY
         GO TO 400
      END IF
C
C
C*          2.2   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 220  CONTINUE
C
      IF(IX.EQ.22) THEN
C
C*          1.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 222000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.3   SUBSTITUTED VALUES FOLLOWS.
C                 ---------------------------
 230  CONTINUE
C
      IF(IX.EQ.23) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='SUBSTITUTED VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)=0
            NWTR (NWT)=223000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='associated field'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
            CWTEN(NWT)='SUBSTITUTED VALUE MARKER'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.4   FIRST ORDER STATISTICS FOLLOWS.
C                 -------------------------------
 240  CONTINUE
C
      IF(IX.EQ.24) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='FIRST ORDER STATISTICS FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=224000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
            CWTEN(NWT)='FIRST ORDER STATISTICS VALUE MARKER'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.5   DIFFERENCE STATISTICAL VALUES FOLLOW.
C                 -------------------------------------
 250  CONTINUE
C
      IF(IX.EQ.25) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='DIFFERENCE STATISTICAL VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=225000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
            CWTEN(NWT)='DIFFERENCE STATISTICS VALUE MARKER'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.6   REPLACED/RETAINED VALUES FOLLOWS.
C                 ---------------------------------
 260  CONTINUE
C
      IF(IX.EQ.32) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='REPLACE/RETAINED VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=232000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.JELEM) THEN
                  KERR=30
                  PRINT*,'BUOPER:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
            CWTEN(NWT)='REPLACE/RETAINED VALUE MARKER'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.7   CANCEL BACKWARD DATA REFERENCE.
C                 -------------------------------
 270  CONTINUE
C
      IF(IX.EQ.35) THEN
C
C*          2.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='CANCEL BACKWARD DATA REFERENCE'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 235000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.8   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 280  CONTINUE
C
      IF(IX.EQ.36) THEN
C
C*          2.8.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='BACKWARD REFERENCE BIT MAP'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 236000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.9   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 290  CONTINUE
C
      IF(IX.EQ.37) THEN
         IF(IY.EQ.0) THEN
C
C*          2.8.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='USE PREVIOUSLY DEFINED BIT MAP'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 237000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          2.8.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CANCEL PREDEFINED BIT MAP'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400

         END IF
      END IF
C
C     ------------------------------------------------------------------
C
 300  CONTINUE
C
      KERR=22
      PRINT*,'BUOPERC:'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
C
 400  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUPCK(KBPW,KD,KS,KWPT,KBPT,KSI,KERR)
C
C**** *BUPCK*
C
C
C     PURPOSE.
C     --------
C         PURPOSE OF THIS ROUTINE IS TO PACK VALUE *KS* IN
C         *KSI* BITS, STARTED AT WORD KWPT OF ARRAY *KD* AFTER
C         SKIPPING NBPT BITS.  AT THE END
C         POINTERS *KWPT* AND *KBPT* ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C     *CALL* *BUPCK(KBPW,KD,KS,KWPT,KBPT,KSI,KERR)*
C
C        INPUT :
C            *KS*    - SOURCE
C            *KWPT*  - WORD POINTER
C            *KWPT*  - BIT POINTER
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C        OUTPUT :
C            *KD*    - DESTINATION ARRAY.
C            *KERR*  - RETURN ERROR CODE
C
C     *METHOD.
C      -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C            *CALL SBYTE(KD,KS,KBPT,KSI)*
C
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       09/06/86.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      DIMENSION KD(*)
      DIMENSION IMAXV(31)
C
      DATA IMAXV/1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,
     1  16383,32767,65535,131071,262143,524287,1048575,2097151,
     2  4194305,8388607,16777215,33554431,671108863,134217727,
     3  268435455,536870911,1073741823,2147483647/
C
C     ------------------------------------------------------------------
C*          1.   SET UP BIT PATTERN.
C                -------------------
C
 100  CONTINUE
C
      IF(KS.EQ.NVIND) KS=IMAXV(KSI)
C
      IF(KS.GT.IMAXV(KSI)) THEN
         KERR=28
         RETURN
      END IF
C
      CALL SBYTE(KD,KS,KBPT,KSI)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + KSI
C
      IF(KBPT.GE.KBPW) THEN
         KBPT= KBPT - KBPW
         KWPT= KWPT + 1
      END IF
C
      RETURN
      END
      SUBROUTINE BUPKEY( KEY,KSEC1,KSEC2,KERR )
C
C**** *BUPKEY*
C
C
C     PURPOSE.
C     --------
C          Pack local ECMWF information (rdb key) into KSEC2 array.
C
C
C**   INTERFACE.
C     ----------
C          *CALL* *BUPKEY(KEY,KSEC1,KSEC2,KERR)*
C
C        INPUT :
C               *KEY*     -  array containing RDB information
C                            KEY( 1)-- length of section 2 (bytes)
C                            KEY( 2)-- RDB type
C                            KEY( 3)-- RDB subtype
C                            KEY( 4)-- year
C                            KEY( 5)-- month
C                            KEY( 6)-- day
C                            KEY( 7)-- hour
C                            KEY( 8)-- minute
C                            KEY( 9)-- second
C                            KEY(10)-- longitude1
C                            KEY(11)-- latitude1
C                            KEY(12)-- longitude2
C                            KEY(13)-- latitude2
C                            KEY(14)-- number of subsets
C                            KEY(15)-- ident (numeric)
C                            KEY(16)-- ident ( CCITTIA5) one character
C                            KEY(17)-- ident ( CCITTIA5) one character
C                            KEY(18)-- ident ( CCITTIA5) one character
C                            KEY(19)-- ident ( CCITTIA5) one character
C                            KEY(20)-- ident ( CCITTIA5) one character
C                            KEY(21)-- ident ( CCITTIA5) one character
C                            KEY(22)-- ident ( CCITTIA5) one character
C                            KEY(23)-- ident ( CCITTIA5) one character
C                            KEY(24)-- ident ( CCITTIA5) one character
C                            KEY(25)-- total Bufr message length
C                            KEY(26)-- day    (RDB insertion)
C                            KEY(27)-- hour   (RDB insertion)
C                            KEY(28)-- minute (RDB insertion)
C                            KEY(29)-- second (RDB insertion)
C                            KEY(30)-- day    (MDB insertion)
C                            KEY(31)-- hour   (MDB insertion)
C                            KEY(32)-- minute (MDB insertion)
C                            KEY(33)-- second (MDB insertion)
C                            KEY(34)-- correction number
C                            KEY(35)-- part
C                            KEY(36)-- 0
C                            KEY(37)-- correction number
C                            KEY(38)-- part
C                            KEY(39)-- 0
C                            KEY(40)-- correction number
C                            KEY(41)-- part
C                            KEY(42)-- 0
C                            KEY(43)-- correction number
C                            KEY(44)-- part
C                            KEY(45)-- 0
C                            KEY(46)-- the lowest Q/C % confidence
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) to ksec1(JSEC1) - local ADP centre
C                                        information(PACKED FORM)
C        OUTPUT:
C               *KSEC2*   -  array containing section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(JSEC2) local ADP centre
C                                        information(PACKED FORM)
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
C
      CHARACTER*4 CECMWF,CUSER
C
      DIMENSION KSEC1(JSEC1),KSEC2(JSEC2),KEY(JKEY)
      DIMENSION IDUM(8),KSEC3(JSEC3)
C
      DATA IDUM/8*0/

C
C*          1.  PACK LOCAL ADP CENTRE INFORMATION INTO KSEC2 ARRAY.
C                --------------------------------------------------
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUIVAR(KERR)
         CECMWF='ECMF'
      END IF

C
C      IF(KSEC1(3).EQ.98) THEN
         IW=2
         IB=0
         CALL BUPCK(NBPW,KSEC2(IW),KEY(2)       ,IW,IB, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(2).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(3)       ,IW,IB, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(3).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(4)       ,IW,IB,12,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(4).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(5)       ,IW,IB, 4,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(5).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(6)       ,IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(6).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(7)       ,IW,IB, 5,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(7).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(8)       ,IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(8).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(9)       ,IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(9).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0            ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUPCK(NBPW,KSEC2(IW),KEY(10)      ,IW,IB,26,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(10).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0            ,IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUPCK(NBPW,KSEC2(IW),KEY(11)      ,IW,IB,25,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(11).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0            ,IW,IB, 7,KERR)
         IF(KERR.GT.0) RETURN
c
         IF(KEY(14).GT.1.OR.KEY(2).EQ.2.OR.KEY(2).EQ.3
     1              .OR.KEY(2).EQ.12) THEN
            CALL BUPCK(NBPW,KSEC2(IW),KEY(12)   ,IW,IB,26,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error packing key(12).'
               RETURN
            END IF
            CALL BUPCK(NBPW,KSEC2(IW),0         ,IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUPCK(NBPW,KSEC2(IW),KEY(13)   ,IW,IB,25,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error packing key(13).'
               RETURN
            END IF
            CALL BUPCK(NBPW,KSEC2(IW),0         ,IW,IB, 7,KERR)
            IF(KERR.GT.0) RETURN
            IF(KEY(14).GT.255.OR.KEY(3).GE.121.AND.KEY(3).LE.130) THEN
               CALL BUPCK(NBPW,KSEC2(IW),KEY(14),IW,IB,16,KERR)
               IF(KERR.GT.0) THEN
                  PRINT*,'Error packing key(14).'
                  RETURN
               END IF
               CALL BUPCK(NBPW,KSEC2(IW),KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) THEN
                  PRINT*,'Error packing key(15).'
                  RETURN
               END IF
               CALL BUPKS(NBPW,KSEC2(IW),IDUM  ,IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
            ELSE
               CALL BUPCK(NBPW,KSEC2(IW),KEY(14),IW,IB, 8,KERR)
               IF(KERR.GT.0) THEN
                  PRINT*,'Error packing key(14).'
                  RETURN
               END IF
               CALL BUPCK(NBPW,KSEC2(IW),KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) THEN
                  PRINT*,'Error packing key(15).'
                  RETURN
               END IF
               CALL BUPKS(NBPW,KSEC2(IW),IDUM      ,IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
            END IF
            GO TO 140
         ELSE
            CALL BUPKS(NBPW,KSEC2(IW),KEY(16),IW,IB,8,0,9,KERR)
            IF(KERR.GT.0) THEN
               PRINT*,'Error packing key(16).'
               RETURN
            END IF
            CALL BUPKS(NBPW,KSEC2(IW),IDUM(1),IW,IB,8,0,8,KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
C
C*          1.4 SUB KEY INFORMATION.
C               --------------------
 140  CONTINUE
C
         CALL BUPCK(NBPW,KSEC2(IW),KEY(25),IW,IB,16,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(25).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(26),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(26).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(27),IW,IB, 5,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(27).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(28),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(28).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(29),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(29).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUPCK(NBPW,KSEC2(IW),KEY(30),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(30).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(31),IW,IB, 5,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(31).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(32),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(32).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(33),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(33).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),0      ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUPCK(NBPW,KSEC2(IW),KEY(34),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(34).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(35),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(35).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(36),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(36).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(37),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(37).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(38),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(38).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(39),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(39).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(40),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(40).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(41),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(41).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(42),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(42).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(43),IW,IB, 6,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(43).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(44),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(44).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(45),IW,IB, 1,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(45).'
            RETURN
         END IF
         CALL BUPCK(NBPW,KSEC2(IW),KEY(46),IW,IB, 8,KERR)
         IF(KERR.GT.0) THEN
            PRINT*,'Error packing key(46).'
            RETURN
         END IF
C
C      ELSE
C         WRITE(*,'(1H )')
C         WRITE(*,'(1H ,A)') 'BUPKEY : Key definition not packed.'
C         WRITE(*,'(1H )')
C      END IF
C
      RETURN
      END
      SUBROUTINE BUPKS(KBPW,KDEST,KSOURC,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)
C
C**** *BUPKS*
C
C
C     PURPOSE.
C     --------
C          Purpose of this routine is to pack bit string of
C     KSIZE bits, started at word KWPT of array KSOURC after
C     skippinh KBPT bits. Result is put into KDEST. At the end
C     pointers KWPT and KBPT are adjusted.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUPKS(KBPW,KDEST,KSOURC,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)*
C
C
C        INPUT :
C            KBPW      - number of bits per computer word
C            KSOURC    - source (continuous bit string of
C                          arbitrary length)
C            KWPT      - word pointer
C            KBPT      - bit pointer
C            KSIZE     - number of bits used for packing
C            KSKIPB    - number of bits to skip between elements
C            K         - iteration
C
C        OUTPUT :
C            KDEST     - destination
C            KERR      - return error code
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          SBYTES     - pack bit pathern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KDEST(*),KSOURC(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      IF(KSIZE.GT.KBPW) THEN
         KERR= 34
         PRINT*,'BUPKS :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL SBYTES(KDEST,KSOURC,KBPT,KSIZE,KSKIPB,K)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + K*(KSIZE+KSKIPB)
C
      IF(KBPT.GE.KBPW) THEN
         IW  = KBPT/ KBPW
         KBPT= KBPT - IW * KBPW
         KWPT= KWPT +IW
      END IF
C
      RETURN
      END
      SUBROUTINE BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)
C
C**** *BUPMRK*
C
C
C     PURPOSE.
C     --------
C
C          Process marker operator, relacing it with corresponding
C     table B element descriptor.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpacks bit pattern
C          BUNPKS         - unpacks bit pattern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KBUFF(KBUFL),IBV(JELEM),ILIST(JELEM)
      DIMENSION IMASK(8)
      DATA IMASK/1,2,4,8,16,32,64,128/
C
      save ibv
C
C     ------------------------------------------------------------------
C*          1.  FINED OPERATOR 223000 TO 237255.
C               --------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
C           1.1 CHECK IF DATA ARE COMRESSED.
C               ----------------------------
 110  CONTINUE
C
      IB=0
      if(iand(KSEC3(4),imask(7)).ne.0) ib=1
C
C*          1.2 FIND POSITION OF OPREATORS.
C               ---------------------------
 120  CONTINUE
C
      M0OLD=M0
      DO 121 J=M0OLD,M
C
      NR223=0
      NR224=0
      NR225=0
      NR232=0
C
      IF(NWTR(J).EQ.222000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 122 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
               i222=mbmp
               nr222=mbmpl-mbmp+1
c
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,i222,nr222,IBV,KERR)
               IF(KERR.GT.0) RETURN
c
               GO TO 121
            END IF
 122        CONTINUE
         END IF
         go to 121
      END IF
C
      IF(NWTR(J).EQ.223000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP223000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 123 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
c
               i223=mbmp
               nr223=mbmpl-mbmp+1
c
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,i223,nr223,IBV,KERR)
               IF(KERR.GT.0) RETURN
c
               GO TO 200
            END IF
 123        CONTINUE
         END IF
         GO TO 200
      END IF
      IF(NWTR(J).EQ.224000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP224000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 124 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
c
               i224=mbmp
               nr224=mbmpl-mbmp+1
c
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,i224,nr224,IBV,KERR)
               IF(KERR.GT.0) RETURN
c
               GO TO 300
            END IF
 124        CONTINUE
         END IF
         GO TO 300
      END IF
      IF(NWTR(J).EQ.225000) THEN
C
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP225000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 125 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
c
               i225=mbmp
               nr225=mbmpl-mbmp+1
c
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,i225,nr225,IBV,KERR)
               IF(KERR.GT.0) RETURN
c
               GO TO 400
            END IF
 125        CONTINUE
         END IF
         GO TO 400
      END IF
C
      IF(NWTR(J).EQ.232000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         IF(MREL.EQ.0) MREL=J-1
C
         M0=J
         NP232000=J
C
C        CHECK IF BACKWARD REFERENCE BIT MAP DEFINED
C
         IF(NWTR(J+1).EQ.236000) THEN
            NP236000=J+1
            OBF=.TRUE.
            J2=J+2
C
C           DELAYED REPLICATION FACTOR CAN FOLLOW 236000
C
            IF(NWTR(J2).EQ.031001.OR.NWTR(J2).EQ.031000.OR.
     1         NWTR(J2).EQ.031002) J2=J+3
C
            DO 126 I=J2,M
            IF(NWTR(I).EQ.031031) THEN
C
C              FIND  FIRST POINTER TO DATA PRESENT INDICATOR
C
               IF(OBF) MBMP=I
               OBF=.FALSE.
            ELSE
C
C              LAST POINTER TO DATA PRESENT INDICATOR
C
               MBMPL=I-1
c
               i232=mbmp
               nr232=mbmpl-mbmp+1
c
               CALL BUGETBM(KBUFL,KBUFF,KSEC3,i232,nr232,IBV,KERR)
               IF(KERR.GT.0) RETURN
c
               GO TO 500
            END IF
 126        CONTINUE
         END IF
         GO TO 500
      END IF
C
C
      IF(NWTR(J).EQ.235000) THEN
C
C        SET POINTER TO THE LAST DATA ELEMENT
C
         MREL=0
         MBMP=0
         MBMPL=0
         M0=J
C
      END IF
C
      IF(NWTR(J).EQ.237255) THEN
C
C        CANCEL BACKWARD BIT MAP REFERENCE.
C
         MBMP=0
         MBMPL=0
C
      END IF

      GO TO 121
C
C     -----------------------------------------------------------------
C*          2.  PROCESS SUBSTITUTED VALUES OPERATOR.
C               ------------------------------------
C
 200  CONTINUE
C
C*          2.1 FIND FIRST ACCURANCE OF 223255.
C               -------------------------------
 210  CONTINUE
C
      NP223255=0
      DO 211 I=NP223000,M
      IF(NWTR(I).EQ.223255) THEN
         NP223255=I
         GO TO 220
      END IF
 211  CONTINUE
C
      GO TO 121
C
C*          2.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 220  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I223=0
         OF223=.TRUE.
         DO 221 I=NP223000,NP223255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF223) THEN
               I223=I
               OF223=.FALSE.
            END IF
            NR223=NR223+1
         END IF
 221     CONTINUE
      ELSE
         I223=MBMP
         NR223=MBMPL-MBMP+1
         GO TO 250
      END IF
C
C*          2.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 230  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I223,NR223,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          2.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 250  CONTINUE
C
      ISUBST=MREL-NR223+1
C
C*          2.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 260  CONTINUE
C
      DO 261 I=1,NR223
      IF(IBV(I).EQ.0) THEN
c
 262     continue
c
         if(nwtr(np223255).eq.223255) then
            NWTDW (NP223255)=NWTDW(ISUBST)
c           NWTR  (NP223255)=NWTR (ISUBST)
            NWTRV (NP223255)=NWTRV(ISUBST)
            NWTS  (NP223255)=NWTS (ISUBST)
            CWTEN (NP223255)=CWTEN(ISUBST)
            CWTU  (NP223255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP223255=NP223255+1
         else
            np223255=np223255+1
            go to 262
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 261  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          3.  PROCESS FIRST ORDER STATISTICS OPERATOR.
C               ----------------------------------------
 300  CONTINUE
C
C*          3.1 FIND FIRST ACCURANCE OD 224255.
C               -------------------------------
 310  CONTINUE
C
      NP224255=0
      DO 311 I=NP224000,M
      IF(NWTR(I).EQ.224255) THEN
         NP224255=I
         GO TO 320
      END IF
 311  CONTINUE
C
      GO TO 121
C
C*          3.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 320  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I224=0
         OF224=.TRUE.
         DO 321 I=NP224000,NP224255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF224) THEN
               I224=I
               OF224=.FALSE.
            END IF
            NR224=NR224+1
         END IF
 321     CONTINUE
      ELSE
         I224=MBMP
         NR224=MBMPL-MBMP+1
         GO TO 350
      END IF
C
C*          3.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 330  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I224,NR224,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          3.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 350  CONTINUE
C
      ISUBST=MREL-NR224+1
C
C*          3.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 360  CONTINUE
C
      DO 361 I=1,NR224
      IF(IBV(I).EQ.0) THEN
c
 362     continue
c
         if(nwtr(NP224255).eq.224255) then
            NWTDW (NP224255)=NWTDW(ISUBST)
c           NWTR  (NP224255)=NWTR (ISUBST)
            NWTRV (NP224255)=NWTRV(ISUBST)
            NWTS  (NP224255)=NWTS (ISUBST)
            CWTEN (NP224255)=CWTEN(ISUBST)
            CWTU  (NP224255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP224255=NP224255+1
         else
            NP224255=NP224255+1
            go to 362
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 361  CONTINUE
C
      GO TO 121
C
C
C     -----------------------------------------------------------------
C*          4.  PROCESS DIFFERENCE STATISTICS OPERATOR.
C               ---------------------------------------
 400  CONTINUE
C
C
C*          4.1 FIND FIRST ACCURANCE OD 223255.
C               -------------------------------
 410  CONTINUE
C
      NP225255=0
      DO 411 I=NP225000,M
      IF(NWTR(I).EQ.225255) THEN
         NP225255=I
         GO TO 420
      END IF
 411  CONTINUE
C
      GO TO 121
C
C*          4.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 420  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I225=0
         OF225=.TRUE.
         DO 421 I=NP225000,NP225255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF225) THEN
               I225=I
               OF225=.FALSE.
            END IF
            NR225=NR225+1
         END IF
 421     CONTINUE
      ELSE
         I225=MBMP
         NR225=MBMPL-MBMP+1
         GO TO 450
      END IF
C
C*          4.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 430  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I225,NR225,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          4.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 450  CONTINUE
C
      ISUBST=MREL-NR225+1
C
C*          4.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 460  CONTINUE
C
      DO 461 I=1,NR225
      IF(IBV(I).EQ.0) THEN
C
 462     continue
C
         if(nwtr(NP225255).eq.225255) then
c           NWTR  (NP225255)=NWTR (ISUBST)
C
C           CHANGE REFERENCE VALUE TO BE CENTRED AROUND ZERO
C           AND INCREASE DATA WIDTH BY 1
C
            NWTRV (NP225255)=-2**NWTDW(ISUBST)
            NWTDW (NP225255)=NWTDW(ISUBST)+1
C
            NWTS  (NP225255)=NWTS (ISUBST)
            CWTEN (NP225255)=CWTEN(ISUBST)
            CWTU  (NP225255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP225255=NP225255+1
         else
            NP225255=NP225255+1
            go to 462
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 461  CONTINUE
C
      GO TO 121
C
C     -----------------------------------------------------------------
C*          5.  PROCESS REPLACE/RETAINED OPERATOR.
C               ----------------------------------
 500  CONTINUE
C
C
C*          5.1 FIND FIRST ACCURANCE OD 232255.
C               -------------------------------
 510  CONTINUE
C
      NP232255=0
      DO 511 I=NP232000,M
      IF(NWTR(I).EQ.232255) THEN
         NP232255=I
         GO TO 520
      END IF
 511  CONTINUE
C
      GO TO 121
C
C*          5.2 COUNT NUMBER OF DATA PRESENT INDICATORS.
C               ----------------------------------------
 520  CONTINUE
C
      IF(MBMP.EQ.0) THEN
         I232=0
         OF232=.TRUE.
         DO 521 I=NP232000,NP232255
         IF(NWTR(I).EQ.031031) THEN
            IF(OF232) THEN
               I232=I
               OF232=.FALSE.
            END IF
            NR232=NR232+1
         END IF
 521     CONTINUE
      ELSE
         I232=MBMP
         NR232=MBMPL-MBMP+1
         GO TO 550
      END IF
C
C*          5.3 CALCULATE WORD AND BIT POINTER TO FIRST
C               ---------------------------------------
C               DATA PRESENT INDICATOR.
C               -----------------------
 530  CONTINUE
C
      CALL BUGETBM(KBUFL,KBUFF,KSEC3,I232,NR232,IBV,KERR)
      IF(KERR.GT.0) RETURN
C
C*          5.5 DEFINE POINTER REFERING BACK TO DATA.
C               -------------------------------------
 550  CONTINUE
C
      ISUBST=MREL-NR232+1
C
C*          5.6 REPLACE MARKERS WITH CORRESPONDING DATA WIDTHS.
C               -----------------------------------------------
C
 560  CONTINUE
C
      DO 561 I=1,NR232
      IF(IBV(I).EQ.0) THEN
c
 562     continue
c
         if(nwtr(NP232255).eq.232255) then
            NWTDW (NP232255)=NWTDW(ISUBST)
c           NWTR  (NP232255)=NWTR (ISUBST)
            NWTRV (NP232255)=NWTRV(ISUBST)
            NWTS  (NP232255)=NWTS (ISUBST)
            CWTEN (NP232255)=CWTEN(ISUBST)
            CWTU  (NP232255)=CWTU (ISUBST)
            ISUBST=ISUBST+1
            NP232255=NP232255+1
         else
            NP232255=NP232255+1
            go to 562
         end if
      ELSE
         ISUBST=ISUBST+1
      END IF
 561  CONTINUE
C
C     -----------------------------------------------------------------
 121  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)
C
C**** *BUPRCO*
C
C
C     PURPOSE.
C     --------
C
C          Process Bufr operator.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KJ*      -  pointer to array kstack
C               *KDD*     -  data descriptor
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KSTACK*  - list of descriptors
C               *KERR*    - return error code
C
C     *METHOD.
C      -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUUATB         - update augmented table b
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CWTEN*64,CWTU*24
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSTACK(*)
C
C     ------------------------------------------------------------------
C
C*          1.   DETERMINE *F *X AND *Y.
C                -----------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
C
      IF( IF.NE.2 ) THEN
         KERR=21
         PRINT*,' BUPRCO :'
         CALL BUERR(KERR)
         GO TO 400
      END IF
C
C*          1.1   CHANGE DATA WIDTH ?
C                 -------------------
 110  CONTINUE
C
      IF(IX.EQ.1) THEN
         IF(IY.EQ.0) then
            NDWINC=0
         else
           NDWINC= IY-128
         end if
         GO TO 400
      END IF
C
C*          1.2   CHANGE SCALE ?
C                 --------------
 120  CONTINUE
C
      IF(IX.EQ.2) THEN
C
C*          1.2.1  UPDATE SCALE MULTIPLIER.
C                  ------------------------
C
         IF(IY.EQ.0) then
            NSCAM=0
         ELSE
            NSCAM=IY-128
         END IF
         GO TO 400
      END IF
C
C*          1.3  CHANGE REFERENCE VALUE ?
C                ------------------------
 130  CONTINUE
C
      IF(IX.EQ.3) THEN
C
C*          1.3.1  UPDATE AUGMENTED TABLE B.
C                  -------------------------
         CALL BUUATB(KBUFL,KBUFF,KJ,IY,KSTACK,KELEM,KERR)
         GO TO 400
      END IF
C
C*          1.4   ADD ASSOCIATED FIELD ?
C                 ----------------------
 140  CONTINUE
C
      IF(IX.EQ.4) THEN
C
C*          1.4.1   UPDATE ASSOCIATED FIELD WIDTH.
C                   ------------------------------
         NAFDW= IY
         IF(IY.EQ.0) NAFDW=0
         GO TO 400
      END IF
C
C*          1.5   SIGNIFY CHARACTER ?
C                 -------------------
 150  CONTINUE
C
      IF(IX.EQ.5) THEN
C
C*          1.5.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='CHARACTERS'
         CWTU (NWT)=' '
         NWTR (NWT)= kdd      ! 0
         NWTDW(NWT)= IY * 8
         NWTEN(NWT)=658367
         NWTS (NWT)=0
         NWTRV(NWT)=0
         GO TO 400
      END IF
C
C*          1.5.2 SIGNIFY DATA WIDTH FOR IMMEDISTELY
C                 FOLLOWED LOCAL DESCRIPTOR
C
 152  CONTINUE
C
      IF(IX.EQ.6) THEN
         NWT = NWT + 1
         KJ=KJ+1
         CWTEN(NWT)='UNKNOWN'
         CWTU (NWT)='UNKNOWN'
         NWTR (NWT)= KSTACK(KJ)
         NWTDW(NWT)= IY
         NWTS (NWT)= 0
         NWTRV(NWT)= 0
         NWTEN(NWT)= NVIND
         M=NWT
C
C        CHECK IF LOCAL TABLE ENTRY KNOWN
C        --------------------------------
C
C         DO 153 I=1,JTAB
C         IF(NWTR(NWT).EQ.NTABBTR(I)) THEN
C            CWTEN(NWT)=CTABBEN(I)
C            CWTU (NWT)=CTABBU (I)
C            IF(CWTU(NWT)(1:3).EQ.'CCI') NWTEN(NWT)=65367
C            NWTS (NWT)=NTABBS (I)
C            NWTRV(NWT)=NTABBRV(I)
C            NWTDW(NWT)=NTABBDW(I)
C            NWTEN(NWT)=NTABBTR(I)
C            GO TO 400
C         END IF
C  153    CONTINUE
C
         GO TO 400
      END IF
C
C
C*          1.6   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 160  CONTINUE
C
      IF(IX.EQ.62) THEN
C
      IF(IY.EQ.0) GO TO 400
C
C*          1.7.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 222000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.KELEM) THEN
            KERR=25
            PRINT*,'BUPRCO:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.  PROCESSING NEW OPERATORS.
C               -------------------------
 200  CONTINUE
C
C
C*          2.1   DATA NOT PRESENT.
C                 -----------------
 210  CONTINUE
C
      IF(IX.EQ.21) THEN
         N221=IY
         GO TO 400
      END IF
C
C
C*          2.2   QUALITY INFORMATION FOLLOWS.
C                 ----------------------------
 220  CONTINUE
C
      IF(IX.EQ.22) THEN
C
C*          1.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='QUALITY INFORMATION FOLLOW'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 222000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
         M=M+1
         IF(M.GT.KELEM) THEN
            KERR=25
            PRINT*,'BUPRCO:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C
C*          2.3   SUBSTITUTED VALUES FOLLOWS.
C                 ---------------------------
 230  CONTINUE
C
      IF(IX.EQ.23) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='SUBSTITUTED VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=223000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.KELEM) THEN
                  KERR=25
                  PRINT*,'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='SUBSTITUTED VALUE MARKER'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.4   FIRST ORDER STATISTICS FOLLOWS.
C                 -------------------------------
 240  CONTINUE
C
      IF(IX.EQ.24) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='FIRST ORDER STATISTICS FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=224000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            IF(NAFDW.NE.0) THEN
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.KELEM) THEN
                  KERR=25
                  PRINT*,'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='FIRST ORDER STATISTICS VALUE MARKER'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.5   DIFFERENCE STATISTICAL VALUES FOLLOW.
C                 -------------------------------------
 250  CONTINUE
C
      IF(IX.EQ.25) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='DIFFERENCE STATISTICAL VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=225000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.KELEM) THEN
                  KERR=25
                  PRINT*,'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='DIFFERENCE STATISTICS VALUE MARKER'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C
C*          2.6   REPLACED/RETAINED VALUES FOLLOWS.
C                 ---------------------------------
 260  CONTINUE
C
      IF(IX.EQ.32) THEN
         IF(IY.EQ.0) THEN
            NWT=NWT+1
            CWTEN(NWT)='REPLACE/RETAINED VALUES FOLLOW'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=232000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
            if(nafdw.ne.0) then
               NWT=NWT+1
               CWTEN(NWT)='ASSOCIATED FIELD'
               CWTU (NWT)=' '
               NWTDW(NWT)= 0
               NWTR (NWT)= 0
               NWTRV(NWT)= 0
               NWTEN(NWT)= 0
               NWTS (NWT)= 0
               M=M+1
               IF(M.GT.KELEM) THEN
                  KERR=25
                  PRINT*,'BUPRCO:'
                  CALL BUERR(KERR)
                  RETURN
               END IF
            end if
            OMARKER=.TRUE.
            NWT=NWT+1
c            CWTEN(NWT)='REPLACE/RETAINED VALUE MARKER'
c            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)=KDD
c            NWTRV(NWT)= 0
c            NWTEN(NWT)= 0
c            NWTS (NWT)= 0
            M=M+1
            IF(M.GT.KELEM) THEN
               KERR=25
               PRINT*,'BUPRCO:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C*          2.7   CANCEL BACKWARD REFERENCE.
C                 --------------------------
 270  CONTINUE
C
      IF(IX.EQ.35) THEN
C
C*          1.7.2  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='CANCEL BACKWARD DATA REFERENCE'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 235000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
c         MREL=0
         M=M+1
         IF(M.GT.KELEM) THEN
            KERR=25
            PRINT*,'BUPRCO:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.8   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 280  CONTINUE
C
      IF(IX.EQ.36) THEN
C
C*          2.8.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='BACKWARD REFERENCE BIT MAP'
         CWTU (NWT)=' '
         NWTDW(NWT)= 0
         NWTR (NWT)= 236000
         NWTRV(NWT)= 0
         NWTEN(NWT)= 0
         NWTS (NWT)= 0
C
         M=M+1
         IF(M.GT.JELEM) THEN
            KERR=30
            PRINT*,'BUOPER:'
            CALL BUERR(KERR)
            RETURN
         END IF
         GO TO 400
      END IF
C
C*          2.9   DEFINE BACKWARD REFERENCE BIT MAP.
C                 ----------------------------------
 290  CONTINUE
C
      IF(IX.EQ.37) THEN
         IF(IY.EQ.0) THEN
C
C*          2.9.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='USE PREVIOUSLY DEFINED BIT MAP'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= 237000
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         ELSE
C
C*          2.9.1  ADD SPECIAL ENTRY TO WORKING TABLE.
C                  -----------------------------------
            NWT = NWT + 1
            CWTEN(NWT)='CANCEL REFERENCE TO PREDEFINED BIT MAP'
            CWTU (NWT)=' '
            NWTDW(NWT)= 0
            NWTR (NWT)= KDD
            NWTRV(NWT)= 0
            NWTEN(NWT)= 0
            NWTS (NWT)= 0
C
            M=M+1
            IF(M.GT.JELEM) THEN
               KERR=30
               PRINT*,'BUOPER:'
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 400
         END IF
      END IF
C
C     ------------------------------------------------------------------
C
 300  CONTINUE
C
      KERR=22
      PRINT*,'BUPRCO:'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
C
 400  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUPRQ(KPMISS,KPRUS,KOKEY)
C
C**** *BUPRQ*
C
C
C     PURPOSE.
C     --------
C            Sets variable KPMISS,KPRUS into common block.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUPRQ(KPMISS,KPRUS,KOKEY)*
C
C        INPUT :
C               *KPMISS*   -  Integer variable
C                            0 - default, paks max value for data width -1 bits.
C
C                            1 - paks value as missing value
C               *KPRUS*   -   An integer 
C                            0 - If data descriptors the same reuse pointers
C                            1 - Always recalculate pointers 
C               *KOKEY*    - 0 - default, packs Ecmwf RDB key
C                            1 - packs section 2 if needed but not length
C                                of bufr message in key
C
C
C     METHOD.
C     -------
C
C           During packing the value to be packed can happen to be too big
C           to fit into corresponding data width. This subroutine  allows user
C           to chose between:
C    
C            1)  pack big value as max value represented with data width -1
C                bits (default)
C            2)  pack big value as missing value
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
C          M. DRAGOSAVAC    *ECMWF*       15/01/95.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      COMMON /BCPRQ/ NPMISS,NPRUS,NOKEY 
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      IF(KPMISS.LT.0.AND.KPMISS.GT.1) KPMISS=0
      IF(KPRUS.LT.0.AND.KPRUS.GT.1)   KPRUS=0
      IF(KOKEY.LT.0.AND.KOKEY.GT.1)   NOKEY=0
      NPMISS=KPMISS
      NPRUS=KPRUS
      NOKEY=KOKEY
C
      RETURN
      END
      SUBROUTINE BUPRS0(KSEC0)
C
C**** *BUPRS0*
C
C
C     PURPOSE.
C     --------
C           Print section 0 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS0(KSEC0)*
C
C        INPUT :
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSEC0(JSEC0)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 0.
C                ----------------
 100  CONTINUE
C
      WRITE(*,'(1H1)')
C
      WRITE(*,'(1H ,A)')    '         BUFR SECTION 0    '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A,I5)') 'Length of section 0 (bytes)         ',
     1                       KSEC0(1)
      WRITE(*,'(1H ,A,I5)') 'Total length of Bufr message (bytes)',
     1                       KSEC0(2)
      WRITE(*,'(1H ,A,I5)') 'Bufr Edition number                 ',
     1                       KSEC0(3)
C
      RETURN
      END
      SUBROUTINE BUPRS1(KSEC1)
C
C**** *BUPRS1*
C
C
C     PURPOSE.
C     --------
C           Print section 1 of bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS1(KSEC1)*
C
C        INPUT :
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C
C                            For Bufr Edition >= 3
C
C                            KSEC1(16) - Originating sub-centre
C                            KSEC1(18) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C                            
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSEC1(JSEC1)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 1.
C                ----------------
 100  CONTINUE
C
      WRITE(*,'(1H1)')
C
      WRITE(*,'(1H ,A)')    '        BUFR SECTION 1    '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A,I5)') 'Length of section 1 (bytes)    ', KSEC1( 1)
      WRITE(*,'(1H ,A,I5)') 'Bufr Edition number            ', KSEC1( 2)
      if(ksec1(2).ge.3) then
      WRITE(*,'(1H ,A,I5)') 'Originating sub-centre         ', KSEC1(16)
      end if
      WRITE(*,'(1H ,A,I5)') 'Originating centre             ', KSEC1( 3)
      WRITE(*,'(1H ,A,I5)') 'Update sequence number         ', KSEC1( 4)
      WRITE(*,'(1H ,A,I5)') 'Flag (presence of section 2)   ', KSEC1( 5)
      WRITE(*,'(1H ,A,I5)') 'Bufr message type              ', KSEC1( 6)
      WRITE(*,'(1H ,A,I5)') 'Bufr message subtype           ', KSEC1( 7)
      WRITE(*,'(1H ,A,I5)') 'Version number of local table  ', KSEC1( 8)
      WRITE(*,'(1H ,A,I5)') 'Year                           ', KSEC1( 9)
      WRITE(*,'(1H ,A,I5)') 'Month                          ', KSEC1(10)
      WRITE(*,'(1H ,A,I5)') 'Day                            ', KSEC1(11)
      WRITE(*,'(1H ,A,I5)') 'Hour                           ', KSEC1(12)
      WRITE(*,'(1H ,A,I5)') 'Minute                         ', KSEC1(13)
      WRITE(*,'(1H ,A,I5)') 'Version number of Master table ', KSEC1(15)
      WRITE(*,'(1H ,A,I5)') 'Bufr Master table              ', KSEC1(14)
C
      RETURN
      END
      SUBROUTINE BUPRS2(KSUP,KEY)
C
C**** *BUPRS2*
C
C
C     PURPOSE.
C     --------
C           Print section 2 of bufr message (expanded RDB key).
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS2(KSUP,KEY)*
C
C        INPUT :
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KEY*     -  array containing section 2 information
C                            KEY( 1)-- length of section 2 (bytes)
C                            KEY( 2)-- RDB type
C                            KEY( 3)-- RDB subtype
C                            KEY( 4)-- year
C                            KEY( 5)-- month
C                            KEY( 6)-- day
C                            KEY( 7)-- hour
C                            KEY( 8)-- minute
C                            KEY( 9)-- second
C                            KEY(10)-- longitude1
C                            KEY(11)-- latitude1
C                            KEY(12)-- longitude2
C                            KEY(13)-- latitude2
C                            KEY(14)-- number of subsets
C                            KEY(15)-- ident (numeric)
C                            KEY(16)-- ident ( CCITTIA5) one character
C                            KEY(17)-- ident ( CCITTIA5) one character
C                            KEY(18)-- ident ( CCITTIA5) one character
C                            KEY(19)-- ident ( CCITTIA5) one character
C                            KEY(20)-- ident ( CCITTIA5) one character
C                            KEY(21)-- ident ( CCITTIA5) one character
C                            KEY(22)-- ident ( CCITTIA5) one character
C                            KEY(23)-- ident ( CCITTIA5) one character
C                            KEY(24)-- ident ( CCITTIA5) one character
C                            KEY(25)-- total Bufr message length
C                            KEY(26)-- day    (RDB insertion)
C                            KEY(27)-- hour   (RDB insertion)
C                            KEY(28)-- minute (RDB insertion)
C                            KEY(29)-- second (RDB insertion)
C                            KEY(30)-- day    (MDB insertion)
C                            KEY(31)-- hour   (MDB insertion)
C                            KEY(32)-- minute (MDB insertion)
C                            KEY(33)-- second (MDB insertion)
C                            KEY(34)-- correction number
C                            KEY(35)-- part
C                            KEY(36)-- 0
C                            KEY(37)-- correction number
C                            KEY(38)-- part
C                            KEY(39)-- 0
C                            KEY(40)-- correction number
C                            KEY(41)-- part
C                            KEY(42)-- 0
C                            KEY(43)-- correction number
C                            KEY(44)-- part
C                            KEY(45)-- 0
C                            KEY(46)-- the lowest Q/C % confidence
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSUP(JSUP),KEY(JKEY)
C
      CHARACTER*9 CIDENT
      CHARACTER*13 YFM
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 2.
C                ----------------
 100  CONTINUE
C
      YFM='(1H ,A,TR0,A)'
C
      IF(KSUP(2).LE.1) THEN
         PRINT*,'Prtkey : RDB key not defined in section 2.'
         RETURN
      END IF
C
      WRITE(*,'(1H1)')
C
      WRITE(*,'(1H ,A)')       '        BUFR SECTION 2    '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A,I9)')    'Length of section 2       ', KEY(1)
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A)')       '      Report Data Base Key  '
      WRITE(*,'(1H )')
C
      IKTYPE=0
      IF(KEY(2).EQ.2) IKTYPE=2
      IF(KEY(2).EQ.3) IKTYPE=2
      IF(KEY(2).EQ.12)IKTYPE=2
      IF(KEY(2).EQ.08)IKTYPE=2
      IF(IKTYPE.EQ.0.AND.KSUP(6).GT.1) IKTYPE=2
C
      IF(IKTYPE.EQ.2) THEN
C      IF(KEY(2).EQ.2.OR.KEY(2).EQ.3.OR.KEY(2).EQ.12) THEN
C
         WRITE(*,'(1H ,A,I9)') 'RDB data type             ', KEY(2)
         WRITE(*,'(1H ,A,I9)') 'RDB data subtype          ', KEY(3)
         WRITE(*,'(1H ,A,I9)') 'Year                      ', KEY(4)
         WRITE(*,'(1H ,A,I9)') 'Month                     ', KEY(5)
         WRITE(*,'(1H ,A,I9)') 'Day                       ', KEY(6)
         WRITE(*,'(1H ,A,I9)') 'Hour                      ', KEY(7)
         WRITE(*,'(1H ,A,I9)') 'Minute                    ', KEY(8)
         WRITE(*,'(1H ,A,I9)') 'Second                    ', KEY(9)
         RLAT1=(KEY(11)-9000000)/100000.
         RLON1=(KEY(10)-18000000)/100000.
         WRITE(*,'(1H ,A,F9.2)')'Latitude  1               ', RLAT1
         WRITE(*,'(1H ,A,F9.2)')'Longitude 1               ', RLON1
         RLAT2=(KEY(13)-9000000)/100000.
         RLON2=(KEY(12)-18000000)/100000.
         WRITE(*,'(1H ,A,F9.2)')'Latitude  2               ', RLAT2
         WRITE(*,'(1H ,A,F9.2)')'Longitude 2               ', RLON2
         WRITE(*,'(1H ,A,I9)') 'Number of observations    ', KEY(14)
         WRITE(*,'(1H ,A,I9)') 'Identifier                ', KEY(15)
         WRITE(*,'(1H ,A,I9)') 'Total Bufr message length ', KEY(25)
         WRITE(*,'(1H ,A,I9)') 'Day    (RDB insertion)    ', KEY(26)
         WRITE(*,'(1H ,A,I9)') 'Hour   (RDB insertion)    ', KEY(27)
         WRITE(*,'(1H ,A,I9)') 'Minute( (RDB insertion)   ', KEY(28)
         WRITE(*,'(1H ,A,I9)') 'Second (RDB insertion)    ', KEY(29)
         WRITE(*,'(1H ,A,I9)') 'Day    (MDB arrival)      ', KEY(30)
         WRITE(*,'(1H ,A,I9)') 'Hour   (MDB arrival)      ', KEY(31)
         WRITE(*,'(1H ,A,I9)') 'Minute (MDB arrival)      ', KEY(32)
         WRITE(*,'(1H ,A,I9)') 'Second (MDB arrival       ', KEY(33)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(34)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(35)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(37)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(38)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(40)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(41)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(43)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(44)
         WRITE(*,'(1H ,A,I9)') 'Quality control % conf    ', KEY(46)
      ELSE
         WRITE(*,'(1H ,A,I9)') 'RDB data type             ', KEY(2)
         WRITE(*,'(1H ,A,I9)') 'RDB data subtype          ', KEY(3)
         WRITE(*,'(1H ,A,I9)') 'Year                      ', KEY(4)
         WRITE(*,'(1H ,A,I9)') 'Month                     ', KEY(5)
         WRITE(*,'(1H ,A,I9)') 'Day                       ', KEY(6)
         WRITE(*,'(1H ,A,I9)') 'Hour                      ', KEY(7)
         WRITE(*,'(1H ,A,I9)') 'Minute                    ', KEY(8)
         WRITE(*,'(1H ,A,I9)') 'Second                    ', KEY(9)
         RLAT1=(KEY(11)-9000000)/100000.
         RLON1=(KEY(10)-18000000)/100000.
         WRITE(*,'(1H ,A,F9.2)')'Latitude  1               ', RLAT1
         WRITE(*,'(1H ,A,F9.2)')'Longitude 1               ', RLON1
         IDD=0
         CIDENT=' '
         DO 201 ID=16,24
         IDD=IDD+1
         CIDENT(IDD:IDD)=CHAR(KEY(ID))
 201     CONTINUE
         IDD=INDEX(CIDENT,' ')
         if(idd.eq.0) idd=10
         IDD=10-IDD
         WRITE(YFM(10:10),'(I1)',ERR=202) IDD
         GO TO 203
 202     YFM(10:10)='9'
 203     WRITE(*,FMT=YFM)      'Identifer                 ', CIDENT
         WRITE(*,'(1H ,A,I9)') 'Total Bufr message length ', KEY(25)
         WRITE(*,'(1H ,A,I9)') 'Day    (RDB insertion)    ', KEY(26)
         WRITE(*,'(1H ,A,I9)') 'Hour   (RDB insertion)    ', KEY(27)
         WRITE(*,'(1H ,A,I9)') 'Minute (RDB insertion)    ', KEY(28)
         WRITE(*,'(1H ,A,I9)') 'Second (RDB insertion)    ', KEY(29)
         WRITE(*,'(1H ,A,I9)') 'Day    (MDB arrival)      ', KEY(30)
         WRITE(*,'(1H ,A,I9)') 'Hour   (MDB arrival)      ', KEY(31)
         WRITE(*,'(1H ,A,I9)') 'Minute (MDB arrival)      ', KEY(32)
         WRITE(*,'(1H ,A,I9)') 'Second (MDB arrival       ', KEY(33)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(34)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(35)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(37)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(38)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(40)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(41)
         WRITE(*,'(1H ,A,I9)') 'Correction number         ', KEY(43)
         WRITE(*,'(1H ,A,I9)') 'Part of message           ', KEY(44)
         WRITE(*,'(1H ,A,I9)') 'Quality control % conf    ', KEY(46)
      END IF
C
      RETURN
      END
      SUBROUTINE BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KELEM,CNAMES)
C
C**** *BUPRS3*
C
C
C     PURPOSE.
C     --------
C           Print section 3 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,
C                          KELEM,CNAMES)*
C
C        INPUT :
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KTDLEN*  -  number of data descriptors in section 3
C               *KTDLST*  -  array containing data descriptors in section 3
C               *KTDEXL*  -  number of entries in list of expanded data
C                            descriptors
C               *KTDEXP*  -  array containig expanded data descriptors
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      DIMENSION KSEC3(JSEC3)
      DIMENSION KTDLST(KTDLEN),KTDEXP(KTDEXL)
C
      CHARACTER*64 CNAMES(KELEM)
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT SECTION 3.
C                ----------------
 100  CONTINUE
C
      WRITE(*,'(1H1)')
C
      WRITE(*,'(1H ,A)')    '         BUFR SECTION 3    '
      WRITE(*,'(1H )')
      WRITE(*,'(1H ,A,I5)') 'Length of section 3 (bytes)         ',
     1                       KSEC3(1)
      WRITE(*,'(1H ,A,I5)') 'Reserved                            ',
     1                       KSEC3(2)
      WRITE(*,'(1H ,A,I5)') 'Number of data subsets              ',
     1                       KSEC3(3)
      WRITE(*,'(1H ,A,I5)') 'Flag (data type/data compression)   ',
     1                       KSEC3(4)
C
      WRITE(*,'(1H ,//)')
      WRITE(*,'(1H ,A)')    '       Data descriptors (unexpanded)'
C
      WRITE(*,'(1H )')
      DO 110 I=1,KTDLEN
       WRITE(*,'(1H ,I4,2X,I6.6)') I,KTDLST(I)
 110  CONTINUE
C
      WRITE(*,'(1H ,/)')
      WRITE(*,'(1H ,A)')    '       Data descriptors (expanded)'
      WRITE(*,'(1H )')
      DO 120 I=1,KTDEXL
       WRITE(*,'(1H ,I4,2X,I6.6,2X,A)') I,KTDEXP(I),CNAMES(I)
 120  CONTINUE

      RETURN
      END
      SUBROUTINE BUPRT(K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
     1                 CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)
C
C**** *BUPRT*
C
C
C     PURPOSE.
C     --------
C           Print expanded Bufr messag.
C
C
C**   INTERFACE.
C     ----------
C
C           *CALL* *BUPRT(K,KSUB1,KSUB2,KELEM,CNAMES,CUNITS,
C                         CVALS,KVALS,VALUES,KSUP,KSEC1,KERR)*
C
C        INPUT :
C               *K*       -  switch to print with/witout content of code tables
C                            0  - no  code table content
C                            1  - yes code table content
C               *KSUB1*   -  starting subset
C               *KSUB2*   -  ending subset
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *CVALS*   -  character array containing bufr code table
C                            entries
C               *KVALS*   -  dimension of VALUES array
C               *VALUES*  -  real array (expanded data values)
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(BYTE by BYTE)
C        OUTPUT:
C               *KERR*    -  returned error code
C
C
C
C
C     METHOD.
C     -------
C
C            NONE
C
C     EXTERNALS.
C     ----------
C
C            NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER YCHAR*30,YLONG*320
C
      DIMENSION KSUP(JSUP),KSEC1(JSEC1)
      DIMENSION VALUES(KVALS)
C
C
C     ------------------------------------------------------------------
C
C*          1.   PRINT BUFR MESSAGE.
C                -------------------
 100  CONTINUE
C
      KERR=0
C
      ISUB1=KSUB1
      ISUB2=KSUB2
      IF(ISUB1.LE.0.OR.ISUB2.LE.0) THEN
         WRITE(*,'(A)')    ' Warning - NEGATIVE KSUB1 OR KSUB2.'
         WRITE(*,'(A,I5)') ' Warning - number of subsets is ',KSUP(6)
         RETURN
      END IF
      IF(ISUB1.GT.KSUP(6)) THEN
         WRITE(*,'(A,I5)') ' Warning - number of subsets is ',KSUP(6)
         RETURN
      END IF
      IF(ISUB2.GT.KSUP(6)) THEN
         ISUB2=KSUP(6)
         WRITE(*,'(A,I5)') ' Warning - KSUB2 replaced by ',KSUP(6)
      END IF
C
      JQCP1= 0
C
      IF(K.EQ.0) THEN
         JQPR=0
         JQUA=0
         JQC=0
c          DO 171 J171=1,KSUP(5)
c          IF(CNAMES(J171)(1:8).EQ.'DATA PRE') THEN
c            JQPR=J171
c            GO TO 172
c          END IF
c  171     CONTINUE
c  172     DO 173 J173=1,KSUP(5)
c          IF(CNAMES(J173)(1:9).EQ.'QUALITY I') JQUA=J173
c  173     CONTINUE
c          DO 174 J174=1,KSUP(5)
c          IF(CNAMES(J174)(1:3).EQ.'% C') THEN
c            JQC =J174
c            GO TO 175
c          END IF
c  174     CONTINUE
c 
c  175     CONTINUE
C
c         WRITE(*,'(1H1)')
C
c         WRITE(*,'(1H ,A)')    'EXPANDED BUFR MESSAGE  '
c         WRITE(*,'(1H ,//)')
c         WRITE(*,'(1H ,A,I6)') 'BUFR MESSAGE  DATA TYPE   ',KSEC1(6)
c         WRITE(*,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
c         WRITE(*,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
         NTYPE=KSEC1(7)
         IF(JQUA.EQ.0) THEN      !if(jqua.NE.0) then
            JQUA=KSUP(5)
C
            DO 103 JB=ISUB1,ISUB2
C
            iln=0
            WRITE(*,'(1H )')
C
            DO 104 JA=1,JQUA
C
            iln=iln+1
            JAJB=JA+(JB-1)*KELEM
C
            if(VALUES(JAJB).ge.(rvind-eps).and.
     1         VALUES(JAJB).le.(rvind+eps)) then
               WRITE(*,9918) iln,CNAMES(JA),CUNITS(JA)
            ELSE
               IF(CUNITS(JA)(1:4).EQ.'CCIT') THEN
                  I=NINT(VALUES(JAJB)/1000)
                  NCHAR=VALUES(JAJB)-I*1000
                  NW=NCHAR/80
                  NWOFF=NCHAR-NW*80
                  IF(NWOFF.NE.0) NW=NW+1
C
                  YLONG=' '
                  YLONG(1:80)=CVALS(I)
C
                  II=I
                  DO 125 JC=1,NW-1
                  II=II+1
                  KF=JC*80+1
                  KL=(JC+1)*80
                  YLONG(KF:KL)=CVALS(II)
 125              CONTINUE
C
                  NLINE=NCHAR/30
                  IDIF =NCHAR-NLINE*30
                  IF(IDIF.NE.0) NLINE=NLINE+1
                  YCHAR=' '
                  YCHAR=YLONG(1:30)
C
                  WRITE(*,9919)iln,CNAMES(JA),VALUES(JAJB),
     1                         CUNITS(JA),YCHAR
C
                  IF(NLINE.GT.1) THEN
                     DO 130 JJ=1,NLINE-1
C
                     K2=JJ*30+1
                     K1=(JJ+1)*30
                     YCHAR=' '
                     YCHAR=YLONG(K2:K1)
C
                     WRITE(*,9920) YCHAR
 130                 CONTINUE
C
                   END IF
               ELSE
                  WRITE(*,9917) iln,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
               END IF
            END IF
C
 104        CONTINUE
 103        CONTINUE
C
         ELSE
            JQPRM1=JQPR-1
            JQC=JQC-1
C
            DO 101 JB=ISUB1,ISUB2
C
            iln=0
            JQCP1=0
C
            WRITE(*,'(1H )')
C
            DO 102 JA=1,JQUA-1
C
            iln=iln+1
            JAJB=JA+(JB-1)*KELEM
            JQPJB=JQPRM1+JA+(JB-1)*KELEM
C
            IF(VALUES(JQPJB).EQ.0.0) THEN
               JQCP1=JQCP1+1
               JQCPP1=JQC+JQCP1+(JB-1)*KELEM
               if(VALUES(JAJB).ge.(rvind-eps).and.
     1             VALUES(JAJB).le.(rvind+eps)) then
                  WRITE(*,9918) iln,CNAMES(JA),CUNITS(JA)
               ELSE
                  WRITE(*,9916) iln,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA),
     1            CNAMES(JQC+JQCP1),VALUES(JQCPP1),
     1            CUNITS(JQC+JQCP1)
               END IF
            ELSE
               if(VALUES(JAJB).ge.(rvind-eps).and.
     1            VALUES(JAJB).le.(rvind+eps)) then
                  WRITE(*,9918) iln,CNAMES(JA),CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               ELSE
                  WRITE(*,9917) iln,CNAMES(JA),VALUES(JAJB),
     1            CUNITS(JA)
                  IF(NTYPE.EQ.5.OR.NTYPE.EQ.3) JQCP1=JQCP1+1
               END IF
            END IF
C
C
 102        CONTINUE
 101        CONTINUE
C
         END IF
      END IF
C
      IF(K.EQ.1) THEN
C
C---------------------------------------------------------------------
          WRITE(*,'(1H1)')
C
          WRITE(*,'(1H ,A)') 'WARNING : Printing content of code'//
     1   ' tables not yet implemented.'
          RETURN
C---------------------------------------------------------------------
C
C          WRITE(*,'(1H ,A)')    'EXPANDED BUFR MESSAGE  '
C          WRITE(*,'(1H ,//)')
C          WRITE(*,'(1H ,A,I6)') 'RDB DATA TYPE             ',KSEC1(6)
C          WRITE(*,'(1H ,A,I6)') 'RDB DATA SUBTYPE          ',KSEC1(7)
C          WRITE(*,'(1H ,A,I6)') 'TOTAL BUFR LENGTH (BYTES) ',KSUP(8)
C
C          DO 150 JB=1,KSUP(6)
C
C          WRITE(*,'(1H )')
C
C          DO 160 JA=1,KSUP(5)
C
C          JAJB=JA+(JB-1)*KELEM
C
C          if(VALUES(JAJB).ge.(rvind-eps).and.
C     1       VALUES(JAJB).le.(rvind+eps)) then
C             WRITE(*,9903) CNAMES(JA)(1:32),CUNITS(JA)
C             WRITE(*,9903) CNAMES(JA)(33:64)
C          ELSE
C             IF(CUNITS(JA)(1:10).EQ.'CODE TABLE'.OR.
C     1         CUNITS(JA)(1:9) .EQ.'CCITTIA5'     ) THEN
C                I=NINT(VALUES(JAJB)/1000)
C                NCHAR=VALUES(JAJB)-I*1000
C                NW=NCHAR/80
C                NWOFF=NCHAR-NW*80
C                IF(NWOFF.NE.0) NW=NW+1
Cc
C                YLONG(1:80)=CVALS(I)
C
C                II=I
C                DO 165 JC=1,NW-1
C                II=II+1
C                KF=JC*80+1
C                KL=(JC+1)*80
C                YLONG(KF:KL)=CVALS(II)
C  165           CONTINUE
C
C                NLINE=NCHAR/30
C                IDIF =NCHAR-NLINE*30
C                IF(IDIF.NE.0) NLINE=NLINE+1
C                YCHAR=YLONG(1:30)
C
C                WRITE(*,9904)CNAMES(JA)(1:32),VALUES(JAJB),
C     1         CUNITS(JA),YCHAR
C                WRITE(*,9904)CNAMES(JA)(33:64)
C
C                IF(NLINE.GT.1) THEN
C                   DO 170 JJ=1,NLINE-1
C
C                   K2=JJ*30+1
C                   K1=(JJ+1)*30
C                   YCHAR=YLONG(K2:K1)
C
C                   WRITE(*,9905) YCHAR
C  170              CONTINUE
C
C                END IF
C             ELSE
C                WRITE(*,9906) CNAMES(JA)(1:32),VALUES(JAJB),
C     1                       CUNITS(JA)
C                WRITE(*,9906) CNAMES(JA)(33:64)
C             END IF
C          END IF
C
C  160     CONTINUE
C  150     CONTINUE
C
        END IF
C
C
C       RETURN
C
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
C     ------------------------------------------------------------------
 9903 FORMAT(1H ,A,'     MISSING',2X,A)
 9904 FORMAT(1H ,A,F20.4,2X,A,2X,A)
 9905 FORMAT(1H ,100X,A)
 9906 FORMAT(1H ,i4,1x,A,F14.4,2X,A)
 9916 FORMAT(1H ,i4,1x,A15,1X,F20.4,1X,A20,1X,A15,1X,F3.0,1X,A15)
 9917 FORMAT(1H ,i4,1x,A15,1X,F20.4,1X,A24)
 9918 FORMAT(1H ,i4,1x,A15,1X,'             MISSING',1X,A24)
 9919 FORMAT(1H ,i4,1x,A15,1X,F20.4,1X,A24,1X,A)
 9920 FORMAT(1H ,62X,A)
      END
      SUBROUTINE BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
C
C**** *BUPRTBOX*
C
C
C     PURPOSE.
C     --------
C
C
C
C**   INTERFACE.
C     ----------
C
C               *call* *buprtbox(kbox,kapp,klen,kboxr,vals,cboxn,cboxu)*
C
C        INPUT :
C               *kbox*    -  number of rows      
C               *kapp*    -  number of columns
C               *klen*    -  offset for start of next column
C               *kboxr*   -  array containing Bufr table B reference numbers
C               *vals*    -  array containing unpacked values
C               *cboxn*   -  array containing element names
C               *cboxu*   -  array containing element units
C
C     METHOD.
C     -------
C
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/94.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
      parameter(JELEM=40000)
      dimension ioper(100),rprint(60)
      dimension kboxr(360000),vals(360000),ibval(jelem),ibprint(60)
      character*64 cboxn(40000)
      character*24 cboxu(40000)
C
C
C     ------------------------------------------------------------------
C*                 1. Print boxed expanded bufr message
C                     ---------------------------------
 100  continue
C
      if(kbox.le.6) then
         print*,'There is no usefull data to be printed.'
         kbox=0
        return
      end if
c
      if(kapp.gt.60) then
         print*,'There is more than 60 applications in the data'
         print*,'Only first 60 will be processed'
         kapp=60
      end if
c
      if(kapp.gt.1) then
         irep=(kapp-1)/10
         ioff=(kapp-1)-irep*10
         if(ioff.ne.0) irep=irep+1
      else
         irep=1
         ioff=0
      end if
c
      ist=2
      iend=11
c      if(irep.eq.1.and.kapp.eq.1) iend=ioff+1
      if(irep.eq.1) iend=ioff+1
c
      do 2005 j=1,irep
c
      print*,' '
      do 2002 i=1,kbox
      iiii=1
      rprint(iiii)=vals(i)
c
      do 2003 ii=ist,iend
      iiii=iiii+1
      iii=i+(ii-1)*klen      
      rprint(iiii)=vals(iii)
      ibprint(iiii)=kboxr(iii)
 2003 continue
c     write(*,'(1h ,i4,1x,a32,1x,15(1x,i6,1x,f8.1))') 
c    1        i,cboxn(i),(ibprint(nn),rprint(nn),nn=1,kapp)
      write(*,'(1h ,i4,1x,a32,1x,f14.1,30(1x,f8.1))')
     1        i,cboxn(i),(rprint(nn),nn=1,iiii)
 2002 continue
c
      if(ioff.ne.0.and.j.eq.(irep-1)) then
         ist=iend+1
         iend=iend+ioff
      else
         ist=iend+1
         iend=iend+10
      end if
c
 2005 continue
c
c
      return
      end
      SUBROUTINE BUREP(KPT,KDLEN,KDATA,KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *BUREP*
C
C
C     PURPOSE.
C     --------
C          Resolve data descriptor replication problem.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUREP(KPT,KDLEN,KDATA,KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C               *KPT*      - pointer too kdata array
C               *KDLEN*    -  dimension of KDATA array
C               *KDATA*    -  array containing data needed for data descriptor
C                            expansion
C               *KDD*      - data descriptor
C        OUTPUT:
C               *KJ*       - pointer to kstack array
C               *KJ1*      - pointer to last element in kstack
C               *KSTACK*   - list of data descriptors
C               *KERR*     - return code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUETDR            - resolve table D reference
C          BUEPWT            - update working tables
C          GBYTE             - unpack bit pathern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CWTEN*64,CWTU*24
      DIMENSION ILIST(JELEM)
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
C
C     ------------------------------------------------------------------
C
C*          1.   STORE K, NUMBER OF DESCRIPTORS TO BE REPLICATED.
C                ------------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
      K   = IX
      IF(IY.EQ.0) ODREPF=.TRUE.
C
C*          1.1   DELAYED REPLICATION ?
C                 ---------------------
 110  CONTINUE
C
      IF( IY .NE. 0 ) THEN
C
C*          1.2   STORE NUMBER OF DESCRIPTORS, K, AND REPLICATION
C                 -----------------------------------------------
C                 FACTOR JR.
C                 ----------
 120     CONTINUE
C
         JR = IY
         GO TO 500
      END IF
C
C     ------------------------------------------------------------------
C
C*          2.   GET NEXT DESCRIPTOR.
C                --------------------
 200  CONTINUE
C
      KJ =KJ + 1
      KDD= KSTACK(KJ)
C
C     ------------------------------------------------------------------
C
C*          2.1  REPLICATION FACTOR ?
C                --------------------
 210  CONTINUE
C
      IF(KDD.NE.31001.AND.KDD.NE.31002.and.
     1   kdd.ne.31000.and.
     1   kdd.ne.31011.and.kdd.ne.31012 )THEN
C
C*          2.1.1  SEQUENCE DESCRIPTOR ?
C                  ---------------------
C
         IF=KDD/100000
C
         IF( IF.EQ.3) THEN
C
C*          2.1.1.1  SOLVE TABLE D REFERENCE.
C                    ------------------------
            CALL BUETDR(KJ,KJ1,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
            GO TO 200
         END IF
C
         IF( IF.EQ.2) THEN
            CALL BUOPER(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 200
         END IF
C
         KERR=36
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
C*          3.  UPDATE WORKING TABLE.
C               ---------------------
 300  CONTINUE
C
      if(kdd.eq.31031.or.kdd.eq.31192) then
         nwt=nwt+1
         nwtr(nwt)=kdd
         nwts(nwt)=0
         nwtrv(nwt)=0
         nwtdw(nwt)=1
         m=m+1
      elseif(kdd.eq.33007.or.kdd.eq.63192) then
         nwt=nwt+1
         nwtr(nwt)=kdd
         nwts(nwt)=0
         nwtrv(nwt)=0
         nwtdw(nwt)=7
         m=m+1
      else
         CALL BUEPWT (KDD,KERR)
         IF(KERR.GT.0) RETURN
      end if
C
C     ------------------------------------------------------------------
C
C*          4.  STORE JR, THE REPLICATION FACTOR FROM DATA.
C               ------------------------------------------------------
 400  CONTINUE
C
      KPT=KPT+1
      IF(KPT.GT.KDLEN) THEN
         KERR=31
         CALL BUERR(KERR)
         RETURN
      END IF
      JR=KDATA(KPT)
c      IF(JR.EQ.0) THEN
c         KERR=18
c         CALL BUERR(KERR)
c         RETURN
c      END IF
C
      JRTK=JR*K+KJ1-K
      IF(JRTK.GT.JELEM) THEN
         KERR=30
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     CHECK IF NEXT DESCRIPTOR CANCEL OPERATOR FOR DELAYED
C     REPLICATION
C
      IIIF=KSTACK(KJ+1)/100000
      IIII=KSTACK(KJ+1)-IIIF*100000
      IIIX=IIII/1000
      IIIY=IIII-IIIX*1000
C
      IF(IIIF.EQ.2.AND.IIIY.EQ.0) THEN
         KJ=KJ+1
         CALL BUOPER(KPT,KDLEN,KDATA,KJ,KSTACK(KJ),KSTACK,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
C     ------------------------------------------------------------------
C*          5.  GET NEXT K DESCRIPTORS.
C               -----------------------
 500  CONTINUE
C
      DO 501 J=1,K
C
      ILIST(J)=KSTACK(KJ+J)
C
 501  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.  ADD JR TIMES K DESCRIPTORS IN PLACE OF K
C               ----------------------------------------
C               DESCRIPTORS OBTAINED.
C               ---------------------
 600  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.1  PUSH DOWN DESCRIPTORS IN KSTACK FOR (JR-1)*K PLACES
C                ---------------------------------------------------
C                STARTING AT KJ1 AND ENDING AT KJ+K.
C                -----------------------------------
 610  CONTINUE
C
      JRKM1=(JR-1)*K
C
      DO 611 J=KJ1,KJ+K,-1
C
      KSTACK(J+JRKM1)=KSTACK(J)
C
 611  CONTINUE
C
C*          6.2  INSERT LIST IN THE STACK.
C                -------------------------
 620  CONTINUE
C
      DO 622 J=1,JR
C
      KJJM1K=KJ+(J-1)*K
C
      DO 623 J1=1,K
C
      KSTACK(KJJM1K+J1)=ILIST(J1)
C
 623  CONTINUE
 622  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.3  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C                ----------------------------------------
 630  CONTINUE
C
      KJ1 = KJ1  + (JR-1)*K
C
C     ------------------------------------------------------------------
C*          6.4  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 640  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C
C     ------------------------------------------------------------------
 700  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUREPC(KPT,KDLEN,KDATA,KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *BUREPC*
C
C
C     PURPOSE.
C     --------
C          Resolve data descriptor replication problem.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUREPC(KPT,KDLEN,KDATA,KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C               *KPT*      - pointer too kdata array
C               *KDLEN*    -  dimension of KDATA array
C               *KDATA*    -  array containing data needed for data descriptor
C                            expansion
C               *KDD*      - data descriptor
C        OUTPUT:
C               *KJ*       - pointer to kstack array
C               *KJ1*      - pointer to last element in kstack
C               *KSTACK*   - list of data descriptors
C               *KERR*     - return code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUETDR            - resolve table D reference
C          BUEPWT            - update working tables
C          GBYTE             - unpack bit pathern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C               CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CWTEN*64,CWTU*24
      DIMENSION ILIST(JELEM)
      DIMENSION KSTACK(*)
      DIMENSION KDATA(KDLEN)
C
C     ------------------------------------------------------------------
C
C*          1.   STORE K, NUMBER OF DESCRIPTORS TO BE REPLICATED.
C                ------------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
      K   = IX
      if(iy.eq.0) odrepf=.true.
C
C*          1.1   DELAYED REPLICATION ?
C                 ---------------------
 110  CONTINUE
C
      IF( IY .NE. 0 ) THEN
C
C*          1.2   STORE NUMBER OF DESCRIPTORS, K, AND REPLICATION
C                 -----------------------------------------------
C                 FACTOR JR.
C                 ----------
 120     CONTINUE
C
         JR = IY
         GO TO 500
      END IF
C
C     ------------------------------------------------------------------
C
C*          2.   GET NEXT DESCRIPTOR.
C                --------------------
 200  CONTINUE
C
      KJ =KJ + 1
      KDD= KSTACK(KJ)
C
C     ------------------------------------------------------------------
C
C*          2.1  REPLICATION FACTOR ?
C                --------------------
 210  CONTINUE
C
      IF(KDD.NE.31001.AND.KDD.NE.31002.and.
     1   KDD.NE.31000.AND.
     1   kdd.ne.31011.and.kdd.ne.31012 )THEN
C
C*          2.1.1  SEQUENCE DESCRIPTOR ?
C                  ---------------------
C
         IF=KDD/100000
C
         IF( IF.EQ.3) THEN
C
C*          2.1.1.1  SOLVE TABLE D REFERENCE.
C                    ------------------------
            CALL BUETDR(KJ,KJ1,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
            GO TO 200
         END IF
C
         IF( IF.EQ.2) THEN
            CALL BUOPERC(KPT,KDLEN,KDATA,KJ,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 200
         END IF
C
         KERR=36
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
C*          3.  UPDATE WORKING TABLE.
C               ---------------------
 300  CONTINUE
C
      if(kdd.eq.31031.or.kdd.eq.31192) then
         nwt=nwt+1
         nwtr(nwt)=kdd
         nwts(nwt)=0
         nwtrv(nwt)=0
         nwtdw(nwt)=1
         m=m+1
      elseif(kdd.eq.33007.or.kdd.eq.63192) then
         nwt=nwt+1
         nwtr(nwt)=kdd
         nwts(nwt)=0
         nwtrv(nwt)=0
         nwtdw(nwt)=7
         m=m+1
      else
         CALL BUEPWTC (KDD,KERR)
         IF(KERR.GT.0) RETURN
      end if
C
C     ------------------------------------------------------------------
C
C*          4.  STORE JR, THE REPLICATION FACTOR FROM DATA.
C               ------------------------------------------------------
 400  CONTINUE
C
      KPT=KPT+1
      IF(KPT.GT.KDLEN) THEN
         KERR=31
         CALL BUERR(KERR)
         RETURN
      END IF
      JR=KDATA(KPT)
c      IF(JR.EQ.0) THEN
c         KERR=18
c         CALL BUERR(KERR)
c         RETURN
c      END IF
C
      IF(JR.EQ.0) THEN
         KJ=KJ+K
         GO TO 640
      END IF
C
      JRTK=JR*K+KJ1-K
      IF(JRTK.GT.JELEM) THEN
         KERR=30
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     CHECK IF NEXT DESCRIPTOR CANCEL OPERATOR FOR DELAYED
C     REPLICATION
C
      IIIF=KSTACK(KJ+1)/100000
      IIII=KSTACK(KJ+1)-IIIF*100000
      IIIX=IIII/1000
      IIIY=IIII-IIIX*1000
C
      IF(IIIF.EQ.2.AND.IIIY.EQ.0) THEN
         KJ=KJ+1
         CALL BUOPERC(KPT,KDLEN,KDATA,KJ,KSTACK(KJ),KSTACK,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
C     ------------------------------------------------------------------
C*          5.  GET NEXT K DESCRIPTORS.
C               -----------------------
 500  CONTINUE
C
      DO 501 J=1,K
C
      ILIST(J)=KSTACK(KJ+J)
C
 501  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.  ADD JR TIMES K DESCRIPTORS IN PLACE OF K
C               ----------------------------------------
C               DESCRIPTORS OBTAINED.
C               ---------------------
 600  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.1  PUSH DOWN DESCRIPTORS IN KSTACK FOR (JR-1)*K PLACES
C                ---------------------------------------------------
C                STARTING AT KJ1 AND ENDING AT KJ+K.
C                -----------------------------------
 610  CONTINUE
C
      JRKM1=(JR-1)*K
C
      DO 611 J=KJ1,KJ+K,-1
C
      KSTACK(J+JRKM1)=KSTACK(J)
C
 611  CONTINUE
C
C*          6.2  INSERT LIST IN THE STACK.
C                -------------------------
 620  CONTINUE
C
      DO 622 J=1,JR
C
      KJJM1K=KJ+(J-1)*K
C
      DO 623 J1=1,K
C
      KSTACK(KJJM1K+J1)=ILIST(J1)
C
 623  CONTINUE
 622  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.3  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C                ----------------------------------------
 630  CONTINUE
C
      KJ1 = KJ1  + (JR-1)*K
C
C     ------------------------------------------------------------------
C*          6.4  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 640  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C
C     ------------------------------------------------------------------
 700  CONTINUE
C
      RETURN
      END
      SUBROUTINE BURQC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP,KSEC3,KERR)
C
C**** *BURQC*
C
C
C     PURPOSE.
C     --------
C
C          Create parameters needed for partial expansion of
C     Bufr message with compressed data according to requested
C     input lists.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BURQC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP,KSEC3,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KERR*    -  returned error code
C
C     METHOD.
C      -------
C
C          Word and bit pointers are calculated for every element
C     in the expanded list of elements. If partial expansion requested,
C     indeces to required elements are determined, as well as corresponding
C     quality control, statistics etc. information.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpacks bit pattern
C          BUNPKS         - unpacks bit pattern in repeated way
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
      COMMON /BCOMP/ INWTEN(JELEM),INWTR (JELEM),INWTS (JELEM),
     1                INWTRV (JELEM),INWTDW(JELEM),
     2                INWORDP(JWORK),INBITP(JWORK)
C             INWTEN   -  woking table
C             INWTR     -  working table reference
C             INWTS     -  working scale
C             INWTRV    -  working reference value
C             INWTDW    -  working data width
C             INWTIW    -  working data width of increments
C             INWTIWS   -  working total data width of element set
C
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      COMMON /BCOMREQ/ NREQ(2),NRQL,NRQ(JELEM),RQVAL(JELEM)
C
C             *NREQ*    -  flag
C                          bit number     meaning
C
C                              1        - 0 no bit map delivered to user
C                                         1    bit map delivered to user
C                              2        - 0 no partial expansion
C                                         1    partial expansion
C                              3        - 0 no Q/C required
C                                       - 1    Q/C required
C                              4        - 0 no statistics required
C                                       - 1    statistics
C                              5        - 0 no diffrence statistics
C                                       - 1    difference statistics
C                              6        - 0 no substituted values
C                                       - 1    substituted values
C             *NRQL*    -  number of requested elements
C             *NRQ*     -  list of requested table B reference
C             *RQVAL*   -  list of values signifying requested element
C                          (say pressure  at 50000 Pa)
C
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
      DIMENSION KBUFF(KBUFL)
C
      DIMENSION IRQEI(JELEM),IQCI(JELEM),IQCDPI(JELEM)
      DIMENSION IBVAL(JELEM),IBV(JELEM)
      DIMENSION IC7(JELEM),IC8(JELEM),IC7R(JELEM),IC8R(JELEM)
      DIMENSION NQP(JELEM),ITYPE(100)
c
      DIMENSION IMASK(8)
C
      SAVE IBV,IBVAL
C
      DATA IMASK /1,2,4,8,16,32,64,128/
C
C
C     ------------------------------------------------------------------
C
C*          1.  DEFINE WORD/BIT POINTERS TO EVERY ELEMENT.
C               ------------------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
c
      NFCM=1
      NFUCM=0
c
c     CHECK REQUEST VALIDITY
c
      IF(NREQ(1).EQ.0.AND.NREQ(2).NE.0) THEN
         KERR=38
         CALL BUERR(KERR)
         RETURN
      END IF
c
      MREL=0
C
      IBP32= 32 + NBPTB
C
      IB1=0
      if(iand(NREQ(2),imask(1)).ne.0) IB1=1
      IB2=0
      if(iand(NREQ(2),imask(2)).ne.0) IB2=1
      IB3=0
      if(iand(NREQ(2),imask(3)).ne.0) IB3=1
      IB4=0
      if(iand(NREQ(2),imask(4)).ne.0) IB4=1
      IB5=0
      if(iand(NREQ(2),imask(5)).ne.0) IB5=1
      IB6=0
      if(iand(NREQ(2),imask(6)).ne.0) IB6=1
C
C*          1.1 COMPRESSED DATA.
C               ----------------
 110  CONTINUE
C
      IBIT=IBP32
      IWORD=IBIT/NBPW
C
      NWORDP(1)=NWPTB+IWORD
      NBITP (1)=IBIT-IWORD*NBPW
C
      DO 111 I=2,M
      IWRD=NWORDP(I-1)
      IBTP=NBITP (I-1)
C
      IF(NWTDW(I-1).EQ.0) THEN
         NBITP(I) =NBITP(I-1)
         NWORDP(I)=NWORDP(I-1)
         GO TO 111
      END IF
C
      IBTP=IBTP+NWTDW(I-1)
      IF(IBTP.GE.NBPW) THEN
         IW=IBTP/NBPW
         IBTP=IBTP-IW*NBPW
         IWRD=IWRD+IW
      END IF
C
      CALL BUNPCK(NBPW,KBUFF,IDWINC,IWRD,IBTP,6,KERR)
      IF(KERR.GT.0) RETURN
      IF(IDWINC.GT.JBPW) THEN
            KERR=15
            PRINT*,'BURQC :'
            CALL BUERR(KERR)
            RETURN
      END IF
      IF(IWRD.GT.KBUFL) THEN
            KERR=26
            PRINT*,'BURQC :'
            CALL BUERR(KERR)
            RETURN
      END IF
C
      IF(CWTU(I-1).EQ.'CCITTIA5') THEN
         NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC*8
      ELSE
         NWTIWS(I-1)=NWTDW(I-1)+6+N*IDWINC
      END IF
C
      IBIT = NBITP(I-1) + NWTIWS(I-1)
      IWORD= IBIT/NBPW
C
      NBITP (I)= IBIT - IWORD*NBPW
      NWORDP(I)= NWORDP(I-1) + IWORD
C
      IF(NWORDP(I).GT.KBUFL) THEN
            KERR=26
            PRINT*,'BURQC :'
            CALL BUERR(KERR)
            RETURN
      END IF
 111  CONTINUE
C
C     -----------------------------------------------------------------
C*          2. CREATE POINTERS FOR REQUESTED ELEMENTS.
C              ---------------------------------------
 200  CONTINUE
C
C
C*          2.1 CHECK IF SUBSET OF ELEMENTS REQUESTED.
C
 210  CONTINUE
C
      IF(NREQ(1).EQ.0) THEN
         DO 212 I=1,M
         INWTEN(I)=NWTEN(I)
         INWTR (I)=NWTR (I)
         INWTS (I)=NWTS (I)
         INWTDW(I)=NWTDW(I)
         INWTRV(I)=NWTRV(I)
         CNAMES(I)=CWTEN(I)
         CUNITS(I)=CWTU(I)
         INWORDP(I)=NWORDP(I)
         INBITP(I)=NBITP(I)
 212     CONTINUE
         RETURN
      END IF
C
      IF(NREQ(1).EQ.1) THEN
         DO 213 I=1,M
         if(NWTR(I).GT.JELEM.and.
     1      NWTR(I).ne.999999) THEN
            MREL=I-1
            GO TO 2133
         END IF
 213     CONTINUE
         MREL=M
c
 2133    CONTINUE
         J=0
         IF(NRQL.EQ.0) THEN
            DO 2131 I=1,MREL
            J=J+1
            INBITP(J)=NBITP(I)
            INWORDP(J)=NWORDP(I)
            INWTEN(J)=NWTEN(I)
            INWTR(J) =NWTR(I)
            INWTS(J) =NWTS(I)
            INWTDW(J)=NWTDW(I)
            INWTRV(J)=NWTRV(I)
            CNAMES(J)=CWTEN(I)
            CUNITS(J)=CWTU(I)
 2131       CONTINUE
c
            M=J
            RETURN
         ELSE
            M=MREL
            GO TO 220
         END IF
      END IF
c
      IF(NREQ(1).EQ.2) THEN
         DO 214 I=1,M
         IF(NWTR(I).EQ.235000) THEN
            M=I-1
            GO TO 2141
         END IF
 214     CONTINUE
c
 2141    CONTINUE
c
         J=0
         IF(NREQ(2).EQ.0) THEN
            DO 2142 I=1,M
            J=J+1
            INBITP(J)=NBITP(I)
            INWORDP(J)=NWORDP(I)
            INWTEN(J)=NWTEN(I)
            INWTR(J)=NWTR(I)
            INWTS(J)=NWTS(I)
            INWTDW(J)=NWTDW(I)
            INWTRV(J)=NWTRV(I)
            CNAMES(J)=CWTEN(I)
            CUNITS(J)=CWTU(I)
 2142       CONTINUE
            RETURN
         ELSE
            GO TO 220
         END IF
      END IF
c
      IF(NREQ(1).EQ.3) THEN
         DO 215 I=1,M
         IF(NWTR(I).EQ.235000) THEN
c
c           COPY CLASS 1 - 8 ELEMENTS
c
            DO 2151 III=1,M
            IF(NWTR(III).GT.8000) THEN
               J=0
               IF(NREQ(2).EQ.0) THEN
                  DO 2161 II=1,III-1
                  J=J+1
                  INBITP(J)=NBITP(II)
                  INWORDP(J)=NWORDP(II)
                  INWTR(J)=NWTR(II)
                  INWTS(J)=NWTS(II)
                  INWTRV(J)=NWTRV(II)
                  INWTDW(J)=NWTDW(II)
                  INWTEN(J)=NWTEN(II)
                  CNAMES(J)=CWTEN(II)
                  CUNITS(J)=CWTU(II)
 2161             CONTINUE
                  GO TO 2160
              ELSE
                  DO 21611 II=1,III-1
                  J=J+1
                  NBITP(J)=NBITP(II)
                  NWORDP(J)=NWORDP(II)
                  NWTR(J)=NWTR(II)
                  NWTS(J)=NWTS(II)
                  NWTRV(J)=NWTRV(II)
                  NWTDW(J)=NWTDW(II)
                  NWTEN(J)=NWTEN(II)
                  CWTEN(J)=CWTEN(II)
                  CWTU(J)=CWTU(II)
21611             CONTINUE
                  GO TO 2160

              END IF
            END IF
 2151       CONTINUE
c
 2160       CONTINUE
c
            IF(NREQ(2).EQ.0) THEN
               DO 216 II=I+1,M
               J=J+1
               INBITP(J)=NBITP(II)
               INWORDP(J)=NWORDP(II)
               INWTR(J)=NWTR(II)
               INWTS(J)=NWTS(II)
               INWTRV(J)=NWTRV(II)
               INWTDW(J)=NWTDW(II)
               INWTEN(J)=NWTEN(II)
               CNAMES(J)=CWTEN(II)
               CUNITS(J)=CWTU(II)
 216           CONTINUE
               M=J
               RETURN
            ELSE
               DO 2169 II=I+1,M
               J=J+1
               NBITP(J)=NBITP(II)
               NWORDP(J)=NWORDP(II)
               NWTR(J)=NWTR(II)
               NWTS(J)=NWTS(II)
               NWTRV(J)=NWTRV(II)
               NWTDW(J)=NWTDW(II)
               NWTEN(J)=NWTEN(II)
               CWTEN(J)=CWTEN(II)
               CWTU(J)=CWTU(II) 
 2169          CONTINUE
               M=J
               GO TO 220
            END IF
         END IF
C
 215     CONTINUE
C
         KERR=39
         CALL BUERR(KERR)
         RETURN
      ELSE
         KERR=40
         CALL BUERR(KERR)
         RETURN
      END If
C
C
C*          2.2 CREATE INDECES TO CLASS 7/8
C               ---------------------------
 220  CONTINUE
C
      DO 225 I=1,M
      IF(NWTR(I).GT.JELEM.AND.
     1   NWTR(I).NE.999999) THEN
         MREL=I-1
         GO TO 226
      end if
 225  CONTINUE
c
      MREL=M
c
 226  CONTINUE
c
      J=0
      JJ=0
      DO 221 I=1,MREL
      ICLASS=NWTR(I)/1000
      IF(ICLASS.EQ.7) THEN
         J=J+1
         IC7(J)=I
      END IF
C
      IF(ICLASS.EQ.8) THEN
         JJ=JJ+1
         IC8(JJ)=I
      END IF
 221  CONTINUE
C
      IC7L=J
      IC8L=JJ
      J=0
      JJ=0
      DO 223 I=1,NRQL
      ICLASS=NRQ(I)/1000
      IF(ICLASS.EQ.7.AND.(ABS(RQVAL(I)-RVIND).GT.EPS)) THEN
         J=J+1
         IC7R(J)=I
      END IF
C
      IF(ICLASS.EQ.8.AND.(ABS(RQVAL(I)-RVIND).GT.EPS)) THEN
         JJ=JJ+1
         IC8R(JJ)=I
      END IF
 223  CONTINUE
C
      IC7RL=J
      IC8RL=JJ
C
C*          2.3 CREATE INDECES TO THE REQUESTED ELEMENTS.
C               -----------------------------------------
 230  CONTINUE
C
      IF(IC7RL.EQ.0.AND.IC8RL.EQ.0) THEN
         IL=MREL
         IRL=NRQL
      END IF
      IF(IC7RL.NE.0.AND.IC8RL.EQ.0) THEN
         IL=IC7(1)
         IRL=IC7R(1)-1
      END IF
      IF(IC8RL.NE.0.AND.IC7RL.EQ.0) THEN
         IL=IC8(1)
         IRL=IC8R(1)-1
      END IF
      IF(IC7RL.NE.0.AND.IC8RL.NE.0) THEN
         IL=IC7(1)
         IF(IC7(1).GT.IC8(1)) IL=IC8(1)
         IRL=IC7R(1)
         IF(IC7R(1).GT.IC8R(1)) IRL=IC8R(1)
         IRL=IRL-1
      END IF
c
      DO 299 I=1,MREL
      IRQEI(I)=0
 299  CONTINUE
c
      ORQEI=.FALSE.
      DO 231 I=1,IL
      IF(IB2.EQ.0.OR.NRQL.EQ.0) THEN
         ORQEI=.TRUE.
         IRQEI(I)=I
      ELSE
         DO 232 K=1,IRL
         IF(NWTR(I).EQ.NRQ(K)) THEN
            ORQEI=.TRUE.
            IRQEI(I)=I
         END IF
 232     CONTINUE
      END IF
 231  CONTINUE
C
      IF(IC7RL.NE.0) THEN
C
C        CLASS 7 SIGNIFYING
C
         IST=1
         DO 233 IJ=1,IC7RL
         III=IC7R(IJ)
         DO 234 I=IST,IC7L
         II=IC7(I)
         IBITP=NBITP(II)
         IWORD=NWORDP(II)

         CALL BUNPCK(NBPW,KBUFF,IVAL,IWORD,IBITP,NWTDW(II),KERR)
         if(kerr.ne.0) then
            call buerr(kerr)
            return
         end if
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWORD,IBITP,6,KERR)
         if(kerr.ne.0) then
            call buerr(kerr)
            return
         end if
         CALL BUNPCK(NBPW,KBUFF,INC,IWORD,IBITP,IDWINC,KERR)
         IF(INC.EQ.NMASK(IDWINC)) IVAL=NMASK(NWTDW(II))
c
         IF(IVAL.NE.NMASK(NWTDW(II))) THEN
            ISCALE=NWTS(II)
            IREF  =NWTRV(II)
            IVAL  =IVAL+IREF+INC
            VAL   =RVIND
            IF(IVAL.NE.NMASK(NWTDW(II))) THEN
               IF(ISCALE.GT.0) THEN
                  VAL=IVAL/10.**ISCALE
               ELSE
                  IISCALE=IABS(ISCALE)
                  VAL=IVAL*10.**IISCALE
               END IF
            END IF
         ELSE
            VAL=RVIND
         END IF
C
         INEXT=IC7(I+1)-1
         IF(IC7(I+1).LE.0) INEXT=MREL
         IF(ABS(RQVAL(III)-VAL).LT.EPS.OR.
     1      ABS(RQVAL(III)-RVIND).LT.EPS) THEN
            DO 236 KI=II,INEXT
            IF(IB2.EQ.0) THEN
               ORQEI=.TRUE.
               IRQEI(KI)=KI
            ELSE
               IE=IC7R(IJ+1)-1
               IF(IJ.EQ.IC7RL) IE=NRQL
               IF(IC8RL.NE.0) THEN
                  DO 238 JA=1,IC8RL
                  IF(IC8R(JA).GT.III.AND.IC8R(JA).LT.IE) THEN
                     IE=IC8R(JA)-2
                     GO TO 298
                  END IF
 238              CONTINUE
               END IF
 298           CONTINUE
               DO 237 IK=III,IE
               IF(NWTR(KI).EQ.NRQ(IK)) THEN
                  ORQEI=.TRUE.
                  IRQEI(KI)=KI
               END IF
 237           CONTINUE
            END IF
 236        CONTINUE
C
            GO TO 233
         END IF
 234     CONTINUE
 233     CONTINUE
C
      END IF
C
      IF(IC8RL.NE.0) THEN
C
C        CLASS 8 SIGNIFYING
C
         IST=1
         DO 241 IJ=1,IC8RL
         III=IC8R(IJ)
         DO 242 I=IST,IC8L
         II=IC8(I)
         IBITP=NBITP(II)
         IWORD=NWORDP(II)
         CALL GBYTE(KBUFF(IWORD),IVAL,IBITP,NWTDW(II))
         ISCALE=NWTS(II)
         IREF  =NWTRV(II)
         IVAL  =IVAL+IREF
         VAL   =RVIND
         IF(IVAL.NE.NMASK(NWTDW(II))) THEN
            IF(ISCALE.GT.0) THEN
               VAL=IVAL/10.**ISCALE
            ELSE
               IISCALE=IABS(ISCALE)
               VAL=IVAL*10.**IISCALE
            END IF
         END IF
C
         INEXT=IC8(I+1)-2
         IF(IC8(I+1).EQ.0) INEXT=MREL
C
         OT=.FALSE.
         if(val.lt.(rvind-eps).or.val.gt.(rvind+eps)) then
            IVAL=NINT(VAL)
            IRQV=NINT(RQVAL(III))
C
            IF(NWTR(II).EQ.008001) THEN
               OT=.TRUE.
               IF(iand(IVAL,IRQV).NE.IRQV) OT=.FALSE.
            ELSE
               OT=.TRUE.
               IF(ABS(VAL-RQVAL(III)).GT.EPS) OT=.FALSE.
            END IF
         END IF
C
         IF(OT) THEN
            ICL=NWTR(II-1)/1000
            IF(ICL.EQ.7) IRQEI(II-1)=II-1
            DO 244 KI=II,INEXT
            IF(IB2.EQ.0) THEN
               orqei=.true.
               IRQEI(KI)=KI
            ELSE
               IE=IC8R(IJ+1)-1
               IF(IJ.EQ.IC8RL) IE=NRQL
               if(ic7rl.ne.0) then
                  do 248 ja=1,ic7rl
                  if(ic7r(ja).gt.iii.and.ic7r(ja).lt.ie) then
                     ie=ic7r(ja)-1
                     go to 297
                  end if
 248              continue
               end if
 297           continue
               DO 245 IK=III,IE
               IF(NWTR(KI).EQ.NRQ(IK)) THEN
                 orqei=.true.
                 IRQEI(KI)=KI
               END IF
 245           CONTINUE
            END IF
 244        CONTINUE
C
         END IF
 242     CONTINUE
 241     CONTINUE
C
      END IF
C
      IF(.NOT.ORQEI) THEN
         KERR=45
         CALL BUERR(KERR)
         M=0
         RETURN
      END IF
C
C
C
C*          3. FIND POINTERS TO QUALITY CONTROL,START OF DATA
C               ----------------------------------------------
C               PRESENT INDICATORS AND %CONFIDENCE.
C               -----------------------------------
C
 300  CONTINUE
C
      i=0
      if(ib3.ne.0) then
         i=i+1
         itype(i)=222000
      end if
c
      if(ib4.ne.0) then
         i=i+1
         itype(i)=224000
      end if
c
      if(ib5.ne.0) then
         i=i+1
         itype(i)=225000
      end if
c
      if(ib6.ne.0) then
         i=i+1
         itype(i)=223000
      end if
c
      KEND=I
      IF(KEND.EQ.0) THEN
         J=0
         DO 4011 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            II=IRQEI(I)
            J=J+1
            INWORDP(J)=NWORDP(II)
            INBITP(J)=NBITP(II)
            INWTR(J)=NWTR(II)
            INWTS(J)=NWTS(II)
            INWTRV(J)=NWTRV(II)
            INWTDW(J)=NWTDW(II)
            INWTEN(J)=NWTEN(II)
            CNAMES(J)=CWTEN(II)
            CUNITS(J)=CWTU(II)
         END IF
 4011    CONTINUE
         GO TO 900
      END IF
c
      olist=.false.
      o236=.false.
      o237=.false.
c
      do 500 kt=1,kend
         JQPR=0
         JQUA=0
         JQCA=0
         JQC =0
         jqcc=0
         jqcs=0
         km=mrel
C
c
c        find pointers to operators
c
         kz=0
         do 3031 i=km,m
         if(nwtr(i).eq.itype(kt)) then
            kz=kz+1
            nqp(kz)=i
         end if
 3031    continue
c
         if(kz.eq.0) then
            print*,itype(kt),' not present in this message.'
            go to 500
         else
            kz=kz+1
            nqp(kz)=m
         end if
c
         do 3033 kq=1,kz-1
c
         jqua=nqp(kq)
c
         if(nwtr(jqua+1).eq.236000) then
            jq236=jqua+1
            o236=.true.
         end if
c
         if(nwtr(jqua+1).eq.237000) then
            jq237=jqua+1
            o237=.true.
         else
            o237=.false.
         end if
c
C
C        data present indicator
C
         if(.not.o236.and.o237) then
c
c           find last defined bit map
c
            do 420 i=jqua,mrel,-1
            if(nwtr(i).eq.236000) go to 421
 420        continue
c
            kerr=41
            call buerr(kerr)
            return
c
 421        continue
c
            o236=.true.
            o237=.false.
            jq236=i
            jq237=0
            ik=i
            IDPRF=0
            DO 3015 I=ik,m
            IF(NWTR(I).EQ.31031.or.NWTR(I).EQ.31192) THEN
               IDPRF=IDPRF+1
               IF(IDPRF.EQ.1) JQPR=I
            else
               if(idprf.ne.0) go to 30111
            end if
 3015       continue
C
 3016       continue
         end if
C
C        data present indicator
C
         if(.not.o237) then
            IDPRF=0
            DO 301 I=jqua,nqp(kq+1)
            IF(NWTR(I).EQ.31031.or.NWTR(I).EQ.31192) THEN
               IDPRF=IDPRF+1
               IF(IDPRF.EQ.1) JQPR=I
            else
               if(idprf.ne.0) go to 30111
            END IF
 301        CONTINUE
         end if
C
30111   continue
c
c        generating centre
c
         do 309 i=jqua,nqp(kq+1)
         if(nwtr(i).eq.1031) then
            jqcc=i
            go to 302
         end if
 309     continue
c
 302     CONTINUE
C
C        generating application
C
         DO 307 I=jqua,nqp(kq+1)
         IF(NWTR(I).EQ.1032.OR.NWTR(I).EQ.1201.OR.
     1      NWTR(I).EQ.63191) THEN
            JQCA=I
            go to 3071
         END IF
 307     CONTINUE
c
 3071    continue
c
C        percentage confidence
C
         if(itype(kt).eq.222000) then
            DO 305 I=jqua,nqp(kq+1)
            IF(NWTR(I)/1000.EQ.33.OR.NWTR(I).EQ.63192) THEN
               JQC=I
               GO TO 306
            END IF
 305        CONTINUE
         else
c           significance
c
            do 308 i=jqua,nqp(kq+1)
            IF(NWTR(I)/1000.EQ.8) then
               JQCS=i
               go to 3088
            end if
 308        continue
c
 3088       continue
c
            DO 3051 I=jqua,nqp(kq+1)
            IF(NWTR(I).eq.223255.or.NWTR(I).eq.224255.or.
     1         NWTR(I).eq.225255) THEN
               JQC=I
               GO TO 306
            END IF
 3051       CONTINUE
         END IF
c
C
 306     CONTINUE
C
C*          3.1  SET INDICES TO GET Q/C.
C                GET BIT MAP FROM DATA SECTION.
C
 310  CONTINUE
C
         IF(JQUA.EQ.0) THEN
            WRITE(*,'(1H )')
            WRITE(*,'(1H ,A)') 'Q/C not present in this Bufr message.'
            WRITE(*,'(1H )')
         ELSE
C
            if(.not.o237) then
C
               kak=0
               iqpr=jqpr-1
               do 311 k=1,idprf
               iqpr=iqpr+1
               kak=kak+1
               IWPT=NWORDP(IQPR)
               IBPT=NBITP (IQPR)
               CALL GBYTE(KBUFF(IWPT),IBVAL(kak),IBPT,1)
 311           CONTINUE
            end if
C
            IDIF=MREL-IDPRF
C
            Js=0
            IF(IDIF.NE.0) THEN
               DO 313 I=1,IDIF
               Js=Js+1
               IQCDPI(Js)=0
 313           CONTINUE
            END IF
C
            JQPRM1=JQPR-1
            DO 314 I=1,IDPRF
            Js=Js+1
            IQCDPI(Js)=I+JQPRM1
 314        CONTINUE
C
            Js=0
            IF(IDIF.NE.0) THEN
               DO 315 I=1,IDIF
               Js=Js+1
               IQCI(Js)=0
 315           CONTINUE
            END IF
C
            K=JQC-1
            DO 317 I=1,IDPRF
            OK=.TRUE.
            Js=Js+1
            IQCI(Js)=0
            IF(IBVAL(I).EQ.0) THEN
               IF(OK) K=K+1
               IQCI(Js)=K
               OK=.FALSE.
            END IF
 317        CONTINUE
C
         END IF
C
C
C*          4.  CREATE REQUESTED ELEMENT LIST.
C               ------------------------------
 400  CONTINUE
C
      IF(.NOT.OLIST) THEN
         IF(NREQ(2).EQ.0) NRQL=0
         J=0
         DO 401 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            II=IRQEI(I)
            J=J+1
            INBITP(J)=NBITP(II)
            INWORDP(J)=NWORDP(II)
            INWTR(J)  =NWTR(II)
            INWTS(J)  =NWTS(II)
            INWTRV(J) =NWTRV(II)
            INWTDW(J) =NWTDW(II)
            INWTEN(J) =NWTEN(II)
            CNAMES(J)=CWTEN(II)
            CUNITS(J)=CWTU(II)
            OLIST=.TRUE.
         END IF
 401     CONTINUE
      END IF
C
      IF(JQUA.NE.0) THEN
C
C*          4.1 ADD DATA PRESENT INDICATOR AND Q/C.
C               -----------------------------------
 410     CONTINUE
C
c                  add operator 222000
         J=J+1
         INWORDP(J)=NWORDP(JQUA)
         INBITP (J)=NBITP (JQUA)
         INWTR(J)  =NWTR(JQUA)
         INWTS(J)  =NWTS(JQUA)
         INWTRV(J) =NWTRV(JQUA)
         INWTDW(J) =NWTDW(JQUA)
         INWTEN(J) =NWTEN(JQUA)
         CNAMES(J)=CWTEN(JQUA)
         CUNITS(J)=CWTU(JQUA)
C
         if(o236.and..not.o237) then
            j=j+1
            INWORDP(J)=NWORDP(JQ236)
            INBITP (J)=NBITP (JQ236)
            INWTR(J)  =NWTR(JQ236)
            INWTS(J)  =NWTS(JQ236)
            INWTRV(J) =NWTRV(JQ236)
            INWTDW(J) =NWTDW(JQ236)
            INWTEN(J) =NWTEN(JQ236)
            cnames(j)=cwten(jq236)
         end if
c
         if(o237) then
            j=j+1
            INWORDP(J)=NWORDP(JQ237)
            INBITP (J)=NBITP (JQ237)
            INWTR (J) =NWTR (JQ237)
            INWTS (J) =NWTS (JQ237)
            INWTRV(J) =NWTRV(JQ237)
            INWTDW(J) =NWTDW(JQ237)
            INWTEN(J) =NWTEN(JQ237)
            CNAMES(J)=CWTEN(JQ237)
            CUNITS(J)=CWTU (JQ237)
         end if
c
         if(.not.o237) then 
            DO 412 I=1,MREL
            IF(IRQEI(I).NE.0) THEN
               IF(IQCDPI(I).NE.0) THEN
                  J=J+1
                  INWORDP(J)=NWORDP(IQCDPI(I))
                  INBITP (J)=NBITP (IQCDPI(I))
                  INWTR(J)  =NWTR(IQCDPI(I))
                  INWTS(J)  =NWTS(IQCDPI(I))
                  INWTRV(J) =NWTRV(IQCDPI(I))
                  INWTDW(J) =NWTDW(IQCDPI(I))
                  INWTEN(J) =NWTEN(IQCDPI(I))
                  CNAMES(J)=CWTEN(IQCDPI(I))
                  CUNITS(J)=CWTU(IQCDPI(I))
               END IF
            END IF
 412        CONTINUE
         end if
C
c
C        GENERATING CENTRE
C
         if(jqcc.ne.0) then
            J=J+1
            INWORDP(J)=NWORDP(JQCC)
            INBITP (J)=NBITP (JQCC)
            INWTR(J)  =NWTR(JQCC)
            INWTS(J)  =NWTS(JQCC)
            INWTRV(J) =NWTRV(JQCC)
            INWTDW(J) =NWTDW(JQCC)
            INWTEN(J) =NWTEN(JQCC)
            CNAMES(J)=CWTEN(JQCC)
            CUNITS(J)=CWTU(JQCC)
         end if
c
C        Q/C APPLICATION
C
         if(jqca.ne.0) then
            J=J+1
            INWORDP(J)=NWORDP(JQCA)
            INBITP (J)=NBITP (JQCA)
            INWTR(J)  =NWTR(JQCA)
            INWTS(J)  =NWTS(JQCA)
            INWTRV(J) =NWTRV(JQCA)
            INWTDW(J) =NWTDW(JQCA)
            INWTEN(J) =NWTEN(JQCA)
            CNAMES(J)=CWTEN(JQCA)
            CUNITS(J)=CWTU(JQCA)
         end if
c
c        significance
c
         if(jqcs.ne.0) then
            J=J+1
            INWORDP(J)=NWORDP(JQCS)
            INBITP (J)=NBITP (JQCS)
            INWTR(J)  =NWTR(JQCS)
            INWTS(J)  =NWTS(JQCS)
            INWTRV(J) =NWTRV(JQCS)
            INWTDW(J) =NWTDW(JQCS)
            INWTEN(J) =NWTEN(JQCS)
            CNAMES(J)=CWTEN(JQCS)
            CUNITS(J)=CWTU(JQCS)
         end if
C
C
C        class 33 elements
C
         DO 415 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            OK=.TRUE.
            IF(IQCI(I).NE.0) THEN
               IF(OK) J=J+1
               OK=.FALSE.
               INWORDP(J)=NWORDP(IQCI(I))
               INBITP (J)=NBITP (IQCI(I))
               INWTR(J)  =NWTR  (IQCI(I))
               INWTS(J)  =NWTS  (IQCI(I))
               INWTRV(J) =NWTRV (IQCI(I))
               INWTDW(J) =NWTDW (IQCI(I))
               INWTEN(J) =NWTEN (IQCI(I))
               CNAMES(J)=CWTEN (IQCI(I))
               CUNITS(J)=CWTU  (IQCI(I))
            END IF
         END IF
 415     CONTINUE
      END IF
 3033 continue
c
C
C*          5. MAKE ONE TO ONE CORRESPONDENCE BETWEEN ELEMENTS AND
C              Q/C,STATISTICS,DIFFERENCE STATISTICS OR SUBSTITUTED VALUES
C              LEAVING BEHIND DATA PRESENT INDICATORS.
 500  CONTINUE
C
C
C*          9.  UPDATE TOTAL NUMBER OF ELEMENTS.
C               --------------------------------
 900  CONTINUE
C
      M=J
      nfcm=1
      nfucm=0
C
      RETURN
C
      END
      SUBROUTINE BURQUC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP,KSEC3,KERR)
C
C**** *BURQUC*
C
C
C     PURPOSE.
C     --------
C
C          Create parameters needed for partial expansion of
C     Bufr message according to requested input lists.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BURQUC(KBUFL,KBUFF,KELEM,CNAMES,CUNITS,KSUP,KSEC3,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *CNAMES*  -  character array containing element names
C               *CUNITS*  -  character array containig units
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC3*   -  array containing section 3 information
C                            KSEC3( 1)-- length of section 3 (bytes)
C                            KSEC3( 2)-- reserved
C                            KSEC3( 3)-- number of subsets
C                            KSEC3( 4)-- flag (data type,data compression)
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C          Word and bit pointers are calculated for every element
C     in the expanded list of elements. If partial expansion requested,
C     indeces to required elements are determined, as well as corresponding
C     quality control, statistics etc. information.
C
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpacks bit pattern
C          BUNPKS         - unpacks bit pattern in repeated way
C          BUPMRK          - proces marker operator
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
      COMMON /BCOMP/ INWTEN(JELEM),INWTR (JELEM),INWTS (JELEM),
     1                INWTRV (JELEM),INWTDW(JELEM),
     2                INWORDP(JWORK),INBITP(JWORK)
C             INWTEN   -  woking table
C             INWTR     -  working table reference
C             INWTS     -  working scale
C             INWTRV    -  working reference value
C             INWTDW    -  working data width
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMRQ/ NWORDP(JWORK),NBITP(JWORK)
C
C           NWORDP     - array containing word pointers to
C                        requested elements
C           NBITP      - array containing bit pointers to
C                        requested elements
C
C
      COMMON /BCOMREQ/ NREQ(2),NRQL,NRQ(JELEM),RQVAL(JELEM)
C
C             *NREQ*    -  flag
C                          bit number     meaning
C
C                              1        - 0 no bit map delivered to user
C                                         1    bit map delivered to user
C                              2        - 0 no partial expansion
C                                         1    partial expansion
C                              3        - 0 no Q/C required
C                                       - 1    Q/C required
C                              4        - 0 no statistics required
C                                       - 1    statistics
C                              5        - 0 no diffrence statistics
C                                       - 1    difference statistics
C                              6        - 0 no substituted values
C                                       - 1    substituted values
C             *NRQL*    -  number of requested elements
C             *NRQ*     -  list of requested table B reference
C             *RQVAL*   -  list of values signifying requested element
C                          (say pressure  at 50000 Pa)
C
C
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(JSUP),KSEC3(JSEC3)
C
      DIMENSION IRQEI(JELEM),IQCI(JELEM),IQCDPI(JELEM)
      DIMENSION IBVAL(JELEM),IBV(JELEM)
      DIMENSION IC7(JELEM),IC8(JELEM),IC7R(JELEM),IC8R(JELEM)
      DIMENSION NQP(JELEM),ITYPE(100)
C
      DIMENSION IMASK(8)
C
      SAVE IBV,IBVAL
C
      DATA IMASK /1,2,4,8,16,32,64,128/
C
C
C     ------------------------------------------------------------------
C
C*          1.  DEFINE WORD/BIT POINTERS TO EVERY ELEMENT.
C               ------------------------------------------
 100  CONTINUE
C
      NFUCM=1
      NFCM=0
c
      IF(KERR.GT.0) RETURN
c
c     CHECK REQUEST VALIDITY
c
      IF(NREQ(1).EQ.0.AND.NREQ(2).NE.0) THEN
         KERR=38
         CALL BUERR(KERR)
         RETURN
      END IF
C
      MREL=0
c
      IBP32= 32 + NBPTB
C
      IB1=0
      if(iand(NREQ(2),imask(1)).ne.0) IB1=1
      IB2=0
      if(iand(NREQ(2),imask(2)).ne.0) IB2=1
      IB3=0
      if(iand(NREQ(2),imask(3)).ne.0) IB3=1
      IB4=0
      if(iand(NREQ(2),imask(4)).ne.0) IB4=1
      IB5=0
      if(iand(NREQ(2),imask(5)).ne.0) IB5=1
      IB6=0
      if(iand(NREQ(2),imask(6)).ne.0) IB6=1
C
C
C*          1.2 NON-COMPRESSED DATA.
C               --------------------
 120  CONTINUE
C
      IBIT0= 0
C
      IF(N.GT.1) THEN
         DO 121 J=1,M
         IBIT0 = IBIT0 + NWTDW(J)
 121     CONTINUE
      END IF
C
      DO 122 I=1,N
      IBIT=0
      IBIT=IBIT0*(I-1)
      IBIT = IBIT + IBP32
      IWORD= IBIT/NBPW
C
      I1MK=(I-1)*KELEM
      I1= 1+I1MK
C
      NBITP (I1)= IBIT - IWORD*NBPW
      NWORDP(I1)= NWPTB + IWORD
C
      DO 123 J=2,M
      IBIT = IBIT + NWTDW(J-1)
      IWORD= IBIT/NBPW
C
      JI= J+I1MK
C
      NBITP (JI)= IBIT - IWORD*NBPW
      NWORDP(JI)= NWPTB + IWORD
 123  CONTINUE
 122  CONTINUE
C
C     -----------------------------------------------------------------
C*          2. CREATE POINTERS FOR REQUESTED ELEMENTS.
C              ---------------------------------------
 200  CONTINUE
C
C
C*          2.1 CHECK IF SUBSET OF ELEMENTS REQUESTED.
C
 210  CONTINUE
C
      IF(NREQ(1).EQ.0) then
         DO 212 I=1,M
         INWTEN(I)=NWTEN(I)
         INWTR (I)=NWTR (I)
         INWTS (I)=NWTS (I)
         INWTDW(I)=NWTDW(I)
         INWTRV(I)=NWTRV(I)
         CNAMES(I)=CWTEN(I)
         CUNITS(I)=CWTU(I)
         DO 2128 J=1,N
         JM1K=(J-1)*KELEM
         JI=I+JM1K
         INWORDP(JI)=NWORDP(JI)
         INBITP(JI)=NBITP(JI)
 2128    CONTINUE
 212     CONTINUE
         RETURN
      END IF
c
      IF(NREQ(1).EQ.1) THEN
         DO 213 I=1,M
         IF(NWTR(I).GT.JELEM.AND.
     1      NWTR(I).ne.999999) THEN
            MREL=I-1
            GO TO 2133
         END IF
 213     CONTINUE
         MREL=M
c
 2133    CONTINUE
         J=0
         IF(NREQ(2).EQ.0) THEN
            DO 2131 I=1,MREL
            J=J+1
            DO 2132 K=1,N
c
            K1K=(K-1)*KELEM
            JK=J+K1K
            IK=I+K1K
c
            INBITP(JK)=NBITP(IK)
            INWORDP(JK)=NWORDP(IK)
 2132       CONTINUE
            INWTEN(J)=NWTEN(I)
            INWTR(J)=NWTR(I)
            INWTS(J)=NWTS(I)
            INWTDW(J)=NWTDW(I)
            INWTRV(J)=NWTRV(I)
            CNAMES(J)=CWTEN(I)
            CUNITS(J)=CWTU(I)
 2131       CONTINUE
            M=J
            RETURN
         ELSE
            M=MREL
            GO TO 220
         END IF
c
      END IF
c
      IF(NREQ(1).EQ.2) THEN
         DO 214 I=1,M
         IF(NWTR(I).eq.235000) THEN
            M=I-1
            GO TO 2141
         END IF
 214     CONTINUE
c
 2141    CONTINUE
c
         J=0
         IF(NREQ(2).EQ.0) THEN
            DO 2142 I=1,M    
            J=J+1
            DO 2143 K=1,N
c
            K1K=(K-1)*KELEM
            JK=J+K1K
            IK=I+K1K
c
            INBITP(JK)=NBITP(IK)
            INWORDP(JK)=NWORDP(IK)
 2143       CONTINUE
            INWTEN(J)=NWTEN(I)
            INWTR(J)=NWTR(I)
            INWTS(J)=NWTS(I)
            INWTDW(J)=NWTDW(I)
            INWTRV(J)=NWTRV(I)
            CNAMES(J)=CWTEN(I)
            CUNITS(J)=CWTU(I)
 2142       CONTINUE
c
            RETURN
         ELSE
            GO TO 220
         END IF
      END IF
c
      IF(NREQ(1).EQ.3) THEN
         DO 215 I=1,M
         IF(NWTR(I).eq.235000) THEN
c
c           COPY CLASS 1 - 8
c
            DO 2151 III=1,M
            IF(NWTR(III).GT.8000) THEN
               J=0
               IF(NREQ(2).EQ.0) THEN
                  DO 2161 II=1,III-1
                  J=J+1
                  DO 2171 K=1,N
                  k1k=(K-1)*KELEM
                  JK=J+K1K
                  IIK=II+K1K
                  INBITP(JK)=NBITP(IIK)
                  INWORDP(JK)=NWORDP(IIK)
 2171             CONTINUE
                  INWTR(J)  =NWTR(II)
                  INWTS(J)  =NWTS(II)
                  INWTRV(J) =NWTRV(II)
                  INWTDW(J) =NWTDW(II)
                  INWTEN(J) =NWTEN(II)
                  CNAMES(J)=CWTEN(II)
                  CUNITS(J)=CWTU(II)
 2161             CONTINUE
                  GO TO 2160
               ELSE
                  DO 21611 II=1,III-1
                  J=J+1
                  DO 21711 K=1,N
                  K1K=(K-1)*KELEM
                  JK=J+K1K
                  IIK=II+K1K
                  NBITP(JK)=NBITP(IIK)
                  NWORDP(JK)=NWORDP(IIK)
21711             CONTINUE
                  NWTR(J)  =NWTR(II)
                  NWTS(J)  =NWTS(II)
                  NWTRV(J) =NWTRV(II)
                  NWTDW(J) =NWTDW(II)
                  NWTEN(J) =NWTEN(II)
                  CWTEN(J)=CWTEN(II)
                  CWTU(J)=CWTU(II)
21611             CONTINUE
                  GO TO 2160
               END IF
            END IF
 2151       CONTINUE
c----------------------------------------------------------------------
 2160       CONTINUE
            IF(NREQ(2).EQ.0) THEN
            DO 216 II=I+1,M
               J=J+1
               DO 217 K=1,N
               K1K=(K-1)*KELEM
               JK=J+K1K
               IIK=II+K1K
               INBITP(JK)=NBITP(IIK)
               INWORDP(JK)=NWORDP(IIK)
 217           CONTINUE
               INWTR(J)  =NWTR(II)
               INWTS(J)  =NWTS(II)
               INWTRV(J) =NWTRV(II)
               INWTDW(J) =NWTDW(II)
               INWTEN(J) =NWTEN(II)
               CNAMES(J)=CWTEN(II)
               CUNITS(J)=CWTU(II)
 216           CONTINUE
               M=J
               RETURN
            ELSE
               DO 2169 II=I+1,M
               J=J+1
               DO 2179 K=1,N
               K1K=(K-1)*KELEM
               JK=J+K1K
               IIK=II+K1K
               NBITP(JK)=NBITP(IIK)
               NWORDP(JK)=NWORDP(IIK)
 2179          CONTINUE
               NWTR(J)  =NWTR(II)
               NWTS(J)  =NWTS(II)
               NWTRV(J) =NWTRV(II)
               NWTDW(J) =NWTDW(II)
               NWTEN(J) =NWTEN(II)
               CWTEN(J)=CWTEN(II)
               CWTU(J)=CWTU(II)
 2169          CONTINUE
               M=J
               GO TO 220
            END IF
         END IF
 215     CONTINUE
         KERR=39
         CALL BUERR(KERR)
         M=0
         RETURN
      ELSE 
         KERR=40
         CALL BUERR(KERR)
         RETURN
      END IF
c
C
C*          2.2 CREATE INDICES TO CLASS 7/8
C               ---------------------------
 220  CONTINUE
C
      DO 225 I=1,M
      IF(NWTR(I).GT.JELEM.AND.
     1   NWTR(I).ne.999999) THEN
         MREL=I-1
         GO TO 226
      END IF
 225  CONTINUE
c
      MREL=M
c
 226  CONTINUE
c
      J=0
      JJ=0
      DO 221 I=1,MREL
      ICLASS=NWTR(I)/1000
      IF(ICLASS.EQ.7) THEN
         J=J+1
         IC7(J)=I
      END IF
C
      IF(ICLASS.EQ.8) THEN
         JJ=JJ+1
         IC8(JJ)=I
      END IF
 221  CONTINUE
C
      IC7L=J
      IC8L=JJ
      J=0
      JJ=0
      DO 223 I=1,NRQL
      ICLASS=NRQ(I)/1000
      IF(ICLASS.EQ.7.AND.(ABS(RQVAL(I)-RVIND).GT.EPS)) THEN
         J=J+1
         IC7R(J)=I
      END IF
C
      IF(ICLASS.EQ.8.AND.(ABS(RQVAL(I)-RVIND).GT.EPS)) THEN
         JJ=JJ+1
         IC8R(JJ)=I
      END IF
 223  CONTINUE
C
      IC7RL=J
      IC8RL=JJ
C
C*          2.3 CREATE INDECES TO THE REQUESTED ELEMENTS.
C               -----------------------------------------
 230  CONTINUE
C
      IF(IC7RL.EQ.0.AND.IC8RL.EQ.0) THEN
         IL=MREL
         IRL=NRQL
      END IF
      IF(IC7RL.NE.0.AND.IC8RL.EQ.0) THEN
         IL=IC7(1)
         IRL=IC7R(1)-1
      END IF
      IF(IC8RL.NE.0.AND.IC7RL.EQ.0) THEN
         IL=IC8(1)
         IRL=IC8R(1)-1
      END IF
      IF(IC7RL.NE.0.AND.IC8RL.NE.0) THEN
         IL=IC7(1)
         IF(IC7(1).GT.IC8(1)) IL=IC8(1)
         IRL=IC7R(1)
         IF(IC7R(1).GT.IC8R(1)) IRL=IC8R(1)
         IRL=IRL-1
      END IF
c
      DO 299 I=1,MREL
      IRQEI(I)=0
 299  CONTINUE
c
      ORQEI=.FALSE.
      DO 231 I=1,IL
      IF(IB2.EQ.0.OR.NRQL.EQ.0) THEN
         ORQEI=.TRUE.
         IRQEI(I)=I
      ELSE
         DO 232 K=1,IRL
         IF(NWTR(I).EQ.NRQ(K)) THEN
            ORQEI=.TRUE.
            IRQEI(I)=I
         END IF
 232     CONTINUE
      END IF
 231  CONTINUE
C
C
      IF(IC7RL.NE.0) THEN
C
C        CLASS 7 SIGNIFYING
C
         IST=1
         DO 233 IJ=1,IC7RL
         III=IC7R(IJ)
         DO 234 I=IST,IC7L
         II=IC7(I)
         if(ic7r(ij).eq.ic7r(ij+1)) then
         if(nwtr(ii).ne.nwtr(ii+1)) go to 234
         end if
         IBITP=NBITP(II)
         IWORD=NWORDP(II)
         CALL GBYTE(KBUFF(IWORD),IVAL,IBITP,NWTDW(II))
         ISCALE=NWTS(II)
         IREF  =NWTRV(II)
         IVAL  =IVAL+IREF
         VAL   =RVIND
         IF(IVAL.NE.NMASK(NWTDW(II))) THEN
            IF(ISCALE.GT.0) THEN
               VAL=IVAL/10.**ISCALE
            ELSE
               IISCALE=IABS(ISCALE)
               VAL=IVAL*10.**IISCALE
            END IF
         END IF
C
         INEXT=IC7(I+1)-1
         if(i.eq.ic7l) INEXT=MREL
c         IF(IC7(I+1).EQ.0) INEXT=MREL
         IF(ABS(RQVAL(III)-VAL).LT.EPS.OR.
     1      ABS(RQVAL(III)-RVIND).LT.EPS) THEN
            DO 236 KI=II,INEXT
            IF(IB2.EQ.0) THEN
               ORQEI=.TRUE.
               IRQEI(KI)=KI
            ELSE
               IE=IC7R(IJ+1)-1
               IF(IJ.EQ.IC7RL) IE=NRQL
               IF(IC8RL.NE.0) THEN
                  DO 238 JA=1,IC8RL
                  IF(IC8R(JA).GT.III.AND.IC8R(JA).LT.IE) THEN
                     IE=IC8R(JA)-2
                     GO TO 298
                  END IF
 238              CONTINUE
               END IF
 298           CONTINUE
               DO 237 IK=III,IE
               IF(NWTR(KI).EQ.NRQ(IK)) THEN
                  ORQEI=.TRUE.
                  IRQEI(KI)=KI
               END IF
 237           CONTINUE
            END IF
 236        CONTINUE
C
            GO TO 233
         END IF
 234     CONTINUE
 233     CONTINUE
C
      END IF
C
      IF(IC8RL.NE.0) THEN
C
C        CLASS 8 SIGNIFYING
C
         IST=1
         DO 241 IJ=1,IC8RL
         III=IC8R(IJ)
         DO 242 I=IST,IC8L
         II=IC8(I)
         IBITP=NBITP(II)
         IWORD=NWORDP(II)
         CALL GBYTE(KBUFF(IWORD),IVAL,IBITP,NWTDW(II))
         ISCALE=NWTS(II)
         IREF  =NWTRV(II)
         IVAL  =IVAL+IREF
         VAL   =RVIND
         IF(IVAL.NE.NMASK(NWTDW(II))) THEN
            IF(ISCALE.GT.0) THEN
               VAL=IVAL/10.**ISCALE
            ELSE
               IISCALE=IABS(ISCALE)
               VAL=IVAL*10.**IISCALE
            END IF
         END IF
C
         INEXT=IC8(I+1)-2
         IF(IC8(I+1).EQ.0) INEXT=MREL
C
         OT=.FALSE.
         if(VAL.LT.(RVIND-EPS).OR.VAL.GT.(RVIND+EPS)) THEN
            IVAL=NINT(VAL)
            IRQV=NINT(RQVAL(III))
C
            IF(NWTR(II).EQ.008001) THEN
               OT=.TRUE.
               IF(iand(IVAL,IRQV).NE.IRQV) OT=.FALSE.
            ELSE
               OT=.TRUE.
               IF((abs(VAL-RQVAL(III)).gt.eps)) OT=.FALSE.
            END IF
         END IF
C
         IF(OT) THEN
            ICL=NWTR(II-1)/1000
            IF(ICL.EQ.7) IRQEI(II-1)=II-1
            DO 244 KI=II,INEXT
            IF(IB2.EQ.0) THEN
               orqei=.true.
               IRQEI(KI)=KI
            ELSE
               IE=IC8R(IJ+1)-1
               IF(IJ.EQ.IC8RL) IE=NRQL
               IF(IC7RL.NE.0) THEN
                  DO 248 JA=1,IC7RL
                  IF(IC7R(JA).GT.III.AND.IC7R(JA).LT.IE) THEN
                     IE=IC7R(JA)-1
                     GO TO 297
                  END IF
 248              CONTINUE
               END IF
 297           CONTINUE
               DO 245 IK=III,IE
               IF(NWTR(KI).EQ.NRQ(IK)) THEN
                  orqei=.true.
                  IRQEI(KI)=KI
               END IF
 245           CONTINUE
            END IF
 244        CONTINUE
C
         END IF
 242     CONTINUE
 241     CONTINUE
C
      END IF
C
      IF(.NOT.ORQEI) THEN
         KERR=45
         CALL BUERR(KERR)
         M=0
         RETURN
      END IF

C
C
C*          3. FIND POINTERS TO QUALITY CONTROL,START OF DATA
C               ----------------------------------------------
C               PRESENT INDICATORS AND %CONFIDENCE.
C               -----------------------------------
C
 300  CONTINUE
C
      I=0
      IF(IB3.NE.0) THEN
         I=I+1
         ITYPE(I)=222000
      END IF
c
      IF(IB4.NE.0) THEN
         I=I+1
         ITYPE(I)=224000
      END IF
c
      IF(IB5.NE.0) THEN
         I=I+1
         ITYPE(I)=225000
      END IF
c
      IF(IB6.NE.0) THEN
         I=I+1
         ITYPE(I)=223000
      END IF
c
      KEND=I
      IF(KEND.EQ.0) THEN
        J=0
        DO 4011 I=1,MREL
        IF(IRQEI(I).NE.0) THEN
          II=IRQEI(I)
          J=J+1
          DO 4021 K=1,N
          K1K=(K-1)*KELEM
          JK=J+K1K
          IIK=II+K1K
          INWORDP(JK)=NWORDP(IIK)
          INBITP (JK)=NBITP (IIK)
 4021     CONTINUE
          INWTR(J)  =NWTR(II)
          INWTS(J)  =NWTS(II)
          INWTRV(J) =NWTRV(II)
          INWTDW(J) =NWTDW(II)
          INWTEN(J) =NWTEN(II)
          CNAMES(J)=CWTEN(II)
          CUNITS(J)=CWTU(II)
        END IF
4011    CONTINUE
        GO TO 900
      END IF
c
      OLIST=.FALSE.
      O236=.FALSE.
      O237=.FALSE.
c
      DO 500 KT=1,KEND
c
         JQPR=0
         JQUA=0
         JQCA=0
         JQC =0
         JQCC=0
         JQCS=0
         KM=MREL
c
c        find pointers to operators
c
         kz=0
         do 3031 i=km,m
         if(nwtr(i).eq.itype(kt)) then
            kz=kz+1
            nqp(kz)=i
         end if
 3031    continue
c
         if(kz.eq.0) then
            print*,itype(kt),' not present in this message.'
            go to 500
         else
            kz=kz+1
            nqp(kz)=m
         end if
c
         do 3033 kq=1,kz-1
c
         jqua=nqp(kq)
c
         if(nwtr(jqua+1).eq.236000) then
            jq236=jqua+1
            o236=.true.
         end if
c
         if(nwtr(jqua+1).eq.237000) then
            jq237=jqua+1
            o237=.true.
         else
            o237=.false.
         end if
c
C
C        data present indicator
C
         if(.not.o236.and.o237) then
c
c           find last defined bit map
c
            do 420 i=jqua,mrel,-1
            if(nwtr(i).eq.236000) go to 421
 420        continue
c
            kerr=41
            call buerr(kerr)
            return
c
 421        continue
c
            o236=.true.
            o237=.false.
            jq236=i
            jq237=0
            ik=i
            IDPRF=0
            DO 3015 I=ik,m
            IF(NWTR(I).EQ.31031.or.NWTR(I).EQ.31192) THEN
               IDPRF=IDPRF+1
               IF(IDPRF.EQ.1) JQPR=I
            else
               if(idprf.ne.0) go to 30111
            end if
 3015       continue
C
 3016       continue
         end if
C
C        data present indicator
C
         if(.not.o237) then
            IDPRF=0
            DO 301 I=jqua,nqp(kq+1)
            IF(NWTR(I).EQ.31031.or.NWTR(I).EQ.31192) THEN
               IDPRF=IDPRF+1
               IF(IDPRF.EQ.1) JQPR=I
            else
               if(idprf.ne.0) go to 30111
            END IF
 301        CONTINUE
         end if
C
30111   continue
c
c        generating centre
c
         do 309 i=jqua,nqp(kq+1)
         if(nwtr(i).eq.1031) then
            jqcc=i
            go to 302
         end if
 309     continue
c
 302     CONTINUE
C
C        generating application
C
         DO 307 I=jqua,nqp(kq+1)
         IF(NWTR(I).EQ.1032.OR.NWTR(I).EQ.1201.OR.
     1      NWTR(I).EQ.63191) THEN
            JQCA=I
            go to 3071
         END IF
 307     CONTINUE
c
 3071    continue
c
C        percentage confidence
C
         if(itype(kt).eq.222000) then
            DO 305 I=jqua,nqp(kq+1)
            IF(NWTR(I)/1000.EQ.33.OR.NWTR(I).EQ.63192) THEN
               JQC=I
               GO TO 306
            END IF
 305        CONTINUE
         else
c           significance
c
            do 308 i=jqua,nqp(kq+1)
            IF(NWTR(I)/1000.EQ.8) then
               JQCS=i
               go to 3088
            end if
 308        continue
c
 3088       continue

            DO 3051 I=jqua,nqp(kq+1)
            IF(NWTR(I).eq.223255.or.NWTR(I).eq.224255.or.
     1         nwtr(i).eq.225255) THEN
               JQC=I
               GO TO 306
            END IF
 3051       CONTINUE
         end if
c
C
 306     CONTINUE
C
C
C*          3.1  SET INDICES TO GET Q/C.
C                GET BIT MAP FROM DATA SECTION.
C
 310  CONTINUE
C
         IF(JQUA.EQ.0) THEN
            WRITE(*,'(1H )')
            WRITE(*,'(1H ,A)') 'Q/C not present in this Bufr message.'
            WRITE(*,'(1H )')
         ELSE
C
            if(.not.o237) then
               DO 311 K=1,N
C
               K1K=(K-1)*KELEM
               JQPRK=JQPR+K1K
C
               IWPT=NWORDP(JQPRK)
               IBPT=NBITP (JQPRK)
               CALL GBYTES(KBUFF(IWPT),IBV,IBPT,1,0,IDPRF)
               DO 312 KA=1,IDPRF
               KAK=KA+K1K
               IBVAL(KAK)=IBV(KA)
 312           CONTINUE
 311           CONTINUE
            end if
C
            IDIF=MREL-IDPRF
C
            Js=0
            IF(IDIF.NE.0) THEN
               DO 313 I=1,IDIF
               Js=Js+1
               IQCDPI(Js)=0
 313           CONTINUE
            END IF
C
            JQPRM1=JQPR-1
            DO 314 I=1,IDPRF
            Js=Js+1
            IQCDPI(Js)=I+JQPRM1
 314        CONTINUE
C
            Js=0
            IF(IDIF.NE.0) THEN
               DO 315 I=1,IDIF
               Js=Js+1
               DO 316 JJ=1,N
               JJJ=Js+(JJ-1)*KELEM
               IQCI(JJJ)=0
 316           CONTINUE
 315           CONTINUE
            END IF
C
            K=JQC-1
            DO 317 I=1,IDPRF
            OK=.TRUE.
            JS=JS+1
            DO 318 JJ=1,N
            JJ1K=(JJ-1)*KELEM
            JJJ=JS+JJ1K
            IJJ=I+JJ1K
            IQCI(JJJ)=0
            IF(IBVAL(IJJ).EQ.0) THEN
               IF(OK) K=K+1
               IQCI(JJJ)=K
               OK=.FALSE.
            END IF
 318        CONTINUE
 317        CONTINUE
C
         END IF
C
C
C           4.  Create requested element list
C               -----------------------------
 400  continue
C
      if(.not.olist) then
         if(nreq(2).eq.0) nrql=0
         j=0
         do 401 i=1,mrel
         if(irqei(i).ne.0) then
            ii=irqei(i)
            j=j+1
            do 402 k=1,n
            k1k=(K-1)*KELEM
            JK=J+K1K
            IIK=II+K1K
            INWORDP(JK)=NWORDP(iik)
            INBITP (JK)=NBITP (iik)
 402        CONTINUE
            INWTR(J)  =NWTR(II)
            INWTS(J)  =NWTS(II)
            INWTRV(J) =NWTRV(II)
            INWTDW(J) =NWTDW(II)
            INWTEN(J) =NWTEN(II)
            CNAMES(J)=CWTEN(II)
            CUNITS(J)=CWTU(II)
            olist=.true.
         end if
 401     continue
      end if
C
      IF(JQUA.NE.0) THEN
C
C*          4.1 ADD DATA PRESENT INDICATOR AND Q/C.
C               -----------------------------------
 410     CONTINUE
C
c               add operator 222000
c
         J=J+1
         DO 411 K=1,N
C
         k1k=(K-1)*KELEM
         JK=J+K1K
         JQUAK=JQUA+K1K
C
         INWORDP(JK)=NWORDP(JQUAK)
         INBITP (JK)=NBITP (JQUAK)
 411     CONTINUE
         INWTR(J)  =NWTR(JQUA)
         INWTS(J)  =NWTS(JQUA)
         INWTRV(J) =NWTRV(JQUA)
         INWTDW(J) =NWTDW(JQUA)
         INWTEN(J) =NWTEN(JQUA)
         CNAMES(J)=CWTEN(JQUA)
         CUNITS(J)=CWTU(JQUA)
C
         if(o236.and..not.o237) then
            j=j+1
            do 4111 k=1,n
            k1k=(K-1)*KELEM
            JK=J+K1K
            JQUAK=JQ236+K1K
C
            INWORDP(JK)=NWORDP(JQUAK)
            INBITP (JK)=NBITP (JQUAK)
 4111       CONTINUE
            INWTR(J)  =NWTR(JQ236)
            INWTS(J)  =NWTS(JQ236)
            INWTRV(J) =NWTRV(JQ236)
            INWTDW(J) =NWTDW(JQ236)
            INWTEN(J) =NWTEN(JQ236)
            CNAMES(J)=CWTEN(JQ236)
            CUNITS(J)=CWTU (JQ236)
         end if
c
         if(o237) then
            j=j+1
            do 4112 k=1,n
            k1k=(K-1)*KELEM
            JK=J+K1K
            JQUAK=JQ237+K1K
C
            INWORDP(JK)=NWORDP(JQUAK)
            INBITP (JK)=NBITP (JQUAK)
 4112       CONTINUE
            INWTR (J) =NWTR (JQ237)
            INWTS (J) =NWTS (JQ237)
            INWTRV(J) =NWTRV(JQ237)
            INWTDW(J) =NWTDW(JQ237)
            INWTEN(J) =NWTEN(JQ237)
            CNAMES(J)=CWTEN(jq237)
            CUNITS(J)=CWTU (Jq237)
         end if
c
         if(.not.o237) then
            DO 412 I=1,MREL
            IF(IRQEI(I).NE.0) THEN
               IF(IQCDPI(I).NE.0) THEN
                  J=J+1
                  DO 413 K=1,N
C
                  k1k=(K-1)*KELEM
                  JK=J+K1K
                  IQCDK=IQCDPI(I)+K1K
C
                  INWORDP(JK)=NWORDP(IQCDK)
                  INBITP (JK)=NBITP (IQCDK)
 413              CONTINUE
                  INWTR(J)  =NWTR(IQCDPI(I))
                  INWTS(J)  =NWTS(IQCDPI(I))
                  INWTRV(J) =NWTRV(IQCDPI(I))
                  INWTDW(J) =NWTDW(IQCDPI(I))
                  INWTEN(J) =NWTEN(IQCDPI(I))
                  CNAMES(J)=CWTEN(IQCDPI(I))
                  CUNITS(J)=CWTU(IQCDPI(I))
               END IF
            END IF
 412        CONTINUE
         end if
C
C        GENERATING CENTRE
C
         if(jqcc.ne.0) then
            J=J+1
            DO 417 K=1,N

            k1k=(K-1)*KELEM
            JK=J+K1K
            JQCCK=JQCC+K1K
C
            INWORDP(JK)=NWORDP(JQCCK)
            INBITP (JK)=NBITP (JQCCK)
 417        CONTINUE
            INWTR(J)  =NWTR(JQCC)
            INWTS(J)  =NWTS(JQCC)
            INWTRV(J) =NWTRV(JQCC)
            INWTDW(J) =NWTDW(JQCC)
            INWTEN(J) =NWTEN(JQCC)
            CNAMES(J)=CWTEN(JQCC)
            CUNITS(J)=CWTU(JQCC)
         end if
c
C        generating application
C
         if(jqca.ne.0) then
            J=J+1
            DO 414 K=1,N

            k1k=(K-1)*KELEM
            JK=J+K1K
            JQCAK=JQCA+K1K
C
            INWORDP(JK)=NWORDP(JQCAK)
            INBITP (JK)=NBITP (JQCAK)
 414        CONTINUE
            INWTR(J)  =NWTR(JQCA)
            INWTS(J)  =NWTS(JQCA)
            INWTRV(J) =NWTRV(JQCA)
            INWTDW(J) =NWTDW(JQCA)
            INWTEN(J) =NWTEN(JQCA)
            CNAMES(J)=CWTEN(JQCA)
            CUNITS(J)=CWTU(JQCA)
         end if
c
c        significance
c
         if(jqcs.ne.0) then
            J=J+1
            DO 4147 K=1,N

            k1k=(K-1)*KELEM
            JK=J+K1K
            JQCAK=JQCS+K1K
C
            INWORDP(JK)=NWORDP(JQCAK)
            INBITP (JK)=NBITP (JQCAK)
 4147       CONTINUE
            INWTR(J)  =NWTR(JQCS)
            INWTS(J)  =NWTS(JQCS)
            INWTRV(J) =NWTRV(JQCS)
            INWTDW(J) =NWTDW(JQCS)
            INWTEN(J) =NWTEN(JQCS)
            CNAMES(J)=CWTEN(JQCS)
            CUNITS(J)=CWTU(JQCS)
         end if
C
C        calss 33 elements
C
         DO 415 I=1,MREL
         IF(IRQEI(I).NE.0) THEN
            OK=.TRUE.
            DO 416 JJ=1,N
            JJ1K=(JJ-1)*KELEM
            IJJ=I+JJ1K
            IF(IQCI(IJJ).NE.0) THEN
               IF(OK) J=J+1
               OK=.FALSE.
               INWTR(J)  =NWTR  (IQCI(IJJ))
               INWTS(J)  =NWTS  (IQCI(IJJ))
               INWTRV(J) =NWTRV (IQCI(IJJ))
               INWTDW(J) =NWTDW (IQCI(IJJ))
               INWTEN(J) =NWTEN (IQCI(IJJ))
               CNAMES(J)=CWTEN (IQCI(IJJ))
               CUNITS(J)=CWTU  (IQCI(IJJ))
C
               JJJ=J+JJ1K
               JQCIJJ=IQCI(IJJ)+JJ1K
C
               INWORDP(JJJ)=NWORDP(JQCIJJ)
               INBITP (JJJ)=NBITP (JQCIJJ)
            END IF
 416        CONTINUE
C
         END IF
 415     CONTINUE
      END IF
c
 3033 continue
c
 500  continue
C
C*          5. MAKE ONE TO ONE CORRESPONDENCE BETWEEN ELEMENTS AND
C              Q/C,STATISTICS,DIFFERENCE STATISTICS OR SUBSTITUTED VALUES
C              LEAVING BEHIND DATA PRESENT INDICATORS.
C
      IF(IB3.NE.0) THEN
      END IF
C
      IF(IB4.NE.0) THEN
      END IF
C
      IF(IB5.NE.0) THEN
      END IF
C
      IF(IB6.NE.0) THEN
      END IF
C
C*          9.  UPDATE TOTAL NUMBER OF ELEMENTS.
C               --------------------------------
 900  CONTINUE
C
      M=J
      nfcm=0
      nfucm=1
C
      RETURN
C
      END
      SUBROUTINE BUS012( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
C
C**** *BUS012*
C
C
C     PURPOSE.
C     --------
C          Expands section 0,1 AND 2 of Bufr message.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUS012( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C        OUTPUT:
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KSEC0*   -  array containing section 0 information
C                            KSEC0( 1)-- length of section 0 (bytes)
C                            KSEC0( 2)-- total length of Bufr message (bytes)
C                            KSEC0( 3)-- Bufr Edition number
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C               *KSEC2*   -  array containing section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(47) RDB key
C               *KERR*    -  returned error code
C
C     METHOD.
C     --------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUEXS0          - unpack section 0
C          BUEXS1          - unpack section 1
C          BUEXS2          - unpack section 2
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/07/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
      COMMON /BCOMREQ/ NREQ(2),NRQL,NRQ(JELEM),RQVAL(JELEM)
C
C             *NREQ*    -  flag
C                          bit number     meaning
C
C                              1        - 0 no bit map delivered to user
C                                         1    bit map delivered to user
C                              2        - 0 no partial expansion
C                                         1    partial expansion
C                              3        - 0 no Q/C required
C                                       - 1    Q/C required
C                              4        - 0 no statistics required
C                                       - 1    statistics
C                              5        - 0 no diffrence statistics
C                                       - 1    difference statistics
C                              6        - 0 no substituted values
C                                       - 1    substituted values
C             *NRQL*    -  number of requested elements
C             *NRQ*     -  list of requested table B reference
C             *RQVAL*   -  list of values signifying requested element
C                          (say pressure  at 50000 Pa)
c
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSUP(*),KSEC0(*),KSEC1(*)
      DIMENSION KSEC2(*)
C
      CHARACTER*4 CECMWF,CUSER
C
C     ------------------------------------------------------------------
C*          1.   SET CONSTANTS.
C                --------------

!      write(*,*) 'BUS012: kbufl = ', kbufl
 100  CONTINUE
C
      KERR=0
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUEVAR(KERR)
         CECMWF='ECMF'
      END IF
C
      IF(CUSER.NE.'USER') THEN
         NREQ(1)=0
         nreq(2)=0
         NRQL=0
      END IF
C     -----------------------------------------------------------------
C
C*          2.  EXPAND SECTION 0.
C               -----------------
 200  CONTINUE
C
      CALL BUEXS0( KBUFL,KBUFF,KSUP,KSEC0,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          3.   EXPAND SECTION 1.
C                ------------------
 300  CONTINUE
C
      CALL BUEXS1( KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KERR )
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C
C*          4.  EXPAND SECTION 2.
C               -----------------
 400  CONTINUE
C
      CALL BUEXS2( KBUFL,KBUFF,KSUP,KSEC1,KSEC2,KERR )
      IF(KERR.GT.0) RETURN
C
C*          5.  EXPAND SECTION 3 (PRELIMINARY ITEMS).
C               -------------------------------------
 500  CONTINUE
C
!      print *, 'BUS012: KSEC1 = ', KSEC1(1)
!      print *, 'BUS012: KSEC2 = ', KSEC2(1)
!      print *, 'BUS012: KSUP  = ', KSUP(1)
!      print *, 'BUS012: KBUFL = ', KBUFL

      CALL BUEXS3P(KBUFL,KBUFF,KSUP,KERR)
C
      RETURN
      END
      SUBROUTINE BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
C
C**** *BUSEL*
C
C
C     PURPOSE.
C     --------
C          Returns list of Data Descriptors as in Section 3  of Bufr
C          message and total/requested list of elements.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)*
C
C
C        OUTPUT:
C               *KTDLEN*  -  number of data descriptors in section 3
C               *KTDLST*  -  array containing data descriptors in section 3
C               *KTDEXL*  -  number of entries in list of expanded data
C                            descriptors
C               *KTDEXP*  -  array containig expanded data descriptors
C               *KERR*    -  returned error code
C
C
C     METHOD.
C     -------
C
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMEL/ NTDLEN,NTDLST(JELEM),NTDEXL,NTDEXP(JELEM)
C
C             NTDLEN - number of Data descriptors in section 3
C             NTDLST - list of Data descriptors
C             NTDEXL - number of expanded Data Descriptors
C             NTDEXP - list of expanded Data descriptors
C
C
      DIMENSION  KTDLST(*),KTDEXP(*)
C
C
C     -----------------------------------------------------------------

C*          1.  PUT LIST OF ELEMENTS FROM COMMON BLOCK
C               --------------------------------------
C               TO REQUESTED ARRAYS.
C               --------------------
 100  CONTINUE
C
      KTDLEN=NTDLEN
      KTDEXL=NTDEXL
C
      DO 101 I=1,NTDLEN
      KTDLST(I)=NTDLST(I)
 101  CONTINUE
C
      DO 102 I=1,NTDEXL
      KTDEXP(I)=NTDEXP(I)
 102  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUSRP(KBUFL,KBUFF,KSEC3,KJ,KJ1,KDD,KSTACK,KELEM,KERR)
C
C**** *BUSRP*
C
C
C     PURPOSE.
C     --------
C          Resolve data descriptor replication problem.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSRP(KBUFL,KBUFF,KSEC3,KJ,KJ1,KDD,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KSEC3*   -  array containig section 3 information
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C               *KDD*      - data descriptor
C        OUTPUT:
C               *KJ*       - pointer to kstack array
C               *KJ1*      - pointer to last element in kstack
C               *KSTACK*   - list of data descriptors
C               *KERR*     - return code
C
C     METHOD.
C     -------
C          NONE.
C
C     EXTERNALS.
C     ----------
C
C          BUSTDR            - resolve table D reference
C          BUUPWT              - update working tables
C          GBYTE             - unpack bit pattern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      CHARACTER CWTEN*64,CWTU*24
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      DIMENSION KBUFF(KBUFL),ILIST(JELEM)
      DIMENSION KSTACK(*),KSEC3(JSEC3),IMASK(8)
C
      DATA IMASK/1,2,4,8,16,32,64,128/
C
C     ------------------------------------------------------------------
C
C*          1.   STORE K, NUMBER OF DESCRIPTORS TO BE REPLICATED.
C                ------------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      IF  = KDD / 100000
      IDIF= KDD - IF * 100000
      IX  = IDIF / 1000
      IY  = IDIF - IX * 1000
      K   = IX
C
      IF(IY.EQ.0) ODREPF=.TRUE.
C
C*          1.1   DELAYED REPLICATION ?
C                 ---------------------
 110  CONTINUE
C
      IF( IY .NE. 0 ) THEN
C
C*          1.2   STORE NUMBER OF DESCRIPTORS, K, AND REPLICATION
C                 -----------------------------------------------
C                 FACTOR JR.
C                 ----------
 120     CONTINUE
C
c         K  = IX
         JR = IY
         GO TO 500
      END IF
C
C     ------------------------------------------------------------------
C
C*          2.   GET NEXT DESCRIPTOR.
C                --------------------
 200  CONTINUE
C
      KJ =KJ + 1
      KDD= KSTACK(KJ)
C
C     ------------------------------------------------------------------
C
C*          2.1  REPLICATION FACTOR ?
C                --------------------
 210  CONTINUE
C
      IF(KDD.NE.31001.AND.KDD.NE.31002.and.
     1   KDD.NE.31000.and.
     1   kdd.ne.31011.and.kdd.ne.31012 )THEN
C
C*          2.1.1  SEQUENCE DESCRIPTOR ?
C                  ---------------------
C
         IF=KDD/100000
C
         IF( IF.EQ.3) THEN
C
C*          2.1.1.1  SOLVE TABLE D REFERENCE.
C                    ------------------------
            CALL BUSTDR(KJ,KJ1,KDD,KSTACK,KERR)
            IF(KERR.GT.0) THEN
               DO 252 IQ=1,JELEM
               NSTACK(IQ)=0.
 252           CONTINUE
               RETURN
            END IF
            GO TO 200
         END IF
C
         IF( IF.EQ.2) THEN
            CALL BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)
            IF(KERR.GT.0) THEN
               CALL BUERR(KERR)
               RETURN
            END IF
            GO TO 200
         END IF
C
         KERR=36
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C
C*          3.  UPDATE WORKING TABLE.
C               ---------------------
 300  CONTINUE
C
      if(kdd.eq.31031.or.kdd.eq.31192) then
         nwt=nwt+1
         nwtr(nwt)=kdd
         nwts(nwt)=0
         nwtrv(nwt)=0
         nwtdw(nwt)=1
         cwten(nwt)='DATA PRESENT INDICATOR'
         cwtu (nwt)='NUMERIC'
         m=m+1
      elseif(kdd.eq.33007.or.kdd.eq.63192) then
         nwt=nwt+1
         nwtr(nwt)=kdd
         nwts(nwt)=0
         nwtrv(nwt)=0
         nwtdw(nwt)=7
         cwten(nwt)='% CONFIDENCE'
         cwtu (nwt)='NUMERIC'
         m=m+1
      else
         CALL BUUPWT(KDD,KELEM,KERR)
         IF(KERR.GT.0) RETURN
      end if
C
C     CHECK IF NEXT DESCRIPTOR CANCEL OPERATOR FOR DELAYED
C     REPLICATION
C
      IIIF=KSTACK(KJ+1)/100000
      IIII=KSTACK(KJ+1)-IIIF*100000
      IIIX=IIII/1000
      IIIY=IIII-IIIX*1000
C
      IF(IIIF.EQ.2.AND.IIIY.EQ.0) THEN
         KJ=KJ+1
         KDD=KSTACK(KJ)
         CALL BUPRCO(KBUFL,KBUFF,KJ,KDD,KSTACK,KELEM,KERR)
         IF(KERR.GT.0) THEN
            CALL BUERR(KERR)
            RETURN
         END IF
      END IF
C
C     ------------------------------------------------------------------
C
C*          4.  LOCATE AND STORE JR, THE REPLICATION FACTOR FROM DATA.
C               ------------------------------------------------------
 400  CONTINUE
C
      IB=0
      IF(IAND(ksec3(4),IMASK(7)).NE.0) IB=1
C
C     RESOLVE MARKERS
C
      IF(OMARKER) THEN
         CALL BUPMRK(KBUFL,KBUFF,KSEC3,KELEM,KERR)
         IF(KERR.GT.0) RETURN
      END IF
C
      IF(IB.EQ.0) THEN
C
C        UNCOMPRESSED DATA
C
         IBIT=0
C
         DO 401 J=1,NWT-1
C
         IBIT=IBIT+NWTDW(J)
C
 401     CONTINUE
C
         IBIT = IBIT + 32 + NBPTB
C
         IWORD= IBIT/NBPW
         ISKIP= IBIT - IWORD*NBPW
         IWORD= IWORD + NWPTB
C
         CALL GBYTE(KBUFF(IWORD),JR,ISKIP,NWTDW(NWT))
         IF(JR.EQ.0) THEN
            KJ=KJ+K
            GO TO 640
         END IF
c         IF(JR.EQ.0) THEN
c            KERR=18
c            PRINT*,'BUSRP :'
c            CALL BUERR(KERR)
c            WRITE(*,'(A)') (CWTEN(IH),IH=1,5)
c            RETURN
c         END IF
      ELSE
C
C        COMPRESSED DATA
C
         IBIT=32+NBPTB
         IWORD=IBIT/NBPW
C
         IWORDP=NWPTB+IWORD
         IBITP =IBIT-IWORD*NBPW
C
         DO 402 I=2,NWT
         IWRD=IWORDP
         IBTP=IBITP
C
         IF(NWTDW(I-1).EQ.0) THEN
            IBITP =IBITP
            IWORDP=IWORDP
            GO TO 402
         END IF
C
         IBTP=IBTP+NWTDW(I-1)
         IF(IBTP.GE.NBPW) THEN
            IW=IBTP/NBPW
            IBTP=IBTP-IW*NBPW
            IWRD=IWRD+IW
         END IF
C
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IWRD,IBTP,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            PRINT*,'BUSRP:'
            CALL BUERR(KERR)
            RETURN
         END IF
         IF(IWRD.GT.KBUFL) THEN
            KERR=26
            PRINT*,'BUSRP:'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         IF(CWTU(I-1)(1:3).EQ.'CCI') THEN
            IWTIWS=NWTDW(I-1)+6+N*IDWINC*8
         ELSE
            IWTIWS=NWTDW(I-1)+6+N*IDWINC
         END IF
C
         IBIT = IBITP + IWTIWS
         IWORD= IBIT/NBPW
C
         IBITP = IBIT - IWORD*NBPW
         IWORDP= IWORDP + IWORD
C
         IF(IWORDP.GT.KBUFL) THEN
            KERR=26
            PRINT*,'BUSRP :'
            CALL BUERR(KERR)
            RETURN
         END IF
 402     CONTINUE
C
C        UNPACK JR DELAYED REPLICATION FACTOR
C
         IW=IWORDP
         IBT=IBITP
         CALL BUNPCK(NBPW,KBUFF,IR0,IW,IBT,NWTDW(NWT),KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KBUFF,IDWINC,IW,IBT,6,KERR)
         IF(KERR.GT.0) RETURN
         IF(IDWINC.GT.JBPW) THEN
            KERR=15
            PRINT*,' BUSRP :'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         IF(IDWINC.NE.0) THEN
            CALL BUNPCK(NBPW,KBUFF,INCR,IW,IBT,IDWINC,KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
         IF(IR0.EQ.NMASK(NWTDW(NWT))) THEN
            JR=NMASK(NWTDW(NWT))
            KERR=19
            PRINT*,'BUSRP :'
            CALL BUERR(KERR)
            RETURN
         ELSE
            IWTPR0=NWTRV(NWT)+IR0
C
            IF(IDWINC.EQ.0) THEN
               JR=IWTPR0
            ELSE
               IF(INCR.EQ.NMASK(IDWINC)) THEN
                  JR=NMASK(NWTDW(NWT))
                  KERR=19
                  PRINT*,'BUSRP :'
                  CALL BUERR(KERR)
                  RETURN
               ELSE
                  JR= IWTPR0 + INCR
               END IF
            END IF
         END IF
      END IF
C
      IF(JR.EQ.0) THEN
         KJ=KJ+K
         GO TO 640
      END IF
c
      JRTK=JR*K+KJ1-K
      IF(JRTK.GT.JELEM) THEN
         KERR=19
         PRINT*,'BUSRP :'
         CALL BUERR(KERR)
         PRINT*,'Replication factor =',JR
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          5.  GET NEXT K DESCRIPTORS.
C               -----------------------
 500  CONTINUE
C
      DO 501 J=1,K
C
      ILIST(J)=KSTACK(KJ+J)
C
 501  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.  ADD JR TIMES K DESCRIPTORS IN PLACE OF K
C               ----------------------------------------
C               DESCRIPTORS OBTAINED.
C               ---------------------
 600  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.1  PUSH DOWN DESCRIPTORS IN KSTACK FOR (JR-1)*K PLACES
C                ---------------------------------------------------
C                STARTING AT KJ1 AND ENDING AT KJ+K.
C                -----------------------------------
 610  CONTINUE
C
      JRKM1=(JR-1)*K
C
      DO 611 J=KJ1,KJ+K,-1
C
      KSTACK(J+JRKM1)=KSTACK(J)
C
 611  CONTINUE
C
C*          6.2  INSERT LIST IN THE STACK.
C                -------------------------
 620  CONTINUE
C
      DO 622 J=1,JR
C
      KJJM1K=KJ+(J-1)*K
C
      DO 623 J1=1,K
C
      KSTACK(KJJM1K+J1)=ILIST(J1)
C
 623  CONTINUE
 622  CONTINUE
C
C     ------------------------------------------------------------------
C*          6.3  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C                ----------------------------------------
 630  CONTINUE
C
      KJ1 = KJ1  + (JR-1)*K
C
C     ------------------------------------------------------------------
C*          6.4  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 640  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C
C     ------------------------------------------------------------------
 700  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUSRQ(KREQ,KRQL,KRQ,RQV,KERR)
C
C**** *BUSRQ*
C
C
C     PURPOSE.
C     --------
C          Set list of Bufr table B element references
C     for partial expansion.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSRQ(KREQ,KRQL,KRQ,RQV,KERR)*
C
C        INPUT :
C               *KREQ*    -  flag
C                            bit number     meaning
C                                1        - 0 no bit map delivered to user
C                                           1    bit map delivered to user
C                                2        - 0 no partial expansion
C                                           1    partial expansion
C                                3        - 0 no Q/C required
C                                         - 1    Q/C required
C                                4        - 0 no statistics required
C                                         - 1    statistics
C                                5        - 0 no diffrence statistics
C                                         - 1    difference statistics
C                                6        - 0 no substituted values
C                                         - 1    substituted values
C               *KRQL*    -  number of requested elements
C               *KRQ*     -  list of requested table B reference
C               *RQV*     -  list of values signifying requested element
C                            (say pressure  at 50000 Pa)
C        OUTPUT:
C               *KERR*    -  returned error code
C
C
C     METHOD.
C     -------
C
C          None.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMREQ/ NREQ(2),NRQL,NRQ(JELEM),RQVAL(JELEM)
C
C             *NREQ*    -  flag
C                          bit number     meaning
C
C                              1        - 0 no bit map delivered to user
C                                         1    bit map delivered to user
C                              2        - 0 no partial expansion
C                                         1    partial expansion
C                              3        - 0 no Q/C required
C                                       - 1    Q/C required
C                              4        - 0 no statistics required
C                                       - 1    statistics
C                              5        - 0 no diffrence statistics
C                                       - 1    difference statistics
C                              6        - 0 no substituted values
C                                       - 1    substituted values
C             *NRQL*    -  number of requested elements
C             *NRQ*     -  list of requested table B reference
C             *RQVAL*   -  list of values signifying requested element
C                          (say pressure  at 50000 Pa)
C
C
      COMMON /BCOMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
C
      CHARACTER*4 CUSER,CECMWF
      DIMENSION KRQ  (*),RQV(*),Kreq(*)
C
C     ------------------------------------------------------------------
C*          1. Move requested elements into common block /comreq/.
C              ---------------------------------------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      CUSER='USER'
C
      NRQL=KRQL
      NREQ(1)=KREQ(1)
      NREQ(2)=KREQ(2)
C
      DO 101 I=1,KRQL
      NRQ(I)=KRQ(I)
      RQVAL(I)=RQV(I)
 101  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUSTDR(KJ,KJ1,KDD,KSTACK,KERR)
C
C**** *BUSTDR*
C
C
C     PURPOSE.
C     --------
C          Solve Bufr table D reference.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSTDR(KJ,KJ1,KDD,KSTACK,KERR)*
C
C        INPUT :
C                 *KDD*      - data descriptor
C        OUTPUT:
C                 *KJ*       - pointer to kstack array
C                 *KJ1*      - pointer to last element in kstack
C                 *KSTACK*   - list of data descriptors
C                 *KERR*     - return error code
C
C
C     METHOD.
C     -------
C
C           NONE.
C
C     EXTERNALS.
C     ----------
C
C           NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCOMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      CHARACTER CTABBEN*64,CTABBU*24
C
      DIMENSION ILIST(JELEM),KSTACK(*)
C
C     ------------------------------------------------------------------
C
C*          1.   OBTAIN LIST OF DESCRIPTORS FROM BUFR TABLE D.
C                ---------------------------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      DO 110 J=1,JTAB
C
      IF(KDD.EQ.NTABDTR(J)) THEN
         I=J
         GO TO 120
      END IF
C
 110  CONTINUE
C
      KERR=20
      PRINT*,' BUSTDR :',KDD
      CALL BUERR(KERR)
      RETURN
C
 120  CONTINUE
C
      J1=NTABDST(I)
      J2=NTABDL (I)
      J3=0
C
      DO 121 J=J1,J1+J2-1
C
      J3 = J3 +1
      ILIST(J3) = NTABDSQ(J)
C
 121  CONTINUE
C
C     ------------------------------------------------------------------
C*          2.  PUSH DOWN DATA DESCRIPTION DESCRIPTORS
C               --------------------------------------
C               TO MAKE ROOM FOR LIST.
C               ----------------------
 200  CONTINUE
C
      J2M1=J2-1
C
      DO 210 J=KJ1,KJ+1,-1
C
      KSTACK(J+J2M1) = KSTACK(J)
C
 210  CONTINUE
C
C     ------------------------------------------------------------------
C*          3.  INSERT LIST IN PLACE OF SEQUENCE DESCRIPTORS.
C               ---------------------------------------------
 300  CONTINUE
C
      KJM1=KJ-1
C
      DO 310 J=1,J3
C
      KSTACK(KJM1+J)= ILIST(J)
C
 310  CONTINUE
C
C     ------------------------------------------------------------------
C*          4.  ADJUST DESCRIPTOR COUNT FOR LIST LENGTH.
C               ----------------------------------------
 400  CONTINUE
C
      KJ  = KJ  - 1
      KJ1 = KJ1 +J3 -1
C     ------------------------------------------------------------------
C*          4.1  ADJUST NUMBER OF DATA DESCRIPTORS NOT PRESENT.
C                ----------------------------------------------
 610  CONTINUE
C
      IF(N221.NE.0)  N221= KJ1  - KJ + 1
C     -----------------------------------------------------------------
 500  CONTINUE
C
      RETURN
C
 9901 FORMAT(1H ,' BUSTDR : table D reference not found, error=',I2)
C
      END
      SUBROUTINE BUUATB(KBUFL,KBUFF,KJ,KY,KSTACK,KELEM,KERR)
C
C**** *BUUATB*
C
C
C     PURPOSE.
C     --------
C          Update augmented Bufr table B.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUATB(KBUFL,KBUFF,KJ,KY,KSTACK,KELEM,KERR)*
C
C        INPUT :
C               *KBUFL*   -  length of bufr message (words)
C               *KBUFF*   -  array containing bufr message
C               *KJ*      -  pointer to kstack array
C               *KY*      -  operand of the data descriptor operator
C               *KELEM*   -
C        OUTPUT:
C               *KSTACK* - list of elements
C               *KERR*   - return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          GBYTE        - pack bit pattern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       04/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCOMATBC/ CATBEN(JTAB),CATBU (JTAB)
C
C             CATBEN      - augmented table B element name
C             CATBU       - augmented table B units
C
C
      COMMON /BCOMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCOMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C             CWTU     -  working table units
C
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24
C
      DIMENSION KBUFF(KBUFL)
      DIMENSION KSTACK(*)
C     ------------------------------------------------------------------
C
C*          1.   UPDATE AUGMENTED TABLE B .
C                --------------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
C*          1.1 Y = 0 ?
C               -------
 110  CONTINUE
C
      IF( KY.EQ.0) THEN
C
C*          1.1.1 CLEAR AUGMENTED TABLE B.
C                 ------------------------
         NJA= 0
C
         DO 111 J=1,JTAB
C
         NATBTR(J)= 0
         NATBS (J)= 0
         NATBRV(J)= 0
         NATBDW(J)= 0
         CATBEN(J)=' '
         CATBU (J)=' '
C
 111     CONTINUE
C
         GO TO 300
      END IF
C
C*          1.2  GET NEXT DESCRIPTOR FROM STACK.
C                -------------------------------
 120  CONTINUE
C
      KJ=KJ + 1
      KDD = KSTACK(KJ)
C
C*          1.3  ELEMENT DESCRIPTOR  ?
C                ---------------------
 130  CONTINUE
C
      IF  = KDD /100000
      IDIF= KDD -IF*100000
      IX  = IDIF/1000
      IY  = IDIF-IX*1000
C
      IF(IF.EQ.0) THEN
C
C*          1.3.1 ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
         NWT = NWT + 1
         CWTEN(NWT)='REFERENCE VALUE'
         CWTU (NWT)=' '
         NWTDW(NWT)= KY
         NWTS (NWT)=0
         NWTRV(NWT)=0
         M=M+1
C
C*          1.3.2 ADD ENTRY TO AUGMENTED TABLE B .
C                 --------------------------------
         DO 131 J=1,JTAB
C
         IF(NTABBTR(J).EQ.KDD) THEN
            I=J
            GO TO 133
         END IF
C
 131     CONTINUE
C
         KERR=23
         PRINT*,'BUUATB :'
         CALL BUERR(KERR)
C
         GO TO 300
C
 133  CONTINUE
C
         NJA=NJA + 1
C
         NATBTR(NJA)=NTABBTR(I)
         NATBS (NJA)=NTABBS (I)
         NATBRV(NJA)=NTABBRV(I)
         NATBDW(NJA)=NTABBDW(I)
         CATBEN(NJA)=CTABBEN(I)
         CATBU (NJA)=CTABBU (I)
C
C*          1.3.3 COMPLITE ENTRY WITH NEW REFERENCE VALUE
C                 ----------------------------------------
C                 FROM DATA SECTION.
C                 ------------------
C
         IBIT=0
C
         DO 134 J=1,NWT-1
C
         IBIT =IBIT + NWTDW(J)
C
 134     CONTINUE
C
         IBIT = IBIT +32 + NBPTB
         IWORD= IBIT/NBPW
         ISKIP= IBIT - IWORD*NBPW
         IWORD= IWORD + NWPTB
C
C
C           1.3.4 CHECK IF REFERENCE VALUE NEGATIVE
C
         CALL GBYTE(KBUFF(IWORD),ISGN,ISKIP,1)
         IF(ISGN.EQ.1) THEN
            IBIT=IBIT+1
            IWORD= IBIT/NBPW
            ISKIP= IBIT - IWORD*NBPW
            IWORD= IWORD + NWPTB
C
            CALL GBYTE(KBUFF(IWORD),NATBRV(NJA),ISKIP,KY-1)
            NATBRV(NJA)=-NATBRV(NJA)
         ELSE
            CALL GBYTE(KBUFF(IWORD),NATBRV(NJA),ISKIP,KY)
         END IF
C
C           1.3.5 UPDATA WORKING TABLE ENTRIES.
C                 -----------------------------
 135     CONTINUE
C
         if(kdd.eq.31031.or.kdd.eq.31192) then
            nwt=nwt+1
            nwtr(nwt)=kdd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=1
            cwten(nwt)='DATA PRESENT INDICATOR'
            cwtu (nwt)='NUMERIC'
            m=m+1
         elseif(kdd.eq.33007.or.kdd.eq.63192) then
            nwt=nwt+1
            nwtr(nwt)=kdd
            nwts(nwt)=0
            nwtrv(nwt)=0
            nwtdw(nwt)=7
            cwten(nwt)='% CONFIDENCE'
            cwtu (nwt)='NUMERIC'
            m=m+1
         else
            CALL BUUPWT(KDD,KELEM,KERR)
            IF(KERR.GT.0) RETURN
         end if
C
         GO TO 120
C
      END IF
C
C     ------------------------------------------------------------------
C
C*           1.4   CHANGE REFERENCE VALUE ?
C                  ------------------------
 140  CONTINUE
C
      IF( IF.EQ.2.AND.IX.EQ.3) THEN
         IF(IY.EQ.255) GO TO 300
      END IF
C     ------------------------------------------------------------------
 200  CONTINUE
C
      KERR=23
      PRINT*,'BUUATB :'
      CALL BUERR(KERR)
C
C     ------------------------------------------------------------------
 300  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUUKEY( KSEC1,KSEC2,KEY,KSUP,KERR )
C
C**** *BUUKEY*
C
C
C     PURPOSE.
C     --------
C          Expands local ECMWF information from section 2.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)*
C
C        INPUT :
C               *KSEC1*   -  array containing section 1 information
C                            KSEC1( 1)-- length of section 1 (bytes)
C                            KSEC1( 2)-- Bufr Edition number
C                            KSEC1( 3)-- originating centre
C                            KSEC1( 4)-- update sequence number
C                            KSEC1( 5)-- flag (presence of section 2)
C                            KSEC1( 6)-- bufr message type
C                            KSEC1( 7)-- bufr message subtype
C                            KSEC1( 8)-- version number of local table used
C                            KSEC1( 9)-- year
C                            KSEC1(10)-- month
C                            KSEC1(11)-- day
C                            KSEC1(12)-- hour
C                            KSEC1(13)-- minute
C                            KSEC1(14)-- Bufr Master table
C                            KSEC1(15)-- version number of Master table used
C                            KSEC1(16) - KSEC1(JSEC1) -- local ADP centre
C                                        information(PACKED FORM)
C               *KSEC2*   -  array containing section 2 information
C                            KSEC2( 1)-- length of section 2 (bytes)
C                            KSEC2( 2) to KSEC2(JSEC2) local ADP centre
C                                        information(PACKED FORM)
C        OUTPUT:
C               *KEY*     -  array containing RDB information
C                            KEY( 1)-- length of section 2 (bytes)
C                            KEY( 2)-- RDB type
C                            KEY( 3)-- RDB subtype
C                            KEY( 4)-- year
C                            KEY( 5)-- month
C                            KEY( 6)-- day
C                            KEY( 7)-- hour
C                            KEY( 8)-- minute
C                            KEY( 9)-- second
C                            KEY(10)-- longitude1
C                            KEY(11)-- latitude1
C                            KEY(12)-- longitude2
C                            KEY(13)-- latitude2
C                            KEY(14)-- number of subsets
C                            KEY(15)-- ident (numeric)
C                            KEY(16)-- ident ( CCITTIA5) one character
C                            KEY(17)-- ident ( CCITTIA5) one character
C                            KEY(18)-- ident ( CCITTIA5) one character
C                            KEY(19)-- ident ( CCITTIA5) one character
C                            KEY(20)-- ident ( CCITTIA5) one character
C                            KEY(21)-- ident ( CCITTIA5) one character
C                            KEY(22)-- ident ( CCITTIA5) one character
C                            KEY(23)-- ident ( CCITTIA5) one character
C                            KEY(24)-- ident ( CCITTIA5) one character
C                            KEY(25)-- total Bufr message length
C                            KEY(26)-- day    (RDB insertion)
C                            KEY(27)-- hour   (RDB insertion)
C                            KEY(28)-- minute (RDB insertion)
C                            KEY(29)-- second (RDB insertion)
C                            KEY(30)-- day    (MDB insertion)
C                            KEY(31)-- hour   (MDB insertion)
C                            KEY(32)-- minute (MDB insertion)
C                            KEY(33)-- second (MDB insertion)
C                            KEY(34)-- correction number
C                            KEY(35)-- part
C                            KEY(36)-- 0
C                            KEY(37)-- correction number
C                            KEY(38)-- part
C                            KEY(39)-- 0
C                            KEY(40)-- correction number
C                            KEY(41)-- part
C                            KEY(42)-- 0
C                            KEY(43)-- correction number
C                            KEY(44)-- part
C                            KEY(45)-- 0
C                            KEY(46)-- the lowest Q/C % confidence
C               *KSUP*    -  array containing suplementary information
C                         -  KSUP( 1) -- IDIM1, dimension of KSEC1
C                         -  KSUP( 2) -- IDIM2, dimension of KSEC2
C                         -  KSUP( 3) -- IDIM3, dimension of KSEC3
C                         -  KSUP( 4) -- IDIM4, dimension of KSEC4
C                         -  KSUP( 5) -- M (number of elements in values array,
C                                           first index)
C                         -  KSUP( 6) -- N (number of subsets,second index of
C                                           values array)
C                         -  KSUP( 7) -- JVC (number of elements in CVAL array)
C                         -  KSUP( 8) -- total bufr message length in bytes
C                         -  KSUP( 9) -- IDIM0, dimension of KSEC0
C               *KERR*    -  returned error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          BUNPCK          - unpack bit pattern
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       17/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCOMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
C
      CHARACTER*4 CECMWF,CUSER
C
      DIMENSION KSEC1(JSEC1),KSEC2(JSEC2),KEY(JKEY)
      DIMENSION IDUM(8)     ,KSUP (JSUP ),KSEC3(JSEC3)
C
C
C*          1. UNPACK LOCAL ADP CENTRE INFORMATION.
C              ------------------------------------
 100  CONTINUE
C
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUEVAR(KERR)
         CECMWF='ECMF'
      END IF
C
      DO 101 I=1,JKEY
      KEY(I)=0
 101  CONTINUE
c
      KEY(1)=KSEC2(1)
      IW=2
      IB=0
      IF(KSEC2(1).EQ.52) THEN
         CALL BUNPCK(NBPW,KSEC2,KEY(2),IW,IB, 8,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(3),IW,IB, 8,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(4),IW,IB,12,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(5),IW,IB, 4,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(6),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(7),IW,IB, 5,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(8),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(9),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(10),IW,IB,26,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(11),IW,IB,25,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 7,KERR)
         IF(KERR.GT.0) RETURN
C
         IF(KSUP(6).GT.1.OR.KEY(2).EQ.2.OR.KEY(2).EQ.3.OR.
     1      KEY(2).EQ.12.or.key(2).eq.8) THEN
C
            CALL BUNPCK(NBPW,KSEC2,KEY(12),IW,IB,26,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(13),IW,IB,25,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 7,KERR)
            IF(KERR.GT.0) RETURN
            IF(KSUP(6).GT.255.OR.KEY(3).GE.121.AND.
     1         KEY(3).LE.130.OR.KEY(3).EQ.31) THEN
               CALL BUNPCK(NBPW,KSEC2,KEY(14),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPKS(NBPW,KSEC2,IDUM(1),IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
            ELSE
               CALL BUNPCK(NBPW,KSEC2,KEY(14),IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPKS(NBPW,KSEC2,IDUM(1),IW,IB,8,0,4,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
            END IF
            GO TO 140
         ELSE
            CALL BUNPKS(NBPW,KSEC2,KEY(16),IW,IB,8,0,9,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPKS(NBPW,KSEC2,IDUM(1)  ,IW,IB,8,0,8,KERR)
            IF(KERR.GT.0) RETURN
         END IF
C
C
C*          1.4 SUB KEY INFORMATION.
C               --------------------
 140  CONTINUE
C
         CALL BUNPCK(NBPW,KSEC2,KEY(25),IW,IB,16,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(26),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(27),IW,IB, 5,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(28),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(29),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUNPCK(NBPW,KSEC2,KEY(30),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(31),IW,IB, 5,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(32),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(33),IW,IB, 6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,IDUMMY   ,IW,IB, 1,KERR)
         IF(KERR.GT.0) RETURN
C
         CALL BUNPCK(NBPW,KSEC2,KEY(34),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(35),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(36),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(37),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(38),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(39),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(40),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(41),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(42),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(43),IW,IB,6,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(44),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(45),IW,IB,1,KERR)
         IF(KERR.GT.0) RETURN
         CALL BUNPCK(NBPW,KSEC2,KEY(46),IW,IB,8,KERR)
         IF(KERR.GT.0) RETURN
         KSUP( 2)=46
      ELSE
         IF(KSEC2(1).EQ.28) THEN
            CALL BUNPCK(NBPW,KSEC2,KEY(2),IW,IB, 8,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(3),IW,IB, 8,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(4),IW,IB,12,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(5),IW,IB, 4,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(6),IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(7),IW,IB, 5,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(8),IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(9),IW,IB, 6,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 1,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(10),IW,IB,26,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,KEY(11),IW,IB,25,KERR)
            IF(KERR.GT.0) RETURN
            CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 5,KERR)
            IF(KERR.GT.0) RETURN
            IF(KSUP(6).GT.1.OR.KEY(2).EQ.2.OR.KEY(2).EQ.3) THEN
               CALL BUNPCK(NBPW,KSEC2(IW),KEY(12),IW,IB,26,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(13),IW,IB,25,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,IDUMMY  ,IW,IB, 5,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(14),IW,IB, 8,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPCK(NBPW,KSEC2,KEY(15),IW,IB,16,KERR)
               IF(KERR.GT.0) RETURN
            ELSE
               CALL BUNPKS(NBPW,KSEC2,KEY(16),IW,IB,8,0,9,KERR)
               IF(KERR.GT.0) RETURN
               CALL BUNPKS(NBPW,KSEC2,IDUM(1)  ,IW,IB,8,0,8,KERR)
               IF(KERR.GT.0) RETURN
            END IF
         ELSE
            WRITE(*,'(1H )')
            WRITE(*,'(1H ,A)') 'BUUKEY : Key definition not known'
            WRITE(*,'(1H )')
         END IF
      END IF
C
C     Check if ident is right justified.
C
      IF(KEY(16).EQ.32) THEN
         J=15
         DO 102 I=16,24
         IF(KEY(I).EQ.32) GO TO 102
         J=J+1
         KEY(J)=KEY(I)
         KEY(I)=32
 102     CONTINUE
      END IF
C
      RETURN
      END
      SUBROUTINE BUUNP(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KERR)
C
C**** *BUUNP*
C
C
C     PURPOSE.
C     --------
C          Purpose of this routine is to unpack bit string of
C     KSIZE bits, started at word KWPT of array KSOURC after
C     skipping KBPT bits. Result is put into KDEST.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUNP(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KERR)*
C
C        INPUT :
C                 *KBPW*      - number of bits in computer word
C                 *KSOURC*    - source (continuous bit string of
C                               arbitrary length)
C                 *KWPT*      - word pointer
C                 *KBPT*      - bit pointer
C                 *KSIZE*     - number of bits to be extracted
C        OUTPUT:
C                 *KDEST*     - destination
C                 *KERR*      - return error code
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          GBYTE     - unpack bit pattern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KSOURC(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KERR.GT.0) RETURN
C
      IF(KSIZE.GT.KBPW) THEN
         KERR=13
         PRINT*,' BUUNP :'
         CALL BUERR(KERR)
         RETURN
      END IF
C
      CALL GBYTE(KSOURC(KWPT),KDEST,KBPT,KSIZE)
C
      RETURN
      END
      SUBROUTINE BUUNPS(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)
C
C**** *BUUNPS*
C
C
C     PURPOSE.
C     --------
C          Purpose of this routine is to unpack bit string of
C     KSIZE bits, started at word KWPT of array KSOURC after
C     skipping KBPT bits. Result is put into KDEST.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUNPS(KBPW,KSOURC,KDEST,KWPT,KBPT,KSIZE,KSKIPB,K,KERR)*
C
C        INPUT :
C                 *KBPW*      - number of bits per computer word
C                 *KSOURC*    - source (continuous bit string of
C                               arbitrary length)
C                 *KWPT*      - word pointer
C                 *KBPT*      - bit pointer
C                 *KSIZE*     - number of bits to be extracted
C                 *KSKIPB*    - number of bits to skip between elements
C                 *K*         - iteration
C        OUTPUT:
C                 *KDEST*     - destination
C                 *KERR*      - return error code
C
C     METHOD.
C     -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C          GBYTES     - unpack bit pattern
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      DIMENSION KSOURC(*),KDEST(*)
C
C     ------------------------------------------------------------------
C*          1.   EXTRACT BIT PATTERN.
C                --------------------
 100  CONTINUE
C
      IF(KSIZE.GT.KBPW) THEN
         KERR=13
         PRINT*,' BUUNPS :'
         CALL BUERR(KERR)
      END IF
C
      CALL GBYTES(KSOURC(KWPT),KDEST,KBPT,KSIZE,KSKIPB,K)
C
      RETURN
      END
      SUBROUTINE BUUPWT(KDD,KELEM,KERR)
C
C**** *BUUPWT*
C
C
C     PURPOSE.
C     --------
C          Updates working tables setting element name,unit,scale,
C     reference value and data width.
C
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUUPWT(KDD,KELEM,KERR)*
C
C        INPUT :
C               *KDD*     -  data descriptor
C               *KELEM*   -  dimension of CNAMES, CUNITS array
C        OUTPUT:
C               *KERR*    -  return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       01/02/91.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCOMWT/ NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,M0,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      COMMON /BCOMWTC/ CWTEN(JELEM),CWTU (JELEM)
C
C             CWTEN    -  working table element naame
C               CWTU     -  working table units
C
C
      COMMON /BCOMTAB/ NTABBTR(JTAB),NTABBS (JTAB),NTABBRV(JTAB),
     1                NTABBDW(JTAB),NTABDTR(JTAB),NTABDST(JTAB),
     2                NTABDL (JTAB),NTABDSQ(JTAB*20),NTABP(64,255)
C
C             NTABBTR    - table B,  table reference              array
C             NTABBS     - table B,  scale                        array
C             NTABBRF    - table B,  reference value              array
C             NTABBDW    - table B,  data width                   array
C             NTABDTR    - table D,  table reference              array
C             NTABDST    - table D,  starting pointers            array
C             NTABDL     - table D,  lengths                      array
C             NTABDSQ    - table D,  list of sequence descriptors array
C
C
      COMMON /BCOMTABC / CTABBEN(JTAB),CTABBU (JTAB)
C
C             CTABBEN      -  table B, ELEMENT NAME           array
C             CTABBU       -  table B, unit                   array
C
C
      COMMON /BCOMATB/ NJA,NATBTR(JTAB),NATBS (JTAB),
     1                NATBRV(JTAB),NATBDW(JTAB)
C
C
C             NATBTR      - augmented table B table reference
C             NATBS       - augmented table B scale
C             NATBRV      - augmented table B reference value
C             NATBDW      - augmented table B data width
C
C
      COMMON /BCOMATBC/ CATBEN(JTAB),CATBU (JTAB)
C
C             CATBEN      - augmented table B element name
C             CATBU       - augmented table B units
C
C
      COMMON /BCOMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      CHARACTER CATBEN*64,CWTEN*64,CTABBEN*64
      CHARACTER CATBU*24,CWTU*24,CTABBU*24
      CHARACTER YCH6*6
C
C
C     ------------------------------------------------------------------
C*          1.   UPDATE WORKING TABLE.
C                ---------------------
 100  CONTINUE
C
      IF( KERR.NE.0 ) RETURN
C
      ICLASS=KDD/1000
      iyyy  =kdd-iclass*1000+1
      iclass=iclass+1
C
C*          1.1  ASSOCIATED FIELD ?
C                ------------------
 110  CONTINUE
C
      IF(NAFDW.EQ.0) GO TO 140
C
C*          1.2  UNITS ELEMENT DESCRIPTOR ?
C                --------------------------
 120  CONTINUE
C
      i=ntabp(iclass,iyyy)
      if(i.eq.0) then
         KERR=23
         PRINT*,'BUUPWT : ',KDD
         CALL BUERR(KERR)
         DO 1 IQ=1,JELEM
         NSTACK(IQ)=0.
 1       CONTINUE
         RETURN
      end if
C
      IF(CTABBU(I)(1:4).EQ.'CODE') GO TO 140
      IF(CTABBU(I)(1:4).EQ.'FLAG') GO TO 140
      IF(CTABBU(I)(1:3).EQ.'NUM' ) GO TO 140
C
C*          1.3   ADD SPECIAL ENTRY TO WORKING TABLE.
C                 -----------------------------------
 130  CONTINUE
C
      NWT=NWT+1
      CWTEN(NWT)='ASSOCIATED FIELD'
      CWTU (NWT)=' '
      NWTDW(NWT)=NAFDW
      NWTR (NWT)= 999999
      NWTEN(NWT)= 0
      NWTS (NWT)= 0
      NWTRV(NWT)= 0
C
C*                UPDATE CNAMES AND CUNITS
C
      M=M+1
      IF(M.GT.KELEM) THEN
         KERR=25
         PRINT*,'BUUPWT:'
         CALL BUERR(KERR)
         RETURN
      END IF
C
C     ------------------------------------------------------------------
C*          1.4   SEARCH AUGMENTED TABLE *B ENTRIES .
C                 -----------------------------------
 140  CONTINUE
C
      DO 141 J=1,NJA
C
      IF(NATBTR(J).EQ.KDD) THEN
         II=J
C
C*             MODIFY ENTRY FOR OPERATOR IN FORCE.
C              -----------------------------------
C
C*             ADD ENTRY TO WORKING TABLE.
C              ---------------------------
C
         NWT=NWT+1
         NWTR (NWT) = KDD
         NWTS (NWT) = NATBS (II) + NSCAM
         NWTRV(NWT) = NATBRV(II) 
         NWTDW(NWT) = NATBDW(II) + NDWINC
C
C        CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
         IF(N221.NE.0) THEN
            IX=KDD/1000
            IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
            N221=N221-1
         END IF
c
         if(nwtr(nwt).eq.31011.or.nwtr(nwt).eq.31012) then
            if(nwtr(nwt-1).eq.31011.or.nwtr(nwt-1).eq.31012) then
               NWTDW(NWT)=0
            end if
         end if

C
C*            UPDATE M, CNAMES, CUNITS.
C             -------------------------
C
         CWTEN(NWT) = CATBEN(II)
         CWTU (NWT) = CATBU (II)
         NWTEN(NWT) = 0
         IF(CATBU(II)(1:3).EQ.'CCI') NWTEN(NWT)=658367
C
         M = M + 1
         IF(M.GT.KELEM) THEN
            KERR=25
            PRINT*,'BUUPWT:'
            CALL BUERR(KERR)
            RETURN
         END IF
C
         RETURN
      END IF
C
 141  CONTINUE
C
C*          1.5  GET TABLE *B ENTRY .
C                ---------------------
 150  CONTINUE
C
      I=NTABP(ICLASS,IYYY)
      IF(I.EQ.0) THEN
         KERR=23
         PRINT*,'BUUPWT : ',KDD
         CALL BUERR(KERR)
         DO 2 IQ=1,JELEM
         NSTACK(IQ)=0.
 2       CONTINUE
         RETURN
      END IF
C
 155  CONTINUE
C
C     -----------------------------------------------------------------
C*          1.6   MODIFY ENTRY FOR OPERATOR IN FORCE.
C                 -----------------------------------
 160  CONTINUE
C
C*                ADD ENTRY TO WORKING TABLE.
C                 ---------------------------
      NWT=NWT+1
      NWTR (NWT) = KDD
      NWTS (NWT) = NTABBS (I) + NSCAM
      NWTRV(NWT) = NTABBRV(I) 
      NWTDW(NWT) = NTABBDW(I) + NDWINC
C
C     CHECK IF DATA ARE PRESENT IN DATA SECTION.
C
      IF(N221.NE.0) THEN
         IX=KDD/1000
         IF(IX.GT.9.AND.IX.NE.31) NWTDW(NWT)=0
         N221=N221-1
      END IF
c
      if(nwtr(nwt).eq.31011.or.nwtr(nwt).eq.31012) then
         if(nwtr(nwt-1).eq.31011.or.nwtr(nwt-1).eq.31012) then
            NWTDW(NWT)=0
         end if
      end if
C
 175  CONTINUE
C
C     ------------------------------------------------------------------
C*          1.8  UPDATE M, CNAMES, CUNITS.
C                -------------------------
 180  CONTINUE
C
      CWTEN(NWT) = CTABBEN(I)
      CWTU (NWT) = CTABBU(I)
      NWTEN(NWT) = 0
      IF(CTABBU(I)(1:3).EQ.'CCI') NWTEN(NWT)=658367
C
       M = M + 1
       IF(M.GT.KELEM) THEN
          KERR=25
          PRINT*,'BUUPWT:'
          CALL BUERR(KERR)
          RETURN
       END IF
C
C     -----------------------------------------------------------------
C
 200  CONTINUE
C
      RETURN
      END
      SUBROUTINE BUXDES(K,KSEC1,KTDLEN,KTDLST,KDLEN,KDATA,KELEM,
     2                  KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
C
C**** *BUXDES*
C
C
C     PURPOSE.
C     --------
C          Expand data descriptors to show user template he
C          described by aray ktdlst and kdata.
C
C
C**   INTERFACE.
C     ----------
C
C               *CALL BUXDES(K,KSEC1,KTDLEN,KTDLST,KDLEN,KDATA,KELEM,
C                            KTDEXL,KTDEXP,CNAMES,CUNITS,KERR)
C
C        INPUT :
C
C               *K*       -  An integer, printing switch 0 - no print
C                                                        1 - print
C               *KSEC1*   -  Integer array of at least 40 words containing
C                            Section 1 information
C               *KTDLEN*  -  Integer number of data descriptors in section 3
C               *KTDLST*  -  Integer array of at least KTDLEN words
C                            containing data descriptors for Bufr section 3
C               *KDLEN*   -  Integer (dimension of KDATA array)
C               *KDATA*   -  Integer array containing data needed for data
C                            descriptor expansion (delayed replication factors)
C                            which appear in the values array
C
C               *KELEM*   -  Integer number of elements in Bufr template.
C
C        OUTPUT:
C               *KTDEXL*  - An integer containing number of expanded elements
C               *KTDEXP*  - An integer array containing expanded list of descriptors
C               *CNAMES*  - Character*64 array of KELEM containing element names
C               *CUNITS*  - Character*24 array of KELEM containing element units
C               *KERR*    -  Return error code
C
C
C     METHOD.
C     -------
C
C
C     EXTERNALS.
C     ----------
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/06/93.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1          JSEC4=   2,JELEM=40000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=360000,JKEY=46)
C
C
      COMMON /BCMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
C             NBPW          -  number of bits per computer word
C             NWPT          -  word pointer
C             NBPT          -  bit pointer
C             NWPTB         -  pointer to word at the begining of next section
C             NBPTB         -  pointer to bit at the begining of next section
C             NMASK         -  bit mask array
C             NVIND         -  missing value indicator (integer)
C             RVIND         -  missing value indicator (real)
C             NBENP         -  previous Bufr Edition number
C             NLTVNP        -  previous Local table version number
C             NXXP          -  bufr master table used
C             NYYP          -  version number of master table used
C             NZZP          -  version number of local table used
C
C
      COMMON /BCMDEFC/ CECMWF,CUSER
C
C             CECMWF        -  character string to control default set up
C             CUSER         -  character string to control user set up
C
C
      COMMON /BCMBEF / M,MM,N,JCV
C
C             M       -  Number of elements
C             MM      -  Number of elements
C             N       -  Number of data sub_sets
C             JCV     -  Number of character values
C
C
      COMMON /BCMWT/  NDWINC,NSCAM,NAFDW,NWT ,ODREPF,
     1               N221,MREL,NFCM,NFUCM,MBMP,OMARKER,
     2               MBMPL,NSTACK(JELEM),NWTEN(JELEM),
     3               NWTR (JELEM),NWTS (JELEM),NWTRV (JELEM),
     4               NWTDW(JELEM),NWTIW(JELEM),NWTIWS(JELEM)
C
C             NDWINC   -  data width increment
C             NSCAM    -  scale multiplier
C             NAFDW    -  augmented field data width
C             NWT      -  pointer to working table
C             NSTACK   -  list of data descriptors
C             ODREPF   -  replication (logical)
C             N221     -  data not present for n221 elements
C             MREL     -  pointer to the last data element
C             NFCM     -  first compressed message
C             MBMP     -  pointer to the begining of bit map
C             NWTR     -  working table reference
C             NWTS     -  working scale
C             NWTRV    -  working reference value
C             NWTDW    -  working data width
C             NWTIW    -  working data width of increments
C             NWTIWS   -  working total data width of element set
C
C
      DIMENSION  KSEC1(JSEC1),KSEC3(JSEC3)
      DIMENSION  VALUES(JCVAL)
      DIMENSION  KTDLST(KTDLEN)
      DIMENSION  KDATA(KDLEN)
      DIMENSION  KTDEXP(KELEM)
C
      CHARACTER*4   CECMWF,CUSER
      CHARACTER*64  CNAMES(KELEM)
      CHARACTER*24  CUNITS(KELEM)

C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      KERR=0
      IF(CECMWF.NE.'ECMF') THEN
         CALL BUIVAR(KERR)
         KPT   = 0
         CECMWF='ECMF'
      END IF
C
C     -----------------------------------------------------------------
C*          2.   LOAD BUFR TABLES.
C                -----------------
 200  CONTINUE
C
      CALL BUETAB(KSEC1,KERR)
      IF(KERR.GT.0) RETURN
C
C     ------------------------------------------------------------------
C          6.  EXPAND DATA DESCRIPTORS.
C              ------------------------
 600  CONTINUE
C

      CALL BUEDD(KPT,KTDLEN,KTDLST,KDLEN,KDATA,KSEC3,
     1           KVALS,VALUES,KELEM,CNAMES,CUNITS,KERR)
C
      IF(KERR.GT.0) RETURN
C
      KTDEXL =M
      DO 301 I=1,KTDEXL
      KTDEXP(I)=NWTR(I)
 301  CONTINUE
C
      IF(K.EQ.1) THEN
C
         WRITE(*,'(1H ,//)')
         WRITE(*,'(1H ,A)')    '       Data descriptors (unexpanded)'
C
         WRITE(*,'(1H )')
         DO 110 I=1,KTDLEN
          WRITE(*,'(1H ,I4,2X,I6.6)') I,KTDLST(I)
 110     CONTINUE
C
         WRITE(*,'(1H ,/)')
         WRITE(*,'(1H ,A)')    '       Data descriptors (expanded)'
         WRITE(*,'(1H )')
         WRITE(*,'(1H ,A,A)')'                Element name',
     1   '                            Unit'
         WRITE(*,'(1H )')
C
         DO 120 I=1,KTDEXL
          WRITE(*,'(1H ,I4,2X,I6.6,2X,A,2x,A)') I,KTDEXP(I),
     1                      CNAMES(I)(1:40),CUNITS(I)(1:18)
 120     CONTINUE
      END IF
C
C
C     -----------------------------------------------------------------
      RETURN
C
      END
