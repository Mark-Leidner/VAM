C
C
	SUBROUTINE PBBUFR(KUNIT,KARRAY,KINLEN,KOUTLEN,KRET)
C
C******************************************************************************
C
C	KUNIT = 	unit number for the file returned from PBOPEN
C
C	KARRAY =	FORTRAN array big enough to hold the BUFR product
C
C	KINLEN =	size in BYTES of the FORTRAN array
C
C	KOUTLEN =	size in BYTES of the BUFR product read	
C
C	KRET =		 0  if a BUFR product has been successfully read
C
C			-1  if end-of-file is hit before a BUFR product is read
C
C			-3  if the size of KARRAY is not sufficient for the
C			    BUFR product
C
C******************************************************************************
C
C Arguments
	INTEGER KARRAY(1)
	INTEGER KUNIT,KINLEN,KOUTLEN,KRET
                                  
C Local argument(s)
	INTEGER NREAD, IRET
                    
C Get the BUFR product
	CALL BUFRREAD( KARRAY, KINLEN, NREAD, IRET, KUNIT )
                                                    
C Escape if the user buffer is too small to hold even the early sections of
C the product or EOF encountered
	IF ( IRET .EQ. -4 ) THEN
	    KOUTLEN = NREAD
	    KRET = -1
	    RETURN
	ENDIF
      
C Escape if no BUFR product is found in the file
	IF ( IRET .EQ. -1 ) THEN
	    KOUTLEN = 0
	    KRET = -1
	    RETURN
	ENDIF
      
C Check if the array is big enough for the BUFR product
	IF ( IRET .EQ. -3 ) THEN
	    KOUTLEN = NREAD
	    KRET = -3
	    RETURN
	ENDIF
      
C Set success code if product retrieved
	IF ( NREAD .GE. 0 )  THEN
	    KOUTLEN = NREAD
	    KRET = 0
	ENDIF
      
	RETURN
       
	END
