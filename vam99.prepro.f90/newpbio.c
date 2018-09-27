/*
	newpbio.c
*/
#include "bufrgrib.h"
#include "fort2c.h"
                   
#define NAMEBUFFLEN 256
#define MODEBUFFLEN 10
                      
/*************************************************************************
*  FUNCTION:  pbopen - Open file (from FORTRAN)
**************************************************************************
*/
void PBOPEN(FILE ** unit, _fcd name, _fcd mode, long * iret, long l1, long l2)
/*
* Purpose:	Opens file, return UNIX FILE pointer.
*
* Function returns: long iret:  -1 = Could not open file.
*                               -2 = Invalid file name.
*                               -3 = Invalid open mode specified
*                                0 = OK.
*
*    Note: l1 and l2 are the lengths of the character strings in
*          name and mode on SGI.
*/
{
	char *fname;
	char *modes;
	char *p;
	char  flags[4];
                
#if (!defined CRAY) && (!defined VAX)
	char namebuff[NAMEBUFFLEN], modebuff[MODEBUFFLEN];
                                                   
/* Put the character strings into buffers and ensure that there is a
   null terminator (for SGI case when FORTRAN CHARACTER variable is full
   right to end with characters */
	strncpy( namebuff, name, NAMEBUFFLEN - 1);
	strncpy( modebuff, mode, MODEBUFFLEN - 1);
	namebuff[l1] = '\0';
	modebuff[l2] = '\0';
#endif
      
	strcpy(flags,"");
                  
	*unit = NULL;
	*iret = 0;
           
	/* convert fortran to c string : file name */
                                              
#if (!defined CRAY) && (!defined VAX)
	if(!(fname = fcd2char(namebuff)))
#else
	if(!(fname = fcd2char(name)))
#endif
	{
		*iret = -2;
		return;
	}
  
	/* strip trailing blanks */
                            
/* <-------------------------------------------------
	p  = fname;
	while(*p)
	{
		if(*p == ' ') *p = 0;
		p++;
	}
-----------------------------------------------------> */
	p  = fname + strlen(fname) - 1 ;
	while(*p == ' ')
	{
	    *p = 0;
	    p--;
	}
		
	/* convert fortran to c string : open modes  */
	
#if (!defined CRAY) && (!defined VAX)
	if(!(modes = fcd2char(modebuff)))
#else
	if(!(modes = fcd2char(mode)))
#endif
	{
		free(fname);
		*iret = -2;
		return;
	}
  
  
	/* build open flags from "modes" */
                                    
	p = modes;
	while(*p && ( strlen(flags) < 3 ) )
	{
		switch(*p)
		{
			case '+':
				strcat(flags, "+");
				break;
          
			case 'a':
			case 'A':
				strcat(flags, "a");
				break;
          
			case 'c':
			case 'C':
            
			case 'w':
			case 'W':
				strcat(flags, "w");
				break;
          
			case 'r':
			case 'R':
				strcat(flags, "r");
				break;
          
			default:
				*iret = -3;
				return;
           
		}
		p++;
	}
  
	/* if read/write change flags */
                                 
	if ( !strcmp(flags,"wr") || !strcmp(flags, "rw") )
		strcpy(flags, "r+w" );
                        
	*unit = fopen(fname, flags );
                              
	if(*unit == NULL)
	{
		*iret = -1;
	}
  
  
	free(fname);
	free(modes);
             
}
 
/*************************************************************************
*  FUNCTION:  pbseek - Seek (from FORTRAN)
**************************************************************************
*/
void PBSEEK(FILE ** unit, long * offset, long * whence, long * iret)
/*
*
* Purpose:	Seeks to specified location in file.
*
* Function returns:	long status : 	-2 = error in handling file,
*					-1 = end-of-file
*                              otherwise,  = byte offset from start of file.
*
*	whence	= 0, from start of file
*		= 1, from current position
*		= 2, from end of file.	
*/
{
	long my_offset = *offset;
                          
	if ( *whence == 2) my_offset = - abs(my_offset);
				/* must use negative offset if working
				   from end-of-file	*/
                          
	*iret = fseek(*unit, my_offset, *whence);
                                          
	if(*iret != 0)
	{
		if ( ! feof(*unit) )
		{
			*iret = -2;		/* error in file-handling */
			perror("pbseek");
		}
		else
			*iret = -1;		/* end-of-file	*/
                                 
		clearerr(*unit);
		return;
	}
  
	*iret = ftell(*unit);		/* byte offset from start of file */
                                                            
}
		
  
  
  
/*************************************************************************
*  FUNCTION:  pbread - Read (from FORTRAN)
**************************************************************************
*/
void PBREAD(FILE ** unit, char * buffer, long * nbytes, long * iret)
/*
*
* Purpose:      Reads a block of bytes from a file.
*
* Function returns:     long status :   -2 = error in reading file,
*                                       -1 = end-of-file,
*                               otherwise, = number of bytes read.
*/
{
 
        if ( (*iret = fread(buffer, 1, *nbytes, *unit) ) != *nbytes)
        {
                                                /*      Read problem */
                if ( ! feof(*unit) )
                {
                        *iret = -2;             /*  error in file-handling  */
                        perror("pbread");
                        clearerr(*unit);
                        return;
                }
                else
                {
                        *iret = -1;             /*  end-of-file */
                        clearerr(*unit);
                }
        }
         
}
 
 
/*************************************************************************
*  FUNCTION:  pbread2 - Read (from FORTRAN)
**************************************************************************
*/
void PBREAD2(FILE ** unit, char * buffer, long * nbytes, long * iret)
/*
*
* Purpose:	Reads a block of bytes from a file.
*
* Function returns:	long status : 	-2 = error in reading file,
*                               otherwise, = number of bytes read.
*/
{
 
	if ( (*iret = fread(buffer, 1, *nbytes, *unit) ) != *nbytes)
	{
						/*	Read problem */
        	if ( ! feof(*unit) )
        	{
            		*iret = -2;             /*  error in file-handling  */
            		perror("pbread");
        		clearerr(*unit);
        		return;
        	}
    	}
      
}
 
/*************************************************************************
*  FUNCTION:  pbwrite - Write (from FORTRAN)
**************************************************************************
*/
void PBWRITE( FILE ** unit, char * buffer, long * nbytes, long *iret)
/*
* Purpose:	Writes a block of bytes to a file.
*
* Function returns:	long status : -1 = Could not write to file.
*                                    >=0 = Number of bytes written.
*/
{
	if ( (*iret = fwrite(buffer, 1, *nbytes, *unit) ) != *nbytes)
	{				/* Problem with write */
       		perror("pbwrite");
        	*iret = -1;
	}
  
}
 
 
 
/*************************************************************************
*  FUNCTION:  pbclose - close (from FORTRAN)
**************************************************************************
*/
long PBCLOSE( FILE ** unit, long * iret)
/*
*
* Purpose:	Closes file.
*
* Function returns:	long status : non-0 = error in handling file.
*                                     	  0 = OK.
*/
{
	*iret = fclose(*unit);
                       
	if(*iret != 0) perror("pbclose");
                                  
}
 
