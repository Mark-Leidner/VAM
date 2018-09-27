/*
	readprod.c
*/
#include "bufrgrib.h"
#include <stdio.h>
#ifndef VAX
#include <unistd.h>
#endif
#include <string.h>
#ifdef sun4
#include <memory.h>
#endif
      
#define len3oct(p) (256*(256*(long)*((p)) + (long)*((p+1)) ) + (long)*((p+2)))
#define SMALL 40
#define LARGEBUF 200000
#define GRIB 0x47524942
#define BUFR 0x42554652
#define BUDG 0x42554447
#define TIDE 0x54494445
#define DIAG 0x44494147
                       
long readprod( char *, char * , long * ,
              long (*read_func)(char *, long , void *), void * );
static long file_read(char *, long , void * stream);
static long prodsize(long, char *, long, long *, long (*read_func)(), void *);
static long gribsize(char * , long, long * ,  long (*read_func)(), void * );
static long bufrsize(char * , long, long * ,  long (*read_func)(), void * );
static long tide_budg_size(char *, long, long *, long (*read_func)(), void *);
static long lentotal(char *, long *, long, long , long , long ,
                     long (*read_func)(), void *);
                                                  
static long file_read(char * buff, long leng, void * file)
/*
    buff = buffer to fill,
    leng = size of buff in bytes,
    file = file pointer from fopen().
                                     
    Returns the number of bytes read.
    On EOF, returns negative value for number of bytes read .
*/
{
long nbytes;
            
    nbytes = (long) fread( buff, 1, leng, (FILE *) file);
                                                         
/* If EOF, return negative number of bytes read */
    if ( feof((FILE *) file ) )
    {
        clearerr( (FILE *) file );
        return (-nbytes);
    }
     
    return nbytes;
}
 
static long prodsize(long code, char * hold, long leng, long * holdsize,
                     long (*read_func)(), void * stream)
/*
    Returns size of BUFR, GRIB, BUDG, TIDE, DIAG product in bytes.
                                                                  
    hold = buffer holding product,
    leng = size of buffer in bytes,
    holdsize = number of bytes of product already read into hold.
    read_func = function used to read the product,
    stream = data describing the input stream (eg. FILE *).
*/
{
    *holdsize = 4;
    switch( code )
    {
        case BUFR:
            memcpy(hold,"BUFR",4);
            return bufrsize( hold, leng, holdsize, read_func, stream);
                                                                      
        case BUDG:
            memcpy(hold,"BUDG",4);
            return tide_budg_size( hold, leng, holdsize, read_func, stream);
                                                                            
        case GRIB:
            memcpy(hold,"GRIB",4);
            return gribsize( hold, leng, holdsize, read_func, stream);
                                                                      
        case TIDE:
            memcpy(hold,"TIDE",4);
            return tide_budg_size( hold, leng, holdsize, read_func, stream);
                                                                            
        case DIAG:
            memcpy(hold,"DIAG",4);
            return tide_budg_size( hold, leng, holdsize, read_func, stream);
                                                                            
        default:
            return 0;
    }
}
 
long readprod( char * prod_id, char * buffer, long * size,
              long (*read_func)(char *, long , void *), void * stream)
/*
    Reads a BUFR, GRIB, BUDG, TIDE, DIAG product.
                                                 
    prod_id = "BUFR", "GRIB", "BUDG", "TIDE", "DIAG" ...
    buffer = buffer to hold product,
    size = on input, size of buffer in bytes,
           on output, number of bytes read.
    read_func = function used to read the product,
    stream = data describing the input stream (eg. FILE *).
                                                           
    Returns
            -1 if can't find product  (eg. end-of-file)
            -2 if internal processing error (malloc fail, file seek error)
            -3 if buffer too small for whole product.
            -4 if user buffer too small to even start product processing.
                                                                         
*/
{
static char * hold = NULL;           /* buffer used to read product */
long holdsize = SMALL, numread;
long found = 0, num = 0, length, prodlen;
long given_buflen = *size;
char p;
unsigned long code=0, wanted = 0;
int status = 0, i;
                  
/* See if user gave a buffer for holding the product */
    if ( buffer == NULL )
    {                                /* No buffer given, get some */
        if ( hold == NULL )          /* but only first time round */
        {
            hold = (char *) malloc( LARGEBUF);
            if ( hold == NULL )
            {
                perror("malloc failed in readnext");
                return (-2);
            }
        }
        given_buflen = LARGEBUF;
    }
    else                             /* User buffer given */
        hold = buffer;
                      
/* Make sure the user gave some buffer */
   if (  given_buflen < holdsize ) return -4;
                                             
/* Look for product identifier */
    if ( prod_id != NULL )
        for ( i = 0; i < 4; i++ )
            wanted = ( (wanted << 8) + *(prod_id+i) ) & 0xFFFFFFFF;
                                                                   
/* Read some bytes from the product */
    do
    {
        numread = read_func( &p, 1L, stream );
        if ( numread <= 0 )
        {
            *size = 0;
            return -1;
        }
         
        code = ( (code << 8) + p ) & 0xFFFFFFFF;
                                                
        if ( prod_id == NULL )
        {
            switch(code)
            {
                case BUDG:
                case BUFR:
                case GRIB:
                case TIDE:
                case DIAG:
                    found = 1;
            }
        }
        else
        {
            if ( code == wanted )
                found = 1;
        }
         
    } while ( ! found );
                        
/* Find the product length */
    prodlen = prodsize( code, hold, given_buflen,
                        &holdsize, read_func, stream);
                                                      
    if( prodlen == -4 ) return -4;   /* User buffer too small for searching
                                        through product to calculate its
                                        length. */
                                                  
/* Pick up the rest of the product, or as much as possible */
                                                             
/* Handle case that the user gave a NULL buffer and zero length */
    if( *size == 0 )
    {                                   /* move to end of product */
        status = fseek( (FILE *)stream, prodlen - holdsize, SEEK_CUR);
        if (status)
        {
            perror("fseek error in readprod");
            return (-2);
        }
        *size = prodlen;             /* give actual product length */
        return -3;                   /* indicate buffer too small */
    }
     
/* Otherwise read product into the buffer */
    length = (given_buflen > prodlen) ? prodlen : given_buflen ;
    if ( length > holdsize )
    {
        numread = read_func( hold+holdsize, length-holdsize ,stream );
        if ( given_buflen < prodlen )
        {
          *size = prodlen;           /* give actual product length */
          status = fseek((FILE*)stream,prodlen-given_buflen,SEEK_CUR);
                                     /* move to end of product */
            if (status)
            {
                perror("fseek error in readprod");
                return (-2);
            }
            return -3;               /* indicate buffer too small */
        }
    }
     
    *size = prodlen;                 /* give actual product length */
    return *size;
                 
}
 
#define BIT1 0x80
#define BIT2 0x40
#define BIT7 0x02
#define BIT8 0x01
                 
                 
static long gribsize(char * hold, long leng, long * holdsize,
                     long (*read_func)(), void * stream)
/*
    Calculates the size in bytes of a GRIB product.
                                                   
    hold = buffer to hold product,
    leng = length of buffer,
    holdsize = number of bytes of the product in the hold buffer.
               Note that this increases if necessary as more bytes
               are read.
    read_func = function used to read the product,
    stream = data describing the input stream (eg. FILE *).
*/
{
long length, numread, num, hsize = *holdsize;
long section2, section3 ;
unsigned char * phold = (unsigned char *) hold;
                                               
    /* Need more bytes to decide on which version of GRIB */
                                                            
    if ( leng < 24 ) return -4;      /* User buffer too small */
                                                                
    /* put first 24 bytes in buffer*/
                                     
    num = 24;
    if ( hsize < num)
    {
        numread = read_func( hold + hsize, num - hsize, stream);
        if ( numread <= 0 )          /* eg. on EOF */
        {
            *holdsize -= numread;
            return *holdsize;
        }
        hsize = num;
    }
    *holdsize = hsize;
                      
    /* See if the GRIB version is 0 ... */
                                          
    if ( len3oct(phold+4) == 24 )
    {
       length = 28;		     /* GRIB + section 1 */
                                                
        /* Check for presence of sections 2 and 3 */
        section2 = ( hold[11] & BIT1 ) ;
        section3 = ( hold[11] & BIT2 ) ;
                                        
        /* Add up all lengths of sections */
        return lentotal(hold, holdsize, leng, length, section2, section3,
                        read_func, stream);
    }
     
    /* ... or version 1 ... */
                              
    if ( ( hold[21] != 0 ) || ( hold[22] != 0 ) )
        return len3oct(phold+4);
                                
    /* ... or version -1 ... */
                               
    length = 24;		     /* GRIB + section 1 */
                                             
    /* Check for presence of sections 2 and 3 */
    section2 = ( hold[7] & BIT8 ) ;
    section3 = ( hold[7] & BIT7 ) ;
                                   
                                   
    /* Add up all lengths of sections */
    return lentotal(hold, holdsize, leng, length, section2, section3,
                    read_func, stream);
                                       
}
 
static long lentotal(char *hold, long *holdsize, long leng, long length,
                     long section2, long section3,
                     long (*read_func)(), void *stream)
/*
    Returns the total length in bytes of all sections of the GRIB product.
                                                                          
    hold = buffer to hold product,
    leng = length of buffer,
    holdsize = number of bytes of the product in the hold buffer.
               Note that this increases if necessary as more bytes
               are read.
    length = length of (section 0 + section 1).
    section2 is TRUE if section 2 is present in the product.
    section3 is TRUE if section 3 is present in the product.
    read_func = function used to read the product,
    stream = data describing the input stream (eg. FILE *).
*/
{
long sect_5_len = 4;		     /*	section 5 length: 7777 */
                                                       
long numread, hsize = *holdsize;
unsigned char * phold = (unsigned char *) hold;
long next, next_sec = 4;
                        
   /* Adjust count of sections to check */
    if ( section2 ) next_sec--;
    if ( section3 ) next_sec--;
                               
   /* Get the size of the next section */
                                         
    if ( leng < length ) return -4;  /* User buffer too small */
    if ( hsize < length)
    {
        numread = read_func( hold + hsize, length - hsize, stream);
        if ( numread <= 0 )          /* eg. on EOF */
        {
            *holdsize -= numread;
            return *holdsize;
        }
        hsize = length;
    }
    *holdsize = hsize;
                      
    /* Get the size of remaining sections */
                                            
    for ( next = next_sec; next < 5 ; next++ )
    {
        if ( leng < (length+4) ) return -4; /* User buffer too small */
        if ( hsize < (length+4))
        {
            numread = read_func( hold+hsize, (length+4)-hsize, stream);
            if ( numread <= 0 )      /* eg. on EOF */
            {
                *holdsize -= numread;
                return *holdsize;
            }
            hsize = length + 4;
        }
        *holdsize = hsize;
        length += len3oct(phold+length);
    }
     
    /* Add on the size of section 5 */
                                      
    if ( leng < (length+sect_5_len) ) return -4; /* User buffer too small */
    length += sect_5_len;
                         
    return length;
}
 
static long bufrsize(char * hold, long leng, long * holdsize,
                     long (*read_func)(), void * stream)
/*
    Returns the size in bytes of the BUFR code product.
                                                       
    hold = buffer to hold product,
    leng = length of buffer,
    holdsize = number of bytes of the product in the hold buffer.
           Note that this increases if necessary as more bytes are
           read.
    read_func = function used to read the product,
    stream = data describing the input stream (eg. FILE *).
*/
{
unsigned char * phold = (unsigned char *) hold;
long numread, hsize = *holdsize;
long sect_0_len = 4;		     /*	section 0 length: BUFR */
long sect_5_len = 4;		     /*	section 5 length: 7777 */
long num, length;
long next, next_len, next_sec = 3;
                                  
                                  
    /* Need more bytes to decide on which version of BUFR */
    if ( leng < 24 ) return -4;      /* User buffer too small */
    num = 24;                        /* put first 24 bytes in buffer*/
    if ( hsize < num)
    {
        numread = read_func( hold + hsize, num - hsize, stream);
        if ( numread <= 0 )          /* eg. on EOF */
        {
            *holdsize -= numread;
            return *holdsize;
        }
        hsize = num;
    }
    *holdsize = hsize;
                      
    /* If it's Edition 2, or later, octets 5-7 give full product size */
    if ( hold[7] > 1 ) return len3oct(phold+4);
                                               
   /* Otherwise, we have to step through the individual sections
   adding up the lengths */
                           
   /* Add on the length of section 1 and ensure enough of product is in
   memory to continue */
    length = sect_0_len + len3oct(phold+4);
    if ( leng < (length+4) ) return -4; /* User buffer too small */
    if ( hsize < (length+4))
    {
        numread = read_func( hold+hsize, (length+4)-hsize, stream);
        if ( numread <= 0 )          /* eg. on EOF */
        {
            *holdsize -= numread;
            return *holdsize;
        }
        hsize = length + 4;
        *holdsize = hsize;
    }
     
    /* Check for presence of section 2 */
    if ( hold[11] & BIT1 ) next_sec = 2;
                                        
    /* Get the size of remaining sections */
    for ( next = next_sec; next < 5 ; next++ )
    {
        length += len3oct(phold+length);
        if ( leng < (length+4) ) return -4; /* User buffer too small */
        if ( hsize < (length+4))
        {
            numread = read_func( hold+hsize,(length+4)-hsize, stream);
            if ( numread <= 0 )      /* eg. on EOF */
            {
                *holdsize -= numread;
                return *holdsize;
            }
            hsize = length + 4;
            *holdsize = hsize;
        }
    }
     
    /* Add on the size of section 5 */
                                      
    length += sect_5_len;
    if ( leng < length ) return -4; /* User buffer too small */
                                                               
    return length;
                  
}
 
void PSEUREAD(char * buffer, long * buffsize, long * readsize,
              long * status, FILE ** stream)
/*
    Called as a FORTRAN subroutine:
                                   
	CALL PSEUREAD( KARRAY, KINLEN, KOUTLEN, IRET, KUNIT )
                                                      
*/
{
long holdsize = *buffsize;
                          
/*  Read GRIB product */
    *status =  readprod( NULL, buffer, &holdsize, file_read, *stream);
    *readsize = (long) abs( holdsize );
    return;
           
}
 
 
void GRIBREAD(char * buffer, long * buffsize, long * readsize,
              long * status, FILE ** stream)
/*
    Called as a FORTRAN subroutine:
                                   
	CALL GRIBREAD( KARRAY, KINLEN, KOUTLEN, IRET, KUNIT )
                                                      
*/
{
long holdsize = *buffsize;
                          
/*  Read GRIB product */
    *status = readprod("GRIB", buffer, &holdsize, file_read, *stream);
    *readsize = (long) abs( holdsize );
    return;
           
}
 
void BUFRREAD(char * buffer, long * buffsize, long * readsize,
              long * status, FILE ** stream)
/*
    Called as a FORTRAN subroutine:
                                   
	CALL BUFRREAD( KARRAY, KINLEN, KOUTLEN, IRET, KUNIT )
                                                      
*/
{
long holdsize = *buffsize;
                          
/*  Read BUFR product */
    *status = readprod("BUFR", buffer, &holdsize, file_read, *stream);
    *readsize = (long) abs( holdsize );
    return;
           
}
 
static long tide_budg_size(char * hold, long leng, long * holdsize,
                           long (*read_func)(), void * stream)
/*
    Returns the size in bytes of the TIDE/BUDG/DIAG code product.
                                                                 
    hold = buffer to hold product,
    leng = length of buffer,
    holdsize = number of bytes of the product in the hold buffer.
               Note that this increases if necessary as more bytes
               are read.
    read_func = function used to read the product,
    stream = data describing the input stream (eg. FILE *).
*/
{
unsigned char * phold = (unsigned char *) hold;
long numread, hsize = *holdsize;
long sect_0_len = 4;		     /*	section 0 length:
                                        TIDE/BUDG/DIAG */
long sect_5_len = 4;		     /*	section 5 length: 7777 */
long num, length;
long next, next_len, next_sec = 3;
                                  
                                  
    /* Need more bytes to get length of section 1 */
    num = 8;                        /* put first 8 bytes in buffer */
    if ( leng < num ) return -4;    /* User buffer too small */
    if ( hsize < num)
    {
        numread = read_func( hold + hsize, num - hsize, stream);
        if ( numread <= 0 )         /* eg. on EOF */
        {
            *holdsize -= numread;
            return *holdsize;
        }
        hsize = num;
    }
    *holdsize = hsize;
                      
   /* Have to step through individual sections adding up the lengths */
                                                                       
   /* Add on the length of section 1 and ensure enough of product is in
   memory to continue */
    length = sect_0_len + len3oct(phold+4);
    if ( leng < (length+4) ) return -4; /* User buffer too small */
    if ( hsize < (length+4))
    {
        numread = read_func( hold+hsize, (length+4)-hsize, stream);
        if ( numread <= 0 )          /* eg. on EOF */
        {
            *holdsize -= numread;
            return *holdsize;
        }
        hsize = length + 4;
        *holdsize = hsize;
    }
     
/* Get the size of remaining section */
    length += len3oct(phold+length);
                                    
/* Add on the size of section 5 */
    length += sect_5_len;
                         
    if ( leng < length ) return -4;  /* User buffer too small */
    return length;
}
