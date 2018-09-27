/*
  bufrgrib.h
*/
#include <stdio.h>
#include <string.h>
                   
/*	defines for BUFR functions */
#define ARRSIZE 100
#define TRUE 1
#define FALSE 0
#define BOOL int
#define BOOLEAN int
                   
/* defines for BUFR/GRIB functions */
                                     
#ifndef CRAY
#define PBOPEN	pbopen_
#define PBREAD	pbread_
#define PBREAD2	pbread2_
#define PBWRITE	pbwrite_
#define PBSEEK	pbseek_
#define PBCLOSE	pbclose_
#define PBSIZE	pbsize_
#define GETGRIBLEN getgriblen_
#define GETBUFR getbufr_
#define BUFRREAD bufrread_
#define GRIBREAD gribread_
#define PSEUREAD pseuread_
#endif
      
#if (defined __hpux) || (defined rs6000) || (defined VAX)
#define PBOPEN  pbopen
#define PBREAD  pbread
#define PBREAD2 pbread2
#define PBWRITE pbwrite
#define PBSEEK  pbseek
#define PBCLOSE pbclose
#define PBSIZE	pbsize
#define GETGRIBLEN getgriblen
#define GETBUFR getbufr
#define BUFRREAD bufrread
#define GRIBREAD gribread
#define PSEUREAD pseuread
#endif
      
#ifdef VAX
#define off_t char *
#include <types.h>
#include <file.h>
#endif
      
#define BUFSIZE 200
