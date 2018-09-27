/*
	fort2c.h
*/
#ifdef CRAY
#include <fortran.h>
#else
#define _fcd char *
#define _fcdtocp(a) a
#define _fcdlen(a) strlen(a)
#endif
      
#ifdef VAX
typedef struct {short length; short magic; char * address;}DESC;
#define _fcdtocp(a) fcdtocp(a)
#define _fcd DESC *
#endif
      
char *fcd2char();	/* fortran to c string convertion (alloc memory) */
                                                                     
                                                                     
                                                                     
