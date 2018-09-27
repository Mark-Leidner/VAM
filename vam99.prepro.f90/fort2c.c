/*
	fort2c.c
*/
#include "fort2c.h"
                   
char *fcd2char(fortchar)
_fcd fortchar;
{
#ifndef VAX
	char *name = _fcdtocp(fortchar);
	int   len  = _fcdlen(fortchar);
#else
        char * name =  fortchar->address;
        short len =  fortchar->length;
#endif
      
        char * newp = (char *) malloc(len+1);
                                             
	if(!newp)
	{
		perror("malloc");
		return((char*)0);
	}
  
	strncpy(newp,name,len);
                        
	while( len && newp[--len] == ' ');
                                   
	newp[++len]='\0';
                  
	return(newp);
}
 
 
