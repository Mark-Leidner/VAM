/* 
 * CONTAINS: HDF Shell functions 
 */ 
   
#include <stdlib.h> 
#include <limits.h> 
#include <math.h> 
#include <time.h> 
#include <errno.h> 
 
#include <hdf.h>
#include <mfhdf.h>
#include <local_nc.h>

#define SHELL 
#include "hdfshell.h" 
#undef SHELL 
 
void hdf_print(int32, int32, void *, char *); 
char *hdftype(int32); 
 
NSCAT_ID *nscat_lu[MAX_LU]; 
 
 
/* Function Prototypes */ 
 
/* 
 * NAME:    hdf_open_file 
 * 
 * PURPOSE: to open a nscat HDF file 
 *             
 * 
 * AUTHOR:  Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:   int hdf_open_file(char *filename, short number_of_buffer_records); 
 *          filename: name of the nscat HDF file. 
 *          number_of_buffer_records: number of buffer records, if negative number, check if the file is a
 *                                    nscat HDF file. If this argument is set to either 0 or 1, no buffer is
 *                                    allocated.  
 *          Return values: 
 *             -1: file not found 
 *             -2: not HDF file 
 *             -3: can not start SD interface; 
 *             -4: not NSCAT HDF level 1.7, 2, or 3 file. 
 *             -5: out of logical unit. 
 *             -6: out of memory
 *             -7: fail to get the hdf id. 
 *              0 and positive number: file logical unit assigned to file. 
 *              
 * COMMENTS: This HDF shell can be used in any HDF files. If the shell is intended to be used in general case, 
 *           we need to delete the part of NSCAT checking code in this open routine. 
 * 
 *           Mutiple HDF files (upto 100, depending on HDF library and machine) can be opened and accessed  
 *           simutenously  
 * 
 */ 
 
int hdf_open_file(char *filename, short number_of_buffer_records) 
{ 
    static int first=1; 
    FILE *fp; 
    int32 index, sdid; 
    int16 level; 
    int lu, i; 
    char temp[120];
    
    /* for Vdata access */
    int32 cdfid;
    NC *handle; 
     
    /* initialize the lu structure */ 
    if(first){ 
        for(i=0; i < MAX_LU; i++)nscat_lu[i]=NULL; 
        first=0; 
    } 
     
    if(!(fp=fopen(filename, "rb")))return(-1); 
    fclose(fp); 
     
    /* Check Input File & open it*/ 
    if(Hishdf(filename) == 0)return(-2); 
 
    /* open the input file */     
    sdid = SDstart(filename, DFACC_RDONLY); 
    if( sdid == FAIL)return(-3); 
     
    /* check if this is a nscat hdf level 1.7, 2 or 3 file */
    level=0;
    if(number_of_buffer_records < 0){ 
        index=SDfindattr(sdid, "Sensor_Name"); 
        if(index != FAIL){ 
            temp[0]='\0'; 
            SDreadattr(sdid, index, temp); 
            if(strcmp(temp, "NSCAT") != 0) index=FAIL; 
        } 
        if(index == FAIL){ 
            SDend(sdid); 
            return(-4); 
        } 
     
        /* check the data level */ 
        index=SDfindattr(sdid, "Data_Type"); 
        if(index != FAIL){ 
            temp[0]='\0'; 
            SDreadattr(sdid, index, temp); 
            if(strcmp(temp, "L17")== 0) 
                level=LEVEL17; 
            else if(strcmp(temp, "L2") == 0) 
                level=LEVEL2; 
            else if(strcmp(temp, "L3") == 0) 
                level=LEVEL3; 
            else  
                level=0; 
        } 
     
        if(level == 0){ 
            SDend(sdid); 
            return(-4); 
        }
    } 
     
    /* find a logical unit */ 
    lu=-1; 
    for(i=0; i< MAX_LU; i++){ 
        if(nscat_lu[i]==NULL){ 
            lu=i; 
            break; 
        } 
    } 
    if(lu == -1){ 
        SDend(sdid); 
        return(-5); 
    } 
 
    /* allocate the memory for the logical unit */ 
    nscat_lu[lu]=(NSCAT_ID *)malloc(sizeof(NSCAT_ID)); 
    if(nscat_lu[lu] == NULL){ 
        SDend(sdid); 
        return(-6); 
    } 
     
    nscat_lu[lu]->number_of_buffer_records=number_of_buffer_records;
    if(number_of_buffer_records < 0 ) nscat_lu[lu]->number_of_buffer_records = - number_of_buffer_records; 
    nscat_lu[lu]->data_level=level; 
    nscat_lu[lu]->sdid=sdid;
    nscat_lu[lu]->hdf_id = -1;
    strcpy(nscat_lu[lu]->file_name, filename); 
    for(i=0; i<MAX_SDS; i++) 
        nscat_lu[lu]->sds[i]=NULL; 
         
    SDfileinfo(sdid, &(nscat_lu[lu]->ndatasets), &(nscat_lu[lu]->nattrs)); 
    
    /* set the HDF id */
    cdfid = sdid & 0xFF;
    handle = NC_check_id(cdfid);
    if(handle)nscat_lu[lu]->hdf_id=handle->hdf_file;
    if(nscat_lu[lu]->hdf_id == -1){
    	hdf_close_file(lu);
    	return(-7);
    }
      
    return(lu); 
} 
 
/* 
 * NAME:    hdf_query_SDS 
 * 
 * PURPOSE:    to get information about a named SDS (array dataset) 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:    int hdf_query_SDS(int lu, char *name, long *rank, long *dimsizes, long *data_type, long *bytes_per_pixel, long * *bytes_per_line) 
 *        lu (input): logical unit for the hdf file obtained by calling hdf_open_file. 
 *        name (input): the name of the scientific data set to be accessed. 
 *        rank (output): the number of dimensions for the SDS 
 *        dimsizes (output):  a one dimension array which holds the size of each dimension for the named SDS.  
 *                            dimsizes[0] represents the size of the first dimension of the SDS (number of lines). 
 *        data_type (output): the data type for the SDS. Data types, which are the same as defined in hdf.h,  
 *                            are defined in hdfshell.h. 
 *        bytes_per_pixel: numebr of bytes per cell. 
 *        bytes_per_line:  number of bytes per line (record)  
 *        Return values: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 : all right. 
 *              
 * COMMENTS:  
 * 
 */ 
         
int hdf_query_SDS(int lu, char *name, long *rank, long *dimsizes, long *data_type, long *bytes_per_pixel, long *bytes_per_line) 
{   
    SDS_RECORD *record; 
    int16 sds_no; 
    int32 i; 
     
    sds_no=hdf_get_SDS_record(lu, name); 
    if(sds_no < 0) return(sds_no);            /* error in getting the sds record */ 
     
    record=nscat_lu[lu]->sds[sds_no]; 
     
    /* set the return value */ 
    *rank = record->rank; 
    for(i=0; i< record->rank; i++) 
        dimsizes[i]=record->dimsizes[i]; 
         
    *data_type=record->type; 
    *bytes_per_line=record->bytes_per_line; 
    *bytes_per_pixel=record->bytes_per_pixel; 
    return(0); 
} 
     
/* 
 * NAME:    hdf_get_SDS_record 
 * 
 * PURPOSE:    to obtain the SDS record structure id, if the record does not exist, create SDS record structure. 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_get_SDS_record(int lu, char *name) 
 *        lu (input): logical unit for HDF file obtained by calling hdf_open_file. 
 *        name (input): the name of the scientific data object to be accessed. 

 *        Return value: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 & possitive values : internal id for the named SDS. 
 *              
 * COMMENTS: internal use in the shell routines. Not intend for users to call this routine. 
 * 
 */ 
 
int hdf_get_SDS_record(int lu, char *name) 
{   NSCAT_ID *id_temp; 
    SDS_RECORD *record; 
    int16 sds_no, no_buffer; 
    int32 i, sds_index, sdsid, num_bytes; 
         
    id_temp=nscat_lu[lu]; 
    if(id_temp == NULL)return(-1); 
     
    sds_no=-1; 
    for(i=0; i< MAX_SDS; i++){ 
        if(id_temp->sds[i] != NULL){ 
            if(strcmp(id_temp->sds[i]->name, name) == 0){ 
                sds_no=i; 
                break; 
            } 
        } 
    } 
 
    /* first time accesses this sds object, allocating memory and initialize the record struct */ 
    if(sds_no == -1){ 
        for(i=0; i< MAX_SDS; i++){ 
            if(id_temp->sds[i] == NULL){ 
                sds_no=i; 
                break; 
            } 
        } 
  
        /* check if the named sds object exists */ 
        sds_index=SDnametoindex(id_temp->sdid, name); 
        if(sds_index == FAIL)return(-2); 
         
        /* allocating memory */ 
        record=id_temp->sds[sds_no]=(SDS_RECORD *)malloc(sizeof(SDS_RECORD)); 
        if(!record)return(-6); 
         
        /* initialize the record */ 
        record->sdsid = SDselect(id_temp->sdid, sds_index); 
        SDgetinfo(record->sdsid, record->name, &(record->rank), record->dimsizes, &(record->type), &(record->nattrs)); 
        record->start[0]= record->dimsizes[0]; 
  
        for(i=1; i< record->rank; i++){ 
            record->start[i]=0; 
            record->edge[i]=record->dimsizes[i]; 
        } 
        record->edge[0]=id_temp->number_of_buffer_records; 
        if(record->edge[0]== 0)record->edge[0]=1;             /* no buffer reading */ 
         
        /* calculate bytes per line & per pixel */ 
        record->bytes_per_line=record->bytes_per_pixel=DFKNTsize(record->type | DFNT_NATIVE); 
        for(i=1; i< record->rank; i++) 
            record->bytes_per_line = record->bytes_per_line * record->dimsizes[i]; 
             
        /* not allocate the buffer */ 
        record->buffer=NULL; 
 
    } 
    return(sds_no); 
} 
     
/* 
 * NAME:    hdf_getparam 
 * 
 * PURPOSE:    to get one line (record) from a named SDS each time. Buffered access. 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int getparam(int lu, char *name, int line, void *buffer) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the data object to be accessed. 
 *        line (input): the line (record) number in the data object to be retrieved. 0 based. 
 *        buffer (output): memory supplied by the calling function which should be large enough to hold the contents of 
 *                         whole record. 
 *        Return values: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of bytes retrieved. 
 *              
 * COMMENTS: If users set the buffer size in hdf_open_file, and there are no memory availabe, this routine will 
 *           automatically swith to unbufferred read. 
 * 
 */ 
         
int hdf_getparam(int lu, char *name, int line, void *buffer) 
{    
    SDS_RECORD *record; 
    int16 sds_no, no_buffer; 
    int32 i, sds_index, sdsid, num_bytes, temp_edge0; 
      
    sds_no=hdf_get_SDS_record(lu, name); 
    if(sds_no < 0)return(sds_no); 
    record=nscat_lu[lu]->sds[sds_no]; 
     
    /* if the required line is in the dimsizes */ 
    if(line >= record->dimsizes[0] || line < 0)return(-3); 
  
    /* allocate the buffer for buffered access */ 
    if((record->edge[0] > 1) && (record->buffer == NULL)){ 
        record->buffer=malloc(record->bytes_per_line * record->edge[0]); 
        if(!record->buffer)record->edge[0]=1;  /* no enough memory, change to unbuffered read */         
    } 
     
    /* no buffer reading */ 
    if(record->edge[0] == 1){  
        record->start[0]=line; 
        if(SDreaddata(record->sdsid, record->start, NULL, record->edge, buffer) == FAIL)return(-4); 
    } 
    else {                    /* bufferred reading */ 
        if(line < record->start[0] || line >= (record->start[0] + record->edge[0])){ 
            record->start[0]=line; 
            temp_edge0=record->edge[0]; 
            if(record->start[0]+record->edge[0] > record->dimsizes[0]) 
                record->edge[0]=record->dimsizes[0]-record->start[0]; 
            if(SDreaddata(record->sdsid, record->start, NULL, record->edge, record->buffer) == FAIL)return(-4); 
            record->edge[0]=temp_edge0; 
        } 
         
        /* find the right location in the memory and memcpy the contents to output buffer */ 
        num_bytes=(line - record->start[0])*record->bytes_per_line; 
        memcpy(buffer, record->buffer+num_bytes, record->bytes_per_line); 
    } 
    return(record->bytes_per_line); 
}
 
/* 
 * NAME:    hdf_getvdata 
 * 
 * PURPOSE:    to get vdata records from the HDF file, or obtain information about a vdata. 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_getvdata(int lu, char *name, char *fieldnames, int *start, int *nrcds, void *buffer) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the vdata data object to be accessed.
 *        fieldnames(in/out): Input with comma delimited fields to be retrieved. The data in the buffer
 *                  will be in the same sequence as the fields listed in fieldnames. If this argument is 
 *                  set to NULL, all fields in a vdata will be retrieved. If start is set to -1, and this 
 *                  argument is not set to NULL, the name of all fields in the table (vdata) will be 
 *                  retrieved. For this case, make sure that the fieldnames is larger enough to hold the 
 *                  field names.
 *
 *        start (in/out): input with the start record in a vdata to be retrieved. 0 based.
 *                  If the input value of start is set to -1 or the function fails (-3), 
 *                  return with number of bytes per record corresponding to fields defined in the fieldnames.
 *                  Therefore, if input value of start is set to -1, the number of total bytes per record
 *                  in the vdata will be retrieved. To get the number of bytes for individual fields, set the
 *                  fieldnames to the individual fields, and set start to a negetive number other than -1.
 *		  nrcds (in/out): the number of records to be retrieved.
 *                  If the input value of start is set to -1 or the function fails (-3), 
 *                  return with the number of records in the table. 
 *        buffer (out): memory supplied by the calling function which should be large enough to hold 
 *                  the contents of whole records. 
 *        Return values: 
 *        -1: illegal logical unit 
 *        -2: no such vdata 
 *        -3: record out of the range (the function will return the number of total records 
 *            in vdata to nrcds, and the number of bytes per record to start) 
 *        -4: HDF read error. 
 *        -5: attach failed(vdata NAME ERROR). 
 *        -6: fields to be retrieved are not found in the vdata  
 *        0 and positive number: number of bytes retrieved. If start is set to -1 and everything is 
 *                               right, it will return 0.
 *              
 * COMMENTS: If users set the buffer size in hdf_open_file, and there are no memory availabe, this routine will 
 *           automatically swith to unbufferred read. 
 * 
 */ 
         
int hdf_getvdata(int lu, char *name, char *fieldnames, int *start, int *nrcds, void *buffer) 
{    
    NSCAT_ID *file;
    int32 ref_id, vdata_id, nrecords, vsize;
    char fields[32767];
    int istart; 

    file=nscat_lu[lu]; 

	file->hdf_id = lu;
	
    if(file == NULL)return(-1);
   

    /* no Vstart required */

	/* check whether 'table' exist */
  	if ((ref_id = VSfind(file->hdf_id, name)) == FAIL) return(-2);

	/* try attach to said tablename */
    if ((vdata_id = VSattach(file->hdf_id, ref_id, "r")) == FAIL)return(-5);
    
	VSinquire(vdata_id, &nrecords, NULL, fields, &vsize, NULL);
	istart = *start;
	if(*start < 0 || ((*nrcds)+ (*start)) > nrecords){
		*nrcds = nrecords;
		if(istart != -1){
			if(fieldnames != NULL && (vsize = VSsizeof(vdata_id, fieldnames)) == FAIL){
				VSdetach(vdata_id);
				return(-6);
			}
		}
		else 
			if(fieldnames != NULL)strcpy(fieldnames, fields); 
		
		VSdetach(vdata_id);
		*start = vsize;
		if(istart == -1)
			return(0);
		else
			return(-3);
	}
	
	if(fieldnames == NULL)fieldnames = fields;
	if(VSsetfields(vdata_id, fieldnames) == FAIL){
		VSdetach(vdata_id);
		return(-6);
	}
	
    /* VSseek will always be correct because the start already be checked before */
    VSseek(vdata_id,(int32)(*start));

    if(VSread(vdata_id, buffer, (int32)(*nrcds),FULL_INTERLACE) == FAIL){
    	VSdetach(vdata_id);
    	return(-4);
    }

	/* detatch */
	vsize = VSsizeof(vdata_id, fieldnames);
	VSdetach(vdata_id);
	return((*nrcds) * vsize);
} 
 
/* 
 * NAME:    hdf_subset_param 
 * 
 * PURPOSE:    to get a subset from a named parameter (SDS) 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_subset_param(int lu, char *name, long start[], long stride[], long edge[], void *buffer) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the data object to be accessed. 
 *        start[]: array specifying the starting location for each dimision. The value is 0 based 
 *        stride[]: array specifying the subsampling along each dimension. If a stride value is specified for a dimension, 
 *                  that many values will be skipped over when reading along that dimension. Specifying stride=NULL reads  
 *                  contiguous data. No matter what stride value is provded, data is always placed, data is always placed 
 *                  contiguously in buffer. 
 *        edge[]: array specifying the number of values to read along each dimension. 
 *        buffer (output): memory supplied by the calling function which should be large enough to hold the data. 
 *        Return value: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of bytes retrieved. 
 *              
 * COMMENTS:  
 * 
 */ 
         
int hdf_subset_param(int lu, char *name, long *start, long *stride, long *edge, void *buffer) 
{    
    SDS_RECORD *record; 
    int16 sds_no, no_buffer; 
    int32 i, sds_index, sdsid, num_bytes, temp_edge0; 
 
     
    /* get the corresponding SDS record */  
    sds_no=hdf_get_SDS_record(lu, name); 
    if(sds_no < 0)return(sds_no); 
    record=nscat_lu[lu]->sds[sds_no]; 
     
    /* check if the required data within range */ 
    for(i=0; i< record->rank; i++){ 
        if(start[i] < 0 || start[i] >= record->dimsizes[i])return(-3); 
        if(edge[i]<= 0)return(-3); 
        num_bytes=edge[i]; 
        if(stride != NULL){ 
            if(stride[i]<0)return(-3); 
            num_bytes=(num_bytes-1)*(stride[i]+1)+1; 
        } 
        if(num_bytes+start[i] > record->dimsizes[i])return(-3); 
    }     
 
    /* read the data */  
/*
    if(SDreaddata(record->sdsid, start, stride, edge, buffer) == FAIL)return(-4); 
*/
    if(SDreaddata(record->sdsid, record->start, NULL, record->edge, buffer) == FAIL)return(-4); 
    num_bytes=record->bytes_per_pixel; 
    for(i=0; i < record->rank; i++) 
        num_bytes=num_bytes*edge[i]; 
    return(num_bytes);     
} 
 
/* 
 * NAME:    hdf_get_attributes 
 * 
 * PURPOSE:    to get attributes attached to the file (file header) or attached to individual SDS (parameter array) 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_get_attributes(int lu, char *name, char *buffer) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the data object (parameter). If this is NULL, retrieve the global attribute, 
 *                      otherwise, local attributes (attributes attached to the named SDS). 
 *        buffer (output): buffer to hold the attributes. The memory should be allocated by the calling function, 
 *                        and should be large enough to hold all attributes. 
 *                        Attributes will be returned in ASCII in the parameter_value_language (PVL) form 
 *                        (e.g. Paramter_name=paramter_value). The delimitor between parameters is newline (\n). 
 *        Return value: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of attributes actually retrieved. 
 *              
 * COMMENTS: This is header reader routine. 
 * 
 */ 
int hdf_get_attributes(int lu, char *name, char *buffer) 
{     NSCAT_ID *id_temp; 
    SDS_RECORD *record; 
    int32 sds_no, nattrs, i,j, num_type, nbytes; 
    float64 attrvalue[128]; 
    char temp_name[120], *ch, *pointer; 
     
    id_temp=nscat_lu[lu]; 
    if(id_temp == NULL)return(-1); 
    
    if(name && strlen(name) == 0)name=NULL;
    sds_no=id_temp->sdid; 
    nattrs=id_temp->nattrs; 
    if(name){         /* local attributes */ 
        sds_no=hdf_get_SDS_record(lu,name); 
        if(sds_no < 0)return(sds_no); 
        nattrs=id_temp->sds[sds_no]->nattrs; 
        sds_no=id_temp->sds[sds_no]->sdsid; 
    } 
                        
    /* retrieve the attributes and put in PVL form */ 
    ch=buffer; 
    i=0; 
    for( j = 0; j < nattrs; j++){ 
        if(SDattrinfo(sds_no, j, temp_name, &num_type, &nbytes) ==SUCCEED){ 
            sprintf(ch, "%s=", temp_name); 
            ch=ch+strlen(ch); 
            SDreadattr(sds_no, j, (void *)attrvalue); 
            if(num_type == DFNT_CHAR8 || num_type == DFNT_UCHAR8){ 
                pointer = (char *)attrvalue; 
                *(pointer + nbytes) ='\0'; 
            } 
            hdf_print(num_type, nbytes, attrvalue, ch); 
            ch=ch+strlen(ch); 
            *ch='\n'; 
            ch++; 
            *ch='\0'; 
            i++;         
        } 
    } 
    return(i); 
} 
 
/* 
 * NAME:    hdf_get_dataset_names 
 * 
 * PURPOSE:    to get names for all datasets (SDSs) in the HDF file 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_get_attributes(int lu, char *buffer) 
 *        lu (input): logical unit for the hdf file 
 *        buffer (output): buffer to hold names for datasets (SDSs) in a HDF file. The memory should  
 *                         be allocated by the calling function and should be large enough to hold all names  
 *                         together. The delimitor between names is newline (\n). 
 *        Return value: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of dataset names in the buffer. 
 *              
 * COMMENTS:  
 * 
 */ 
int hdf_get_dataset_names(int lu, char *buffer) 
{     NSCAT_ID *id_temp; 
    int32 ndatasets, sdsid, rank, num_type, nattrs,i; 
    float64 attrvalue[128]; 
    char sds_name[120], *ch; 
    int32 dimsizes[MAX_DIM]; 
     
    id_temp=nscat_lu[lu]; 
    if(id_temp == NULL)return(-1); 
     
    /* retrieve the name */ 
    ndatasets=0; 
    ch=buffer; 
    for(i = 0; i < id_temp->ndatasets; i++){ 
        sdsid = SDselect(id_temp->sdid, i); 
        if(SDiscoordvar(sdsid)==FALSE){ 
            ndatasets++; 
            SDgetinfo(sdsid, sds_name, &rank, dimsizes, &num_type, &nattrs); 
            sprintf(ch, "%s\n", sds_name); 
            ch=ch+strlen(ch); 
        } 
        SDendaccess(sdsid); 
    } 
    return(ndatasets); 
}         
 
/* 
 * NAME:    hdf_close_SDS 
 * 
 * PURPOSE:    to end access a hdf SDS for memory release 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_close_SDS(int lu, char *name) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the SDS data object. 
 *        Return value: 
 *        0: all right 
 *        -1: illegal logical unit 
 *        -2: no such sds opened 
 *              
 * COMMENTS: If the user will no longer accesses the named SDS but will access other data objects in the same hdf, 
 *           s/he should call this function to release memory. If s/he fininshes access to the file, hdf_close_file  
 *           should be called.    
 * 
 */ 
         
int hdf_close_SDS(int lu, char *name) 
{   NSCAT_ID *id_temp; 
    SDS_RECORD *record; 
    int32 sds_no,i; 
     
    id_temp=nscat_lu[lu]; 
    if(id_temp == NULL)return(-1); 
     
    sds_no=-1; 
    for(i=0; i< MAX_SDS; i++){ 
        if(id_temp->sds[i] != NULL){ 
            if(strcmp(id_temp->sds[i]->name, name) == 0){ 
                sds_no=i; 
                break; 
            } 
        } 
    } 
     
    if(sds_no == -1)return(-2); 
    record=id_temp->sds[sds_no]; 
     
    /* end the access to the sds */ 
    SDendaccess(record->sdsid); 
    if(record->buffer)free(record->buffer); 
    free(record); 
    id_temp->sds[sds_no]=NULL; 
    return(0); 
} 
                  
         
/* 
 * NAME:    hdf_close_file 
 * 
 * PURPOSE:    to end access to a hdf file, and release all allocated memory 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: int hdf_close_file(int lu) 
 *        lu: logical unit for the hdf file 
 *        Return value: 
 *        0: all right 
 *        -1: illegal logical unit 
 *              
 * COMMENTS:  
 * 
 */ 
         
int hdf_close_file(int lu) 
{   NSCAT_ID *id_temp;  
    int32 i; 
     
    id_temp=nscat_lu[lu]; 
    if(id_temp == NULL)return(-1); 
 
    for(i=0; i< MAX_SDS; i++) 
        if(id_temp->sds[i] != NULL)hdf_close_SDS(lu, id_temp->sds[i]->name); 
    
    /* Vend(id_temp->hdf_id);  */   /* not needed */
     
    SDend(id_temp->sdid); 
    free(id_temp); 
    nscat_lu[lu]=NULL;     
    return(0); 
} 
 
/* 
 * NAME:    hdf_print 
 * 
 * PURPOSE:    to print HDF numerical values into a buffer in ASCII form 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: void hdf_printf(int32 num_type, int32 count, void *value, char *buffer) 
 *        num_type(input): HDF numerical type. 
 *        count (input): number of numerical values in void value memory 
 *        value (input): buffer for the numerical values 
 *        buffer (output): buffer to hold ASCII numerical values. 
 *        Return value: no return value 
 *              
 * COMMENTS: this routine is used internally in HDF shell. No intend for users to use.  
 * 
 */ 
 
void hdf_print(int32 num_type, int32 count, void *value, char *ch) 
{   int16 step, i; 
    char *type; 
 
    switch(num_type){ 
    case DFNT_FLOAT64: 
        step = 8; 
        break; 
    case DFNT_FLOAT32: 
    case DFNT_INT32: 
    case DFNT_UINT32: 
        step = 4; 
        break; 
    case DFNT_INT16: 
    case DFNT_UINT16: 
        step=2; 
        break; 
    case DFNT_UINT8: 
    case DFNT_INT8: 
        step = 1; 
        break; 
    case DFNT_UCHAR8: 
    case DFNT_CHAR8: 
        step = -1; 
        break; 
    default: 
        step = 0; 
    } 
    type = (char *)value; 
    for(i=0; i < count; i++){ 
        switch (num_type) { 
        case 5:    /* float32 type. */ 
            sprintf(ch, "%E",*(float32 *)type); 
            break; 
        case 6:    /* float64 type. */ 
            sprintf(ch, "%E", *(float64 *)type); 
            break; 
        case 20:/* int8 type. */ 
            sprintf(ch, "%d", *(int8 *)type); 
            break; 
        case 21:/* uint8 type. */ 
            sprintf(ch, "%u", *(uint8 *)type); 
            break; 
        case 22:/* int16 type. */ 
            sprintf(ch, "%d", *(int16 *)type); 
            break; 
        case 23:/* uint16 type. */ 
            sprintf(ch, "%u", *(uint16 *)type); 
            break; 
        case 24:/* int32 type. */ 
            sprintf(ch, "%d", *(int32 *)type); 
            break; 
        case 25:/* uint32 type. */ 
            sprintf(ch, "%u", *(uint32 *)type); 
            break; 
        case DFNT_CHAR8: 
            sprintf(ch, "%s", (char8 *)type); 
            break; 
        case DFNT_UCHAR8: 
            sprintf(ch, "%s", (uchar8 *)type); 
            break; 
        default:  
            sprintf(ch, "__Conversion Error__: Unknown type"); 
        } 
        if(step == -1)break; 
        if(i != count-1){ 
            ch=ch+strlen(ch); 
            sprintf(ch, ","); 
            ch++; 
            type = type + step; 
        } 
    } 
}

