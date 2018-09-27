#include <hdf.h> 
#define SHELL 
#include "hdfshell.h" 
#undef SHELL 
extern NSCAT_ID *nscat_lu[]; 
 
/* these functions are FORTRAN-C filter functions which will be called by FORTRAN interface. Those routines 
 * are used internally by the FORTRAN interface.  
 */ 
 
/* NAME:    fhdfopen_ 
 * 
 * PURPOSE:    to open a nscat HDF file, FORTRAN-C interface 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: void fhdfopen_(_fcd *filename, intf *number_of_buffer_records, intf *fnlen, inf *rtv); 
 *        filename: name of the nscat HDF file. 
 *        number_of_buffer_records: number of buffer records, 0 or possitive number  
 *        fnlen: string length of filename
 *        rtv: Return values 
 *        -1: file not found 
 *        -2: not HDF file 
 *        -3: can not start SD interface; 
 *        -4: not NSCAT HDF level 1.7, 2, or 3 file. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: file logical unit assigned to file. 
 *              
 * COMMENTS: This routine is called by FORTRAN function nfopen  
 * 
 */ 
 
void fhdfopen_(_fcd filename, intf *number_of_buffer_records, intf *fnlen, intf *rtv) 
{ 
    char *fn;
    fn = HDf2cstring(filename, (intn)*fnlen); 
    *rtv = hdf_open_file(fn, (short)(*number_of_buffer_records)); 
    HDfreespace((VOIDP)fn); 
    return; 
} 
 
/* 
 * NAME:    fquery_ 
 * 
 * PURPOSE:    to get information about a named SDS (array dataset). FORTRAN-C interface.
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:    void fquery_(intf *lu, _fcd name, intf *rank, intf *dimsizes, intf *data_type, intf *bytes_per_pixel, intf *bytes_per_line, intf *len, intf *rtv) 
 *        lu (input): logical unit for the hdf file obtained by calling hdf_open_file. 
 *        name (input): the name of the scientific data set to be accessed. 
 *        rank (output): the number of dimensions for the SDS 
 *        dimsizes (output): a one dimension array which holds the size of each dimension for the named SDS.  
 *                           dimsizes[0] represents the size of the first dimension of the SDS (number of lines). 
 *        data_type (output):the data type for the SDS. Data types, which are the same as defined in hdf.h,  
 *                           are defined in hdfshell.h. 
 *        bytes_per_pixel: numebr of bytes per cell. 
 *        bytes_per_line: number of bytes per line (record)
 *        len: string length of name  
 *        rtv: Return values 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 : all right. 
 *              
 * COMMENTS: Called by FORTRAN function nfquery  
 * 
 */ 
         
void fquery_(intf *lu, _fcd name, intf *rank, intf *dimsizes, intf *data_type, intf *bytes_per_pixel, intf *bytes_per_line, intf *len, intf *rtv) 
{   
    long i, rk, dsize[MAX_DIM], dtype, bytes, lbytes;
    char *nm; 
    nm = HDf2cstring(name, *len); 

    *rtv = hdf_query_SDS(*lu, nm, &rk, dsize, &dtype, &bytes, &lbytes); 

    HDfreespace((VOIDP)nm); 
    if(*rtv < 0)return; 
    *rank=rk; 
    *data_type=dtype; 
    *bytes_per_pixel = bytes; 
    *bytes_per_line = lbytes; 
    for(i = 0; i<rk; i++) 
        dimsizes[i] = dsize[rk - i -1]; 
    return;  
} 
     
/* 
 * NAME:    fgetpara_ 
 * 
 * PURPOSE:    to get one line (record) from a named SDS each time. Buffered access. FORTRAN-C interface. 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:    void fgetpara_(intf *lu, _fcd name, intf *line, VOIDP buf, intf *len, intf *rtv) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the data object to be accessed. 
 *        line (input): the line (record) number in the data object to be retrieved. 0 based. 
 *        buf (output): memory supplied by the calling function which should be large enough to hold the contents of 
 *                whole record.
 *        len (input) : length of the name 
 *        rtv: return values: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of bytes retrieved. 
 *              
 * COMMENTS: called by FORTRAN function nfgetpar
 * 
 */ 
         
void fgetpara_(intf *lu, _fcd name, intf *line, VOIDP buf, intf *len, intf *rtv) 
{    
    char *nm; 
    int ln;
     
    ln = *line; 
    nm = HDf2cstring(name, *len); 
    *rtv = hdf_getparam(*lu, nm, ln, buf); 
    HDfreespace((VOIDP) nm);
    return; 
} 

/* 
 * NAME:    fcgvd 
 * 
 * PURPOSE:    to get vdata records from the HDF file. 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: void fcgvd(intf *lu, _fcd nm, intf *lnm, _fcd fldnms, intf *lfl, intf *start, 
 *                  intf *nrcds, void *buffer, intf *rtv) 
 * Comments: Internal routine for FORTRAN interface. It uses the wrapping methods to call C 
 *           funcation. 
 */ 
void nfcgvd(intf *lu, _fcd nm, intf *lnm, _fcd fldnms, intf *lfl, intf *start,
            intf *nrcds, VOIDP buffer, intf *rtv)
{
	char *name, *fieldnames;
 	int strt, nrds;
 	
 	name = HDf2cstring(nm, *lnm);
 	if(*start == -1){
 		if(*lfl <= 1)
 			fieldnames = NULL;
 		else
 			fieldnames = _fcdtocp(fldnms);
 	}
 	else {
 		fieldnames = HDf2cstring(fldnms, *lfl);
 		if(strlen(fieldnames) == 0){
 			HDfreespace((VOIDP)fieldnames);
 			fieldnames = NULL;
 		}
 	}
 	strt = *start;
 	nrds = *nrcds;
 
 	*rtv = hdf_getvdata(*lu, name, fieldnames, &strt, &nrds, buffer);
 	
 	if(*start == -1 && fieldnames != NULL)HDc2fstr(fieldnames, *lfl);
 	if(*start != -1 && fieldnames != NULL)HDfreespace((VOIDP)fieldnames);
 		
 	*start= strt;
 	*nrcds= nrds;
	return;
} 	
 		
/* NAME:    fsubset_ 
 * 
 * PURPOSE:    to get a subset from a named parameter (SDS). FORTRAN-C interface 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: void fsubset_(intf lu, _fcd *name, intf start[], intf stride[], intf edge[], void buffer[], intf *len, intf *rtv) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the data object to be accessed. 
 *        start[]: array specifying the starting location for each dimision. The value is 0 based 
 *        stride[]: array specifying the subsampling along each dimension. If a stride value is specified for a dimension, 
 *                  that many values will be skipped over when reading along that dimension. Specifying stride=NULL reads  
 *                  contiguous data. No matter what stride value is provded, data is always placed, data is always placed 
 *                  contiguously in buffer. 
 *        edge[]: array specifying the number of values to read along each dimension. 
 *        buffer (output): memory supplied by the calling function which should be large enough to hold the data. 
 *        len: string length of name
 *        rtv: return value: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of bytes retrieved. 
 *              
 * COMMENTS: called by FORTRAN function nfsubset  
 * 
 * (void fsubset_(intf *lu, _fcd name, intf *start, intf *stride, intf *edge, VOIDP buf, intf *len, intf *rtv)
 *  this calles was changed/modified for the call below in order to compile correctly) 
 */ 
         
void fsubset_(intf *lu, _fcd name, intf start[], intf stride[], intf edge[], VOIDP buf[], intf *len, intf *rtv) 
{    
     
    int16 sds_no; 
    char *nm; 
    int rank, i, nostride; 
    long cstart[MAX_DIM], cstride[MAX_DIM], cedge[MAX_DIM]; 
 
    nm = HDf2cstring(name, *len); 
     
    /* get the corresponding SDS record */  
    *rtv=sds_no=hdf_get_SDS_record(*lu, nm); 
    if(sds_no < 0){ 
        HDfreespace(nm); 
        return; 
    } 
     
    rank = nscat_lu[*lu]->sds[sds_no]->rank; 
     
    /* set the access array */ 
    nostride = TRUE; 
    for( i = 0; i < rank; i++) { 
        cstart[i] = start[rank - i -1]; 
        cedge [i] = edge[rank - i -1]; 
        if((cstride[i] = stride[rank - i - 1]) != 1)nostride = FALSE; 
    } 
     
    if(nostride) 
        *rtv = hdf_subset_param(*lu, nm, cstart, NULL, cedge, buf); 
    else 
        *rtv = hdf_subset_param(*lu, nm, cstart, cstride, cedge, buf); 
    HDfreespace((VOIDP)nm);
    return; 
} 
 
/* 
 * NAME:    fattr_ 
 * 
 * PURPOSE:    to get attributes attached to the file (file header) or attached to individual SDS (parameter array) 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE: void fattr_(intf *lu, _fcd name, _fcd buf, intf *lname, intf *lbuf, intf *rtv) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the data object (parameter). If this is NULL, retrieve the global attribute, 
 *                      otherwise, local attributes (attributes attached to the named SDS). 
 *        buffer (output): buffer to hold the attributes. The memory should be allocated by the calling function, 
 *                        and should be large enough to hold all attributes. 
 *                        Attributes will be returned in ASCII in the parameter_value_language (PVL) form 
 *                        (e.g. Paramter_name=paramter_value). The delimitor between parameters is newline (\n). 
 *        lname(input): string length of name
 *        lbuf (input): size of buf
 *        rtv: return value: 
 *        -1: illegal logical unit 
 *        -2: no such sds 
 *        -3: record out of the range (EOF); 
 *        -4: HDF read error. 
 *        -5: out of logical unit. 
 *        -6: out of memory 
 *        0 and positive number: number of attributes actually retrieved. 
 *              
 * COMMENTS: called by FORTRAN function nfattr 
 * 
 */ 
void fattr_(intf *lu, _fcd name, _fcd buf, intf *lname, intf *lbuf, intf *rtv) 
{ 
    char *nm, *buffer; 

    nm = HDf2cstring(name, *lname); 
    buffer = (char *)HDgetspace((uint32)(*lbuf+1)); 
    *rtv = hdf_get_attributes (*lu, nm, buffer); 
    HDfreespace(nm); 
    HDpackFstring(buffer, _fcdtocp(buf), *lbuf); 
    HDfreespace((VOIDP)buffer);
    return;     
} 
 
/* 
 * NAME:    fnames_ 
 * 
 * PURPOSE:    to get names for all datasets (SDSs) in the HDF file. FORTRAN-C interface 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:    void fnames_(intf *lu, _fcd buf, intf *lbuf, intf *rtv) 
 *        lu (input): logical unit for the hdf file 
 *        buf (output): buffer to hold names for datasets (SDSs) in a HDF file. The memory should  
 *                         be allocated by the calling function and should be large enough to hold all names  
 *                         together. The string is a FORTRAN string
 *        lbuf (input): The size of buf 
 *        rvt: Return value: 
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
void fnames_(intf *lu, _fcd buf, intf *lbuf, intf *rtv) 
{ char *buffer;
     
    buffer = (char *)HDgetspace((uint32)(*lbuf+1)); 
     
    *rtv = hdf_get_dataset_names(*lu, buffer); 
    if(*rtv >= 0)        
        HDpackFstring(buffer, _fcdtocp(buf), *lbuf); 
    HDfreespace((VOIDP)buffer);
    return;     
}      
         
 
/* 
 * NAME:    fclosesd_ 
 * 
 * PURPOSE:    to end access a hdf SDS for memory release. FORTRAN-C interface 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:    void fclosesd_(intf *lu, _fcd name, intf *lname, intf *rtv) 
 *        lu (input): logical unit for the hdf file 
 *        name (input): the name of the SDS data object.
 *        lname: string length of name
 *        rtv: Return value: 
 *        0: all right 
 *        -1: illegal logical unit 
 *        -2: no such sds opened 
 *              
 * COMMENTS: Called by FORTRAN function nfclosesd   
 * 
 */ 
         
void fclosesd_(intf *lu, _fcd name, intf *lname, intf *rtv) 
{
    char *nm; 
     
    nm = (char *)HDf2cstring(name, *lname); 
     
    *rtv = hdf_close_SDS(*lu, nm); 
    HDfreespace(nm); 
    return;  
} 
                  
         
/* 
 * NAME:    hdf_close_file 
 * 
 * PURPOSE:    to end access to a hdf file, and release all allocated memory. FORTRAN-C interface 
 *             
 * 
 * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
 * 
 * USAGE:    void fclosef_(intf *lu, intf *rtv)
 *        lu: logical unit for the hdf file 
 *        Return value: 
 *        0: all right 
 *        -1: illegal logical unit 
 *              
 * COMMENTS:  
 * 
 */ 
         
void fclosef_(intf *lu, intf *rtv) 
{ 
    *rtv = hdf_close_file(*lu);
    return; 
} 

