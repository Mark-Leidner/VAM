/* This is an example of how the HDF shell library can be used to access NSCAT HDF file.
 * This program also can be used to test if the library is correct. This test program only works for
 * the NSCAT level 1.7 HDF file. 
 *
 * Author: Dr. Liping Di, Principal Scientist, Hughes Information Technology, 301-441-4104 lpd@ulabsgi.gsfc.nasa.gov
 *
 */
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h>
#include "hdfshell.h"

void maxmin_data(void *, long, long, short);
char *hdftype(long); 
 
void main() 
 
{    int lu, ret, i, j, k; 
     char *ch, ch1, buf[32000], temp[80], temp1[80]; 
     long sum; 
     long rank, dimsizes[20], data_type, bytes_per_pixel, bytes_per_line; 
     double *data;
     long start[20];
     short pass;
     char *sdsname[100]; 
   
     printf("Enter the name of the HDF file: "); 
     scanf("%s", temp); 
 
     /* opening file with a buffer*/ 
     lu=hdf_open_file(temp, 10); 
     if(lu < 0){ 
         printf("Can not open the HDF file %s, err=%d\n", temp, lu); 
         exit(1); 
     } 
     
     /* test getting global attributes (header information) */ 
     if((ret = hdf_get_attributes(lu, NULL, buf)) < 0){ 
         printf("Fail to get the file header, err=%d\n", ret); 
         exit(1); 
     } 
     else {
        printf("Global attributes:\n");
        for(i=0; i< ret; i++){
            if(i== 0)
        	    ch=strtok(buf, "\n");
            else
        	    ch=strtok(NULL, "\n");
            printf("%s\n",ch);
            if(i%20 == 0 && i != (ret-1) && i != 0){
                fflush(stdin);
                printf("\nReturn for more global attribute...");
		        if(getchar()== 'q')exit(0);
		    }
		}
	}
    if(ret == 0)printf("There is no global attribute in this file\n");
    printf("\nReturn for SDS information...");
    fflush(stdin);
    getchar();
 
    /* test getting scientific data set (SDS) names in the file */
    printf("\nScientific Data Sets:\n");         
    if((ret = hdf_get_dataset_names(lu, buf))< 0) { 
        printf("Fail to get the dataset names, err=%d\n", ret); 
        exit(1); 
    }
    else if (ret == 0){
    	printf("\nThere is no scientific data set in the file\n");
    	exit(1);
    } 
    else { 
        for(i=0; i< ret; i++){
            if(i== 0)
        	    ch=strtok(buf, "\n");
            else
        	    ch=strtok(NULL, "\n");
        	sdsname[i]=ch;
        }
        j=-1;
        for(i=0; i< ret; i++){
            printf("%d). %s\n", i, sdsname[i]);
            if((i+1)%20 == 0 && i != (ret-1)){
                fflush(stdin);
                printf("\nReturn for more SDS name..., enter a number for selecting a SDS ");
                for(k=0; (k < 80) && ((ch1 = getchar()) != EOF) && (ch1 != '\n'); k ++)
                     temp1[k]= ch1;
                temp1[k] = '\0';
                if(strlen(temp1) != 0){
                	j=atoi(temp1);
                	if(j >= 0 && j < ret)break;
                }
		    }
		}
		if(j == -1){ 
		    fflush(stdin);
			printf("\nPlease enter a number for selecting a SDS ");
			scanf("%s", temp1);
			j=atoi(temp1);
		}
	}
    
    strcpy(temp1, sdsname[j]);
    
    /* testing getting information for a specific SDS */ 
    if((ret = hdf_query_SDS(lu, temp1, &rank, dimsizes, &data_type, &bytes_per_pixel, &bytes_per_line)) < 0){ 
        printf("Fail to get the dataset info, err=%d\n", ret); 
        exit(1); 
    } 
    else {  
        printf("\nInfo for %s:\nrank=%d, ", temp1, rank);
        for(i=0; i< rank; i++)
        	printf("dim %d=%d, ", i, dimsizes[i]);
        printf("\ndata_type=%s\nbytes_per_pixel=%d, bytes_per_line=%d\n",hdftype(data_type), bytes_per_pixel, bytes_per_line); 
    }
     
    /* get local attributes for the selected SDS */ 
    ret=hdf_get_attributes(lu, temp1, buf); 
    if(ret < 0){ 
        printf("Err geting local attribute for %s, err=%d\n", temp1, ret); 
        exit(1); 
    } 
    else  
        printf("\nLocal attributes for %s:\n%s\n", temp1, buf); 
         
    /* calculate mean and maximum, minmum by accessing data line-by-line  */ 
    data = (double *)malloc((bytes_per_line/8 + 1)*8);
    printf("Sequential reading:"); 
    for(i=0; i<dimsizes[0]; i++){ 
        ret=hdf_getparam(lu, temp1, i, data); 
        if(ret != bytes_per_line){ 
            printf("Err reading data sequentially at line %d\n", i); 
            exit(1); 
        } 
        
        pass = 3;
        if(i == 0 ) pass = 2;
        if(i == dimsizes[0]-1)pass=4;
        if(dimsizes[0] == 1)pass=1;
        maxmin_data(data, data_type, bytes_per_line/bytes_per_pixel, pass);
    } 
     
    printf("\n"); 
     
    /* inverse reading from last line to first line, it should be slow */ 
    printf("Inverse reading: ");
    for(i=dimsizes[0]-1; i >= 0; i--){ 
        ret=hdf_getparam(lu, temp1, i, data); 
        if(ret != bytes_per_line){ 
            printf("Err reading data inversely at line %d, err=%d\n", i, ret); 
            exit(1); 
        } 
         
        pass = 3;
        if(i == dimsizes[0] - 1 ) pass = 2;
        if(i == 0)pass=4;
        if(dimsizes[0] == 1)pass=1;
        maxmin_data(data, data_type, bytes_per_line/bytes_per_pixel, pass); 
    } 
     
    printf("\n"); 
     
    /* block reading using subseting routine */
    free(data);
    data = (double *)malloc((dimsizes[0]*bytes_per_line/8 + 1)*8); 
	printf("Block reading: ");
	for(i=0; i< rank; i++)
	    start[i]=0;
    ret = hdf_subset_param(lu, temp1, start, NULL, dimsizes, data); 
    if(ret < 0) { 
        printf("Err reading block data, err=%d\n", ret); 
        exit(1); 
    } 

	maxmin_data(data, data_type, dimsizes[0]*bytes_per_line/bytes_per_pixel, 1);     
	printf("\n");     
	
	/* close a HDF file. this call will release all allocated memory.  */
    ret = hdf_close_file(lu); 
    if(ret < 0){ 
        printf("Err closing HDF, err=%d\n", ret); 
        exit(1); 
    } 
     
    /* unbuffered open for testing unbuffered access */ 
    lu=hdf_open_file(temp, 1); 
    if(lu < 0 ){ 
        printf("Can not open the file %s again, err=%d\n", temp, lu); 
        exit(1); 
    } 
     
    /* line-by-line unbuffered access */
    printf("Unbuffered reading: ");  
    for(i=0; i< dimsizes[0]; i++){ 
        ret=hdf_getparam(lu, temp1, i, data); 
        if(ret < bytes_per_line){ 
            printf("Err reading %s again at line %d, err=%d\n", temp1, i, ret); 
            exit(1); 
        } 
        pass = 3;
        if(i == 0) pass = 2;
        if(i ==  dimsizes[0] - 1 )pass=4;
        if(dimsizes[0] == 1)pass=1;
        maxmin_data(data, data_type, bytes_per_line/bytes_per_pixel, pass); 
    } 
     
    printf("\n");
    ret=hdf_close_file(lu); 
    if(ret < 0){ 
        printf("Fail to close file again"); 
    } 
     
} 

/* calculate the minimum and maximum value
 * This routine allows to pass multiple buffer
 * pass = 1, one buffer
 * pass = 2, the first buffer of a mutiple-buffer group
 * pass = 3, intermediate buffer of a mutiple-buffer group
 * pass = 4, the last buffer of a mutiple-buffer group
 */   
void maxmin_data(void *Map_Data, long type, long npixels, short pass )
{	
	char *map8;
	static char max8, min8;
	short *map16;
	static short max16, min16;
	unsigned char  *umap8;
	static unsigned char umax8, umin8;
	unsigned short *umap16;
	static unsigned short umax16, umin16;
	long *map32;
	static long max32, min32;
	unsigned long *umap32;
	static unsigned long umax32, umin32;
	float *fmap32;
	static float fmax32, fmin32;
	double *fmap64;
	static double fmax64, fmin64;
    long i;
    static double sum;

	switch ( type ){
	
	case DFNT_INT8:
		map8 = (char *)Map_Data;
		/* initial the max and min value */
		if(pass == 1 || pass == 2){
             max8 = min8 = *map8;
	         sum=0;
		}
		for(i = 0; i < npixels; i++){
			if(map8[i] < min8) min8 = map8[i];
			if(map8[i] > max8) max8 = map8[i];
			sum = sum + map8[i];
		}
		
		if(pass == 1 || pass == 4)printf("Max=%d   Min=%d  Sum=%E", max8, min8, sum);
		break;
    
    case DFNT_UINT8:
		umap8 = (unsigned char *)Map_Data;
		/* initial the max and min value */
        if(pass == 1 || pass == 2){
		    umax8 = umin8 = *umap8;
		    sum=0;
		}
		for(i = 0; i < npixels; i++){
			if(umap8[i] < umin8) umin8 = umap8[i];
			if(umap8[i] > umax8) umax8 = umap8[i];
			sum = sum + umap8[i];
		}
		
		if(pass == 1 || pass == 4)printf("Max=%u   Min=%u Sum=%E", umax8, umin8, sum);
		break;

	case DFNT_INT16:
		map16 = (short *)Map_Data;
		/* initial the max and min value */
		if(pass == 1 || pass == 2){
		    max16 = min16 = *map16;
		    sum = 0;
		}
		for(i = 0; i < npixels; i++){
			if(map16[i] < min16) min16 = map16[i];
			if(map16[i] > max16) max16 = map16[i];
			sum = sum + map16[i];
		}
		
		if(pass == 1 || pass == 4)printf("Max=%d   Min=%d Sum=%E", max16, min16, sum);
		break;

	case DFNT_UINT16:
		umap16 = (unsigned short *)Map_Data;
		if(pass == 1 || pass == 2){
		    umax16 = umin16 = *umap16;
		    sum = 0;
		}
		for(i = 0; i < npixels; i++){
			if(umap16[i] < umin16) umin16 = umap16[i];
			if(umap16[i] > umax16) umax16 = umap16[i];
			sum = sum + umap16[i];
		}
		if(pass == 1 || pass == 4)printf("Max=%u   Min=%u Sum=%E", umax16, umin16, sum);
		break;
	
	case DFNT_INT32:
		map32 = (long *)Map_Data;
		if(pass == 1 || pass == 2){
		    max32 = min32 = *map32;
		    sum = 0;
		}
		for(i = 0; i < npixels; i++){
			if(map32[i] < min32) min32 = map32[i];
			if(map32[i] > max32) max32 = map32[i];
			sum = sum + map32[i];
		}
		if(pass == 1 || pass == 4)printf("Max=%d   Min=%d Sum=%E", max32, min32, sum);
		break;
	
	case DFNT_UINT32:
		umap32 = (unsigned long *)Map_Data;
		if(pass == 1 || pass == 2){
		    umax32 = umin32 = *umap32;
		    sum=0;
		}
		for(i = 0; i < npixels; i++){
			if(umap32[i] < umin32) umin32 = umap32[i];
			if(umap32[i] > umax32) umax32 = umap32[i];
			sum = sum + umap32[i];
		}
		if(pass == 1 || pass == 4)printf("Max=%u   Min=%u Sum=%E", umax32, umin32, sum);
		break;

    case DFNT_FLOAT32:
		fmap32 = (float *)Map_Data;
		if(pass == 1 || pass == 2){
		    fmax32 = fmin32 = *fmap32;
		    sum = 0;
		}
		for(i = 0; i < npixels; i++){
			if(fmap32[i] < fmin32) fmin32 = fmap32[i];
			if(fmap32[i] > fmax32) fmax32 = fmap32[i];
			sum = sum + fmap32[i];
		}
		if(pass == 1 || pass == 4)printf("Max=%f   Min=%f Sum=%E", fmax32, fmin32, sum);
		break;
		
    case DFNT_FLOAT64:
		fmap64 = (double *)Map_Data;
		if(pass == 1 || pass == 2){
		    fmax64 = fmin64 = *fmap64;
		    sum = 0;
		}
		for(i = 0; i < npixels; i++){
			if(fmap64[i] < fmin64) fmin64 = fmap64[i];
			if(fmap64[i] > fmax64) fmax64 = fmap64[i];
			sum = sum + fmap64[i];
		}
		if(pass == 1 || pass == 4)printf("Max=%E   Min=%E SUM=%E", fmax64, fmin64, sum);
		break;
		
	default:
		printf("unknown data type");
	}

}

char *hdftype(long num_type)
{	char *type;
	type = " ";
	switch (num_type) {
	case 5:	/* float32 type. */
		type = "FLOAT32";
		break;
	case 6:	/* float64 type. */
		type = "FLOAT64";
		break;
	case 20:/* int8 type. */
		type = "INT8";
		break;
	case 21:/* uint8 type. */
		type = "UINT8";
		break;
	case 22:/* int16 type. */
		type = "INT16";
		break;
	case 23:/* uint16 type. */
		type = "UINT16";
		break;
	case 24:/* int32 type. */
		type = "INT32";
		break;
	case 25:/* uint32 type. */
		type = "UINT32";
		break;
	case DFNT_CHAR8:
		type = "CHAR8";
		break;
	case DFNT_UCHAR8:
		type = "UCHAR8";
		break;
	default: 
		type = "Unknown";
	}
	return(type);
}

