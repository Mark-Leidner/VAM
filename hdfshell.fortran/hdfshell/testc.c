#include <stdlib.h> 
#include <stdio.h> 

#include <hdf.h>
#include "hdfshell.h" 
 
void main() 
 
{    int 	lu, ret,i,j, vstart, nrcds; 
     char 	buf[32000], temp[80], fieldnames[100]; 
     long 	sum, meta[8]; 
     long 	rank, dimsizes[3], data_type, bytes_per_pixel, bytes_per_line; 
     short 	data[820][24], data1[24], max, min, begin[10]; 
     long 	start[2]={0,0}; 
     unsigned 	short data2[24], umax, umin;
     char 	*point; 
     /* vdata variables */
     int32	file_id;
     int32	vdata_id;
     int32	vdata_ref;
     int32	vdata_size;
     int32	interlace;
     int32	vdata_nrec;
     int32	n_rec;
     int16	indexbuf[820];
     char	vdata_name[30];
     char	timebuf[25];
     char	fields[30];
     char	time_tags[820][25];
     int	found;
   
     printf("Enter the name of the HDF file: "); 
     scanf("%s", temp); 
 
     /* test open file with buffer*/ 
     lu=hdf_open_file(temp, 10); 
     if(lu < 0){ 
         printf("Can not open the HDF file %s, err=%d\n", temp, lu); 
         exit(1); 
     } 
     
     /* test getting global attribute */ 
     if((ret = hdf_get_attributes(lu, NULL, buf)) < 0){ 
         printf("Fail to get the file header, err=%d\n", ret); 
         exit(1); 
     } 
     else 
        printf("Global attributes=\n%s", buf); 
 
    /* test getting SDS names */         
    if((ret = hdf_get_dataset_names(lu, buf))< 0) { 
        printf("Fail to get the dataset names, err=%d\n", ret); 
        exit(1); 
    } 
    else 
        printf("Data sets are:\n%s", buf); 
 
    /* testing getting information for a specific SDS, get info for WVC_Lat */ 
    if((ret = hdf_query_SDS(lu, "WVC_Lat", &rank, dimsizes, &data_type, &bytes_per_pixel, &bytes_per_line)) < 0){ 
        printf("Fail to get the dataset info, err=%d\n", ret); 
        exit(1); 
    } 
    else  
        printf("Info for WVC_Lat, rank=%d\ndim0=%d, dim1=%d\ndata_type=%d\n,bytes_per_pixel=%d, bytes_per_line=%d\n",rank, 
              dimsizes[0], dimsizes[1], data_type, bytes_per_pixel, bytes_per_line); 
     
    /* get local attribute for WVC_Lat */ 
    ret=hdf_get_attributes(lu, "WVC_Lat", buf); 
    if(ret < 0){ 
        printf("Err geting local attribute for WVC_Lat, err=%d\n", ret); 
        exit(1); 
    } 
    else  
        printf("Local attributes for WVC_Lat:\n%s\n", buf); 
    
    /* get information for the vdata SwathIndex and Swath Meta */

    vstart=-1;			/* set start = 1 to get the number of records and size of a record */
    vdata_ref = -1;
    found = 0;

    file_id = Hopen(temp, DFACC_RDONLY, 0);
    Vstart(file_id);

     while((vdata_ref = VSgetid(file_id, vdata_ref)) != -1) {
              vdata_id = VSattach(file_id, vdata_ref, "r");
              VSinquire(vdata_id, &n_rec, &interlace, fields,
                   &vdata_size, vdata_name);
              if(!strncmp(vdata_name, "SwathIndex",10)) {
                 found = 1;
                 break;
              }else VSdetach(vdata_id);
           }
   
           if (!found)
              printf("index vdata not found\n");
           else {
              ret = VSsetfields(vdata_id, fields);
              for (j=0; j<820; j++) {
                 vdata_nrec = 1;
                 ret = VSread(vdata_id, (uint8 *)indexbuf, vdata_nrec, interlace);
   /*
                 printf("%d\n", *indexbuf);
   */
              }
           }
 
              /* get the time tags corresponding to the index vdata */
   
            vdata_ref = -1;
            found = 0;
     
            while ((vdata_ref = VSgetid(file_id, vdata_ref)) != -1) {
               vdata_id = VSattach(file_id, vdata_ref, "r");
               VSinquire(vdata_id, &n_rec, &interlace, fields,
                        &vdata_size, vdata_name);
               if (!strncmp (vdata_name, "NSCAT L17", 9)) {
                  found = 1;
                  break;
               } else VSdetach(vdata_id);
            }
     
            if (!found)
               printf ("NSCAT L17 time tags where not found\n");
            else {
               ret = VSsetfields(vdata_id, fields);
               vdata_nrec = 1;
               for (i=0; i < n_rec; i++) {
                  ret = VSread(vdata_id, (uint8 *)timebuf, vdata_nrec, interlace);
        /*
                  strcpy(time_tags[i], timebuf);
                  printf("%.24s\n", time_tags[i]);
        */
               }
    
            }
 
    /* calculate mean and maximum, minmum, and mixed with vdata access */
    /* for access efficiency, it is best to retrieve multiply records within one hdf_getvdata call */ 
    max=-10000; 
    min=10000; 
    sum=0;
    point = (char *)meta; 
    for(i=0; i<dimsizes[0]; i++){ 
        ret=hdf_getparam(lu, "WVC_Lat", i, data1); 
        if(ret != bytes_per_line){ 
            printf("Err reading data sequentially at line %d\n", i); 
            exit(1); 
        }
        
        /* read the first ten records of vdata in SwathIndex and Swath Meta */
        for(j=0; j< 24; j++){ 
            if(data1[j] > max)max=data1[j]; 
            if(data1[j] < min)min=data1[j]; 
            sum = sum + data1[j]; 
        } 
    } 
     
    printf("Sequential reading: max=%d, min=%d, sum=%d\n", max, min, sum); 
     
    /* inverse reading */ 
    max=-10000; 
    min=10000; 
    sum=0; 
    for(i=dimsizes[0]-1; i >= 0; i--){ 
        ret=hdf_getparam(lu, "WVC_Lat", i, data1); 
        if(ret != bytes_per_line){ 
            printf("Err reading data inversely at line %d, err=%d\n", i, ret); 
            exit(1); 
        } 
         
        for(j=0; j< 24; j++){ 
            if(data1[j] > max) max=data1[j]; 
            if(data1[j] < min) min=data1[j]; 
            sum = sum + data1[j]; 
        } 
    } 
     
    printf("Inverse reading: max=%d, min=%d, sum=%d\n", max, min, sum); 
     
    /* block reading */ 
    ret = hdf_subset_param(lu, "WVC_Lat", start, NULL, dimsizes, data); 
    if(ret < 0) { 
        printf("Err reading block data, err=%d\n", ret); 
        exit(1); 
    } 
     
    max=-10000; 
    min=10000; 
    sum=0; 
    for(i=0; i< dimsizes[0]; i++) 
    for(j=0; j< 24; j++){ 
        if(max < data[i][j]) max=data[i][j]; 
        if(min > data[i][j]) min=data[i][j]; 
        sum=sum+data[i][j]; 
    } 
    printf("Block reading: max=%d, min=%d, sum=%d\n", max,min,sum); 
     
    /* close SDS for testing */ 
    ret=hdf_close_SDS(lu, "WVC_Lat"); 
    if(ret < 0){ 
        printf("Err hdf SDS closing, err=%d\n", ret); 
        exit(1); 
    } 
     
    /* get local attributes for WVC_Lon */ 
    ret = hdf_get_attributes(lu, "WVC_Lon", buf); 
    if(ret < 0){ 
        printf("Err reading the local attributes for WVC_Lon, err=%d", ret); 
        exit(1); 
    } 
    else 
        printf("Local attributes for WVC_Lon:\n%s\n", buf); 
         
    /* calculate maximum and minimu for WVC_Lon */ 
    sum=0; 
    umax=0; 
    umin=40000; 
    for(i=0; i< dimsizes[0]; i++){ 
        ret=hdf_getparam(lu, "WVC_Lon", i, data2); 
        if(ret < bytes_per_line){ 
            printf("Err reading WVC_Lon, err=%d\n", ret); 
            exit(1); 
        } 
        for(j=0; j<24; j++){ 
            if(umax < data2[j])umax=data2[j]; 
            if(umin > data2[j])umin=data2[j]; 
            sum=sum+data2[j]; 
        } 
    } 
    printf("Result for WVC_Lon, max=%u, min=%u, sum=%d\n", umax,umin,sum); 
         
    ret = hdf_close_file(lu); 
    if(ret < 0){ 
        printf("Err closing HDF, err=%d\n", ret); 
        exit(1); 
    } 
     
    /* unbuffered open */ 
    lu=hdf_open_file(temp, 1); 
    if(lu < 0 ){ 
        printf("Can not open the file %s again, err=%d\n", temp, lu); 
        exit(1); 
    } 
     
    max=-10000; 
    min=10000; 
    sum=0; 
     
    for(i=0; i< dimsizes[0]; i++){ 
        ret=hdf_getparam(lu, "WVC_Lat", i, data1); 
        if(ret < bytes_per_line){ 
            printf("Err reading WVC_Lat again at line %d, err=%d\n", i, ret); 
            exit(1); 
        } 
        for(j=0; j<24; j++){ 
            if(max < data1[j])max=data1[j]; 
            if(min > data1[j])min=data1[j]; 
            sum=sum+data1[j]; 
        } 
    } 
     
    printf("Result for unbuffer reading WVC_Lat: max=%d, min=%d, sum=%d\n", max,min,sum); 
    ret=hdf_close_file(lu); 
    if(ret < 0){ 
        printf("Fail to close file again"); 
    } 
     
} 

