C
C Fortran Interface
C
C  

C/* NAME:    nfopen
C *
C * PURPOSE:    to open a nscat HDF file
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function nfopen(filename, nrecords);
C *        character *(*)filename: name of the nscat HDF file.
C *        integer nrecords: number of buffer records, 0 or possitive number 
C *        Return values:
C *        -1: file not found
C *        -2: not HDF file
C *        -3: can not start SD interface;
C *        -4: not NSCAT HDF level 1.7, 2, or 3 file.
C *        -5: out of logical unit.
C *        -6: out of memory
C *        0 and positive number: file logical unit assigned to file.
C *             
C * COMMENTS: This HDF shell can be used in any HDF files. If the shell is intended to be used in general case,
C *           we need to delete the part of NSCAT checking code in this open routine.
C *
C *           Mutiple HDF files (upto 100, depending on HDF library and machine) can be opened and accessed 
C *           simutenously 
C *
C */
        integer function nfopen(filename, nrecords)
        
        character*(*) filename
        integer nrecords, ret
        
        call fhdfopen(filename, nrecords, len(filename), ret)
        nfopen = ret
        return
        end

C/*
C * NAME:    nfquery
C *
C * PURPOSE:    to get information about a named SDS (array dataset)
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function nfquery(lu, name, rank, dimsizes, data_type, bytespixel,bytesline)
C *        integer lu (input): logical unit for the hdf file obtained by calling hdf_open_file.
C *        character *(*)name (input): the name of the scientific data set to be accessed.
C *        integer rank (output): the number of dimensions for the SDS
C *        integer dimsizes(*) (output): a one dimension array which holds the size of each dimension for the named SDS. 
C *                            dimsizes(1) represents the dimension of the SDS which varies fastest.
C *        integer data_type (output): the data type for the SDS. Data types, which are the same as defined in hdf.h, 
C *                            are defined in hdfshell.h.
C *        integer bytespixel: numebr of bytes per cell.
C *        integer bytesline: number of bytes per line (record) 
C *        Return values:
C *        -1: illegal logical unit
C *        -2: no such sds
C *        -3: record out of the range (EOF);
C *        -4: HDF read error.
C *        -5: out of logical unit.
C *        -6: out of memory
C *        0 : all right.
C *             
C * COMMENTS: 
C *
C */

        integer function nfquery(lu, name, rank, dimsizes, data_type,
     1                           bytespixel, bytesline)
        character*(*) name
        integer lu, rank, data_type, bytespixel, bytesline, ret        
        integer dimsizes(*)
        
        call fquery(lu, name, rank, dimsizes, data_type, bytespixel, 
     1             bytesline, len(name), ret)
        nfquery=ret
        return
        end
    
C/*
C * NAME:    nfgetpar
C *
C * PURPOSE:    to get one line (record) from a named SDS each time. Buffered access.
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function getparam(lu, name, line, buffer)
C *        integer lu (input): logical unit for the hdf file
C *        character *(*) name (input): the name of the data object to be accessed.
C *        integer line (input): the line (record) number in the data object to be retrieved. 0 based.
C *        integer buffer(*) (output): memory supplied by the calling function which should be large enough to hold the contents of
C *                whole record. This buf could be any data type
C *        Return values:
C *        -1: illegal logical unit
C *        -2: no such sds
C *        -3: record out of the range (EOF);
C *        -4: HDF read error.
C *        -5: out of logical unit.
C *        -6: out of memory
C *        0 and positive number: number of bytes retrieved.
C *             
C * COMMENTS: If users set the buffer size in hdf_open_file, and there are no memory availabe, this routine will
C *           automatically swith to unbufferred read.
C *
C */
        integer function nfgetpar(lu, name, line, buf)
        character *(*)name
        integer lu, line, ret
        integer buf(*)
        
        call fgetpara(lu, name, line, buf, len(name), ret)
        nfgetpar = ret
        return
        end
                
C/* 
C * NAME:    nfgetvd 
C * 
C * PURPOSE:    to get vdata records from the HDF file, or obtain information about a vdata. 
C *             
C * 
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov 
C * 
C * USAGE: integer function nfgetvd(lu, name, fieldnames, start, nrcds, buffer) 
C *        integer lu (input): logical unit for the hdf file 
C *        character*(*) name (input): the name of the vdata data object to be accessed.
C *        character*(*) fieldnames(in/out): Input with comma delimited fields to be retrieved. The data in the buffer
C *                  will be in the same sequence as the fields listed in fieldnames. If this argument is 
C *                  set to ' ', all fields in a vdata will be retrieved. If start is set to -1, and this 
C *                  argument is not set to ' ', the name of all fields in the table (vdata) will be 
C *                  retrieved. For this case, make sure that the fieldnames is larger enough to hold the 
C *                  field names.
C *
C *        integer start (in/out): input with the start record in a vdata to be retrieved. 0 based.
C *                  If the input value of start is set to -1 or the function fails (-3), 
C *                  return with number of bytes per record corresponding to fields defined in the fieldnames.
C *                  Therefore, if input value of start is set to -1, the number of total bytes per record
C *                  in the vdata will be retrieved. To get the number of bytes for individual fields, set the
C *                  fieldnames to the individual fields, and set start to a negetive number other than -1.
C *		   integer nrcds (in/out): the number of records to be retrieved.
C *                  If the input value of start is set to -1 or the function fails (-3), 
C *                  return with the number of records in the table. 
C *        integer buffer(*) (out): memory supplied by the calling function which should be large enough to hold 
C *                  the contents of whole records. 
C *        Return values: 
C *        -1: illegal logical unit 
C *        -2: no such vdata 
C *        -3: record out of the range (the function will return the number of total records 
C *            in vdata to nrcds, and the number of bytes per record to start) 
C *        -4: HDF read error. 
C *        -5: attach failed(vdata NAME ERROR). 
C *        -6: fields to be retrieved are not found in the vdata  
C *        0 and positive number: number of bytes retrieved. If start is set to -1 and everything is 
C *                               right, it will return 0.
C *              
C * COMMENTS: If users set the buffer size in hdf_open_file, and there are no memory availabe, this routine will 
C *           automatically swith to unbufferred read. 
C * 
C */ 
        integer function nfgetvd(lu, name, fldnms, start, nrcds, buf)
        character*(*) name, fldnms
        integer lu, start, nrcds, ret
        integer buf(*)
       
        call fcgvd(lu, name, len(name), fldnms, len(fldnms), start, 
     1              nrcds, buf, ret)
        
        nfgetvd = ret
        return
        end
C/*
C * NAME:    nfsubset
C *
C * PURPOSE:    to get a subset from a named parameter (SDS)
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function nfsubset(lu, name, start, stride, edge, buffer)
C *        lu (input): logical unit for the hdf file
C *        name (input): the name of the data object to be accessed.
C *        integer start(*) (input): array specifying the starting location for each dimision. The value is 0 based
C *        integer stride(*) (input): array specifying the subsampling along each dimension. If a stride value is specified for a dimension,
C *               that many values will be skipped over when reading along that dimension. Specifying each element to be 
C *               one will reads contiguous data. No matter what stride value is provded, data is always placed, 
C *               data is always placed contiguously in buffer.
C *        integer edge(*) (output): array specifying the number of values to read along each dimension.
C *        buffer(*) (output): memory supplied by the calling function which should be large enough to 
C *                            hold the data. Could be any data type
C *        Return value:
C *        -1: illegal logical unit
C *        -2: no such sds
C *        -3: record out of the range (EOF);
C *        -4: HDF read error.
C *        -5: out of logical unit.
C *        -6: out of memory
C *        0 and positive number: number of bytes retrieved.
C *             
C * COMMENTS: 
C *
C */

        integer function nfsubset(lu, name, start, stride, edge, buf)
        character *(*) name
        integer lu, ret
        integer start(*), stride(*), edge(*), buf(*)
        
        call fsubset(lu, name, start, stride, edge, buf, len(name), ret)
        nfsubset = ret
        return
        end
                

C/*
C * NAME:    nfattr
C *
C * PURPOSE:    to get attributes attached to the file (file header) or attached to individual SDS (parameter array)
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function nfattr(lu, name, buffer)
C *        integer lu (input): logical unit for the hdf file
C *        character *(*)name (input): the name of the data object (parameter). If this is NULL, retrieve the global attribute,
C *                      otherwise, local attributes (attributes attached to the named SDS).
C *        character *(*) buffer (output): buffer to hold the attributes. The memory should be allocated by the calling function,
C *                        and should be large enough to hold all attributes.
C *                        Attributes will be returned in ASCII in the parameter_value_language (PVL) form
C *                        (e.g. Paramter_name=paramter_value). The delimitor between parameters is newline (\n).
C *        Return value:
C *        -1: illegal logical unit
C *        -2: no such sds
C *        -3: record out of the range (EOF);
C *        -4: HDF read error.
C *        -5: out of logical unit.
C *        -6: out of memory
C *        0 and positive number: number of attributes actually retrieved.
C *             
C * COMMENTS: This is header reader routine.
C *
C */
        integer function nfattr(lu, name, buf)
        character *(*) name, buf
        integer lu, ret
        
        call fattr(lu, name, buf, len(name), len(buf), ret)
        nfattr = ret
        return
        end
        
C/*
C * NAME:    nfnames
C *
C * PURPOSE:    to get names for all datasets (SDSs) in the HDF file
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function nfnames(lu, buffer)
C *        integer lu (input): logical unit for the hdf file
C *        character *(*)buffer (output): buffer to hold names for datasets (SDSs) in a HDF file. The memory should 
C *                      be allocated by the calling function and should be large enough to hold all names 
C *                      together. The delimitor between names is newline (\n).
C *        Return value:
C *        -1: illegal logical unit
C *        -2: no such sds
C *        -3: record out of the range (EOF);
C *        -4: HDF read error.
C *        -5: out of logical unit.
C *        -6: out of memory
C *        0 and positive number: number of dataset names in the buffer.
C *             
C * COMMENTS: 
C *
C */
        integer function nfnames(lu, buf)
        character *(*) buf
        integer lu, ret
        
        call fnames(lu, buf, len(buf), ret)
        nfnames = ret
        return
        end
        
C/*
C * NAME:    nfclosesd
C *
C * PURPOSE:    to end access a hdf SDS for memory release
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer function nfclosed (lu, name)
C *        integer lu (input): logical unit for the hdf file
C *        character *(*) name (input): the name of the SDS data object.
C *        Return value:
C *        0: all right
C *        -1: illegal logical unit
C *        -2: no such sds opened
C *             
C * COMMENTS: If the user will no longer accesses the named SDS but will access other data objects in the same hdf,
C *           s/he should call this function to release memory. If s/he fininshes access to the file, hdf_close_file 
C *           should be called.   
C *
C */
        integer function nfclosesd(lu, name)
        character *(*) name
        integer lu, ret
        
        call fclosesd(lu, name, len(name), ret)
        nfclosesd = ret
        return
        end        

C/*
C * NAME: nfclose
C *
C * PURPOSE:    to end access to a hdf file, and release all allocated memory
C *            
C *
C * AUTHOR:    Dr. Liping Di, Principal Scientist, Hughes STX, 301-441-4104, lpd@ulabsgi.gsfc.nasa.gov
C *
C * USAGE:    integer nfclose (lu)
C *        integer lu (input): logical unit for the hdf file
C *        Return value:
C *        0: all right
C *        -1: illegal logical unit
C *             
C * COMMENTS: 
C *
C */
        
        integer function nfclose(lu)
        integer lu, ret
        
        call fclosef(lu, ret)
        nfclose=ret
        return
        end
