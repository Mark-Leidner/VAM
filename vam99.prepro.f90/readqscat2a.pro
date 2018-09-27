;
; Filename:  readqscat2a.pro
;
; Usage:
;
;   To run this program, use the following command:
;
;   IDL> readqscat2a
;
; Description:
;  
;   This file contains one (1) IDL procedure and four (4) functions
;   used to read the QuikSCAT Level 2A data in Hierarchical Data 
;   Format (HDF).  The routines are as follows.
;   
;   1. correct_uint8: a function to correct variables 
;                     stored as 8-bit unsigned integers.
;
;   2. correct_uint16: a function to correct variables 
;                      stored as 16-bit unsigned integers.
;
;   3. get_vdata: a function to read the timetags which 
;                 are stored as VDATA.
;
;   4. get_sds: a function to read the data which are 
;               stored as Scientific Data Sets.
;
;   5. readqscat2a: the main procedure. Calls get_vdata and 
;                    get_sds.
;
; Notes:
;
; 1. The directory on your local system which contains the QuikSCAT
;    L2A data must be input in program readqscat2a.pro.  (Search on
;    "data_dir" to find this line quickly.)
;
; 2. Due to the size of the SDSs contained in these files, the L2A 
;    data are read in slabs.  If you would like to read larger slabs,
;    change variable "slab_size".  Setting slab_size to 1702 will allow
;    *most* L2A SDSs to be read in their entirety.
;
; 3. The correction of Unsigned Integers is currently hard-coded for
;    versions of IDL prior to 5.2.
;
; 4. Please send all comments and questions concerning these routines
;    to qscat@podaac.jpl.nasa.gov.
;
;
; 4/18/99 K.L. Perry, C.S. Hsu, R.S. Dunbar
;
;==================================================================
;   Copyright (c) 1999, California Institute of Technology
;==================================================================
;
; Modified by Mark Cerniglia, AER, Inc  11/99
;
; $Id: readqscat2a.pro,v 1.2 1999/12/22 16:46:56 mcc Exp $
; $Log: readqscat2a.pro,v $
; Revision 1.2  1999/12/22 16:46:56  mcc
; added documentation and command line input of level 2a filename.
;

;==================================================================
; correct_uint8: a function to correct data which are stored
;                as 8-bit unsigned integers for versions of
;                IDL prior to 5.2
;==================================================================
function correct_uint8,sds_data
    w=where(sds_data lt 0,cnt)
    if (cnt gt 0) then begin
      sds_data(w)=sds_data(w)+256
    endif
return,sds_data
end

;==================================================================
; correct_uint16: a function to correct data which are stored
;                 as 16-bit unsigned integers for versions of
;                 IDL prior to 5.2
;==================================================================
function correct_uint16,sds_data
    w=where(sds_data lt 0,cnt)
    if (cnt gt 0) then begin
      sds_data(w)=sds_data(w)+65536.
      w2=where(sds_data lt 0,cnt2)
    endif
return,sds_data
end

;==================================================================
; get_vdata: a function to read the timetags which are stored as
;            VDATA.
;==================================================================
function get_vdata,fid,vdata_name
  result=HDF_VD_FIND(fid,vdata_name)
  vid=HDF_VD_ATTACH(fid,result)
  result=HDF_VD_READ(vid,vdata)
  HDF_VD_DETACH,vid
return,vdata
end

;==================================================================
; get_sds: a function to read the data which are stored
;          as Scientific Data Sets.  This function also
;          multiplies the data by the calibration and 
;          subtracts the offset.
;==================================================================
function get_sds,sd_id,sds_name,slab_start,slab_size

    index=HDF_SD_NAMETOINDEX(sd_id,sds_name)
    sds_id=HDF_SD_SELECT(sd_id,index)
    HDF_SD_GETINFO,sds_id,ndims=ndims,dims=dims,caldata=cal,type=data_type

; make sure edge, start and stride have correct array sizes
    edge=intarr(ndims)
    start=intarr(ndims)
    stride=intarr(ndims)

    for i=0,ndims-1 do begin
      edge(i)=dims(i)
      start(i)=0
      stride(i)=1
    endfor
    edge(ndims-1)=slab_size
    start(ndims-1)=slab_start

    HDF_SD_GETDATA,sds_id,data,stride=stride,start=start,count=edge

;Correct Unsigned Integers
;;  note: Versions of IDL prior to 5.2 do not handle unsigned 
;;        integers properly.  These versions also do not identify 
;;        DFNT_UINT's using HDF_SD_GETINFO.  Therefore, the 
;;        following hard code will be included until IDL 5.2 is 
;;        "standard". --KLP.
;;

;; UINT8
    if (sds_name eq "num_sigma0_per_cell") then begin
      data=correct_uint8(float(data))
    endif

;; UINT16
    if ((sds_name eq "cell_lon") or $
        (sds_name eq "cell_azimuth") or $
        (sds_name eq "kp_beta") or $
        (sds_name eq "sigma0_qual_flag") or $
        (sds_name eq "sigma0_mode_flag") or $
        (sds_name eq "surface_flag")) then begin
      data=correct_uint16(float(data))
    endif

; Apply the scale and offset
    rdata = data*cal.cal - cal.offset
    HDF_SD_ENDACCESS,sds_id

return,rdata
end

;==================================================================
; readqscat2a: the main procedure in this file.  Calls are made 
;               to get_vdata to read the timetags and get_sds to 
;               read the data.  The results are then printed to 
;               the screen.
;==================================================================

pro readqscat2a,filename

;Select a Level 2A file
;***** Change data_dir to suit your local system
;  data_dir="/quikscat/l2a/"
;  filename=pickfile(/READ,path=data_dir,filter='QS_S2A*', $
;                    title='QuikSCAT L2A')

;;;;;;;;;;;;;;;;;;;

 filename=strcompress(filename,/remove_all)

  print,' '
  print,'FILENAME:  ',filename
  print,' '

openw,10,'qscat2a.out_idl'

  printf,10,' '
  printf,10,'FILENAME: '
  printf,10,filename
  printf,10,' '


;;;;;;;;;;;;;;;;;;;;

;Read the Time Tags contained in the VDATA
  fid=HDF_OPEN(filename,/READ)
  wvc_row_time= get_vdata(fid,'wvc_row_time')
  HDF_CLOSE,fid

;Select wind vector cell rows to be read
read,'Enter the first and last wvc row rec numbers to be read [1-1702]: ',irec1,irec2
print
print,irec1,irec2
print

printf,10,' '
printf,10,irec1,irec2
printf,10,' '

  if (irec1 gt irec2) then begin
    itmp=irec1
    irec1=irec2
    irec2=itmp
  endif
  if ((irec1 lt 1) or (irec2 gt 1702)) then begin
    print,'ERROR: wvc rows must be between 1 and 1702'
    stop
  endif

;Read the Scientific Data Sets
  sd_id=HDF_SD_START(filename,/READ)

;; The L2A SDSs are read in slabs of size 1.  An example of
;; reading the SDSs in their entirety is shown below.
;;
;;  index=HDF_SD_NAMETOINDEX(sd_id,'row_number')
;;  sds_id=HDF_SD_SELECT(sd_id,index)
;;  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
;;                  type=data_type,caldata=cal
;;  slab_size=strtrim(dims(0),2)   
;;  row_number=get_sds(sd_id,'row_number',ir,slab_size)
;;  for ir=irec1-1,irec2-1 do begin
;;    print,'TIME: ',string(wvc_row_time(*,ir))
;;    print,'WVC Row Number: ',row_number(ir)
;;  endfor

;  slab_size=1

; Subract 1 from irec1 and irec2 to adjust for IDL running 
; from 0 instead of 1 

;  for ir=irec1-1,irec2-1 do begin

  ir = 0

;    row_number= get_sds(sd_id,'row_number',ir,slab_size)
  index=HDF_SD_NAMETOINDEX(sd_id,'row_number')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                  type=data_type,caldata=cal
  slab_size=strtrim(dims(0),2)   
  row_number=get_sds(sd_id,'row_number',ir,slab_size)

;    num_sigma0= get_sds(sd_id,'num_sigma0',ir,slab_size)
  index=HDF_SD_NAMETOINDEX(sd_id,'num_sigma0')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                  type=data_type,caldata=cal
  slab_size=strtrim(dims(0),2)   
  num_sigma0=get_sds(sd_id,'num_sigma0',ir,slab_size)

;    num_sigma0_per_cell=get_sds(sd_id,'num_sigma0_per_cell',ir,slab_size)
  index=HDF_SD_NAMETOINDEX(sd_id,'num_sigma0_per_cell')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                  type=data_type,caldata=cal
  slab_size=strtrim(dims(1),2)   
  num_sigma0_per_cell=get_sds(sd_id,'num_sigma0_per_cell',ir,slab_size)

;    cell_lat= get_sds(sd_id,'cell_lat',ir,slab_size)
  index=HDF_SD_NAMETOINDEX(sd_id,'cell_lat')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                  type=data_type,caldata=cal
  slab_size=strtrim(dims(1),2)   
  cell_lat=get_sds(sd_id,'cell_lat',ir,slab_size)

; the rest use same slab_size ??
    cell_lon= get_sds(sd_id,'cell_lon',ir,slab_size)
    cell_azimuth= get_sds(sd_id,'cell_azimuth',ir,slab_size)
    cell_incidence= get_sds(sd_id,'cell_incidence',ir,slab_size)
    sigma0= get_sds(sd_id,'sigma0',ir,slab_size)
    sigma0_attn_amsr= get_sds(sd_id,'sigma0_attn_amsr',ir,slab_size)
    sigma0_attn_map= get_sds(sd_id,'sigma0_attn_map',ir,slab_size)
    kp_alpha= get_sds(sd_id,'kp_alpha',ir,slab_size)
    kp_beta= get_sds(sd_id,'kp_beta',ir,slab_size)
    kp_gamma= get_sds(sd_id,'kp_gamma',ir,slab_size)
    sigma0_qual_flag= get_sds(sd_id,'sigma0_qual_flag',ir,slab_size)
    sigma0_mode_flag= get_sds(sd_id,'sigma0_mode_flag',ir,slab_size)
    surface_flag= get_sds(sd_id,'surface_flag',ir,slab_size)
    cell_index= get_sds(sd_id,'cell_index',ir,slab_size)

; Print results to screen

  for ir=irec1-1,irec2-1 do begin
    printf,10,' '
    printf,10,'TIME: ',string(wvc_row_time(*,ir))
    printf,10,'WVC Row Number: ',row_number(ir)
    printf,10,'Number of Sigma0: ',num_sigma0(ir)
    printf,10,'Number of Sigma0 Per Cell:'
    printf,10,num_sigma0_per_cell(*,ir),format='(3(20i3/),1(16i3/))'

    if (num_sigma0(ir) gt 0) then begin
    printf,10,' WVC   Lat   Lon    Azi  IncAng Sigma0  Atten  Kp_alpha   Kp_beta   Kp_gamma  Qual  Mode Surf'

      for j=0,num_sigma0(ir)-1 do begin
        printf,10,cell_index(j,ir),cell_lat(j,ir),cell_lon(j,ir),cell_azimuth(j,ir), $
        cell_incidence(j,ir),sigma0(j,ir),sigma0_attn_map(j,ir),kp_alpha(j,ir), $
        kp_beta(j,ir),kp_gamma(j,ir),sigma0_qual_flag(j,ir),sigma0_mode_flag(j,ir), $
        surface_flag(j,ir), $
        format='(i4,5(1x,f6.2),1x,f6.3,1x,f9.6,2(1x,e10.4),x,i3,1x,i6,1x,i4)'
; 110    format(f4.0,x,5(f6.2,x),f6.3,x,f9.6,2x,2(e10.4,2x),f3.0,x,f6.0,x,f5.0)
      endfor

    endif
  endfor

  HDF_SD_END,sd_id
  close,10

end
;
