         program testf

         integer lu, ret,i,j
         character*32000 buf
         character*80 temp
         integer*4 sum, rank, dimsizes(3), data_type, bytes_per_pixel
         integer*2 data(24,820), data1(24), max, min
         integer*4 start(2), stride(2), bytes_per_line
         integer*2 data2(24)
         integer*4 umax, umin, tv
         integer*4 vstart, nrcds
         character*80 field
         byte vdata(80)
        
         start(1)=0
         start(2)=0
         stride(1)=1
         stride(2)=1
         write(*,*)'Enter the name of the HDF file: '
         read(*,'(a)')temp

C        /* test open file with buffer*/
         lu=nfopen(temp, 10)
         if(lu .lt. 0)then
             write(*,10)temp, lu
10           format(1x, 'Can not open the HDF file ', a , 'err=', I4)
             stop
         endif
    
C        /* test getting global attribute */
         ret = nfattr(lu, ' ', buf)
         if(ret .lt. 0) then
             write(*, 15)ret
15           format(1x, 'Fail to get the file header, err=', I2, /)
             stop
         else
             do i=32000, 1, -1
               if(buf(i:i) .ne. ' ')goto 17
             end do 
17           write(*,20)buf(1:i)
20           format(/, 'Global attributes=', /, a)
         endif
        
C        /* test getting SDS names */
        ret = nfnames(lu, buf)        
        if(ret .lt. 0) then
            write(*,25)ret
25          format(/, 'Fail to get the dataset names, err=',I2)
            stop
        endif
        
        do i=32000, 1, -1
           if(buf(i:i) .ne. ' ')goto 27
        end do 
27      write(*,30)buf(1:i)
30      format(/, 'Data sets are:', /, A)
        

C       /* testing getting information for a specific SDS, get info for WVC_Lat */
        ret = nfquery(lu, 'WVC_Lat', rank, dimsizes, data_type, 
     1                    bytes_per_pixel, bytes_per_line)
        if(ret .lt. 0)then
            write(*,35)ret
35          format(/, 'Fail to get the dataset info, err=', I2)
            stop
        endif
        write(*,40)rank, dimsizes(2), dimsizes(1), data_type, 
     1             bytes_per_pixel, bytes_per_line 
40     format(/, 'Info for WVC_Lat, rank=', I2, /, 'ndim0=', I4, 'dim1='
     1  , I4, /, 'data_type=', I4, /, 'bytes_per_pixel=', I4, 
     1  'bytes_per_line=', I4)
    
C       /* get local attribute for WVC_Lat */
        ret=nfattr(lu, 'WVC_Lat', buf)
        if(ret .lt. 0)then
            write(*, 45)ret
45      format(/,'Err geting local attribute for WVC_Lat, err= ', I4)
            stop
        endif

        do i=32000, 1, -1
           if(buf(i:i) .ne. ' ')goto 47
        end do    
47      write(*, 50)buf(1:i)
50      format(/, 'Local attributes for WVC_Lat:', /, a)

C       /* test vdata function */
C
C       retrieve the information about SwathIndex

        vstart = -1
        write(*,888)vstart
888          format(/,'vstart=', I4)
        ret = nfgetvd(lu, "SwathIndex", field, vstart, nrcds, vdata)
        if(ret .ne. 0)then
        	write(*, 51)ret
51          format(/,'Err1 returning value, err=', I4)
            stop
        endif
        
        write(*, 52)nrcds, vstart, field
52      format(/, 'SwathIndex=', I4, ' rcds, each has', I3, ' bytes,', 
     1  ' fields=', a)
     
        vstart = -1
        write(*,889)vstart
889          format(/,'vstart2=', I4)
        ret = nfgetvd(lu, "NSCAT L17", field, vstart, nrcds, vdata)
        if(ret .ne. 0)then
        	write(*, 251)ret
251          format(/,'Err2 returning value, err=', I4)
            stop
        endif
        
        write(*, 252)nrcds, vstart, field
252      format(/, 'NSCAT L17=', I4, ' rcds, each has', I3, ' bytes,', 
     1  ' fields=', a)
      
C       /* retrieve time from NSCAT L17 vdata at record 200, and 201 */
        vstart = 200
        nrcds = 2
        ret=nfgetvd(lu, 'NSCAT L17', 'Mean_Time', vstart, nrcds, vdata)
        if(ret .lt.0)then
        	write(*, 51)ret
        	stop
        endif
        write(*, 53)(vdata(i), i=1, ret)
53      format(/, 'rcd 200 Mean_Time=',24A1,/,'rcd 201 Mean_Time=',24A1)          

C       /* calculate mean and maximum, minmum */
        max=-10000
        min=10000
        sum=0
        do i=1, dimsizes(2)
            ret=nfgetpar(lu, 'WVC_Lat', i-1, data1)
            if(ret .ne. bytes_per_line)then
                write(*,55)i
55      format(/, 'Err reading data sequentially at line',I4)
                stop
            endif
        
            do j=1, 24
                if(data1(j) .gt. max)max=data1(j)
                if(data1(j) .lt. min)min=data1(j)
                sum = sum + data1(j)
            enddo
        enddo
    
    
        write(*, 60)max,min,sum
60     format(/, 'Sequential reading: max=', I6,'min=', I6, 'sum=', I9)
    
C       /* inverse reading */
        max=-10000
        min=10000
        sum=0
        do i=dimsizes(2), 1, -1
            ret=nfgetpar(lu, 'WVC_Lat', i-1, data1)
            if(ret .ne. bytes_per_line)then
                write(*, 65)i,ret
65      format(/, 'Err reading data inversely at line',I4,'err=',I4)
                stop
            endif
        
            do j=1, 24
                if(data1(j) .gt. max) max=data1(j)
                if(data1(j) .lt. min) min=data1(j)
                sum = sum + data1(j)
            enddo
        enddo

        write(*, 70)max, min, sum
70      format(/, 'Inverse reading: max=', I6, ' min=', I6, 'sum=', I9)
    
C       /* block reading */
        ret = nfsubset(lu, 'WVC_Lat', start, stride, dimsizes, data)
        if(ret .lt. 0) then
            write(*, 75)ret
75          format(/, 'Err reading block data, err', I4)
            stop
        endif
    
        max=-10000
        min=10000
        sum=0
        do i=1, dimsizes(2)
            do j=1, 24
                if(max .lt. data(j,i)) max=data(j,i)
                if(min .gt. data(j,i)) min=data(j,i)
                sum=sum+data(j,i)
            enddo
        enddo
    
        write(*, 80)max, min,sum
80      format(/, 'Block reading: max=', I6, ' min=', I6, ' sum=', I9)

C        /* close SDS for testing */
        ret=nfclosesd(lu, 'WVC_Lat')
        if(ret .lt. 0)then
            write(*, 85)ret
85          format(/, 'Err hdf SDS closing, err=', I4)
            stop
        endif
    
    
C        /* get local attributes for WVC_Lon */
        ret = nfattr(lu, 'WVC_Lon', buf)
        if(ret .lt. 0)then
            write(*, 90)ret
90      format(/,'Err reading local attributes for WVC_Lon, err=',I4)
            stop
        endif
    
        do i=32000, 1, -1
           if(buf(i:i) .ne. ' ')goto 97
        end do
97      write(*,95)buf(1:i)
95      format(/, 'Local attributes for WVC_Lon:', /, a)
        
C       /* calculate maximum and minimu for WVC_Lon */
        sum=0
        umax=0
        umin=40000
        do i=1, dimsizes(2)
            ret=nfgetpar(lu, 'WVC_Lon', i-1, data2)
            if(ret .lt. bytes_per_line)then
                write(*,100)ret
100             format(/, 'Err reading WVC_Lon, err=', I4)
                stop
            endif
            
            do j=1,24
                tv=data2(j)
                if(tv .lt. 0) tv=tv+65536
                if(umax .lt. tv)umax=tv
                if(umin .gt. tv)umin=tv
                sum=sum+tv
            enddo
        enddo
    
        write(*,105)umax, umin, sum
105     format(/,'Result for WVC_Lon, max=',I6, 'min=',I6,'sum=',I9)
        
        i=fclosef(lu, ret)
        if(ret .lt. 0)then
            write(*,110)ret
110         format(/, 'Err closing HDF, err=',I4)
            stop
        endif
    
C    /* unbuffered open */
        lu=nfopen(temp, 1)
        if(lu .lt. 0 )then
            write(*,115)temp, lu
115         format(/,'Can not open the file ', a,'err=', I4)
            stop
        endif
    
    
        max=-10000
        min=10000
        sum=0
    
        do i=1, dimsizes(2)
            ret=nfgetpar(lu, 'WVC_Lat', i-1, data1)
            if(ret .lt. bytes_per_line)then
                write(*,120)i,ret
120     format(/,'Err reading WVC_Lat again at line',I4,' err=',I4)
                stop
            endif
        
            do j=1, 24
                if(max .lt. data1(j))max=data1(j)
                if(min .gt. data1(j))min=data1(j)
                sum=sum+data1(j)
            enddo
        enddo
    
        write(*,125)max,min,sum
125   format(/,'Unbuffer reading WVC_Lat: max=',I6,'min=',I6,'sum=', I9)
        i=nfclose(lu)
        stop
        end
