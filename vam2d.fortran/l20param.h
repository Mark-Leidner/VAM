c	$Id: l20param.h,v 1.1 1998/02/16 21:01:42 leidner Exp $
c	$Log: l20param.h,v $
c	Revision 1.1  1998/02/16 21:01:42  leidner
c	Initial revision
c	
c
        PARAMETER (
     $        LGRID = 1624, ! length of rev grid in 25 km intervals
     $        WGRID = 65,   ! width of rev grid in 25 km intervals
     $        RFACT = 1 )   ! resolution factor (1=25km,2=50km,etc.)
       integer bufl,bufw,bufd,ambigs,outw,mgdrsize
       parameter (
     $           bufl = 76/RFACT,     ! along-track dim. of grouping buffers
     $           bufw = 64/RFACT,     ! cross-track dim. of grouping buffers
     $           bufd = 6*RFACT**2,     ! depth of grouping buffers
     $         ambigs =  4      ! maximum number of ambiguous solutions
     $  )
       parameter (outw = 48/rfact)   ! wvc's in output record, nadir gap removed
       parameter (MGDRSIZE = 9260)
c
