c	$Id: l20files.h,v 1.1 1998/02/16 21:01:42 leidner Exp $
c	$Log: l20files.h,v $
c	Revision 1.1  1998/02/16 21:01:42  leidner
c	Initial revision
c	
c
c	common block for Level data filenames & logical unit numbers
        character*80 l15_file, l17_file, mgdr_file
	integer lun15, lun17, lun20
        common /fyls/ lun15, lun17, lun20, l15_file, l17_file, mgdr_file
c
