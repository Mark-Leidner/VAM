#!#   $Id: make.nscat.winds.q,v 1.1 1997/04/24 21:35:10 leidner Exp $
#!#   $Log: make.nscat.winds.q,v $
#!#   Revision 1.1  1997/04/24 21:35:10  leidner
#!#   Initial revision
#!#
make.nscat.winds <- function(wov,
	date=wov[1], time=wov[2], lognum=wov[3],
	revs=wov[4:18],
	nByrev=wov[19:33], n=wov[34], 
	lat=wov[(1:n)+34],
	lon=wov[(1:n)+(n+34)],
	u=wov[(1:n)+(2*n+34)],
	v=wov[(1:n)+(3*n+34)],
	row=wov[(1:n)+(4*n+34)],
	col=wov[(1:n)+(5*n+34)] )
{
# wov is a wind observation vector usually scanned from an ascii file,
# which contains in order the elements of the wind observation object
# NOTE: See /home/nwp/NWP/scatterometer/vam2d.fortran/wna_wrwind.F
        
	print(list(nWVC=n,nByrev=nByrev))
	list(date=date, time=time, lognum=lognum, revs=revs, nByrev=nByrev,
 		lat=lat, lon=lon, u=u, v=v, row=row, col=col)
 }
