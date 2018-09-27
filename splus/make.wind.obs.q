#!#   $Id: make.wind.obs.q,v 1.1 1997/04/24 21:41:10 leidner Exp $
#!#   $Log: make.wind.obs.q,v $
#!#   Revision 1.1  1997/04/24 21:41:10  leidner
#!#   Initial revision
#!#
make.wind.obs <- function(wov,
	date=wov[1], time=wov[2],
	n=wov[3],
	lat=wov[(1:n)+3],
	lon=wov[(1:n)+(n+3)],
	u=wov[(1:n)+(2*n+3)],
	v=wov[(1:n)+(3*n+3)] )
{
# wov is a wind observation vector usually scanned from an ascii file,
# which contains in order the elements of the wind observation object
# NOTE: See /home/nwp/NWP/scatterometer/vam2d.fortran/wca_wrwind.F

	list(date=date, time=time,
		lat=lat, lon=lon, u=u, v=v)
}
