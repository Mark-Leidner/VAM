#!#   $Id: make.sig0.q,v 1.1 2002/07/09 19:12:47 leidner Exp $
#!#   $Log: make.sig0.q,v $
#!#   Revision 1.1  2002/07/09 19:12:47  leidner
#!#   First revision in CVS (previously in RCS).
#!#
#!#   Revision 1.2  1998/10/30 20:32:30  leidner
#!#   added trajectory value, sd, noise eqn coeffs and row and col indices
#!#
#!#	Revision 1.1  1997/07/08  20:34:51  leidner
#!#	Initial revision
#!#
make.sig0 <- function(bv,
	date=bv[1], time=bv[2], lognum=bv[3],
	revs=bv[4:18],
	nByrev=bv[19:33], n=bv[34], 
	lat=bv[(1:n)+34],
	lon=bv[(1:n)+(n+34)],
	pol=bv[(1:n)+(2*n+34)],
	ant=bv[(1:n)+(3*n+34)],
	theta=bv[(1:n)+(4*n+34)],
	azim=bv[(1:n)+(5*n+34)],
	s0obs=bv[(1:n)+(6*n+34)],
	s05=bv[(1:n)+(7*n+34)],
	s0sd=bv[(1:n)+(8*n+34)],
	kpa=bv[(1:n)+(9*n+34)],
	kpb=bv[(1:n)+(10*n+34)],
	kpc=bv[(1:n)+(11*n+34)],
	row=bv[(1:n)+(12*n+34)],
	col=bv[(1:n)+(13*n+34)] )
{
# bv is a backscatter observation vector usually scanned from an ascii file,
# which contains in order the elements of the backscatter object
# NOTE: See /home/nwp/NWP/scatterometer/vam2d.fortran/wna_wrback.F
        
	print(list(nWVC=n,nByrev=nByrev))
	list(date=date, time=time, lognum=lognum, revs=revs, nByrev=nByrev,
 		lat=lat, lon=lon, pol=pol, ant=ant, theta=theta, azim=azim,
		s0obs=s0obs, s05=s05, s0sd=s0sd, kpa=kpa, kpb=kpb, kpc=kpc,
		row=row,col=col)
 }
