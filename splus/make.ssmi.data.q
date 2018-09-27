make.ssmi.data <- function(ssmiv,
	date=ssmiv[1],time=ssmiv[2],lognum=ssmiv[3],n=ssmiv[4],
	lat=ssmiv[(1:n)+4],
	lon=ssmiv[(1:n)+(n+4)],
	velm=ssmiv[(1:n)+(2*n+4)],
	losm=ssmiv[(1:n)+(3*n+4)],
	px=ssmiv[(1:n)+(4*n+4)],
	py=ssmiv[(1:n)+(5*n+4)],
	row=ssmiv[(1:n)+(6*n+4)],
	col=ssmiv[(1:n)+(7*n+4)],
	rev=ssmiv[(1:n)+(8*n+4)],
	sst=ssmiv[(1:n)+(9*n+4)],
	tice=ssmiv[(1:n)+(10*n+4)],
	iwv=ssmiv[(1:n)+(11*n+4)],
	icw=ssmiv[(1:n)+(12*n+4)],
	rain=ssmiv[(1:n)+(13*n+4)],
	velm5=ssmiv[(1:n)+(14*n+4)],
	losm5=ssmiv[(1:n)+(15*n+4)],
        dt=ssmiv[(1:n)+(16*n+4)],
        u.fgat=ssmiv[(1:n)+(17*n+4)],
        v.fgat=ssmiv[(1:n)+(18*n+4)],
        alpha=ssmiv[(1:n)+(19*n+4)],
        qc.bm=as.integer(ssmiv[(1:n)+(20*n+4)]) )
#
#!#   $Id: make.ssmi.data.q,v 1.1 2005/06/13 17:23:22 leidner Exp $
#!#   $Log: make.ssmi.data.q,v $
#!#   Revision 1.1  2005/06/13 17:23:22  leidner
#!#   First plotting software for SSMI data.
#!#

{
# ssmiv is a ssmi data vector usually scanned from an ascii file,
# which contains in order the elements of the ssmi data object
# NOTE: see /home/nwp/NWP/scatterometer/vam2d.fortran/wsl_wrlos.F

	u<-(-1)*px*losm
	v<-(-1)*py*losm
	list(date=date, time=time, lognum=lognum,
	        velm=velm, losm=losm,
		lat=lat, lon=lon, px=px, py=py,
		row=row, col=col, rev=rev,
		sst=sst, tice=tice, iwv=iwv, icw=icw, rain=rain,
		velm5=velm5, losm5=losm5,
		u=u, v=v, 
                dt=dt, u.fgat=u.fgat, v.fgat=v.fgat, alpha=alpha,
                qc.bm=qc.bm)
}


make.ssmi.interp.data <- function(ssmiv,
	date=ssmiv[1],time=ssmiv[2],lognum=ssmiv[3],n=ssmiv[4],
	lat=ssmiv[(1:n)+4],
	lon=ssmiv[(1:n)+(n+4)],
	velm=ssmiv[(1:n)+(2*n+4)])
{
	list(date=date, time=time, lognum=lognum,
	        lat=lat, lon=lon, velm=velm)
      }


