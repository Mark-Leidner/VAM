#!#   $Id: make.wind.field.q,v 1.1 2002/07/24 20:26:26 leidner Exp $
#!#   $Log: make.wind.field.q,v $
#!#   Revision 1.1  2002/07/24 20:26:26  leidner
#!#   Added to repository to make read.vam.datasets.q functional.
#!#
#!#   Revision 1.2  1997/04/24 21:37:54  leidner
#!#   added lognum
#!#
#!#	Revision 1.1  1997/04/22  20:20:00  leidner
#!#	Initial revision
#!#
make.wind.field <- function(wfv,
	date=wfv[1], time=wfv[2], lognum=wfv[3],
	nx=wfv[4], xs=wfv[5], delx=wfv[6], xe=xs+delx*(nx-1),
	ny=wfv[7], ys=wfv[8], dely=wfv[9], ye=ys+dely*(ny-1),
	n =nx*ny, grid.names=list(seq(xs,xe,delx),seq(ys,ye,dely)),
	u=matrix(wfv[(1:n)+9],nrow=nx,ncol=ny,dimnames=grid.names),
	v=matrix(wfv[(1:n)+(n+9)],nrow=nx,ncol=ny,dimnames=grid.names) )
{
# wfv is a wind field vector usually scanned from an ascii file,
# which contains in order the elements of the wind field object
# NOTE: See ../fortran/wga_wrgrid.f
	list(date=date, time=time, lognum=lognum,
		xs=xs, xe=xe, delx=delx,
		ys=ys, ye=ye, dely=dely,
		u=u, v=v)
}
