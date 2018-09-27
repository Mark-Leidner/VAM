wf2wo <- function (wf, wf0=wf, skip=1, skipx=skip, skipy=skip,
	u0=wf0$u, v0=wf0$v,
	xs0=wf0$xs, xe0=wf0$xe, delx0=wf0$delx,
	ys0=wf0$ys, ye0=wf0$ye, dely0=wf0$dely,
	x0=seq(xs0,xe0,delx0), y0=seq(ys0,ye0,dely0),
	lat0=array(rep(y0,rep(length(x0),length(y0))),dim=c(length(x0),length(y0))),
	lon0=array(rep(x0,length(y0)),dim=c(length(x0),length(y0))),
	xs=wf$xs, xe=wf$xe, delx=wf$delx,
	ys=wf$ys, ye=wf$ye, dely=wf$dely)
{
#!#   $Id: wf2wo.q,v 1.5 2005/07/14 18:50:55 jhalland Exp $
#!#   $Log: wf2wo.q,v $
#!#   Revision 1.5  2005/07/14 18:50:55  jhalland
#!#   Changed longitude adjustment; now only takes place when xe > 180.
#!#
#!#   Revision 1.4  1999/09/22 16:42:42  trn
#!#   Moved comments
#!#
#!#   Revision 1.3  1999/09/10 18:11:09  trn
#!#   Added skip,skipx,skipy parameters for thinning gridded wind field
#!#   ,
#!#
#!#   Revision 1.2  1997/04/24 22:08:56  leidner
#!#   removed automatic interp and added longitude shift for map plotting
#!#
#!#	Revision 1.1  1997/04/22  20:20:00  leidner
#!#	Initial revision
#!#
#
# Parameters:
#
#                wf - gridded (wind field) object which provides defaults
#               wf0 - old gridded object containing u,v
#                   - subscript 0 indicates an item from wf0
#             (u,v) - u- and v-components of wind
#         (lat,lon) - location of wind obs
#      (xs,xe,delx) - start, end and delta for grid in x
#      (ys,ye,dely) - start, end and delta for grid in y
#             (x,y) - coordinates for grid
#      skipx, skipy - extract only every skipx(y) point in lon(lat)
# NOTE: add/or replace to input wf, so non-referenced elements are unchanged
# NOTE: must calculate u,v first to force all needed lazy evaluations
        if (xe < 180) {	
  	  lon0[180<lon0] <- lon0[180<lon0]-360
        } 
	if (dim(u0)[1] != length(x0) || dim(u0)[2] != length(y0)) 
	  stop ('wf2wo: Inconsistent u dims and x,y: s, e, del')
	if (skipx > 1 || skipy > 1) {
	  indxx <- seq(1,dim(u0)[1],skipx) ; indxy <- seq(1,dim(u0)[2],skipy)
	  wf$u <- as.vector(u0[indxx,indxy])
	  wf$v <- as.vector(v0[indxx,indxy])
	  wf$lat <- as.vector(lat0[indxx,indxy])
	  wf$lon <- as.vector(lon0[indxx,indxy])
	} else {
	  wf$u <- as.vector(u0)
	  wf$v <- as.vector(v0)
	  wf$lat <- as.vector(lat0)
	  wf$lon <- as.vector(lon0)
	}
	wf$xs <- xs
	wf$xe <- xe
	wf$delx <- delx
	wf$ys <- ys
	wf$ye <- ye
	wf$dely <- dely
	wf
}
