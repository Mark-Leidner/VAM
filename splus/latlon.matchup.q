latlon.matchup <- function (lat1, lon1, lat2, lon2, eps=0.01)
{
#!# $Id: latlon.matchup.q,v 1.3 2000/01/24 15:39:52 trn Exp $
#!# Match up lat/lon locations. lat1/lon1 must be a subset (can be identical)
#!# of lat2/lon2
#!# $Log: latlon.matchup.q,v $
#!# Revision 1.3  2000/01/24 15:39:52  trn
#!# Allow for specification of lat/lon tolerance, handle case of data in lon1
#!# missing from lon2
#!#
#!# Revision 1.2  1999/09/22 16:42:42  trn
#!# Matches up two sets of lat/lon pairs
#!#
lsame <- function(x,y,eps) (abs(x-y) < eps)
if (length(lat1) != length(lon1)) stop ('unequal length lat1, lon1')
if (length(lat2) != length(lon2)) stop ('unequal length lat2, lon2')
if (length(lat1) > length(lat2)) stop ('lat1 cannot be longer than lat2')

lon1 <- ifelse(lon1<0,lon1+360,lon1) ; lon2 <- ifelse(lon2<0,lon2+360,lon2) 
order1 <- order(lat1,lon1) ; order2 <- order(lat2,lon2) 
order2.1 <- rep(0,length(order1))

i.2 <- 1
for (i.1 in seq(order1)) {
  while (i.2 <= length(order2) && 
	 (!lsame(lat1[order1[i.1]],lat2[order2[i.2]],eps) ||
	  !lsame(lon1[order1[i.1]],lon2[order2[i.2]],eps) ) ) {
    i.2 <- i.2 + 1
  }
  if (i.2 <= length(order2)) {
    order2.1[i.1] <- order2[i.2]
  } else {
    order2.1[i.1] <- NA
    i.2 <- 1
  }
}

#!# order1:   index of lat1/lon1 array for ordering by lat/lon
#!# order2.1: index of lat2/lon2 array that matches up with lat1/lon1 [order1]
list(order1=order1,order2.1=order2.1)

}
