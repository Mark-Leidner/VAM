#!#   $Id: find.closest.q,v 1.3 2005/07/14 18:42:32 jhalland Exp $
#!#   $Log: find.closest.q,v $
#!#   Revision 1.3  2005/07/14 18:42:32  jhalland
#!#   Added less stringent check on euality of lats and lons.
#!#
#!#   Revision 1.2  1999/10/26 16:49:05  jmh
#!#   Commented out print statements
#!#
#!#   Revision 1.1  1998/05/19 18:13:22  leidner
#!#   Initial revision
#!#
find.closest <- function(jpl,vam,u=vam$u,v=vam$v,lat=vam$lat,lon=vam$lon)

{
	if (length(jpl$lat) != length(vam$lat)) stop(mess='no of jpl and vam wvcs not equal...')
	if (max(abs(jpl$lat-vam$lat)) > 0.01) stop(mess='jpl and vam wvc lats do not correspond...')

	ua<-cbind(jpl$u,jpl$u2,jpl$u3,jpl$u4)
	va<-cbind(jpl$v,jpl$v2,jpl$v3,jpl$v4)
	miss<-(is.na(ua) | is.na(va))

#	print(list(len.miss=sum(miss)))
#	print(list(len.ua=length(ua),len.u=length(u)))

	dira<-uv2sd(ua,va)$theta
	dir<-uv2sd(u,v)$theta
	del <- pi - abs(pi - abs(dir - dira))
 
	del[miss]<-pi
	ua[miss]<-0
	va[miss]<-0

	uc<-ua[,1]
	vc<-va[,1]
	delmin<-del[,1]
	ch<-rep(1,length(delmin))
	for (j in 2:4) {
		select<-del[,j]<delmin
		if (sum(select) > 0) {
			ch[select]<-j
			uc[select]<-ua[select,j]
			vc[select]<-va[select,j]
			delmin[select]<-del[select,j]
		}
	}

#	print(table(ch))

	closest<-vam
	closest$u<-uc
	closest$v<-vc
# reset missing values to NA
	miss2<-(is.na(jpl$u) | is.na(jpl$v))
#	print(list(len.miss2=sum(miss2)))
	closest$u[miss2]<- NA
	closest$v[miss2]<- NA
	closest
}




