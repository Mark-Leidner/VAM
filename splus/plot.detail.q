plot.detail <- function(background.name='AVN',thin=3,vam.factor=3,
	   make.ps=T,vam.data.dir='/gaeascratch/',skip=1,
	   sub='a',xs,xe,ys,ye,drawmap=F, vam.all.factor=2*vam.factor, ...)
{
#!# $Id: plot.detail.q,v 1.3 1999/10/27 13:50:08 trn Exp $
#!# Plot standard set of regional plots
#!# $Log: plot.detail.q,v $
#!# Revision 1.3  1999/10/27 13:50:08  trn
#!# Fixed names and default values of vam.factor (size of VAM ambiguity locations)
#!#
#!# Revision 1.2  1999/09/24 21:13:51  trn
#!# Improved control over size of barbs/circles
#!#
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#
  plot.analyses(background.name=background.name, skip=skip,
		make.ps=make.ps,vam.data.dir=vam.data.dir,
		thin=thin,vam.factor=vam.factor,
		sub=sub,xs=xs,xe=xe,ys=ys,ye=ye,drawmap=drawmap,...)
  plot.all.jpl.amb(make.ps=make.ps,vam.data.dir=vam.data.dir,
		   thin=thin,vam.factor=vam.all.factor,
		   sub=sub,xs=xs,xe=xe,ys=ys,ye=ye,drawmap=drawmap,...)
  plot.vam.jpl.amb(make.ps=make.ps,vam.data.dir=vam.data.dir,
		   thin=thin,vam.factor=vam.factor,
		   sub=sub,xs=xs,xe=xe,ys=ys,ye=ye,drawmap=drawmap,...)
}
