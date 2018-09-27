#!#   $Id: plt.q,v 1.7 2001/05/17 21:47:46 jmh Exp $
#!#   $Log: plt.q,v $
#!#   Revision 1.7  2001/05/17 21:47:46  jmh
#!#   Minor changes to the plotting window and plotting goes Tb
#!#
#!#   Revision 1.6  1999/10/19 18:33:14  jmh
#!#   Corrected minor typo that prevented compilation
#!#
#!#   Revision 1.5  1997/08/28 17:27:48  leidner
#!#   added plotting of ssmi fields and new optional arguments controlling graphical parameters
#!#
#!#	Revision 1.4  1997/05/07  13:29:29  leidner
#!#	added wind direction difference plot
#!#
#!#	Revision 1.3  1997/05/01  17:00:31  leidner
#!#	Added conventional obs
#!#
#!#	Revision 1.2  1997/04/25  20:53:14  leidner
#!#	added no.low.winds flag
#!#
#!#	Revision 1.1  1997/04/24  22:30:50  leidner
#!#	Initial revision
#!#
plt <- function(a, gridded=T, make.ps=F, pt="bg", 
        xstart=xs,xend=xe,ystart=ys,yend=ye,file='test.ps',
	u0=as.vector(a$u), v0=as.vector(a$v), u=a$u, v=a$v,
	plot.contour=F, plot.image=F, plot.wind.symbols=T,
	barbcolor=1,barbthick=0.25,barbscale=0.35,loc=F,background=NA,
	title="",labsiz=labsiz,delln=10,dellt=10,...)
{
# Define output filenames and plot titles
	plottype<-list(
		bg=list(title='GDAS background analysis',file='background.ps'),
#		bg=list(title='eta 48km background analysis',file='background.ps'),
		an=list(title='VAM analysis',file='analysis.ps'),
#		an1=list(title='NSCAT winds',file='nscat_wind.ps'),
#		an2=list(title='VAM analysis without los info',file='analysis2.ps'),
		abv=list(title='VAM - GDAS analysis',file='analysis-background.ps'),
#		abv=list(title='VAM (no los) - eta analysis',file='analysis2-background.ps'),
		absp=list(title='VAM - GDAS analysis',file='analysis-background.spd.ps'),
#		absp=list(title='VAM (no los) - eta analysis',file='analysis-background.spd.ps'),
		abdr=list(title='VAM - GDAS analysis',file='analysis-background.dir.ps'),
#		abdr=list(title='VAM (no los) - eta analysis',file='analysis-background.dir.ps'),
		nb=list(title='GDAS winds',file='background@nscat.ps'),
		na=list(title='VAM winds',file='analysis@nscat.ps'),
		nj=list(title='JPL winds',file='jpl.winds.ps'),
		c=list(title='Conventional obs',file='conventional.ps'),
		losm=list(title='SSMI LOS wind',file='ssmi.losm.ps'),
		losm5=list(title='trajectory SSMI LOS wind',file='ssmi.losm5.ps'),
		velm=list(title='SSMI wind speed',file='ssmi.velm.ps'),
		velm5=list(title='trajectory SSMI wind speed',file='ssmi.velm5.ps'), 
		tice=list(title='% total sea ice in cell', file='ssmi.tice.ps'), 
		iwv=list(title='SSMI integrated water vapor',file='ssmi.iwv.ps'), 
		icw=list(title='SSMI integrated cloud water',file='ssmi.icw.ps'),
		rainr=list(title='SSMI rain rate',file='ssmi.rainr.ps'),
		goes.tb=list(title=title,file=file) ) #goes.tb filename from plot.vam.jpl.amb.q

# If plot region arguments are set, redefine plot region
	if (is.na(xstart)) {
		xs<-(-90); xe<-0; ys<-0; ye<-60;  }
        else {
		xs<-xstart; xe<-xend; ys<-ystart; ye<-yend;  }


# Choose title text from available plotting options
	print(list(plotting.options=names(plottype)))
	if (match(pt,names(plottype),nomatch=0)!=0) {
		str<-paste('plottype$',pt,sep='')
		m<-eval(parse(text=str)) }
	else {
		print(pt)
		stop(message='...no such plotting option. Stopping.') }
  
# Make sub-title text
	subtitle<-''
	if (pt == "bg" | pt == "nj" | pt == "nb" | pt == "losm" | pt == "ssmi") {
		sub.names<-c('date','time','revs') }
	else sub.names<-c('date','time','revs','lognum')
	for (i in sub.names) {
		if (match(i,names(a),nomatch=0)!=0) {
			if (i=='revs') {
				rev.list<-a$revs[a$nByrev>0]
				x<-paste(rev.list,collapse=' ') }
			else { str<-paste('a$',i,sep='')
				x<-eval(parse(text=str)) }
			str<-paste(i,x,sep=': ',collapse='')
			subtitle<-paste(subtitle,str,sep='  ',collapse='')
		}
	}
	print(list(title=m$title,subtitle=subtitle))

# Plot data
	ps.options (colors=HeatB.lines.ps)
	ps.options (image.colors=HeatB.images.ps)
	if ( pt == "losm" | pt == "velm" | pt == "losm5" | pt == "velm5" |
	pt == "tice" | pt == "icw" | pt == "iwv" | pt == "rainr" | pt == "goes.tb") {
		ps.options (colors=ssmi.winds.lines.colors)
		ps.options (image.colors=ssmi.winds.image.colors) }
	if (make.ps) { 
		postscript(file=m$file,print.it=F)
		print(list(ps.file=m$file)) }
#	stop(message='testing, testing...')
	par(pty='m',xaxs='i',yaxs='i')
	xx<-list(date=a$date,
		time=a$time,
		xs=xstart,
		xe=xend,
		delx=1,
		ys=ystart,
		ye=yend,
		dely=1,
		delln=delln,
		dellt=dellt)
	plot.background(xx,
		annot=F,
		drawmap=F,
		setpin=T)
	if (gridded & pt != "absp" & pt != "abdr") {
		print(list(plot.contour=plot.contour,
			plot.image=plot.image,
			plot.wind.symbols=plot.wind.symbols))
		plot.wind.obs(
			wf2wo(xx, a, u0=u0, v0=v0),
			plot.contour=plot.contour,
			plot.image=plot.image,
			plot.wind.symbols=plot.wind.symbols,
			windmax=20,
			barbcolor=1,
			levels=seq(0,20,5),
			barbthick=0.25) }
	else if (gridded & pt == "absp") {
 		background.sd <- uv2sd(background$u,background$v)
 		a.sd <- uv2sd(a$u,a$v)
		dif.spd <- ( a.sd$vel - background.sd$vel ) * 1.
		dif.spdv <- as.vector(dif.spd)
		range.dif<-range(dif.spdv)
		bot<-floor(range.dif[1])
		top<-ceiling(range.dif[2])
		bot <- -10
		top <- 10
		print(list(range.dif=range.dif,top=top,bot=bot))
		plot.wind.obs(
			wf2wo(xx, a, u0=u0, v0=v0),
			plot.contour=T,
			plot.image=T,
			plot.wind.symbols=F,
			speed=dif.spdv,
			windmax=top,
			levels=c(seq(bot,-1,1),seq(1,top,1)),
			zlim=c(bot,top),
			no.low.winds=F) }
	else if (gridded & pt == "abdr") {
 		background.sd <- uv2sd(background$u,background$v)
 		a.sd <- uv2sd(a$u,a$v)
		dif.dir <- a.sd$theta - background.sd$theta
		dif.dirv <- as.vector(dif.dir)
		dif.dirv[dif.dirv > pi] <- dif.dirv[dif.dirv > pi] - 2*pi
		dif.dirv[dif.dirv < -pi] <- dif.dirv[dif.dirv < -pi] + 2*pi
		dif.dirv <- dif.dirv*180/pi
		range.dif<-range(dif.dirv)
		bot<-floor(range.dif[1])
		top<-ceiling(range.dif[2])
		print(list(range.dif=range.dif,top=top,bot=bot))
		plot.wind.obs(
			wf2wo(xx, a, u0=u0, v0=v0),
			plot.contour=T,
			plot.image=T,
			plot.wind.symbols=F,
			speed=dif.dirv,
			windmax=180,
			levels=c(seq(-180,-30,30),seq(30,180,30)),
			zlim=c(-180,180) ) }
	else if (pt == "c") {
		plot.wind.obs(a, u=u, v=v,
			plot.contour=F,
			plot.image=F,
			plot.wind.symbols=T,
			windmax=20,
			barbcolor=barbcolor,
			barbthick=0.5,
			barbscale=1,
			location.only=loc) }
	else if (pt == "losm") {
		plot.ssmi.los(a) }
	else if (pt == "losm5") {
		plot.ssmi.los(a,plot.trajectory=T) }
	else if (pt == "velm") {
		plot.ssmi.vel(a) }
	else if (pt == "velm5") {
		plot.ssmi.vel(a,plot.trajectory=T) }
	else if (pt == "tice") {
		plot.ssmi.tice(a) }
	else if (pt == "iwv") {
		plot.ssmi.iwv(a) }
	else if (pt == "icw") {
		plot.ssmi.icw(a, max=0.3) }
	else if (pt == "rainr") {
		plot.ssmi.rainr(a, max=0.3) }
        else if (pt == "goes.tb") {
                plot.goes.tb(a,labsiz=labsiz) }
	else {
		plot.wind.obs(a, u=u, v=v,
			plot.contour=F,
			plot.image=F,
			plot.wind.symbols=T,
			windmax=20,
			barbcolor=barbcolor,
			barbthick=barbthick,
			barbscale=barbscale,
			location.only=loc) }
	plot.background(xx,
		init=F,
		main=m$title,
		sub=subtitle,
		drawmap=T)
	timestamp(line=4.5)
}
