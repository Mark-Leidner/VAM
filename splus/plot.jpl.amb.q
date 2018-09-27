plot.jpl.amb <- function (jpl,date="")
{
#!# $Id: plot.jpl.amb.q,v 1.1 1999/09/22 16:42:42 trn Exp $
#!# Plot bargraphs of ambiguity stats
#!# $Log: plot.jpl.amb.q,v $
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#

#  Count how many of each ambiguity
	counts<-rep(NA,4)
	counts[1]<-sum(!is.na(jpl$u))
	counts[2]<-sum(!is.na(jpl$u2))
	counts[3]<-sum(!is.na(jpl$u3))
	counts[4]<-sum(!is.na(jpl$u4))

#  Compute average wind speed for each ambiguity
	wspds<-rep(NA,4)
	wspds[1]<-mean(uv2sd(jpl$u,jpl$v)$vel,na.rm=T)             
	wspds[2]<-mean(uv2sd(jpl$u2,jpl$v2)$vel,na.rm=T)
	wspds[3]<-mean(uv2sd(jpl$u3,jpl$v3)$vel,na.rm=T)
	wspds[4]<-mean(uv2sd(jpl$u4,jpl$v4)$vel,na.rm=T)

#  Plot the result in two bar graphs
	old.par <- par(mfrow=c(1,2))
	barplot(counts,names=c('amb 1','amb 2','amb 3','amb 4'))
	text<-paste(date,' jpl ambiguous winds',sep="")
	title(text)
	barplot(wspds,names=c('amb 1','amb 2','amb 3','amb 4'),ylab='ave wind speed m/s')
	par(old.par)
}

plot.jpl.winds <- 
  function (jpl, barbcolors=c(3,2,4,5), plotl=c(T,T,T,T),
	    barbscale=0.2, barbthick=0.5, 
	    use.sel=F, select=rep(T,length(jpl$lat)), ...)
{
#!# $Id: plot.jpl.amb.q,v 1.1 1999/09/22 16:42:42 trn Exp $
#!# Plot ambiguities (controlled by plotl); if use.sel, only plot
#!# the ambiguity corresponding to the $sel component
#!# $Log: plot.jpl.amb.q,v $
#!# Revision 1.1  1999/09/22 16:42:42  trn
#!# Initial revision
#!#
# Make masks for each ambiguity position
	  m1 <- select
	  m2 <- select
	  m3 <- select
	  m4 <- select

        if(use.sel){
	  plotl <- c(T,T,T,T)

# Reset masks
	  m1[] <- F; m2[] <- F; m3[] <- F; m4[] <- F
# First, take care of NA's in the selected list
	  jpl$sel[is.na(jpl$sel)] <- 0
	  m1[] <- select & jpl$sel == 1
	  m1[] <- select & jpl$sel == 2
	  m1[] <- select & jpl$sel == 3
	  m1[] <- select & jpl$sel == 4
	}

	if(plotl[4]) plot.wind.obs(jpl,u=jpl$u4[m4],v=jpl$v4[m4],
				   lat=jpl$lat[m4],lon=jpl$lon[m4],
				   barbs=barbscale,
				   barbc=barbcolors[4],barbt=barbthick)
	if(plotl[3]) plot.wind.obs(jpl,u=jpl$u3[m3],v=jpl$v3[m3],
				   lat=jpl$lat[m3],lon=jpl$lon[m3],
				   barbs=barbscale,
				   barbc=barbcolors[3],barbt=barbthick)
	if(plotl[2]) plot.wind.obs(jpl,u=jpl$u2[m2],v=jpl$v2[m2],
				   lat=jpl$lat[m2],lon=jpl$lon[m2],
				   barbs=barbscale,
				   barbc=barbcolors[2],barbt=barbthick)
	if(plotl[1]) plot.wind.obs(jpl,u=jpl$u[m1], v=jpl$v[m1],
				   lat=jpl$lat[m1],lon=jpl$lon[m1],
				   barbs=barbscale,
				   barbc=barbcolors[1],barbt=barbthick) }


