calc.vam.wspd.err <- function (vam, jpl, use.closest=F, use.selected=F)

#
# This function computes the rms scaler wind speed difference between
# vam winds interpolated to NSCAT wvc locations and jpl
# wind ambiguities; use.closest controls which ambiguity is used.
#
##
#         vam - nscat wind object filled with vam analysis winds
#                    interpolated to NSCAT wvc locations
#                    (see "make.nscat.winds.q")
#         jpl - jpl wind object (contains all ambiguities;
#                    see "make.jpl.winds.q")
# use.closest - LOGICAL; use closest jpl ambiguity to determine vam wspd
#                    error?  If set to FALSE, the amb with the highest
#                    maximum likelihood.
{


	if (use.closest) {
	  closest <- find.closest(jpl,vam)
	  mask <- !is.na(closest$u | closest$v)
	  vam.wspd <- uv2sd(vam$u[mask],vam$v[mask])$vel
	  closest.wspd <- uv2sd(closest$u[mask],closest$v[mask])$vel
	  dif <- vam.wspd - closest.wspd
	}else{
	  if(use.selected){
	    selected <- vam
            selected$u <- jpl$u
            selected$v <- jpl$v
	    mask <- !is.na(selected$u | selected$v)
	    vam.wspd <- uv2sd(vam$u[mask],vam$v[mask])$vel
	    selected.wspd <- uv2sd(selected$u[mask],selected$v[mask])$vel
	    dif <- vam.wspd - selected.wspd
	  }else{
	    ua<-cbind(jpl$u,jpl$u2,jpl$u3,jpl$u4)
	    va<-cbind(jpl$v,jpl$v2,jpl$v3,jpl$v4)
	    mle<-cbind(jpl$mle,jpl$mle2,jpl$mle3,jpl$mle4)
	    miss<-(is.na(ua) | is.na(va))
	    mle[miss]<-(-9999.)
	    
	    uc<-ua[,1]
	    vc<-va[,1]
	    maxmle<-mle[,1]
	    ch<-rep(1,length(maxmle))
	    for (j in 2:4) {
	      select<-mle[,j]>maxmle
	      if (sum(select) > 0) {
		ch[select]<-j
		uc[select]<-ua[select,j]
		vc[select]<-va[select,j]
		maxmle[select]<-mle[select,j]
	      }
	    }
	    print(table(ch))
	    mask <- !is.na(uc | vc)
	    print(paste("sum(mask)=",sum(mask)))
	    vam.wspd <- uv2sd(vam$u[mask],vam$v[mask])$vel
	    jpl.wspd <- uv2sd(uc[mask],vc[mask])$vel
	    dif <- vam.wspd - jpl.wspd
	  }
	}
	n <- length(dif)
	sumdif<- sum(dif^2)
	round(sqrt(sumdif/n),digits=3)
}
