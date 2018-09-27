 
> ######################## 13 Feb 2012 #########################

> vmag<-function(v){sqrt(sum(v^2))}
> dot<-function(x,y){sum(x*y)}
> # two-element vector only for following
> vnormal<-function(v){c(-v[2],v[1])}
> project<-function(v,x=c(1,0),n=vnormal(x)){c(dot(v,x),dot(v,n))}
> vplot<-function(v,start=c(0,0),...){arrows(start[1],start[2],start[1]+v[1],start[2]+v[2],...)}

> path=c(-3,1)
> wind=c(100,-100)
> airspeed<-500*0.5144444444 # knots to meters per second
> path=path/vmag(path)
> n.path<-vnormal(path)
> pathwind<-project(wind,path)
> pathair<-c(sqrt(airspeed^2-pathwind[2]^2),-pathwind[2])

> psize=250
> offset<-c(-100,-100)
> plot(c(-psize,psize),c(-psize,psize),type='n',xlab='u (m/s)',ylab='v(m/s)')
> abline(col="grey",a=0,b=path[2]/path[1])
> vplot(psize*path/3,offset)
> vplot(psize*n.path/3,offset)
> vplot(wind,col=2)
> vplot(pathwind[1]*path,col=4)
> vplot(pathwind[2]*n.path,pathwind[1]*path,col=4)
> vplot(pathair[1]*path,col=4)
> vplot(pathair[2]*n.path,pathair[1]*path,col=4)
> vplot(pathair[2]*n.path+pathair[1]*path,col=3)
> vplot((pathair[1]+pathwind[1])*path,col=1)

> pdf('flight.path.pdf',width=8,height=8,onefile=T,title='Aircraft heading')
> dev.off()

 
