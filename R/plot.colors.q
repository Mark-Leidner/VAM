########### color plot for irregular grid
# purpose : to plot color images in lat-lon for the irregular grid
# determine a color for each quadrilateral and plot it

# plotting routines do not recognize rgb or hsv 3-row objects that we can interpolate.
# instead convert rgb to hsv format and then use rows of the hsv 3-row object
# with hsv function to make character colors for actual plotting.
# note that col2rgb works on any color specification.

color.scale <-
  function (values, colors, saturate=TRUE, type='HSV',
            n=length(values), high=NA, low=NA)
# values will be plotted using colors
# arbitrary values will be plotted with a color interpolated linearly from colors
# out of bound values will be plotted as high or low as appropriate
# colors will be 3 row objects (RGB or HSV)
{ if (is.vector(colors)) colors<-col2rgb(colors)
  if (length(high)==1 && !is.na(high)) high<-col2rgb(high)
  if (length(low)==1 && !is.na(low)) low<-col2rgb(low)
# high and low are now length 3 unless NA  
  if (type=='RGB') {
    if (paste(dimnames(colors)[[1]],collapse='.')=='h.s.v')
      colors<-col2rgb(hsv(h=colors['h',],s=colors['s',],v=colors['v',]))
    if (length(high)>1 && paste(dimnames(high)[[1]],collapse='.')=='h.s.v')
      high<-col2rgb(hsv(h=high['h',],s=high['s',],v=high['v',]))
    if (length(low)>1 && paste(dimnames(low)[[1]],collapse='.')=='h.s.v')
      low<-col2rgb(hsv(h=low['h',],s=low['s',],v=low['v',]))}
  else if (type=='HSV') {
    if (paste(dimnames(colors)[[1]],collapse='.')=='red.green.blue')
      colors<-rgb2hsv(colors)
    if (length(high)>1 && paste(dimnames(high)[[1]],collapse='.')=='red.green.blue')
      high<-rgb2hsv(high)
    if (length(low)>1 && paste(dimnames(low)[[1]],collapse='.')=='red.green.blue')
      low<-rgb2hsv(low) }
  if (saturate) {high<-colors[,n,drop=FALSE] ; low<-colors[,1,drop=FALSE]}
  list(values=values,colors=colors,n=n,high=high,low=low,type=type) }

add.color.map <-
  function (z, modelgrid, colormap=color.scale(range(z),c('blue','red')),
            colors=convert2colors(z,colormap),select=1:length(i),mask=maskc,
            xpd=FALSE,border=NA,lwd=1)
# use modelgrid corners to plot polygons for each element in z.
# z can be a vector or a matrix.
# xpd controls clipping of polygons; default is to clip within the plot region.
# setup to go thru all possible polygons, but select could indicate a subset, but
# select, z, and colors must all be the same length.
# further only grid cells with mask TRUE will be plotted.
# first unpack modelgrid
{ for (ii in names(modelgrid)) assign(ii, modelgrid[[ii]])
# convert z and colors to matrices if necessary  
  if (is.matrix(z)) z<-z[ij]
  if (is.matrix(colors)) colors<-colors[ij]
  xpd<-par(xpd=xpd)
  for (p in select) {
    ip<-i[p] ; jp<-j[p]
    if (mask[ip,jp]) {
    pp<-cbind(x=ip+c(0,0,1,1),y=jp+c(0,1,1,0))
    polygon(lonc[pp],latc[pp],col=colors[p],border=border,lwd=lwd)}}
  par(xpd)
}

add.color.bar <-
  function (colormap,low.legend='LT',high.legend='GT',n.legend=n,
            values.legend=NULL,digits=2,
            position='right',inset=-1/6,fill.cex=2,bty='n',...)
# use an object created by color.scale to add color bar to existing plot.
# defaults to adding color bar in right margin
# typically use par(mar=c(3,3,2,5)+0.1,xpd=NA)
# xpd=NA allows the legend to appear anywhere in the device region
# extra parameters are passed to legend.
# if n.legend==n (default) each value is included, otherwise
# n.legend equally spaced values are included.
# low.legend and/or high.legend are included unless set to NULL
{ for ( name in names(colormap)) assign(name, colormap[[name]])
  if (length(position)==1) {position.x=position;position.y=NULL}
  else {position.x=position[1];position.y=position[2]}
# interpolate to new set of values of length n
  if (n.legend!=n) values<-seq(from=values[1],to=values[length(values)],length=n.legend)
# append high and low if non null
  if (!is.null(low.legend)) values<-c(values[1]-1,values)
  if (!is.null(high.legend)) values<-c(values,values[length(values)]+1)
# round values
  values<-round(values,digits)
# create colors for plotting  
  colors<-convert2colors(values,colormap)
# append high and low if non null
  if (!is.null(low.legend)) values[1]<-low.legend
  if (!is.null(high.legend)) values[length(values)]<-high.legend
  if (!is.null(values.legend)) values<-values.legend
  legend(position.x,position.y,rev(values),fill=rev(colors),inset=inset,
         fill.cex=fill.cex,bty=bty,...)
}

convert2colors <-
  function (z, colormap)
# use an object created by color.scale to interpret z values as colors
# interpolation in HSV gives brighter intermediate colors
{ for ( name in names(colormap)) assign(name, colormap[[name]])
  iz <- findInterval(z,values,rightmost.closed=TRUE)
  zc <- matrix(colors[1],nrow=3,ncol=length(z),dimnames=dimnames(colors))
  w <- 0*iz
  select <- iz > 0 & iz < n
  izs <- iz[select]
  ws <- (z[select]-values[izs+1])/(values[izs]-values[izs+1])
  for (i in 1:3) zc[i,select]<-ws*colors[i,izs] + (1-ws)*colors[i,izs+1]
  if (length(low)>1) {select <- iz==0 ; for (i in 1:3) zc[i,select]<-low[i]}
  if (length(high)>1) {select <- iz==n ; for (i in 1:3) zc[i,select]<-high[i]}
  if (type=='RGB') zc<-rgb2hsv(pmax(pmin(zc,255),0))
  zc<-hsv(h=zc['h',],s=zc['s',],v=zc['v',])
  if (length(low)==1) {select <- iz==0 ; zc[select]<-NA}
  if (length(high)==1) {select <- iz==n ; zc[select]<-NA}
  zc}

add.color.gradient <-
  function (colormap, nz=201, zmax=max(values), zmin=min(values),
            zvalues=seq(from=zmin,to=zmax,length=nz), top2bottom=T,
            gradient=convert2colors(zvalues[-1],colormap),
            xmin=par('usr')[1],xmax=par('usr')[2],
            ymin=par('usr')[3],ymax=par('usr')[4])
# use an object created by color.scale to interpret z values as colors
# interpolation in HSV gives brighter intermediate colors
{ for (name in names(colormap)) assign(name, colormap[[name]])
  if (top2bottom) 
    image(c(xmin,xmax),zvalues,matrix(zvalues[-1],nrow=1),col=gradient,add=T)
  else
    image(zvalues,c(ymin,ymax),matrix(zvalues[-1],ncol=1),col=gradient,add=T)
  box()}

  
### for testing:

triangle <-
  function(x=c(0,2,1), y=c(0,0,1), x0=0, y0=0, scale=1, angle=0, col=NA)
{ xy<-rotate(x,y,-angle*pi/180)
  polygon(x0+scale*xy$x,y0+scale*xy$y,col=col) }

square <-
  function(x=c(0,1,1,0), y=c(0,0,1,1), x0=0, y0=0, scale=1, angle=0, col=NA)
{ triangle(x,y,x0,y0,scale,angle,col) }

test.fig <-
  function(colors,n=length(colors))
{ plot(c(0,100),c(0,100),type='n')
  for (i in 1:n) square(x0=50,y0=50,scale=40-i/3,angle=(i-1)*360/n,col=colors[i]) }

add.poly <-
  function (i,j,lonc,latc,color='pink',border=NA,lwd=1)
  {select<-cbind(x=i+c(0,0,1,1),y=j+c(0,1,1,0))
   polygon(lonc[select],latc[select],col=color,border=border,lwd=lwd)}


