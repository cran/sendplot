###################################################
### chunk number 1: 
###################################################
library(sendplot)


###################################################
### chunk number 2: 
###################################################
x1 = 1:7
y1 = 1:7  
x2 = 7:1
y2 = rep(4,7)


###################################################
### chunk number 3: 
###################################################
plot.calls = "plot(x1,y1,col='green', pch=3, cex=1.5,xlab='',ylab='');
              points(x2,y2,pch=4, cex=1.5, col='purple');
              title(xlab='x values', ylab='y values')"


###################################################
### chunk number 4: 
###################################################
 x = c(x1,x2)
 y = c(y1,y2)


###################################################
### chunk number 5: 
###################################################
x.lbls = list()
x.lbls$letter = rep(c("a","b","c","d","e","f","g"),2)
x.lbls$number = 1:14
x.lbls = as.data.frame(x.lbls)
x.lbls


###################################################
### chunk number 6: 
###################################################
x.links = list()
x.links$UB = rep("http://www.buffalo.edu", 14)
x.links$another = rep(NA, 14)
x.links$another[c(1:7)] = "http://www.google.com, http://www.goodsearch.com"
x.links = as.data.frame(x.links)


###################################################
### chunk number 7: 
###################################################
x.links


###################################################
### chunk number 8: 
###################################################
x.links=NA
y.links=NA
xy.links=NA


###################################################
### chunk number 9: 
###################################################
sendxy(plot.call = plot.calls, 
       x=x, y=y,
       x.lbls=x.lbls,  
       bound.pt=FALSE, 
       source.plot=NA, paint=FALSE,
       img.prog=NA,fname.root="testXY",resize="800x1100", 
       up.left=c(124,130),low.right=c(713,885), spot.radius=5)


###################################################
### chunk number 10: 
###################################################
x = 1:4
y = 1:5
z = t(matrix(round(rnorm(20),digits=3), ncol=4))


###################################################
### chunk number 11: 
###################################################
plot.calls = "image(x=x, y=y, z=z);title(main='sendimage example')"


###################################################
### chunk number 12: 
###################################################
x.lbls = list()
x.lbls$sex = c("F", "M", "F", "F")
x.lbls$age = c(27, 73, 46, 50)
x.lbls$stage = c(1,1,3,2)
x.lbls = as.data.frame(x.lbls)
y.lbls = list()
y.lbls$chromosome = c("chr1", "chr2", "chrX", "chr7", "chrY")
y.lbls$location = c(92526, 486844000,2984248632,1387071184,3048286585)
xy.lbls = list()
intensity = matrix(c(-.3,1.0,.3,-.07,-.4,1.2,.4,.3,1.0,-.5,-.06,1.1,.04,.5,.03,-.09,-.04,.06,.01,.03),nrow=5)
xy.lbls$intensity = intensity
QC = matrix(c(T,T,T,T,T,F,T,T,T,F,T,T,T,T,T,F,T,F,T,T), nrow=5)
xy.lbls$QC = QC



###################################################
### chunk number 13: 
###################################################
 sendimage(plot.call = plot.calls, x=x, y=y, z=z,z.value='value',
          x.lbls = x.lbls, y.lbls=y.lbls, xy.lbls=xy.lbls,
          up.left=c(101,99),low.right=c(735,914),
          bound.pt=FALSE, source.plot=NA, paint=FALSE,
          img.prog=NA,fname.root="testImg", spot.radius=10)


###################################################
### chunk number 14: 
###################################################
x =  matrix(rnorm(15), nrow=5, ncol=3)


###################################################
### chunk number 15: 
###################################################
rcol = c("red", "blue", "yellow", "purple", "blue")
ccol = c("black", "green", "black")


###################################################
### chunk number 16: 
###################################################
heatmap.send.legacy(x, RowSideColors=rcol, ColSideColors=ccol,
             z.value="value",
             bound.pt=FALSE, paint=FALSE,source.plot=NA,
             fname.root="heatmapSendPlot",resize="800x1100",
             up.left=c(288,203),low.right=c(620,940),
	     spot.radius=10)


###################################################
### chunk number 17: 
###################################################
library(sendplot)
data("aCGHex")


###################################################
### chunk number 18: 
###################################################
mat=matrix(c(rep(c(rep(2,8),rep(0,2)),1),
       rep(c(rep(1,8),rep(4,2)),14),
       rep(c(rep(3,8),rep(0,2)),2)),
       ncol=10,byrow=TRUE)
mat



###################################################
### chunk number 19: 
###################################################
plot.calls = c("image(x=x,y=y,z=t(z),zlim=c(-0.5,0.5),ylim=range(scanLoc,na.rm=T),col=c(hsv(h=2/6,v=seq(1,0,length=1000)^1.15), hsv(h=0/6,v=seq(0,1,length=1000)^1.15)),axes=F,xlab='',ylab='')",   "plot(ddr,axes = FALSE, xaxs = 'i', leaflab = 'none',main=ttl)",   "image(x=seq(from=-0.5,to=0.5,length=1000),y=1,z=t(zlgnd),zlim=c(-0.5,0.5),col=c(hsv(h=2/6,v=seq(1,0,length=1000)^1.15),hsv(h=0/6,v=seq(0,1,length=1000)^1.15)), axes=F,xlab='',ylab='')",   "image(x=0:1,y=0:1,z=matrix(rep(NA,4),ncol=2),xlim=range(c(W.lw,W.up),na.rm=T),ylim=range(scanLoc,na.rm=T),zlim=c(0,1),axes=F,xlab='',ylab='')")



###################################################
### chunk number 20: 
###################################################
colnames(aCGH$log2.ratios.fitted)=aCGH$inventory$sample.ID
ManDist=dist(t(aCGH$log2.ratios.fitted),method = "manhattan")
hc=hclust(ManDist,method="ward")
scanLoc=aCGH$mapping.info$loc.genome
ddr=as.dendrogram(hc)
colorSet =c("red","darkblue")
iclr=as.integer(factor(aCGH$inventory$sex[hc$order],levels=c("FEMALE","MALE")))
sample.names=as.character(aCGH$inventory$sample.ID[hc$order])
nsmpl = aCGH$data.info$nsmpl
z.value="log2.ratios.fitted"
z.raw = round(aCGH$log2.ratios.fitted[,hc$order],4)
z = z.raw
z[z>0.5]=0.5
z[z<(-0.5)]=-0.5
zlgnd=array(seq(from=-0.5,to=0.5,length=1000),dim=c(1,1000))
rowSort=function(i,x) sort(x[i,])
z.sort=t(mapply(rowSort,1:(dim(z.raw)[1]),MoreArgs=list(x=z.raw)))
lwDX=1:ceiling(nsmpl/4)
upDX=(floor((3/4)*nsmpl)+1):nsmpl
W.up=rowMeans(z.sort[,upDX],na.rm=T)
W.lw=rowMeans(z.sort[,lwDX],na.rm=T)
ttl = "Region: 4q13"



###################################################
### chunk number 21: 
###################################################
mai.mat = matrix(0, ncol=4, nrow=4, byrow=TRUE)
mai.mat[1,] = c(.5,0,.5,0)
mai.mat[2,] = c(0,0,.3,0)
mai.mat[3,] = c(.4,.4,.2,.4)
mai.mat[4,] = c(.5,.2,.5,.2)
mai.prc = FALSE


###################################################
### chunk number 22: 
###################################################
  
 bandDX=((sum(aCGH$Band.Aid$Regions[[4]]$Upper<=min(scanLoc,na.rm=T),na.rm=T)+1)
       :(sum(aCGH$Band.Aid$Regions[[4]]$Lower<=max(scanLoc,na.rm=T),na.rm=T)))
  
lbls=paste(aCGH$Band.Aid$Regions[[4]]$Chrom[bandDX],
    aCGH$Band.Aid$Regions[[4]]$Label[bandDX],sep="")

plot1 = list(c("axis(2,aCGH$Band.Aid$Regions[[4]]$Center[bandDX],tick=F,labels=lbls,las=2,cex.axis=1.5)"),
             c("axis(2,aCGH$Band.Aid$Regions[[4]]$Lower[bandDX], labels=F,cex.axis=1.5)"),
             c("abline(v=(0:nsmpl)+1/2,col=7,lty=1,lwd=1/3)"),
             c("axis(3,at=which(iclr==1),labels=sample.names[which(iclr==1)],cex.axis=1.1,las=2,col.axis=colorSet[1])"),
             c("axis(3,at=which(iclr==2),labels=sample.names[which(iclr==2)],cex.axis=1.1,las=2,col.axis=colorSet[2])")
             )




###################################################
### chunk number 23: 
###################################################
plot3 = list(c("mtext('legend: log2 T/C value',side=3,cex=1.3,line=1/4)"),
             c("axis(1,seq(from=-0.5,to=0.5,length=5),line=0, cex.axis=1.3)"))



###################################################
### chunk number 24: 
###################################################
plot4 = list(c("abline(v=0,col='gray77',lwd=1)"),
             c("points(W.lw,scanLoc,col='green',pch=3,cex=0.5)"),
             c("points(W.up,scanLoc,col='red',pch=3,cex=0.5)"),
             c("lines(W.lw,scanLoc,col='green',pch=3,cex=0.5)"),
             c("lines(W.up,scanLoc,col='red',pch=3,cex=0.5)"),
             c("axis(3)"),
             c("mtext(text='LOS',side=3,line=2,cex=.75)"),
             c("axis(2,at=scanLoc,labels=F, cex.axis=1.3)"))


###################################################
### chunk number 25: 
###################################################
plt.extras = list()
plt.extras$plot1 = plot1
plt.extras$plot2 = NA
plt.extras$plot3 = plot3
plt.extras$plot4 = plot4


###################################################
### chunk number 26: 
###################################################
plot3


###################################################
### chunk number 27: 
###################################################
z.value = "log2ratios.fitted"


###################################################
### chunk number 28: 
###################################################
x.lbls=aCGH$inventory[hc$order,c(1,8)]
y.lbls=aCGH$mapping.info[,c(5,6,8,10,12)]


###################################################
### chunk number 29: 
###################################################
xy.lbls=list(log2.ratio = round(aCGH$log2.ratios[,hc$order],4))


###################################################
### chunk number 30: 
###################################################
 resize="600x900"


###################################################
### chunk number 31: 
###################################################
sendplot(mat=mat, plot.calls=plot.calls, mai.mat=mai.mat,
         x=1:nsmpl,y=scanLoc,z=z,z.value=z.value, type="image",
         plt.extras=plt.extras, x.lbls=x.lbls, y.lbls=y.lbls,xy.lbls=xy.lbls, 
         spot.radius=2,up.left=c(83,97),low.right=c(430,635),resize=resize)



