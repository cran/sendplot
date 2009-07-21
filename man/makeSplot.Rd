\name{makeSplot}

\alias{makeSplot}

\title{Makes Static or Interactive Files}

\description{

  This function acts on an Splot object to make a static (png, jpeg, or
  postscript) image and if desired an interactive html.  
}

\usage{

makeSplot(Splot,
          fname.root="Splot",
          dir="./",
          overwriteSourcePlot = NA,
          makeInteractive=TRUE,
          overrideInteractive=NA,
          Default=TRUE,
          header="v3",
          window.size = "800x1100", 
          returnObj = TRUE)


}

\arguments{
  \item{Splot}{An Object of the class Splot}
  \item{fname.root}{Base name to use for all file created}
  \item{dir}{directory path to where files should be created}
  \item{overwriteSourcePlot}{character or character vector, should
  static image generated be postscript, png, jpeg, or tiff. If this is
  NA, it uses what is specified in Splot object.}
  \item{makeInteractive}{logical, should interactive html file be
    created. If FALSE, only static (ps, png, or jpeg) files is generated}
  \item{overrideInteractive}{logical of length equal to
    Splot\$nfig. Indicates which figures of the layout should be
    interactive; if NA uses what is specified in Splot object}
  \item{Default}{logical, If default tool-tip region is set should it be
    included in html}
  \item{header}{May either be v1,v2, or v3. This determines which
    tooltip header will be in the html file. Each version has different
    features or works well with different web browsers. see sp.header
    for details.}
  \item{window.size}{size of the html window. Only effective when header=v3}
  \item{returnObj}{Should Splot object be returned}
 }

\details{
  
  users are encouraged to read vignette for more details and several
  examples.

}

\value{
  Generates a static image (ps, png, or jpeg).
  If makeInteractive, an interactive .html file is generated
  If returnObj, Splot object is returned. 
}

\references{
  http://www.R-project.org
  
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm


}

\note{

  The interactive html plot currently only works in web browsers that
  implement java script.
  
  The code used to create the javascript embedded in html file is a
  modified version of the javascript code or from the open source
  tooltip library. see reference links

}

\author{Lori A. Shepherd, Daniel P. Gaile}

\seealso{\code{\link{sendplot}}, \code{\link{initSplot}},
  \code{\link{makeImap}}, \code{\link{heatmap.send}},
  \code{\link{imagesend}}, \code{\link{xy.send}}}

\examples{


#
#   Please see vignette for full example
#

library(sendplot)
library("rtiff")

# set up layout matrix
mat = matrix(1,nrow=12, ncol=13)
mat[9:12,] = 2
mat[,9:13] = 3
mat[1:2,] = 4

# set margins
mai.mat = matrix(.5,ncol=4,nrow=4)

# set matrix to be used in image call
myX = c(-1,-10,1,10,-5,0)


plot.calls = c(
"boxplot(count ~ spray, data = InsectSprays,col='lightgray')",
"plot(1:3,1:3,col='blue',xlab='',ylab=''); points(1:2,2:3,col='red')",
"image(1:2,1:3, z=matrix(myX,ncol=3,nrow=2), xlab='',ylab='')",
"plot(cos, xlim=c(-pi,3*pi), n=1001,col ='blue',xlab='',ylab='')"
)


plt.extras=list(
figure1="rect(xleft=c(3,1),
ytop=c(25,5),xright=c(4,2),ybottom=c(20,0));title(main='A', cex=3)",
figure2="polygon(x=c(2,2.5,3,2.5), y=c(1,2.5,1,1.5));title(main='B',
cex=3)",
figure3 ="title(main='C', cex=3)",
figure4="title(main='D', cex=3)"
)


#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct

#
#
# make Splot object
#
#


# initialize Splot object 
Splot = initSplot(mat, plot.calls, mai.mat=mai.mat,
                  plot.extras=plt.extras)

# first look/get plot output  -- makes static image 
Splot = makeSplot(Splot, fname.root="exToy", makeInteractive=FALSE,
                  dir=direct, returnObj=TRUE)






#
#
# Add regions for interactive toop-tip display 
#
#


# makes two rectangle regions interactive in figure 1
Splot = makeImap(Splot, figure=1, xy.type="rect",
                 x.pos=c(3,1), y.pos=c(25,5),
                 x.right.pos=c(4,2),  y.bottom.pos=c(20,0),
                 x.labels = as.data.frame(list(
                                label=c("rect1","rect2"),
                                info=c("im a link", "im a link"))),
                 asLinks = "http://www.buffalo.edu", 
                 y.labels = c("d1",NA),
                 fname.root="exToy", dir=direct, spot.radius=10,
                 font.size=20, font.color="cyan", bg.color="black")

# makes one of the outlier points interactive in the figure 1
Splot = makeImap(Splot, figure=1, xy.type="circle", x.pos=3, y.pos=7,
                 x.labels = list(label="point", info="im a link"),
                 asLinks = "http://www.buffalo.edu", dir=direct,
                 y.labels = "7", fname.root="exToy", spot.radius=20,
                 font.type="arial", font.size="20",
                 font.color="hotpink", bg.color="blue")


# make polygon region interactive in figure 2 
Splot = makeImap(Splot, figure=2, xy.type="polygon",
                 x.pos=c(2,2.5,3,2.5), y.pos=c(1,2.5,1,1.5),
                 x.labels = as.data.frame(list(label = "Polygon")),
                 asLinks="http://www.bioinformatics.buffalo.edu",
                 y.labels = as.data.frame(list(value="data1",
                                               info="Im alink")),
                 fname.root="exToy", dir=direct, bb.cex=5, font.size=30,
                 font.type="sans-serif", font.color="purple")
# make data set of three points interactive in figure 2 
x.lbls = as.data.frame(list(labels=c("point1", "point2", "points3"),
                         info=c("im a link", "im a link", "im alink"),
                         numVl = c("num1", "num2", "num3"),
                         num = c(1,2,3)))
y.links = as.data.frame(list(
  hyp1=c("http://www.buffalo.edu",
         "http://www.bioinformatics.buffalo.edu,http://www.buffalo.edu",
         "http://www.bioinformatics.buffalo.edu"),
   hyp2=c(NA, "http://www.buffalo.edu",NA)
))
asLinks=c("http://www.buffalo.edu",
          "http://www.bioinformatics.buffalo.edu","http://www.buffalo.edu")
Splot = makeImap(Splot, figure=2, xy.type="points", x.pos=1:3, y.pos=1:3,
           x.labels = x.lbls,  y.links=y.links,asLinks=asLinks,
           fname.root="exToy",bb.cex=5, spot.radius=20,dir=direct,
           font.color="green",bg.color="", font.size="14")


# makes each box of image interactive in figure 3 
x.lbls=as.data.frame(list(Xnuml = 1:2,
                          Xvl2 = c("mx1", "mx2")))
y.lbls=as.data.frame(list(ynum1 = 1:3,
                          Yvl2 = c("my1", "my2", "my3")))
xy.lbls=list(label = matrix(c("image.box1","image.box2",
                              "image.box3","image.box4",
                              "image.box5","image.box6"),ncol=2),
             numMat1 = matrix(1:6, ncol=2),
             vlMat2 = matrix(c("xy1","xy2","xy3","xy4","xy5","xy6"),
                             ncol=2))
x.links=as.data.frame(list(
   Xhyp1 = c("http://www.buffalo.edu,http://bioinformatics.buffalo.edu",
             NA)))
y.links=as.data.frame(list(Yhyp2 = c(NA, "http//www.buffalo.edu", NA)))
xy.links=list(XYhyp = matrix(c(NA, "http://www.buffalo.edu", NA,
                              "http://buffalo.edu", NA, NA), ncol=2))
Splot = makeImap(Splot, figure=3, xy.type="image.box",
                 x.pos= c(.5,1.5,2.5), y.pos=c(.5,1.5,2.5,3.5),
                 x.labels=x.lbls, y.labels = y.lbls, xy.labels=xy.lbls,
                 x.links=x.links, y.links=y.links, xy.links=xy.links,
                 fname.root="exToy", bb.cex=5, spot.radius=10,dir=direct)




#
# add Default tool-tip 
#
Splot = addDefault(
        Splot,
        data=c("This is default", "data2"),
        data.labels=c("label", "d2"),
        links=c("http://www.buffalo.edu,http://www.bioinformatics.buffalo.edu",
                "http://www.bioinformatics.buffalo.edu"),
        links.labels=c("hyp1", "hyp2"),
        font.size=10, font.type="arial", bg.color="gray", font.color="yellow")




#
#
# Now that regions have been set as interactive
#    make interactive html 
#
#
Splot = makeSplot(Splot, fname.root="exToy", dir=direct, returnObj=TRUE)





#
#
#  Now demonstarte removing interactive regions for figures
#  Note: to see effect must remake plot with the makeSplot
#  above


# removes outlier point interactive in figure 1
# since it was the second set added subset = 2
Splot = removeImap(Splot, figure=1, subset=2)


# remove default region toop-tip
Splot = removeImap(Splot, figure="Default")


# remove all interactive sets for figure 2
Splot = removeImap(Splot, figure=2)






}

\keyword{methods}
\keyword{aplot}
\keyword{iplot}




