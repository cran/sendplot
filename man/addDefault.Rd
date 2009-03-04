\name{addDefault}

\alias{addDefault}

\title{ADDS DEFAULT TOOL-TIP REGION}

\description{
  The default region in an html image map is any part of the figure not already
  specified with a different region (i.e. rect, circle, poly). This
  function adds tool-tip information to the default region. 
}

\usage{

addDefault(Splot,
          data=NA,
          data.labels=NA,
          links=NA,
          links.labels=NA,
          asLink=NA,
          font.type="Helvetica",   
          font.color="black",  
          font.size="12",     
          bg.color='#D6E3F6', 
          returnVl=TRUE,
          saveFlag=FALSE,
          saveName="Splot.RData")

}


\arguments{
  \item{Splot}{An Object of the class Splot}
  \item{data}{character vector containing data to be displayed in tool-tip}
  \item{data.labels}{names descripting data vector}
  \item{links}{character vector containing complete web address for
    hyperlinks within tool-tip}
  \item{links.labels}{names describing links vector}
  \item{asLink}{complete web address for area to be treated as hyperlink}
  \item{font.type}{font type for tool-tip. Currently support fonts are
    Arial, Helvetica,and sans-serif}
  \item{font.color}{font color for tool-tip}
  \item{font.size}{font size in tool-tip}
  \item{bg.color}{background color of tool-tip}
  \item{returnVl}{logical indicating if Splot object should be returned}
  \item{saveFlag}{logical indicating if Splot object should be saved to
    a file}
  \item{saveName}{if saveFlag, path and file name to save object}
}

\details{

  This function takes in data and links vectors, and converts into proper
  syntax for tool-tip. It also sets up default tool-tip display
  regarding font color, size, and type, as well as background color. 
}

\value{
  The Splot object is updated to include a Default.Obj containing
  vectors in proper syntax and tool-tip display information.
}

\references{
  
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm

}

\note{
  
  There can only be one default for a given layout of figures. If this
  is run more than once it will override previous calls.
  
}

\author{Lori A.Shepherd, Daniel P. Gaile}

\seealso{\code{\link{initSplot}}}

\examples{

library(sendplot)
library(rtiff)

# sets up matrix for layout
mat = matrix(1,nrow=12, ncol=13)
mat[9:12,] = 2
mat[,9:13] = 3
mat[1:2,] = 4

# sets up figure margins
mai.mat = matrix(.5,ncol=4,nrow=4)

# vector of image values
myX = c(-1,-10,1,10,-5,0)

# plot calls
plot.calls = c("boxplot(count ~ spray, data = InsectSprays, col = 'lightgray')", "plot(1:3,1:3, col='blue', xlab='', ylab=''); points(1:2, 2:3, col='red')", "image(1:2,1:3, z=matrix(myX,ncol=3,nrow=2), xlab='', ylab='')","plot(cos, xlim = c(-pi,3*pi), n = 1001, col = 'blue', xlab='', ylab='')" )
plt.extras=list(figure1= "rect(xleft=c(3,1), ytop=c(25,5),xright=c(4,2),  ybottom=c(20,0));title(main='A', cex=3)", figure2="polygon(x=c(2,2.5,3,2.5), y=c(1,2.5,1,1.5));title(main='B', cex=3)", figure3 ="title(main='C', cex=3)", figure4="title(main='D', cex=3)")


# initialize Splot object
Splot = initSplot(mat, plot.calls, mai.mat = mai.mat,plot.extras =plt.extras)

# add default
Splot = addDefault(Splot, data=c("This is default", "data2"), data.labels=c("label", "d2"), links=c("http://www.buffalo.edu, http://www.bioinformatics.buffalo.edu", "http://www.bioinformatics.buffalo.edu"), links.labels=c("hyp1", "hyp2"),font.size=10, font.type="arial", bg.color="gray", font.color="yellow")



#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct


# make plot that has interactive default 
Splot = makeSplot(Splot, fname.root="exToy",dir=direct, returnObj=TRUE)

}

\keyword{methods}





