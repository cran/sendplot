\name{imagesend}

\alias{imagesend}

\title{INTERACTIVE IMAGE}

\description{
This function is a wrapper to sendplot that will create a single
interactive image
}

\usage{

imagesend(plot.call, 
           x.pos,
           y.pos,
           xy.type,
           plot.extras = NA,
           mai.mat=NA, mai.prc=FALSE,
           xy.labels=NA,
           image.size="800x1100",
           spot.radius = 5,
           fname.root="Splot",
           dir="./",
           window.size = "800x1100", 
           ...)  

}

\arguments{
  \item{plot.call}{character vector containing single plot call}
  \item{x.pos}{vector of x locations for interactive points}
  \item{y.pos}{vector of y locations for interactive points}
  \item{xy.type}{Indication of how the xpos and ypos values should be
    treated.Current options are "image.midpoint","image.boundaries", or
    "image.box".See details or vignette.}
  \item{plot.extras}{List of additional plotting calls that should be
    executed for the plot.}
  \item{mai.mat}{1 x 4 matrix of values to be passed in for each plots
    par mai. n is equal to the length of plot.calls. If NA, uses default
    margins.}
  \item{mai.prc}{logical indicating if mai mat values are percentages
    or hard coded values. If mai.proc is T, indicates percentage.}
  \item{xy.labels}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x when xy.type is
    "image.midpoint". All matricies should be of n x m
    where n is the length of y -1 and m is the length of x - 1 when xy.type is
    "image.boundaries" or "image.box".  This information is displayed in
    the interactive plot window}
  \item{image.size}{character indicating size of device.}
  \item{spot.radius}{radius of circle in pixels indicating area that
    will be interactive around the center of graphed points}
  \item{fname.root}{Base name to use for all files created.}
  \item{dir}{directory path to where files should be created. Default
    creates files in working directory}
  \item{window.size}{size of the html window}
  \item{...}{additional arguments to the makeImap function}
}

\details{
  
  This function is a wrapper for the sendplot function to create a
  single interacive image. See \code{initSplot}, \code{makeImap}, and
  \code{makeSplot} for more information.
  
  %%

  Users are encouraged to read the package vignette which includes
  a detailed discussion of all function arguments as well as several
  useful examples.


}

\value{Creates a static and interactive image}

\references{
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

\seealso{\code{\link{initSplot}}, \code{\link{makeImap}},
  \code{\link{makeSplot}}, \code{\link{sendplot-package}}, \code{\link{sendimage}}}

\examples{
library(sendplot)
library(rtiff)
library(stats)

mai.mat = matrix(c(1,1,1,1), ncol=4)

carsX  = as.matrix(mtcars)
carsX <- sweep(carsX, 2, colMeans(carsX, na.rm = TRUE))
        sx <- apply(X=carsX, MARGIN=2, FUN="sd", na.rm = TRUE)
        carsX <- sweep(carsX, 2, sx, "/")

plot.call="image(x=1:dim(carsX)[2],y=1:dim(carsX)[1], z=t(carsX),axes =
FALSE, xlab = '', ylab = '');axis(1,1:dim(carsX)[2],
labels=colnames(carsX),las = 2, line = -0.5, tick = 0,cex.axis =.8);
axis(4,1:dim(carsX)[1], labels=rownames(carsX),las = 2, line = -0.5,
tick = 0,cex.axis =.65)"

xy.labels=list(value=round(carsX,3))

x.labels=data.frame(label=colnames(carsX),
  description=c("Miles/(US) gallon","Number of cylinders",
    "Displacement (cu.in.)",
    "Gross horsepower",
    "Rear axle ratio",
    "Weight (lb/1000)",
    "1/4 mile time",
    "V/S",
    "Transmission (0 = automatic, 1 = manual)",
    "Number of forward gears",
    "Number of carburetors")
  )


#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct

imagesend(plot.call=plot.call, 
           x.pos= 1:dim(carsX)[2],
           y.pos= 1:dim(carsX)[1],
           xy.type = "image.midpoints",
           xy.labels=xy.labels,
           spot.radius = 5,
           fname.root="manImage", dir=direct,
           window.size = "800x1100", 
           x.labels=x.labels, mai.mat=mai.mat)  


}

\keyword{methods}





