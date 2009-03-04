\name{xy.send}

\alias{xy.send}

\title{INTERACTIVE SCATTERPLOT}

\description{
  
This function is a wrapper to sendplot that will create a single
interactive scatterplot

}

\usage{
xy.send(plot.call, 
        x.pos,
        y.pos,
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
  \item{plot.extras}{List of additional plotting calls that should be
    executed for the plot.}
  \item{mai.mat}{1 x 4 matrix of values to be passed in for each plots
    par mai. n is equal to the length of plot.calls. If NA, uses default
    margins.}
  \item{mai.prc}{logical indicating if mai mat values are percentages
    or hard coded values. If mai.proc is T, indicates percentage.}
  \item{xy.labels}{list of matricies. All matricies should be of n x m
    where n is the length of xpos or ypos. This information is displayed in
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
  single interacive scatterplot. See \code{initSplot}, \code{makeImap}, and
  \code{makeSplot} for more information.
  
  %%

  Users are encouraged to read the package vignette which includes
  a detailed discussion of all function arguments as well as several
  useful examples.


}

\value{Creates a static and interactive scatterplot}

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

\seealso{
  \code{\link{initSplot}}, \code{\link{makeImap}},
  \code{\link{makeSplot}}, \code{\link{imagesend}},
  \code{\link{heatmap.send}}, \code{\link{sendplot-package}},
  \code{\link{sendxy}}
 
}

\examples{

library(sendplot)
library(rtiff)


plot.call=c("plot(mtcars$hp,mtcars$mpg,xlab='gross horsepower',
                   ylab='miles per gallon',axes=FALSE,pch=mtcars$cyl,
                   col=mtcars$am+1,cex=0.875,
                   main='Motor Trend Car Road Tests')")

plot.extras=c("axis(1);axis(2);
              legend(200,25,pch=rep(c(4,6,8),2),col=c(rep(1,3),rep(2,3)),
                     legend=paste(rep(c(4,6,8),2),'cylinders,',
                              c('automatic','manual')[c(rep(1,3),rep(2,3))]),cex=0.875)")


#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct


xy.send(plot.call=plot.call,
       y.pos=mtcars$mpg,x.pos=mtcars$hp,
       xy.labels = data.frame(name=rownames(mtcars),mtcars=mtcars), 
       plot.extras=plot.extras,
       image.size="800x600",
       fname.root="exPlotXY", dir = direct, font.size=18)



}

\keyword{methods}





