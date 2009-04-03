\name{initSplot}

\alias{initSplot}

\title{Creates A Sendplot 'Splot' Object}

\description{The initSplot function creates a sendplot 'Splot' object. A
  Splot object holds all necessary elements to make a static layout of
  images and, through other functions, all elements to  make any of
  those images interactive with tool-tip display content}

\usage{

initSplot(mat,
          plot.calls,
          Iflag=NA,
          figTypes=NA,
          mai.mat=NA,
          mai.prc=FALSE,
          plot.extras = NA,
          source.plot=NA,
          image.size="800x1100",
          pointsize=12,
          res=NA,
          ps.paper="letter",
          ps.width=8,
          ps.height=11,
          returnVl=TRUE,
          saveFlag=FALSE,
          saveName="Splot.RData")
}

\arguments{
  \item{mat}{matrix indicating layout. This argument will be passed into
    the graphics package layout call as mat.Each value in the matrix
    must be '0' or a positive integer.  If N is the largest positive
    integer in the matrix, then the integers 1,...,N-1 must also
    appear at least once in the matrix. '0' indicates region of no plotting}
  \item{plot.calls}{character vector containing plot calls}
  \item{Iflag}{Logical vector indicating if the plot in the layout is
   interactive}
  \item{figTypes}{Character vector indicating the type of
    plot. Currently this argument is not needed, but will be useful for
    extensions that will be made in future versions.}
  \item{mai.mat}{n x 4 matrix of values to be passed in for each plots
    par mai. n is equal to the length of plot.calls. If NA, uses default
    margins}
  \item{mai.prc}{logical indicating if mai mat values are percentages
    or hard coded values. If mai.proc is T, indicates percentage.}
  \item{plot.extras}{List of length equal to the number of
    plot.calls. This object is a list of lists. The sublists contain any
    additional plotting calls that should be executed for the plot. Each
    entry must be a character vector. If no additional plotting is
    required, an NA should be used}
  \item{source.plot}{Indicates what image output the application should  produce
    postscript, tiff, png, or jpeg. It can be a character vector of any
    combination of ps, png, jpeg, or tiff. If NA, the default will make a png file}
  \item{image.size}{character indicating resize value of image,'width'x'height'}
  \item{pointsize}{pointsize of image. passed into device call}
  \item{res}{resolution of image, passed into device call if png or jpeg}  
  \item{ps.paper}{postscript paper argument if postscript is created}
  \item{ps.width}{postscript width argument if postscript is created}
  \item{ps.height}{postscript height argument if postscript is created}
  \item{returnVl}{Should Splot object be returned}
  \item{saveFlag}{Should Splot object be saved}
  \item{saveName}{If saveFlag, path file name to save object}
}

\details{

  This functio initializes a Splot object. This object stores
information for constructing a layout of figures, as well as making any
of thos figures interactive in a html webpage utilizing java tool-tip.

%%

  Users are encouraged to read the package vignette which includes
  a detailed discussion of all function arguments as well as several
  useful examples.
}

\value{If returnVl, an object of the class 'Splot'}

\references{

  http://www.R-project.org
  
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm

}

\note{

  This function only sets up an SPlot object. It does not make the interactive figure.
}

\author{Lori A. Shepherd, Dan P. Gaile}

\seealso{\code{\link{sendplot}}, \code{\link{makeImap}},
\code{\link{makeSplot}}, \code{layout}}

\examples{
# Please see vignette or makeSplot for example
}

\keyword{methods}





