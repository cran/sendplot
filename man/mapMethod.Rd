\name{mapMethod}

\alias{mapMethod}

\title{Finds Point Coordinates}

\description{NOT CALLED BY USER. The mapMethod is utilized by the
  getBound function to find the up.left and low.right pixil coordinates
  of a given figures plotting bounding box}

\usage{
mapMethod(automap.method,
          temp)

}

\arguments{
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are "mode" or "median"}
  \item{temp}{binary matrix indicating where tif images differ, output
    from getBounds}
}

\details{

  The mapMethod function takes output generated from the
  \code{getBounds} function. This output is a comparison of two
  pixmapRGB objects as a logical matrix, 0 if equal and 1 if
  different. The map method finds the areas of difference, calculating
  the upper left and lower right figure boundaries. These boundaries are
  used in converting R plot coordinates to pixil coordinates. 
  %
  %
  The boundary regions currently can be calculated by two techniques
  "mode" or "median".
  Median will take the central location of the regions found.
  Mode will take the median of the longest row and column to find the location
  of each region.
  
}

\value{A list with up.left and low.right bounding coordinates in pixils
  or NA if could not map correctly}

\note{NOT CALLED BY USER.  INTERNAL HELPER FUNCTION}

\author{Lori A.Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeImap}}, \code{\link{automapPts}}, \code{\link{getBounds}}}

\examples{
# not called by user
}

\keyword{methods}
\keyword{dplot}





