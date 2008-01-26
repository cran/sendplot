\name{getBounds}

\alias{getBounds}

\title{Compares Two Tif Images To Find Where They Differ}

\description{NOT CALLED BY USER. The getBounds function is utilized by
  the automapPts function to compare two tif images for differences}

\usage{
getBounds(channelClr,
          tif.fin,
          tif.dot,
          automap.method="mode")
}

\arguments{
  \item{channelClr}{Which color channel to compare: "red", "blue", "green"}
  \item{tif.fin}{Path file name to tif image of plots without additional
    bounding point}
  \item{tif.dot}{Path file name to tif image with the given
    figure's additional bounding points displayed}
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are "mode" or "median"}

}

\details{

  This function is used by the automapPts function to retrieve up.left
  and low.right pixil coordinates of a figure. getBounds will check a
  certain channel (red, green, or blue) of two rtiff/pixmap pixmapRGB
  objects. It creates a logical matrix 0 if equal and 1 if
  different. This matrix gets passed into the mapMethod function to
  determine location.     

}

\value{A list with up.left and low.right bounding coordinates in pixils
  or NA if could not map correctly}

\note{NOT CALLED BY USER.  INTERNAL HELPER FUNCTION}

\author{Lori A.Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeImap}}, \code{rtiff}, \code{pixmap},
  \code{\link{automapPts}}, \code{\link{mapMethod}}}

\examples{
# not called by user
}

\keyword{methods}
\keyword{dplot}





