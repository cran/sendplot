\name{automapPts}

\alias{automapPts}

\title{Compares Images To Find Bounding Coordinates In Pixils}

\description{NOT CALLED BY USER. The automapPts function is utilized by
  makeImap to retrieve the up.left and lower.right pixil coordinates of
  a desired interactive figure}

\usage{
automapPts(Splot,
           fname.root="Splot",
           boundFileName="SplotDot",
           dir="./",
           automap.method="mode")


}

\arguments{
  \item{Splot}{An object of the class Splot}
  \item{fname.root}{Base name of the static version of plots}
  \item{boundFileName}{Base name of the version of plots with the given
    figure's additional bounding points displayed}
  \item{dir}{directory path to where files were created}
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are "mode" or "median"}
}

\details{

  This function retrieves the up.left and lower.right pixil coordinates
  of a desired interactive figure.
  The function assumes that a 'clean' image without additional bounding
  points and a 'bounding' image that has additional bounding points (see
  \code{\link{addBounding}}) are created. It converts these images to
  .tif files and uses the rtiff package readTiff to compare the images
  for differences, thus picking up the additional points.  

}

\value{A list with up.left and low.right bounding coordinates in pixils
  or NA if could not map correctly}

\references{
  
 Eric Kort (2006). rtiff: A tiff reader for R.. R package version 1.1.

}

\note{
  NOT CALLED BY USER.  INTERNAL HELPER FUNCTION

  utilizes readTiff from rtiff library
}

\author{Lori A.Shepherd, Daniel P.Gaile}

\seealso{\code{\link{makeImap}}, \code{rtiff}, \code{\link{getBounds}}, \code{\link{mapMethod}}}

\examples{

# not called by user

}

\keyword{methods}
\keyword{dplot}





