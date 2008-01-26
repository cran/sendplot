\name{getPlotsBounds}

\alias{getPlotsBounds}

\title{Retrieves xlim and ylim coordinates for all plots}

\description{NOT CALLED BY USER. The getPlotsBounds function is utilized
by the makeSplot function to retrieve the xlim and ylim coordinates for
all plots of a given layout.}

\usage{
getPlotsBounds(Splot)
}

\arguments{
  \item{Splot}{An Object of the class Splot}
}

\details{

  This function takes a 'snapShot' of the active device. This gives
  details of all graphics operations. The function searches for the plot
  window indicating a new figure region, and retrieves the x and y limits of
  the figures. These limits are used to plot 'phantom' points to aid in
  converting R point coordinates to pixil coordinates.   

}

\value{A list with xlim and ylim ranges for all plots.}

\note{

  NOT CALLED BY USER. INTERNAL HELPER FUNCTION

  Utilizes the internal function getSnapshot()
}

\author{Lori A. Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeSplot}}, \code{\link{makeImap}}}

\examples{
# not called by users 
}

\keyword{methods}





