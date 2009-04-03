\name{addBounding}

\alias{addBounding}

\title{Adds Markers For Auto-detection of Bounding Points}

\description{NOT CALLED BY USER. The addBounding function is utilized by
  the makeImap function to create a file with bounding locations for a
  desired interactive plot.}

\usage{

addBounding(Splot,
            figure,
            bb.clr = "blue",
            bb.cex = 2,
            boundFileName = "SplotDot",
            dir="./")

}

\arguments{
  \item{Splot}{An Object of the class Splot}
  \item{figure}{The numeric indication for which figure's bounding box
    points should be displayed}
  \item{bb.clr}{color of bounding points}
  \item{bb.cex}{size of bounding points}
  \item{boundFileName}{name to use for .png file name}
  \item{dir}{directory path to where files should be created}
}

\details{

  This function adds phantom points to a figure to determine a plots
  bounding box limits. 
  
}

\value{A .png file is created with points at a figures plotting box
  bounds. This file is used for automatic mapping of points}

\note{NOT CALLED BY USER.  INTERNAL HELPER FUNCTION}

\author{Lori A.Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeImap}}}

\examples{
  # not called by user
}

\keyword{methods}
\keyword{aplot}





