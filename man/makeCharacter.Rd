\name{makeCharacter}

\alias{makeCharacter}

\title{Converts Data Frame Entries Into Type Character}

\description{NOT CALLED BY USER. The makeCharacter function is utilized
  by makeSplot to convert data frame entries from type factor to type
  character for more efficient subsetting and writting to files.}

\usage{

 makeCharacter(DF)
}

\arguments{
  \item{DF}{list containing data frames dat and dat2. This object is output from
  the makeScatterDF or makeImageDF function}
}

\details{
  This function is a convenience function. It converts the data frames
  of a MapObj (output from \code{makeImap}) into character
  matricies. This speeds up the writing of data to the html file.   
  
}

\value{List continaing character versions of data frames.}

\note{NOT CALLED BY USER. INTERNAL HELPER FUNCTION}

\author{Lori A. Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeSplot}}}

\examples{
# not called by user
}

\keyword{methods}





