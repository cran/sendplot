\name{writeArea}

\alias{writeArea}
\alias{writeCircle.1}
\alias{writeCircle.2}
\alias{writeRect.1}
\alias{writeRect.2}
\alias{writePoly.1}
\alias{writePoly.2}

\title{Writes Data Frame Entries to File In Interactive Format}

\description{NOT CALLED BY USER. These functions are utilized by the
  writeToHTML functions to access entries of a data frame and convert
  entries into a format for interactive display. Output is written to
  HTML file.}

\usage{

writeCircle.1(DFs, cdat, ndat, obj)
writeCircle.2(DFs, cdat, ndat, obj)
writeRect.1(DFs, cdat, ndat)
writeRect.2(DFs, cdat, ndat, obj)
writePoly.1(DFs, cdat, ndat, obj)
writePoly.2(DFs, cdat, ndat, obj)

}

\arguments{
  \item{DFs}{output from makeCharacter function}
  \item{cdat}{Data frame containing data}
  \item{ndat}{vector containing names/identifiers for cdat}
  \item{obj}{list containing data frames dat and dat2. This object is output from
  the makeScatterDF or makeImageDF function}
}

\details{

  The writeArea functions are utilized by the writeToHTML functions when
  an interactive figure is desired. It takes properly formated data and based
  on what imagemap region (i.e. circle, rect, poly, or default), writes
  data to an HTML file.  

  %
  %
  function.1 is utilized when using header = "v1"
  function.2 is utilized when using header = "v2" or "v3"
  The data is displayed in different ways depending on the header.

  %
  %
  users are encouraged to see vignette for better descriptions and
  examples 

}

\value{Line by Line output written to HTML file}


\references{
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm

}


\note{
  NOT CALLED BY USER. INTERNAL HELPER FUNCTION

  The code used to create the javascript embedded in html file is a
  modified version of the javascript code or from the open source
  tooltip library. see reference links
  
}

\author{Lori A. Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeSplot}}, \code{\link{writeToHTML}}, \code{\link{sp.header}}}

\examples{

# not called by user
}

\keyword{methods}
\keyword{print}





