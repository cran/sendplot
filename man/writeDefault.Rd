\name{writeDefault}

\alias{writeDefault}
\alias{writeDefault1}
\alias{writeDefault2}

\title{Writes Default Tool-tip Region to imagemap of HTML}

\description{NOT CALLED BY USER. If a default toop-tip region has been
  set using \code{addDefault}, the information is converted to proper
  format and wrote to HTML file}

\usage{
writeDefault1(Splot)
writeDefault2(Splot)
}

\arguments{
  \item{Splot}{An Object of the class Splot}
}

\details{

  If a default toop-tip region has been set using \code{addDefault}, the
  information is converted to proper format and wrote to HTML file
  %
  %
  writeDefault1 is utilized when using header = "v1"
  writeDefault2 is utilized when using header = "v2" or "v3"
  The data is displayed in different ways depending on the header.

  %
  %
  users are encouraged to see vignette for better descriptions and
  examples 


}

\value{Line for default tool-tip region added to imagemap section of HTML}

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

\seealso{\code{\link{makeSplot}}, \code{\link{addDefault}},  \code{\link{sp.header}}}

\examples{

# not called by user

}

\keyword{methods}
\keyword{print}





