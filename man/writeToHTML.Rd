\name{writeToHTML}

\alias{writeToHTML}
\alias{writeToHTML1}
\alias{writeToHTML2}

\title{Writes Data Frame Entries to File In Interactive Format}

\description{NOT CALLED BY USER. The writeToHTML functions are utilized
  by makeSplot to access entries of a data frame and convert entries
  into a format for interactive display.}

\usage{
writeToHTML1(obj,DFs, iType)

writeToHTML2(obj,DFs, iType)

}

\arguments{
  \item{obj}{list containing data frames dat and dat2. This object is output from
  the makeScatterDF or makeImageDF function}
\item{DFs}{output from makeCharacter function}
\item{iType}{indication of what type of image map region. Currently
  supported types are circle, rect, poly, or default}
}

\details{

  The writeToHTML functions are utilized by the makeSplot when
  an interactive figure is desired.  It takes properly formated data and based
  on what imagemap region (i.e. circle, rect, poly, or default), writes
  data to an HTML file.  

  %
  %
  HTML1 is utilized when using header = "v1"
  HTML2 is utilized when using header = "v2" or "v3"
  The data is displayed in different ways depending on the header.

  %
  %
  users are encouraged to see vignette for better descriptions and
  examples 
  
}

\value{Line by Line output}


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

\seealso{\code{\link{makeSplot}}, \code{\link{writeArea}}, \code{\link{sp.header}}}

\examples{

# not called by user

}

\keyword{methods}
\keyword{print}





