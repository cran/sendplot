\name{sendplot-package}

\docType{package}

\alias{sendplot-package}
\alias{sendplotPackage}
\alias{sendPlot}
\alias{Sendplot}


\title{sendplot package}

\description{The sendplot package creates an interactive layout of
  plots that may be viewed in a web browser.
}

\details{

  Users are encouraged to view vignette for more details and several
  useful examples.
  %
  %

  The user must initialize a Splot object (\code{initSplot})
  Then the user may optionally add interactive regions to any of the
  figures in the layout. (\code{makeImap}, \code{addDefault})
  A static image (postscript, png or jpeg), and/or an interactive HTML
  file is created (\code{makeSplot})
}

\references{

  http://www.R-project.org
  
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm

}

\note{
  
  The interactive html plot currently only works in web browsers that
  implement java script.
  
  The code used to create the javascript embedded in html file is a
  modified version of the javascript code or from the open source
  tooltip library. see reference links

}

\author{Lori A. Shepherd, Daniel P. Gaile, Lara Sucheston, Andrew Bruno,
Kenneth F. Manly}

\seealso{\code{\link{initSplot}}, \code{\link{makeImap}}, \code{\link{makeSplot}},
  \code{\link{sendplot}}, \code{layout}}

\keyword{package}





