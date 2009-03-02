\name{spheader}
\docType{data}
\alias{v1.header}
\alias{v2.header}
\alias{v3.header}
\alias{sp.header}

\title{HEADER INFORMATION FOR HTML FILE}

\description{
  Stores header information, tooltip information, for html file
}

\format{Character vector containing lines of html header information}

\details{
  This file contains header information for an html file. It also
  contains javascript tooltip necessary for interactive plot. The
  different versions have different features or work on different web
  browsers.
  v1 works well with firefox and displays information in the upper right
  corner of the web browser
  v2 works well with firefox and displays information at the mouse
  location.
  v3 is the same as v2 except it allows control of the html window
  size. The default window size is 800x1100. To change window size,
  alter the window.size argument in \code{makeSplot}, \code{imagesend},
  \code{xy.send}, or \code{heatmap.send}.
  
}

\source{
  http://www.walterzorn.com/tooltip/tooltip\_e.htm
}

\references{
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html
  
}

\note{This dataset is used within the sendplot function. There is no
  need for the user to ever call this dataset.}

\keyword{datasets}





