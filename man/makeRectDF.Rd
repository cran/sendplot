\name{makeRectDF}

\alias{makeRectDF}

\title{Maps R x and y point coordinates to pixil coordinates for
  tool-tip interactivity}

\description{NOT CALLED BY USER. The makeRectDF function is utilized
  by makeImap to map a set of R x and y coordinates to their
  corresponding pixil x and y coordinates. It also sets up data.frames
  of tool-tip information for display purpses}

\usage{
makeRectDF(Splot,
            xlim, ylim,
            x.left,y.top,
            x.right,y.bottom,
            boundingPt,
            x.labels=NA,
            y.labels=NA,
            xy.labels=NA,
            x.links=NA,
            y.links=NA,
            xy.links=NA,
            asLinks=NA,
            x.images=NA,
            y.images=NA,
            xy.images=NA)
}

\arguments{
  
  \item{Splot}{An Object of the class Splot}
  \item{xlim}{x limit of figure}
  \item{ylim}{y limit of figure}
  \item{x.left}{numeric vector of left x values for interactive rectangles}
  \item{y.top}{numeric vector of top y values for interactive rectangles}
  \item{x.right}{numeric vector of right x values for interactive rectangles}
  \item{y.bottom}{numeric vector of bottom y values for interactive rectangles}
  \item{boundingPt}{List with up.left and low.right pixil coordinates of
   the desired interactive figure's plotting region, as determined by
   \code{automapPts}}


 \item{x.labels}{data frame of n x m which contains values relating to
    the x axis. n should be the length of the x.left argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{y.labels}{data frame of n x m which contains values relating to
    the y axis. n should be the length of the y.top argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{xy.labels}{list of matricies. All matricies should be of n x m
    where n is the length of x.left and m is the length of y.top. This
    information is displayed in the interactive plot window}
  \item{x.links}{data frame of n x m which contains web addresses
    for links relating to the x axis. n should be the length of the x.left
    argument. m columns contains information regarding sample. This
    information is displayed as hyperlinks in the interactive plot
    window.}
  \item{y.links}{data frame of n x m which contains web addresses
    for links relating to the y axis. n should be the length of the y.top
    argument. m columns contains information regarding sample. This
    information is displayed as hyperlinks in the interactive plot
    window.}
  \item{xy.links}{list of matricies. All matricies should be of n x m
    where n is the length of y.top and m is the length of x.left. This
    information is displayed in the interactive plot window as
    hyperlinks. The values in these matricies should be complete web
    address}
  \item{asLinks}{contains complete web address for points that should be
    treated as hyperlinks. Should be equal to the length of x.left}
 \item{x.images}{data frame of n x m which contains paths to images relating to the xpos.  n should be the length of the xpos
    argument if xy.type is "image.midpoints" and length of xpos - 1 if xy.type is
    "image.boundaries" or "image.box". m columns contains information
    regarding sample. This information is displayed as images in the
    interactive plot window.}
  \item{y.images}{data frame of n x m which contains paths to images
  relating to the ypos.  n should be the length of the ypos argument if xy.type is "image.midpoints" and length of ypos - 1 if xy.type is
    "image.boundaries" or "image.box". m columns contains information
    regarding sample. This information is displayed as images in the
    interactive plot window.}
  \item{xy.images}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x when xy.type is
    "image.midpoint". All matricies should be of n x m
    where n is the length of y -1 and m is the length of x - 1 when xy.type is
    "image.boundaries" or "image.box". This information is displayed in
    the interactive plot window as images. The values in these
    matricies should be complete paths to images}



 

}

\details{

  This function uses the x and y limits of the R plots and the boundaries
  of the figure to convert plot points to pixil coordinates.

  %%

  The function will make a rectangle region using the coordinates in
  x.left, y.top, x.right, y.bottom.  The first rectangle would have the
  coordinates (x.left[1], y.bottom[1]),(x.left[1], y.top[1]),
  (x.right[1], y.top[1]), (x.right[1], y.bottom[1]). etc.

  %%

  The data matricies are checked for proper lengths and returned as a MapObj.
  
  
}

\value{List containing objects with interactive information.}

\note{NOT CALLED BY USER. INTERNAL HELPER FUNCTION}

\author{Lori A. Shepherd, Daniel P. Gaile}

\seealso{\code{\link{makeImap}}, \code{\link{automapPts}}}

\examples{
# not called by user
}

\keyword{methods}





