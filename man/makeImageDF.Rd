\name{makeImageDF}

\alias{makeImageDF}

\title{Maps R x and y point coordinates to pixil coordinates for
  tool-tip interactivity}

\description{NOT CALLED BY USER. The makeImageDF function is utilized
  by makeImap to map a set of R x and y coordinates to their
  corresponding pixil x and y coordinates. It also sets up data.frames
  of tool-tip information for display purpses}

\usage{
makeImageDF(Splot,
            xy.type,
            xlim, ylim,
            x.pos,y.pos,
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
  \item{xy.type}{Indication of how the xpos and ypos values should be
    treated.Current options are "image.midpoint","image.boundaries", or
    "image.box".See details or vignette.}
  \item{xlim}{x limit of figure}
  \item{ylim}{y limit of figure}
  \item{x.pos}{numeric vector of x values for interactive points}
  \item{y.pos}{numeric vector of y values for interactive points}
  \item{boundingPt}{List with up.left and low.right pixil coordinates of
   the desired interactive figure's plotting region, as determined by
   \code{automapPts}}
  \item{x.labels}{data frame of n x m which contains values relating to
    the xpos. n should be the length of the xpos argument if xy.type is
    "image.midpoints" and length of xpos - 1 if xy.type is
    "image.boundaries" or "image.box". m columns contains information
    regarding sample. This information is displayed in the interactive
    plot window}
  \item{y.labels}{data frame of n x m which contains values relating to
    the yvec. n should be the length of the ypos argument if xy.type is
    "image.midpoints" and length of ypos - 1 if xy.type is
    "image.boundaries" or "image.box". m columns contains information
    regarding sample. This information is displayed in the interactive
    plot window}
  \item{xy.labels}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x when xy.type is
    "image.midpoint". All matricies should be of n x m
    where n is the length of y -1 and m is the length of x - 1 when xy.type is
    "image.boundaries" or "image.box".  This information is displayed in
    the interactive plot window}
  \item{x.links}{data frame of n x m which contains web addresses
    for links relating to the xpos.  n should be the length of the xpos
    argument if xy.type is "image.midpoints" and length of xpos - 1 if xy.type is
    "image.boundaries" or "image.box". m columns contains information
    regarding sample. This information is displayed as hyperlinks in the
    interactive plot window.}
  \item{y.links}{data frame of n x m which contains web addresses
    for links relating to the ypos.  n should be the length of the ypos
    argument if xy.type is "image.midpoints" and length of ypos - 1 if xy.type is
    "image.boundaries" or "image.box". m columns contains information
    regarding sample. This information is displayed as hyperlinks in the
    interactive plot window.}
  \item{xy.links}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x when xy.type is
    "image.midpoint". All matricies should be of n x m
    where n is the length of y -1 and m is the length of x - 1 when xy.type is
    "image.boundaries" or "image.box". This information is displayed in
    the interactive plot window as hyperlinks. The values in these
    matricies should be complete web address}
  \item{asLinks}{contains complete web address for points that should be
    treated as hyperlinks. May be a data.frame or matrix of n x m where
    n is the length of ypos and m is the length of xpos, a vector of
    length xpos indicating xpos specific links that will be repeated, a
    vector of length ypos indicating ypos specific links that will be
    repeated, a non NA value of length 1 that will be repeated for all
    points, or a vector of length xpos*ypos. This assumes xy.type is
    "image.midpoints". If xy.type is "image.boundaries" or "image.box",
    then the lengths should be xpos-1 and ypos - 1. instead of xpos and
    ypos}
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

  image.box will make rectangular regions for an image interactive based
  on the given xpos and ypos boundaries.
  image.midpoints and image.boundaries will make the center of the
  regions of an image interactive based on the given xpos and
  ypos. image.midpoints assumes the xpos and ypos are the actual
  locations. image.boundaries assumes the xpos and ypos are the
  boundary cuts and calculates the midpoints.

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





