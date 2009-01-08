\name{makeImap}

\alias{makeImap}

\title{Adds Interactive Information to 'Splot' Object}

\description{The makeImap function will add all information needed to
  make a given figure in a Splot Object interactive. This function may
  be run more than once for a given figure.}

\usage{
makeImap(Splot,
         figure=1,
         xy.type=NA,
         x.pos,
         y.pos,
         x.right.pos=NA, 
         y.bottom.pos=NA,
         spot.radius = 5,
         x.labels=NA,
         y.labels=NA,
         xy.labels=NA,
         x.links=NA,
         y.links=NA,
         xy.links=NA,
         asLinks=NA,
         x.images=NA,
         y.images=NA,
         xy.images=NA,
         sep.chr=":",
         font.type="Helvetica",
         font.color="black", 
         font.size="12",     
         bg.color="#D6E3F6",
         fname.root="Splot",
         dir="./",
         automap.method="mode",
         bb.clr=NA,
         bb.cex=2,
         returnVl=TRUE,
         saveFlag=FALSE,
         saveName="Splot.RData",
         cleanDir=TRUE)
}

\arguments{
  \item{Splot}{An Object of the class Splot}
  \item{figure}{Indicates which plot figure data corresponds to; this
    matches the numeric indication in the layout matrix.}
  \item{xy.type}{Indication of how the xpos and ypos values should be
    treated. Current options are "points", "image.midpoints",
    "image.boundaries", "image.box", "circle", "rect", and "poly".
    See details or vignette.}
  \item{x.pos}{numeric vector of x values for interactive points. If
    xy.type is "rect", a numeric vector of the x.left position of
    rectangle[s].}
  \item{y.pos}{numeric vector of y values for interactive points. If
    xy.type is "rect", a numeric vector of the y.top position of
    rectangle[s]}
  \item{x.right.pos}{If xy.type is "rect", a numeric vector of the
    x.right position of rectangle[s]}
  \item{y.bottom.pos}{If xy.type is "rect", a numeric vector of the
    y.bottom position of rectangle[s]}
  \item{spot.radius}{radius of circle in pixels indicating area that
    will be interactive around the center of graphed points.used when
    xy.type is "points", "image.midpoints","image.boundaries", or
    "circle"}
  \item{x.labels}{data frame of n x m which contains values relating to
    the xpos. This information is displayed in the interactive plot
    window. See details or vignette}
  \item{y.labels}{data frame of n x m which contains values relating to
    the ypos. This information is displayed in the interactive plot
    window. See details or vignette}
  \item{xy.labels}{list of matricies. All matricies should be of n x m.
    This information is displayed in the interactive plot window. See
    details or vignette}
  \item{x.links}{data frame of n x m which contains web addresses
    for links relating to the xpos. This information is displayed as
    hyperlinks in the interactive plot window. See details or vignette}
  \item{y.links}{data frame of n x m which contains web addresses
    for links relating to the ypos. This information is displayed as
    hyperlinks in the interactive plot window. See details or vignette}
  \item{xy.links}{list of matricies. All matricies should be of n x m.
    This information is displayed in the interactive plot window as
    hyperlinks. The values in these matricies should be complete web
    address. See details or vignette}
  \item{asLinks}{contains complete web address for points that should be
    treated as hyperlinks. See details or vignette}
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
  \item{sep.chr}{seperation character in tool tip that distinguishes
    value from indicator. If a single value, duplicated for all. It may
    be a vector of values corresponding to each tooltip entry.}
  \item{font.type}{font type for tool-tip. Currently support fonts are
    Arial, Helvetica,and sans-serif}
  \item{font.color}{font color for tool-tip}
  \item{font.size}{font size in tool-tip}
  \item{bg.color}{background color of tool-tip}
  \item{fname.root}{Base name to use for .png file name}
  \item{dir}{directory path to where files should be created}
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are "mode" or "median"}
  \item{bb.clr}{vector of possible color choices for automatic detection
    bound point. see details or vignette}
  \item{bb.cex}{Size of automatic detection bound point. see details or
    vignette}
  \item{returnVl}{Should Splot object be returned}
  \item{saveFlag}{Should Splot object be saved}
  \item{saveName}{If saveFlag, path file name to save object}
  \item{cleanDir}{logical indicating if the intermediate files generated
  by the application should be deleted. The files deleted are of no
  consequence to the user; they are needed to identify correct mapping}
}

\details{

  Users are encouraged to see vignette for further details and several
  examples. 
  %
  %
  Two of the most common reasons for automap failing to detect bounding
  points concerns the color of 'phantom' boundings points with the color
  of the graphs, and the figure regions/resize value.
  If the 'phantom' point color is the same as the color used in that area
  of the graph, a difference will not be recorded. The point color is
  determined by bb.clr. By default, if bb.clr is left as NA, we try
  the following colors: blue, red, black, white, green.
  If the size of the plotting region is small, and therefore the point
  size is small, or if the resize value is very large, the difference of
  color for a single point will not register. The point size does not
  equate to a pixel. In this case the user may try increasing the size
  of the 'phantom' points added through the bb.cex argument. 
  %
  %
  xytype refers to how the x.pos and y.pos are treated. The possible
  options are "points", "image.midpoints", "image.boundaries",
  "image.box", "circle", "rect", and "poly".
  "Points","image.midpoints", and "circle" are handled the same. They
  assume the user is passing in x.pos and y.pos locations for a
  circle/point. image.midpoints is slightly different only in that it
  assumes the points refer to the center of regions in an image. The
  html image map will be a "circle". x.pos and y.pos will be the same
  length.
  "image.boundaries" assumes the x.pos and y.pos locations are referring
  to an image. The x.pos and y.pos locations are the boundaries of the
  regions, in other words, they are indications of x-axis and y-axis
  grid lines. The function will automatically calculate midpoints of the
  region and continues as if image.midpoints. 
  "image.box" assumes x.pos and y.pos locations are referring to an
  image. The x.pos and y.pos locations are the boundaries of the
  regions,in other words, they are indications of x-axis and y-axis grid
  lines. The function calculates the boundaries of each rectangular
  image region to make interactive. The html image map will be a "rect".  
  "rect" assumes the interactive regions are rectangular regions. x.pos
  refers to the left x coordinate[s] of the rectangular region[s]. y.pos
  refers to the top y coordinates[s] of the rectangular region[s]. When
  xy.type is "rect", x.right.pos and y.bottom.pos must also be specified
  indicating the right x and bottom y coordinate[s] respectively. x.pos,
  y.pos, x.right.pos, and  y.bottom.pos will all be the same length. The
  html image map will be a "rect".
  "poly" assumes that one, and only one interactive polygon region is
  being added. The x.pos and y.pos therefore are the x and y vertices
  locations. The html image map will be a "poly".   
  %
  %
  The dimensions of x.lables, y.labels, xy.labels, x.links, y.links,
  xy.links, and asLinks will depend on xy.type.
  If xy.type is "points","circle",or "rect" x.labels, y.lables, x.links,
  and y.links will have the dimensions n by m where n is equal to the
  length of x.pos. asLinks will also be of length x.pos, or a single
  value that will be repeated. xy.labels and xy.links will be a list of
  matricies where each matrix is also n by m where n is equal to the
  length of x.pos.
  If xy.type is "image.midpoints" the following is true. x.labels and
  x.links should be n by m where n is equal to the length of x.pos, just
  as y.lables and y.links will be of length y.pos. xy.lables and
  xy.links will be lists of n by m matricies where n is the length of
  y.pos and m is the length of x.pos. asLinks can be of length x.pos,
  y.pos, (x.pos*y.pos), or 1.
  If xy.type is "image.boundaries" or "image.box" are very similar to
  "image.midpoints". Instead of length x.pos and y.pos the length is
  x.pos-1 and y.pos-1.
  If xy.type is "poly", x.lables, y.lables, x.links, y.links, and the
  matricies in xy.lables and xy.links are all 1 by m.  asLinks is a
  single values.   
 }

\value{If returnVl, returns updated Splot object. Also creates .png and
  .tif files used for automatic detection}

\references{
  
  Eric Kort (2006). rtiff: A tiff reader for R.. R package version 1.1.
  
}

\note{

  Automatic detection is currently only functional for linux/unix
  users. A windows extension is being worked on for future version.
  %
  %
  See details and vignette for notes on common reasons for automap
  failing.  
}

\author{Lori A.Shepherd, Daniel P. Gaile}

\seealso{\code{\link{initSplot}},\code{\link{sendplot}}, \code{\link{makeSplot}},\code{rtiff}}

\examples{

# Please see vignette or makeSplot for example

}

\keyword{methods}





