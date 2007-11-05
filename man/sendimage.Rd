\name{sendimage}

\alias{sendimage}
\alias{sendImage}

\title{INTERACTIVE IMAGE}

\description{
This function is a wrapper to sendplot that will create a single
interactive image
}

\usage{
sendimage(plot.call,
          x, y, z,
          z.value="value",
          x.lbls = NA,y.lbls=NA,xy.lbls=NA,
          mai=NA, plt.extras=NA,
          bound.pt=TRUE, kolourpaint=TRUE,
          resize="800x1100",
          ps.paper="letter",ps.width=8,ps.height=11,
          fname.root="test",dir="./",
          up.left=c(188,103),low.right=c(648,912),
          spot.radius=10)

}

\arguments{
  \item{plot.call}{character vector containing single plot call}
  \item{x}{vector of x locations for interactive points}
  \item{y}{vector of y locations for interactive points}
  \item{z}{vector of z values for image call}
  \item{z.value}{character vector indicating the label for what the z
    argument holds.}
  \item{x.lbls}{data frame of n x m which contatins values relating to the x
    axis of the plot call. n should be the length of the x argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{y.lbls}{data frame of n x m which contatins values relating to the y
    axis of the plot. n should be the length of the y argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{xy.lbls}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x. This
    information is displayed in the interactive plot window }
  \item{mai}{margins for each side of the plot.If NA uses default margins}
  \item{plt.extras}{List of additional plotting calls that should be
    executed for the plot. }
  \item{bound.pt}{logical indicating if red points should be plotted to
    aid in finding the upper left and lower right coordinates. If
    bound.pt is FALSE, indicates that up.left and low.right arguments
    are correct and will make the html file }
  \item{kolourpaint}{logical indicating if kolourpaint should
    automatically open .png file for the user to view .png file and/or
    to retrieve needed bounding values of the plot call}
  \item{resize}{character indicating resize value. The postscript
    version will be resized to this value when converted to .png.}
  \item{ps.paper}{postscript paper argument}
  \item{ps.width}{poscript width argument}
  \item{ps.height}{postscript height argument}
  \item{fname.root}{Base name to use for posctscript, .png, and html
    file names.}
  \item{dir}{directory path to where files should be created}
  \item{up.left}{The x and y value in pixels of the upper left hand
    corner of the plot call}
  \item{low.right}{The x and y value in pixels of the lower right hand
    corner of the plot call.}
  \item{spot.radius}{radius of circle in pixels indicating area that
    will be interactive around the center of interactive points}

}

\details{
  This function is a wrapper for the sendplot function to create a
  single interacive image. see sendplot for more information.

   The plot call, x values, y values and z values will need to be specified. The
  graph created with the plot.call argument will be interactive in a web
  browser. The function uses information regarding x and y values to make
  a mapping that javascript can utilize. In order to map correctly, the
  x and y values need to be converted into pixel values. This is
  performed automatically, however it requires the knowledge of the
  upper left and lower right pixel coordinates of the bounding box of
  the image. These pixel coordinates change when the postscript
  file is converted into a .png file if the resize option is used. The
  .png file must be opened in some image viewer that allows the user to
  retrieve pixel coordinates. We recommend and have built in an
  automatic open into kolourpaint.  If bound.pt is T, two blue points
  will appear in the image to indicate the edges of the bounding
  box. box. If bound.pt is T, two red points will appear in the
  scatterplot. When the file is opened in kolourpaint, the user need
  only to record the pixel location when the mouse hovers over these blue
  points (NOTE: in kolourpaint the pixel location appears on the bottom
  of the window).When bound.pt is TRUE the function assumes that the user
  needs to find the pixel locations of the bounding box and will not
  make the html file. The interactive html file is only created when
  bound.pt is FALSE. Once the upper left and lower right values are
  recorded, the user should rerun the same function call but updating
  the up.left and low.right with the values retrieved and with bound.pt
  = FALSE. This results in an html with embedded javascript to be
  created. The user may open the html file in a web browser that has
  javascript capabilities; we recommended using mozilla firefox (NOTE:
  Internet Explorer currently does not support this function. Capability
  issues will be resolved in future releases). The information that
  appears when a point is scrolled over is controlled by the function
  arguments x.lbls, y.lbls, and xy.lbls.  The information
  displayed can be from x specific information, y specific information,
  or information that is dependent upon both x and y. x.lbls is a data
  frame of x specific data. The number of rows should be equal to the
  number of x data points. There may be any number of columns. The names
  of the columns are used as the label in the display window. Likewise,
  y.lbls is a data frame of y specific data. The number of rows should
  be equal to the number of y data points. There may be any number of
  columns. The names of the columns are used as the label in the display
  window. xy.lbls however is a list of data frames. This data is
  dependent upon both x and y location. All data frames will be of the
  dimensions (length of y) X (length of x). There may be any number of
  data frames in the list. The name of the data frame, or the name in
  the list, is what is used as the label in the display window. The z
  value used to create the image is included automatically. Its label in
  the display window may be controlled by z.value. If this is not
  specified the default label is value.
  
}

\value{Creates a static .ps and .png file, and an interactive html file}

\references{\url{http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html}}

\note{
  The sendplot function currently only works in the linux environment.  A
  windows compatible version will be available in a future version. 

  The interactive html plot currently only works in web browsers that
  implement java script. This means it will work with Mozilla
  Firefox but not with Internet Explorer at this time.

  The code used to create the javascript embedded in html file is a
  modified version of the javascript code from
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html
  

}

\author{Lori Shepherd}

\seealso{\code{\link{sendplot}}, \code{\link{sendxy}}}

\examples{
# load the library 
   library("sendplot")

# set up vectors x,y, and z
   x = 1:4
   y = 1:8
   z = t(matrix(rnorm(32), ncol=4))

# create plot call for graph
  plot.calls = "image(x=x, y=y, z=z)"


# run sendImage
# note: we have already figured out appropriate up.left and low.right
#      values. if these were not known, the function should be run
#      with bound.pt=T (and maybe kolourpaint=T) to find pixil
#      locations

 sendimage(plot.call = plot.calls, x=x, y=y, z=z,
           up.left=c(162,75),low.right=c(662,947),
           bound.pt=FALSE, kolourpaint=FALSE,
           fname.root="testImg"  )



}

\keyword{methods}





