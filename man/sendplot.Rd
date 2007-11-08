\name{sendPlot}

\alias{sendplot}
\alias{sendPlot}

\title{CREATES WEB BROWSER INTERACTIVE PLOT}

\description{
This function takes in a layout, a list of plot calls, and sample
information. It generates a static image of plots. It also generates an
html file with an interactive version of the image. 
}

\usage{
sendplot(mat, plot.calls,  x,y, mai.mat=NA, xlim=NA, ylim=NA,
         z=NA,z.value="value", type="scatterplot", plt.extras =NA,
         x.lbls=NA, y.lbls=NA, xy.lbls=NA,
         bound.pt = TRUE,source.plot=NA,
         resize="4000x5500", ps.paper="letter",ps.width=8,
         ps.height=11,fname.root="test",dir="./",
         paint=TRUE, img.prog = NA,
         up.left=c(673,715),low.right=c(2874,4481),
         spot.radius=10)
}

\arguments{
  \item{mat}{matrix indicating layout. This argument will be passed into
    the graphics package layout call as mat.Each value in the matrix
    must be '0' or a positive integer.  If N is the largest positive
    integer in the matrix, then the integers {1,...,N-1} must also
    appear at least once in the matrix. 
  }
  \item{plot.calls}{character vector containing plot calls}
  \item{mai.mat}{ n x 4 matrix of values to be passed in for each plots
    par mai. n is equal to the length of plot.calls. If NA, uses default
    margins}
  \item{xlim}{xlim values for the first plot call. This is required to
    set up intereactive plot for scatterplots. May be left NA. If this
    is NA and type is scatterplot the xlim will become the range of x values}
  \item{ylim}{ylim values for the first plot call. This is required to
    set up interactive plot for scatterplots.May be left NA. If this
    is NA and type is scatterplot the ylim will become the range of y values}
  \item{x}{vector of x values for the first plot call}
  \item{y}{vector of y values for the first plot call}
  \item{z}{vector of z values if the fist plot call is an image. If the
    plot call is not an image this may be left as NA}
  \item{z.value}{character vector indicating the label for what the z
    argument holds.}
  \item{type}{type of plot for the first plot call. Currently supports
    types are scatterplot or image}
  \item{plt.extras}{List of length equal to the number of
    plot.calls. This object is a list of lists. The sublists contain any
    additional plotting calls that should be executed for the plot. Each
    entry must be a character vector. If no additional plotting is
    required, an NA should be used}
  \item{x.lbls}{data frame of n x m which contatins values relating to the x
    axis of the first plot. n should be the length of the x argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{y.lbls}{data frame of n x m which contatins values relating to the y
    axis of the first plot. n should be the length of the y argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{xy.lbls}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x. This
    information is displayed in the interactive plot window }
  \item{bound.pt}{logical indicating if red points should be plotted to
    aid in finding the upper left and lower right coordinates of a
    scatterplot. If bound.pt is FALSE, indicates that up.left and
    low.right arguments are correct and will make the html file}
  \item{source.plot}{Indicates whether application should make a
    postscript file and then convert to png file, or if the png file
    should be made directly. This value is either ps, png, or NA. If NA
    the operating system is checked and the appropraite file format is
    output. Unix has a convert function that can convert a ps file to
    png file; we by default use this setup because we feel the
    postscript file maintains better quality. So on unix/linux systems
    if source.plot is NA, source.plot will be set to ps. Windows does
    not have this option, for this reason source.plot will be set to png
    if left NA}
  \item{resize}{character indicating resize value. The postscript
    version will be resized to this value when converted to .png.}
  \item{ps.paper}{postscript paper argument}
  \item{ps.width}{poscript width argument}
  \item{ps.height}{postscript height argument}
  \item{fname.root}{Base name to use for posctscript, .png, and html
    file names.}
  \item{dir}{directory path to where files should be created}
  \item{paint}{logical indicating if application should
    automatically open .png file for the user to view .png file and/or
    to retrieve needed bounding values of the first plot call. see
    details}
  \item{img.prog}{If paint is TRUE, the command line call that will open
    a program to view .png file to retrieve pixil locations of interactive
    plot bounds. If this is left NA, the operating system is checked and
    a default program is used. For unix the default application is
    kolourpaint and for windows it is microsoft paint (mspaint)}
  \item{up.left}{The x and y value in pixels of the upper left hand
    corner of the first plot call. see details}
  \item{low.right}{The x and y value in pixels of the lower right hand
    corner of the first plot call. see details}
  \item{spot.radius}{radius of circle in pixels indicating area that
    will be interactive around the center of graphed points}
}

\details{

  This function allows the ability to create a static image of multiple
  plots.  It also provides the functionality to make a plot interactive
  in a web browser utilizing javascript.
  This function utilizes the R graphics layout command and the package's
  \code{\link{eval.js}} command to make a collage of plots.
  The first plot in the plot.calls argument can be interactive in a web
  browser.  The graphs currently supported are scatterplot and image
  which should be indicated by the argument type.  The function uses
  information regarding x and y values to make a mapping that javascript
  can utilize. In order to map correctly, the x and y values need to be
  converted into pixel values. This is performed automatically, however
  it requires the knowledge of the upper left and lower right pixel
  coordinates of the bounding box of the first plot. These pixel
  coordinates change when the postscript file is converted into a .png
  file if the resize option is used. The .png file must be opened in
  some image viewer that allows the user to retrieve pixel
  coordinates. There are two possible scenerios for making a png file:
  the png file may be made directly, or a postscript file may be made
  first that then must be converted into a png file. We recommend the
  later because we feel it maintains better clarity and quality. Whether
  the ps or png file is made is controlled by source.plot. source.plot
  may be ps, png, or NA. If ps, the postscript file is created; if
  this option is chosen and the operating system is unix/linux there is
  an automatic call to the convert function to convert the postscript
  into a png file. If png, the .png file is created. If source.plot is
  NA, the operating system is checked and the appropriate file format is
  generated. By default, if the operating system is unix/linux, the
  postscript is created and then converted to png. If the operating
  system is windows or mac, the png file is created directly. If the ps
  option is used on windows or mac, it is up to the user to correctly
  convert the postscript to a png file format. As mentioned above, once
  the png file is created the pixil locations of the bounding box must
  be known. If paint is TRUE, the png file is openned with the program
  given by img.prog. If img.prog is NA, the application used to view the
  image is determined by the operating system. In windows,img.prog is
  mspaint which will open the .png file in microsoft paint. In
  linux/unix, img.prog is kolourpaint. img.prog may be any command line
  call to a graphics program for viewing .png files. The general plot
  call in R adds a buffer before the axis, for this reason we add two
  points at the bounding box. If bound.pt is T, two red points will
  appear in the scatterplot. When the file is opened in kolourpaint or
  mspaint, the user need only to record the pixel location when the
  mouse hovers over these red points (NOTE: in kolourpaint and mspaint the
  pixel location appears on the bottom of the window). If an image is
  desired as the main plot, the upper left and lower right corner of the
  image should be the points used. When the file is opened in
  kolourpaint or mspaint, the user need only to record the pixel
  location when the mouse hovers over these edges. If bound.pt is T, two
  blue points will appear in the image at these edges to aid in finding the
  location. When bound.pt is TRUE the function assumes that the user
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
  arguments x.lbls, y.lbls, and xy.lbls. In a scatterplot x.lbls,
  y.lbls, and xy.lbls all refer to the same points. It is, therefore
  only necessary to specify x.lbls with information. In an image, there
  may be a differring number of x and y values. The information
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

  The x and y mappings to the interactive plot are created using the x
  and y vectors passed in as an argument to sendplot. Note: this could
  be handy if for example the user plotted more points to the first plot
  using the plt.extras argument. If the user wanted all points
  interactive, the x and y values of the sendplot argument would be a
  combination of all plotted points. 
  
  The interactive html plot currently only works in web browsers that
  implement java script. This means it will work with Mozilla
  Firefox but not with Internet Explorer at this time.

  The code used to create the javascript embedded in html file is a
  modified version of the javascript code from
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html
  
}

\author{Lori Shepherd}

\seealso{\code{layout}, \code{postscript}, \code{\link{sendxy}}, \code{\link{sendimage}}}

\examples{
# see vignette for more advanced example


#
# first example of scatterplot
#
# note: we assume that the function has already been run once
#       to retrieve the pixel locations of the upper left and lower
#       right corners. If this had not been the case, the function would
#       need to have been run with bound.pt = T, perhaps paint = T
#       
#
# The up.left and low.right values are correct if run in 
#  unix/linux environment - for window users the coordinates will not be
#  correct


library(sendplot)

# create a layout with four plots
mat = matrix(c(rep(c(rep(3,8),rep(5,2)),1),
       rep(c(rep(1,8),rep(4,2)),14),
       rep(c(rep(2,8),rep(6,2)),2)),
       ncol=10,byrow=TRUE)

# create x and y points 
x=rnorm(16)
y=rnorm(16)

# list of plot calls - what plot call to use for the four plots
plot.calls = c("plot(x,y,col='green', pch=3)","plot(0,0, col='purple',pch=22,bg='purple')","plot(1:3,1:3, type='b',pch=21,bg='red',col='red')","curve(x^3-3*x,-2,2, col='blue')")

# create matrix of margin parameters
mai.mat = matrix(0, ncol=4, nrow=4, byrow=TRUE)
m1 = c(.25,0,.25,.5)
m2 = c(.4,0,.25,.5)
m3 = c(.1,0,.1,.5)
m4 = c(.25,0,.25,0)
mai.mat[1,] = m1
mai.mat[2,] = m2
mai.mat[3,] = m3
mai.mat[4,] = m4

# x and y limits of the first graph in the plot call
# in this case xlim and ylim values of plot(x,y,col='green', pch=3)
xlim = range(x, na.rm=TRUE)
xlim = c(xlim[1]-.1, xlim[2]+.1)
ylim = range(y, na.rm=TRUE)
ylim = c(ylim[1]-.1, ylim[2]+.1)

# data frame of sample information to display in interactive plot
# since scatterplot has equal number of pts no need to be x, y, or xy specific
x.lbls = list()
x.lbls$test = rep(c("a","b","c","d"),4)
x.lbls$num = 1:16
x.lbls = as.data.frame(x.lbls)





sendplot(mat, plot.calls, mai.mat,
         xlim=xlim, ylim=ylim,type="scatterplot",
         x=x,y=y,z=NA,
         x.lbls=x.lbls, y.lbls=NA, xy.lbls=NA, source.plot=NA,
         resize="1200x1700", fname.root="testScatterplot",dir="",
         paint=FALSE, bound.pt=FALSE,img.prog = NA,
         spot.radius=5, up.left=c(186,199),low.right=c(767,1264))

# there will now be a static postscript and .png file that may be viewed
# as well as an html file that can be opened with firefox that has
# interactive version 



#
#
# second example of image
#
# note: we assume that the function has already been run once
#       to retrieve the pixel locations of the upper left and lower
#       right corners. If this had not been the case, the function would
#       need to have been run with perhaps paint = T 
#       
#
# The up.left and low.right values are correct if run in 
#  unix/linux environment - for window users the coordinates will not be
#  correct


library(sendplot)

# create a layout with four plots
mat = matrix(c(rep(c(rep(3,8),rep(5,2)),1),
       rep(c(rep(1,8),rep(4,2)),14),
       rep(c(rep(2,8),rep(6,2)),2)),
       ncol=10,byrow=TRUE)

# create x and y points, and z matrix of values for image 
y=c(1:5,10,20,22,30,36) 
x=c(1,2,4,5,8)
z=matrix(rnorm(50), nrow=5, ncol=10)

# list of plot calls - what plot call to use for the four plots
plot.calls = c("image(x=x, y=y, z=z)",
    "plot(0,0, col='purple',pch=22,bg='purple')","plot(1:3,1:3, type='b',pch=21,bg='red',col='red')","curve(x^3-3*x,-2,2, col='blue')")

# create matrix of margin parameters
mai.mat = matrix(0, ncol=4, nrow=4, byrow=TRUE)
m1 = c(.25,0,.25,.5)
m2 = c(.4,0,.25,.5)
m3 = c(.1,0,.1,.5)
m4 = c(.25,0,.25,0)
mai.mat[1,] = m1
mai.mat[2,] = m2
mai.mat[3,] = m3
mai.mat[4,] = m4

# mock data frames of x specific and y specific data
x.lbls = list()
x.lbls$test = c("a","b","c","d","e")
x.lbls$num = 1:5
x.lbls = as.data.frame(x.lbls)
y.lbls = list()
y.lbls$test2 = rep(c("f","g","h","i","j"),2)
y.lbls$num2 = 10:1
y.lbls=as.data.frame(y.lbls)

# mock list of data frames of xy specific data
xy.lbls = list()
xy.lbls$one = matrix(1,nrow=10,ncol=5)
xy.lbls$two = matrix(2,nrow=10,ncol=5)
xy.lbls$aa = matrix("a", nrow=10,ncol=5)





sendplot(mat, plot.calls, mai.mat,
         xlim=NA, ylim=NA,type="image",
         x=x,y=y,z=z, z.value="value",
         x.lbls=x.lbls, y.lbls = y.lbls, xy.lbls=xy.lbls,
         resize="1200x1700", fname.root="testimage",source.plot=NA,
         dir="", paint=FALSE,img.prog = NA, bound.pt=FALSE, spot.radius=8,
         up.left=c(163,156),low.right=c(790,1310))


# there will now be a static postscript and .png file that may be viewed
# as well as an html file that can be opened with firefox that has
# interactive version 



}

\keyword{methods}


