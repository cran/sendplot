\name{sendplot}

\alias{sendplot}

\title{CREATES WEB BROWSER INTERACTIVE PLOT - DEPRECATED}

\description{
This function takes in a layout, a list of plot calls, and sample
information. It generates a static image of plots. It also generates an
html file with an interactive version of the image. 
}

\usage{
sendplot(mat, plot.calls,  x,y, mai.mat=NA, mai.prc=FALSE, xlim=NA, ylim=NA,
         z=NA,z.value="value", type="scatterplot", plt.extras =NA,
         x.lbls=NA, y.lbls=NA, xy.lbls=NA,
         x.links=NA, y.links=NA,
         xy.links=NA,asLinks=NA,
         bound.pt = FALSE,source.plot=NA,
         resize="800x1100", ps.paper="letter",ps.width=8,
         ps.height=11,fname.root="test",dir="./",header="v2",
         paint=FALSE, img.prog = NA,
         up.left=c(288,203),low.right=c(620,940),
         spot.radius=5, automap=FALSE, automap.method="mode")
}

\arguments{
  \item{mat}{matrix indicating layout. This argument will be passed into
    the graphics package layout call as mat.Each value in the matrix
    must be '0' or a positive integer.  If N is the largest positive
    integer in the matrix, then the integers 1,...,N-1 must also
    appear at least once in the matrix. 
  }
  \item{plot.calls}{character vector containing plot calls}
  \item{mai.mat}{ n x 4 matrix of values to be passed in for each plots
    par mai. n is equal to the length of plot.calls. If NA, uses default
    margins}
  \item{mai.prc}{logical indicating if mai mat values are percentages
    or hard coded values. If mai.proc is T, indicates percentage.}
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
  \item{x.lbls}{data frame of n x m which contains values relating to the x
    axis of the first plot. n should be the length of the x argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{y.lbls}{data frame of n x m which contains values relating to the y
    axis of the first plot. n should be the length of the y argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{xy.lbls}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x. This
    information is displayed in the interactive plot window }
  \item{x.links}{data frame of n x m which contains web addresses
    for links relating to the x axis of the first plot. n should be the
    length of the x argument. m columns contains information regarding
    sample. This information is displayed as hyperlinks in the
    interactive plot window.}
  \item{y.links}{data frame of n x m which contains web addresses
    for links relating to the y axis of the first plot. n should be the
    length of the y argument. m columns contains information regarding
    sample. This information is displayed as hyperlinks in the
    interactive plot window.}
  \item{xy.links}{list of matricies. All matricies should be of n x m
    where n is the length of y and m is the length of x. This
    information is displayed in the interactive plot window as
    hyperlinks. The values in these matricies should be complete web
    address}
  \item{asLinks}{contains complete web address for points that should be
    treated as hyperlinks. May be a data.frame or matrix of n x m where
    n is the length of y and m is the length of x, a vector of length x
    indicating x specific links that will be repeated, a vector of
    length y indicating y specific links that will be repeated, a non NA
    value of length 1 that will be repeated for all points, or a vector
    of length x*y}
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
  \item{ps.width}{postscript width argument}
  \item{ps.height}{postscript height argument}
  \item{fname.root}{Base name to use for posctscript, .png, and html
    file names.}
  \item{dir}{directory path to where files should be created}
  \item{paint}{logical indicating if application should
    automatically open .png file for the user to view .png file and/or
    to retrieve needed bounding values of the first plot call. see
    details}
  \item{header}{May either be v1 or v2. This determines which
    tooltip header will be in the html file. Each version has different
    features or works well with different web browsers. see sp.header
    for details.}
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
  \item{automap}{automatic detection of up.left and low.right bound
    points. Fully functional on linux/unix machines only.}
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are mode or median}

}

\details{

  The functions in the sendplot library allow R users to generate
  interactive plots with tool-tip content. A pair of files are created :
  a Portable Network Graphics (PNG) file which is a bitmap image and an
  HTML file which contains embedded Javascript code for dynamically
  generating tool-tips. When opened with a supported browser, the HTML
  file displays the PNG image and the user is able to mouse over and
  view tool-tip windows for user specified image locations. The
  information that appears in the tool-tip windows is user specified and
  highly customizable. The tool-tip functionality is provided by code
  from the  wz\_tooltip.js Javascript library (Zorn 2007) which is
  embedded in the HTML output.

  %%

  The sendplot function constitutes the primary function of the sendplot library. It allows for the generation of interactive xy (i.e., scatter-plot) and image (i.e., heatmap) plots, which can contain any number of decorative (i.e., non-interactive) plots.

  %%

 The creation of interactive plots with tool-tip content requires the development of the following components:

 %%
 
 1. The static plot image. The library supports the following: a simple
 xy-plot (sendxy), a simple image plot (sendimage), a heatmap with
 decorative dendrograms (heatmap.send), or a flexible layout of plots
 which contains one interactive xy-plot or image plot (sendplot). The
 functions in the sendplot library allow for the full complement of
 graphical bells and whistles which are available in R (e.g., custom
 axes, inclusion of legends, math symbols, etc.).

 %%
 
 2. The plotted point to pixel mapping. The sendplot functions output an
 HTML file and a PNG image. The HTML file contains an image map which
 identifies the interactive regions of the PNG image (i.e., the regions
 for which a tool-tip will appear). The image map requires a mapping of
 the plotted point coordinates as specified in the R plotting calls that
 generated them to the corresponding pixel location on the final PNG
 image. The sendplot functions build this map by identifying the
 upper-left and lower-right locations in the original plotting
 coordinate system and in the final pixel coordinate system. The
 functions provide a convenient mechanism to accomplish this.

 %%

 3. The tool-tip content lists. The sendplot functions allow users to specify  x-specific, y-specific, and point specific (e.g., xy-specific) information to be displayed in the tool-tip. 

 
 %%

 
 The sendplot functions on windows machines are typically run in two
 iterations when creating interactive plots for the first time. In the
 first iteration, the PNG file is created and then opened in a program
 such as mspaint or kolourpaint so that the upper-left and lower-right
 pixel coordinates are identified. In the second iteration, the function
 is called again using the pixel coordinates identified in the first
 iteration and the PNG and HTML output files are created. Note: the
 first iteration need not be repeated for calls that use the sample plot
 type and output image size as the upper-left and lower-right pixel will
 not change.

 %%

 On linux machines, there is an option for automatic detection of the
 upper-left and lower-right pixil coordinates. This utilizes ImageMagick's
 convert program install on most linux machines, and the rtiff R library's
 readTiff function. This eliminates the need for a second iteration. For
 windows users, this option is viable if the user has the ability to
 convert a PNG image to a TIF image; two iterations are still
 needed. See vignette for details.  


 %%

  Users are encouraged to read the package vignette which includes
  a detailed discussion of all function arguments as well as several
  useful examples.

        
}

\value{Creates a static .ps and .png file, and an interactive html file}

\references{

  http://www.R-project.org
  
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm
}
\note{

  The x and y mappings to the interactive plot are created using the x
  and y vectors passed in as an argument to sendplot. Note: this could
  be handy if for example the user plotted more points to the first plot
  using the plt.extras argument. If the user wanted all points
  interactive, the x and y values of the sendplot argument would be a
  combination of all plotted points. 
  
  The interactive html plot currently only works in web browsers that
  implement java script.
  
  The code used to create the javascript embedded in html file is a
  modified version of the javascript code or from the open source
  tooltip library. see reference links
  
  This function is deprecated. Please see initSplot, makeImap, and makeSplot  for updated version.

}

\author{Daniel P. Gaile, Lori A. Shepherd}

\seealso{\code{\link{initSplot}}, \code{\link{makeImap}},
  \code{\link{makeSplot}}, \code{\link{imagesend}},
  \code{\link{xy.send}}, \code{\link{heatmap.send}}, \code{layout}}

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



#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct


sendplot(mat, plot.calls, mai.mat,
         xlim=xlim, ylim=ylim,type="scatterplot",
         x=x,y=y,z=NA,
         x.lbls=x.lbls, y.lbls=NA, xy.lbls=NA, source.plot=NA,
         resize="1200x1700", fname.root="testScatterplot",dir=direct,
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
         dir=direct, paint=FALSE,img.prog = NA, bound.pt=FALSE, spot.radius=8,
         up.left=c(163,156),low.right=c(790,1310))


# there will now be a static postscript and .png file that may be viewed
# as well as an html file that can be opened with firefox that has
# interactive version 



}

\keyword{methods}


