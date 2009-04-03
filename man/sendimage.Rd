\name{sendimage}

\alias{sendimage}

\title{INTERACTIVE IMAGE - DEPRECATED}

\description{
This function is a wrapper to sendplot that will create a single
interactive image
}

\usage{
sendimage(plot.call,
          x, y, z,
          z.value="value",
          x.lbls = NA,y.lbls=NA,xy.lbls=NA,
          x.links=NA, y.links=NA,
          xy.links=NA,asLinks=NA,
          mai=NA, mai.prc=FALSE,plt.extras=NA,
          bound.pt=FALSE, source.plot=NA,
          paint=FALSE, img.prog=NA,
          resize="800x1100",
          ps.paper="letter",ps.width=8,ps.height=11,
          fname.root="test",dir="./",header="v2",
          up.left=c(188,103),low.right=c(648,912),
          spot.radius=5, automap=FALSE, automap.method="mode")

}

\arguments{
  \item{plot.call}{character vector containing single plot call}
  \item{x}{vector of x locations for interactive points}
  \item{y}{vector of y locations for interactive points}
  \item{z}{vector of z values for image call}
  \item{z.value}{character vector indicating the label for what the z
    argument holds.}
  \item{x.lbls}{data frame of n x m which contains values relating to the x
    axis of the plot call. n should be the length of the x argument. m columns
    contains information regarding sample. This information is displayed
    in the interactive plot window}
  \item{y.lbls}{data frame of n x m which contains values relating to the y
    axis of the plot. n should be the length of the y argument. m columns
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
  \item{mai}{margins for each side of the plot.If NA uses default margins}
  \item{plt.extras}{List of additional plotting calls that should be
    executed for the plot. }
  \item{mai.prc}{logical indicating if mai mat values are percentages
    or hard coded values. If mai.proc is T, indicates percentage.}
  \item{bound.pt}{logical indicating if red points should be plotted to
    aid in finding the upper left and lower right coordinates. If
    bound.pt is FALSE, indicates that up.left and low.right arguments
    are correct and will make the html file }
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
  \item{paint}{logical indicating if application should
    automatically open .png file for the user to view .png file and/or
    to retrieve needed bounding values of the plot call}
  \item{img.prog}{If paint is TRUE, the command line call that will open
    a program to view .png file to retrieve pixil locations of interactive
    plot bounds. If this is left NA, the operating system is checked and
    a default program is used. For unix the default application is
    kolourpaint and for windows it is microsoft paint (mspaint)}
  \item{resize}{character indicating resize value. The postscript
    version will be resized to this value when converted to .png.}
  \item{ps.paper}{postscript paper argument}
  \item{ps.width}{poscript width argument}
  \item{ps.height}{postscript height argument}
  \item{fname.root}{Base name to use for posctscript, .png, and html
    file names.}
  \item{dir}{directory path to where files should be created}
  \item{header}{May either be v1 or v2. This determines which
    tooltip header will be in the html file. Each version has different
    features or works well with different web browsers. see sp.header
    for details.}  
  \item{up.left}{The x and y value in pixels of the upper left hand
    corner of the plot call}
  \item{low.right}{The x and y value in pixels of the lower right hand
    corner of the plot call.}
  \item{spot.radius}{radius of circle in pixels indicating area that
    will be interactive around the center of interactive points}
  \item{automap}{automatic detection of up.left and low.right bound
    points. Fully functional on linux/unix machines only.}
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are mode or median}

}

\details{
  This function is a wrapper for the sendplot function to create a
  single interacive image. See \code{\link{sendplot}} for more information.

  
 %%

  Users are encouraged to read the package vignette which includes
  a detailed discussion of all function arguments as well as several
  useful examples.

  
}

\value{Creates a static .ps and .png file, and an interactive html file}

\references{
  http://www.onlamp.com/pub/a/onlamp/2007/07/05/writing-advanced-javascript.html

  http://www.walterzorn.com/tooltip/tooltip\_e.htm
}

\note{
 
  The interactive html plot currently only works in web browsers that
  implement java script.
  
  The code used to create the javascript embedded in html file is a
  modified version of the javascript code or from the open source
  tooltip library. see reference links

  This function is deprecated. Please see imagesend for updated version.


}

\author{Lori A. Shepherd, Daniel P. Gaile}

\seealso{\code{\link{imagesend}}, \code{\link{initSplot}},
  \code{\link{makeImap}}, \code{\link{makeSplot}},
  \code{\link{sendplot}}}

\examples{
# load the library 
   library("sendplot")

# set up vectors x,y, and z
   x = 1:4
   y = 1:8
   z = t(matrix(rnorm(32), ncol=4))

# create plot call for graph
  plot.calls = "image(x=x, y=y, z=z)"


#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct



# run sendImage
# note: we have already figured out appropriate up.left and low.right
#      values. if these were not known, the function should be run
#      with bound.pt=T (and maybe kolourpaint=T) to find pixil
#      locations

 sendimage(plot.call = plot.calls, x=x, y=y, z=z,
           up.left=c(100,99),low.right=c(738,917),
           bound.pt=FALSE, source.plot=NA, paint=FALSE,
           img.prog=NA,fname.root="testImg",dir=direct )



}

\keyword{methods}





