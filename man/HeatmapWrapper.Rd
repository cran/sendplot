\name{HeatmapWrapper}

\alias{HeatmapWrapper}
\alias{heatmap.send.legacy}


\title{INTERACTIVE HEATMAP - DEPRECATED}

\description{
This function is a wrapper for the R stats package heatmap. This will
create an interactive heatmap image. NOTE: The majority of the code for
this function is verbatim from the R package stats heatmap
function. This function was designed to work as a wrapper to untilize
the same functionality and plotting as the heatmap function with
sendplot's interactive functionality.

}

\usage{
heatmap.send.legacy(x,Rowv = NULL,
             Colv = if (symm) "Rowv" else NULL, 
             distfun = dist,hclustfun = hclust,
             reorderfun = function(d,w) reorder(d, w),
             add.expr,symm = FALSE,
             revC = identical(Colv,"Rowv"),
             scale = c("row", "column", "none"),
             na.rm = TRUE, margins = c(5, 5),
             ColSideColors,RowSideColors,
             cexRow = 0.2 +  1/log10(nr),
             cexCol = 0.2 + 1/log10(nc),
             labRow = NULL,labCol = NULL,
             main = NULL,xlab = NULL,ylab = NULL,
             keep.dendro = FALSE, 
             verbose = getOption("verbose"),
             mai.mat=NA, mai.prc=FALSE,
             z.value="value",
             x.lbls=NA,y.lbls=NA,xy.lbls=NA,
             x.links=NA, y.links=NA,
             xy.links=NA,asLinks=NA,
             bound.pt = FALSE, source.plot=NA,
             resize="800x1100",
             ps.paper="letter",ps.width=8,ps.height=11,
             fname.root="test",dir="./", header="v2",
             paint=FALSE, img.prog = NA,
             up.left=c(288,203),low.right=c(620,940),
             spot.radius=5, automap=FALSE, automap.method="mode") 

}

\arguments{
  \item{x}{numeric matrix of the values to be plotted}
  \item{Rowv}{determines if and how the row dendrogram should be computed
          and reordered.  Either a 'dendrogram' or a vector of values
          used to reorder the row dendrogram or 'NA' to suppress any
          row dendrogram (and reordering) or by default, 'NULL', see
          heatmap argument}
  \item{Colv}{determines if and how the column dendrogram should be
          reordered.  Has the same options as the 'Rowv' argument above
          and additionally when 'x' is a square matrix, 'Colv =
          "Rowv"' means that columns should be treated identically to
          the rows}
  \item{distfun}{function used to compute the distance (dissimilarity) between
          both rows and columns.  Defaults to 'dist'}
  \item{hclustfun}{function used to compute the hierarchical clustering when
          'Rowv' or 'Colv' are not dendrograms.  Defaults to 'hclust'}
  \item{reorderfun}{function(d,w) of dendrogram and weights for reordering the
          row and column dendrograms.  The default uses
          'reorder.dendrogram'}
  \item{add.expr}{expression that will be evaluated after the call to 'image'. 
          Can be used to add components to the plot}
  \item{symm}{logical indicating if 'x' should be treated *symm*etrically;
          can only be true when 'x' is a square matrix.}
  \item{revC}{logical indicating if the column order should be 'rev'ersed
          for plotting, such that e.g., for the symmetric case, the
          symmetry axis is as usual}
  \item{scale}{character indicating if the values should be centered and
          scaled in either the row direction or the column direction,
          or none.  The default is '"row"' if 'symm' false, and
          '"none"' otherwise}
  \item{na.rm}{logical indicating whether 'NA''s should be removed}
  \item{margins}{numeric vector of length 2 containing the margins (see
          'par(mar= *)') for column and row names, respectively}
  \item{ColSideColors}{(optional) character vector of length 'ncol(x)'
          containing the color names for a horizontal side bar that may
          be used to annotate the columns of 'x'}
  \item{RowSideColors}{ (optional) character vector of length 'nrow(x)'
          containing the color names for a vertical side bar that may
          be used to annotate the rows of 'x'}
  \item{cexRow}{positive number, used as 'cex.axis' in for the row axis
    labeling. The defaults currently only use number of rows}
  \item{cexCol}{positive number, used as 'cex.axis' in for the column axis
    labeling. The defaults currently only use number of columns}
  \item{labRow}{character vectors with row labels to use;
          these default to 'rownames(x)'}
  \item{labCol}{character vectors with column labels to use;
          these default to 'colnames(x)'}
  \item{main}{main title; defaults to none}
  \item{xlab}{x axis title; defaults to none}
  \item{ylab}{y axis title; defautls to none}
  \item{keep.dendro}{logical indicating if the dendrogram(s) should be kept as
          part of the result (when 'Rowv' and/or 'Colv' are not NA)}
  \item{verbose}{logical indicating if information should be printed}
  \item{mai.mat}{ n x 4 matrix of values to be passed in for each plots
    par mai. n is equal to the length of plot.calls. If NA, uses default
    margins. Utitilzing this wrapper n will either be 3,4, or 5.}
  \item{mai.prc}{logical indicating if mai mat values are percentages
    or hard coded values. If mai.proc is T, indicates percentage.}
  \item{z.value}{character vector indicating the label for what the z
    argument holds.}
  \item{x.lbls}{data frame of n x m which contains values relating to the x
    axis of the heatmap plot. n should be equal to the second dimension of the x
    argument.This information is displayed in the interactive plot
    window. This may be left as NA.}
  \item{y.lbls}{data frame of n x m which contains values relating to the y
    axis of the heatmap plot. n should be equal to the first dimension of the x
    argument.This information is displayed in the interactive plot
    window. This may be left as NA }
  \item{xy.lbls}{list of matricies. All matricies should be of n x m
    where n is equal to the first dimension of the x argument and m is
    equal to the second dimension of the x argument. This information is
    displayed in the interactive plot window. This may be left NA}
  \item{x.links}{data frame of n x m which contains web addresses
    for links relating to the x axis of the heatmap plot. n should be
    equal to the second dimension of the x argument. m columns contains
    information regarding sample. This information is displayed as
    hyperlinks in the interactive plot window. This may be left NA}
  \item{y.links}{data frame of n x m which contains web addresses
    for links relating to the y axis of the heatmap plot. n should be
    equal to the first dimension of the x argument. This information is
    displayed as hyperlinks in the interactive plot window. This may be
    left as NA}
  \item{xy.links}{list of matricies.  All matricies should be of n x m
    where n is equal to the first dimension of the x argument and m is
    equal to the second dimension of the x argument. This information is
    displayed in the interactive plot window as hyperlinks. The values
    in these matricies should be complete web address}
  \item{asLinks}{contains complete web address for points that should be
    treated as hyperlinks. May be a data.frame or matrix of n x m  where
    n is equal to the first dimension of the x argument and m is equal
    to the second dimension of the x argument, a vector of length equal
    to the first dimension of the x argument that will be repeated, a
    vector of length equal to the second dimension of the x argument
    that will be repeated,a non NA value of length 1 that will be
    repeated for all points, or a vector of length dim(x)[1]*dim(x)[2]}  
  \item{bound.pt}{logical indicating if blue points should be plotted to
    aid in finding the upper left and lower right coordinates of a
    hte heatmap. If bound.pt is FALSE, indicates that up.left and
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
  \item{dir}{directory path to where files should be created. Default
    creates files in working directory}
  \item{header}{May either be v1 or v2. This determines which
    tooltip header will be in the html file. Each version has different
    features or works well with different web browsers. see sp.header
    for details.}
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
  \item{automap}{automatic detection of up.left and low.right bound
    points. Fully functional on linux/unix machines only.}
  \item{automap.method}{Method to detect upper and lower bounds. Current
    options are mode or median}

}

\details{
  The majority of the code for this function is verbatim from the R
  package stats heatmap function. This function was designed to work as
  a wrapper to untilize the same functionality and plotting as the
  heatmap function with sendplot's interactive functionality. See
  \code{\link{heatmap}} for more details on arguments and details
  concerning the creatation of plots.

  %%
  
  See \code{\link{sendplot}} for more information regarding the creation
  of the interactive output with tool-tip content.

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
  The majority of the code for this function is verbatim from the R
  package stats heatmap function. This function was designed to work as
  a wrapper to untilize the same functionality and plotting as the
  heatmap function with sendplot's interactive functionality.
  
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

  This function is deprecated. Please see heatmap.send for updated version.

}

\author{
  Lori A. Shepherd and Daniel P. Gaile;
  
  Authors of heatmap code used in our code: Andy Liaw, original; R. Gentleman, M. Maechler, W. Huber,revisions}

\seealso{\code{\link{heatmap.send}}, \code{\link{sendplot}},\code{\link{sendxy}}, \code{\link{sendimage}}, \code{\link{heatmap}} }

\examples{

rm(list=ls())
library(sendplot)

# mock matrix of values
temp = matrix(rnorm(15), nrow=5, ncol=3)

# color bars for samples
rcol = c("red", "blue", "yellow", "purple", "blue")
ccol = c("black", "green", "black")

# matrix of values to display in interactive window
temp.inv = list()
temp.inv$sample.ID = c("smp1", "smp2", "smp3")
temp.inv$count = 1:3
temp.inv$bb = 3:1
temp.inv = as.data.frame(temp.inv)

# matrix of values to display in interactive window
loc.inv = list()
loc.inv$spot = c("sp1", "sp2", "sp3","sp4","sp5")
loc.inv$vv = 1:5
loc.inv$bv = 5:1
loc.inv = as.data.frame(loc.inv)


#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct

heatmap.send.legacy(temp, RowSideColors=rcol, ColSideColors=ccol,
x.lbls=temp.inv, y.lbls=loc.inv, bound.pt=FALSE, paint=FALSE,
spot.radius=20, dir=direct)


# or display heatmap without color bands 
heatmap.send.legacy(temp, x.lbls=temp.inv, y.lbls=loc.inv, bound.pt=FALSE,
paint=FALSE, spot.radius=20,dir=direct)


# or without cluster
heatmap.send.legacy(temp, Rowv=NA, Colv=NA, x.lbls=temp.inv, y.lbls=loc.inv, bound.pt=FALSE,
paint=FALSE, spot.radius=20,dir=direct)


}

\keyword{methods}





