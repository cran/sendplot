\name{heatmap.send}

\alias{heatmap.send}


\title{INTERACTIVE HEATMAP}

\description{
This function is a wrapper for the R stats package heatmap. This will
create an interactive heatmap image. NOTE: The majority of the code for
this function is verbatim from the R package stats heatmap
function. This function was designed to work as a wrapper to untilize
the same functionality and plotting as the heatmap function with
sendplot's interactive functionality.

}

\usage{
heatmap.send(x,Rowv = NULL,
             Colv = if (symm) "Rowv" else NULL, 
             distfun = dist,hclustfun = hclust,
             reorderfun = function(d,w) reorder(d, w),
             add.expr,symm = FALSE,
             revC = identical(Colv,"Rowv"),
             scale = c("row", "column", "none"),
             na.rm = TRUE, margins = c(5, 5),
             ColSideColors,RowSideColors,
             MainColor = heat.colors(12),
             cexRow = 0.2 +  1/log10(nr),
             cexCol = 0.2 + 1/log10(nc),
             labRow = NULL,labCol = NULL,
             main = NULL,xlab = NULL,ylab = NULL,
             keep.dendro = FALSE, 
             verbose = getOption("verbose"),
            
             x.labels=NA,y.labels=NA,xy.labels=NA,
             x.links=NA, y.links=NA,
             xy.links=NA,asLinks=NA,
             x.images=NA, y.images=NA,
             xy.images=NA,
             spot.radius=5,source.plot=NA,
             image.size="800x1100",
             fname.root="test",dir="./", header="v3",
             window.size = "800x1100",
             ...) 

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
  \item{MainColor}{color scale for values. Passed into 'image' function
          as col argument}
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

  \item{x.labels}{data frame of n x m which contains values relating to the x
    axis of the heatmap plot. n should be equal to the second dimension of the x
    argument.This information is displayed in the interactive plot
    window. This may be left as NA.}
  \item{y.labels}{data frame of n x m which contains values relating to the y
    axis of the heatmap plot. n should be equal to the first dimension of the x
    argument.This information is displayed in the interactive plot
    window. This may be left as NA }
  \item{xy.labels}{list of matricies. All matricies should be of n x m
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
  \item{x.images}{data frame of n x m which contains paths for images
    relating to the x axis of the heatmap plot. n should be
    equal to the second dimension of the x argument. m columns contains
    information regarding sample. This information is displayed as
    images in the interactive plot window. This may be left NA}
  \item{y.images}{data frame of n x m which contains paths for images
    relating to the y axis of the heatmap plot. n should be
    equal to the first dimension of the x argument. This information is
    displayed as images in the interactive plot window. This may be
    left as NA}
  \item{xy.images}{list of matricies.  All matricies should be of n x m
    where n is equal to the first dimension of the x argument and m is
    equal to the second dimension of the x argument. This information is
    displayed in the interactive plot window as images. The values
    in these matricies should be complete path of images}
  \item{spot.radius}{radius of circle in pixels indicating area that
    will be interactive around the center of graphed points}
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
  \item{image.size}{character indicating size of device.}
  \item{fname.root}{Base name to use for all files created.}
  \item{dir}{directory path to where files should be created. Default
    creates files in working directory}
  \item{header}{May either be v1,v2, or v3. This determines which
    tooltip header will be in the html file. Each version has different
    features or works well with different web browsers. see sp.header
    for details.}
  \item{window.size}{size of the html window. Only effective when header=v3}
  \item{...}{additional arguments to the makeImap function}

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

\value{creates the static and interactive versions of heatmap}

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
  
  The interactive html plot currently only works in web browsers that
  implement java script.
  
  The code used to create the javascript embedded in html file is a
  modified version of the javascript code or from the open source
  tooltip library. see reference links

}

\author{
  Lori A. Shepherd and Daniel P. Gaile;
  
  Authors of heatmap code used in our code: Andy Liaw, original; R. Gentleman, M. Maechler, W. Huber,revisions}

\seealso{\code{\link{initSplot}},\code{\link{makeImap}},\code{\link{makeSplot}},\code{\link{imagesend}},\code{\link{heatmap.send.legacy}}, \code{\link{sendplot}}, \code{\link{heatmap}} }

\examples{

library(sendplot)
library(rtiff)

require(graphics)


x  = as.matrix(mtcars)
rc = rainbow(nrow(x), start=0, end=.3)
cc = rainbow(ncol(x), start=0, end=.3)

xy.labels=list(value=x)


x.labels=data.frame(label=colnames(x),
  description=c("Miles/(US) gallon","Number of cylinders",
    "Displacement (cu.in.)",
    "Gross horsepower",
    "Rear axle ratio",
    "Weight (lb/1000)",
    "1/4 mile time",
    "V/S",
    "Transmission (0 = automatic, 1 = manual)",
    "Number of forward gears",
    "Number of carburetors")
  )


#set up temporary directory
direct = paste(tempdir(),"/",sep="")
direct


heatmap.send(x,scale="column", xy.labels = xy.labels,
                 x.labels=x.labels,
                 RowSideColors = rc, ColSideColors = cc, margin=c(5,10),
                 xlab = "specification variables", ylab= "Car Models",
                 main = "mtcars data",
                 fname.root="exHeat",dir=direct,
                 font.size=18,image.size="600x900")



}

\keyword{methods}





