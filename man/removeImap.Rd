\name{removeImap}

\alias{removeImap}

\title{Removes Interactive Information to 'Splot' Object}

\description{
  The removeImap function will remove interactive information for a
  given figure in a Splot Object. It will remove any information added
  by makeImap or addDefault. 
}

\usage{
removeImap(Splot,
           figure,
           subset=NA,
           returnVl=TRUE,
           saveFlag=FALSE,
           saveName="Splot.RData")
}

\arguments{
  \item{Splot}{An Object of the class Splot}
  \item{figure}{Indicates which plot figure to remove data from; this
    matches the numeric indication in the layout matrix. This may also
    be the value "Default" to remove a set Default tool-tip (see
    \code{addDefault}}
  \item{subset}{If default is a numeric indication of figure, which
    subsets of data to remove. If this is left as NA, all iMaps are
    removed.}
  \item{returnVl}{Should Splot object be returned}
  \item{saveFlag}{Should Splot object be saved}
  \item{saveName}{If saveFlag, path file name to save object}
}

\details{

  The removeImap handles removing the Default toop-tip added by
  \code{addDefault} or any of a given figure's iMaps added by
  \code{makeImap}. 

  %
  %
  
  To remove all of a figure's iMaps, subset is NA. iMaps are stored in
  the order they are added. A subset will be a numeric indication of
  which iMap to remove based on this ordering. 

  %
  %
  Users are encouraged to see vignette for better details and examples. 

}

\value{Returns or Saves the new Splot object with data removed}

\note{
  
  Can only remove data from one figure at a time. 

}

\author{Lori A.Shepherd, Daniel P. Gaile}

\seealso{\code{\link{addDefault}}, \code{\link{makeImap}},
  \code{\link{initSplot}}, \code{\link{makeSplot}}}

\examples{

# Please see vignette or makeSplot for example

}

\keyword{methods}





