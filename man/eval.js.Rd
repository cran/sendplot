\name{eval.js}

\alias{eval.js}

\title{A JAVASCRIPT-LIKE EVAL FUNCTION}

\description{
This function evaluates expressions.
}

\usage{
eval.js(expr,
        envir=parent.frame(),
        enclos=if(is.list(envir)||is.pairlist(envir)) parent.frame())
}

\arguments{
  \item{expr}{character string of an expression to evaluate}
  \item{envir}{passed to eval function. see eval}
  \item{enclos}{passed to eval function. see eval}
}

\details{
Wrapper to the eval functions from the R base package. Evaluates
character expression.
}

\value{will return the evaluated expression}

\references{R base package function eval}

\note{uses function eval from base package}

\author{James Java}

\seealso{\code{eval}}

\examples{

df = list()
df$a = rep(1,5)
df$b = rep("one",5)
df = as.data.frame(df)

#for comparison view
df

eval.js("df$new = NA")
df

 
}

\keyword{methods}





