\name{apsrtableSummary}
\alias{apsrtableSummary}
\alias{apsrtableSummary.gee}
\alias{apsrtableSummary.lrm}
\title{Custom summary functions for output tables}
\arguments{
  \item{x}{A model object to be summarized in a format
  suitable for \code{apsrtable} output.}
}
\value{
  A \code{summary} representation of a model object,
  probably derived from the object's own \code{summary}
  method.
}
\description{
  Provide alternative model summaries specifically for use
  with \code{apsrtable}
}
\details{
  When preparing model objects for output, \code{apsrtable}
  uses primarily the representation of the model provided
  by its \code{summary} method. However, some packages
  return summaries with information that can be confusing
  to \code{apsrtable}.

  In such an event, you have two options: provide a custom
  \code{apsrtableSummary} method, or work with the package
  maintainers to produce a suitable \code{summary} object.
  Ideally, the former is a stopgap for the latter.
}
\examples{
### summary.gee produces z scores but not Pr(z). This converts the relevant columns
### to Pr(z) so that apsrstars() works on it, and places the vector of robust se's in
### an $se position which apsrtable expects.

apsrtableSummary.gee <- function(x) {
  s <- summary(x)
  newCoef <- coef(s)
  ## which columns have z scores? (two of them in robust case)
  zcols <- grep("z",colnames(newCoef))
  newCoef[,zcols] <- pnorm(abs(newCoef[,zcols]), lower.tail=FALSE)
  colnames(newCoef)[zcols] <- "Pr(z)"
  s$coefficients <- newCoef
  ## put the robust se in $se so that notefunction works automatically
  ## the se checker will overwrite [,4] with pt, but this doesn't matter
  ## because the last column Pr(z) is used by apsrstars() anyway
  ## and the se are pulled from $se.
  if( class(x) == "gee.robust") {
    s$se <- coef(s)[,4]
  }
  return(s)
}
}
\author{
  Michael Malecki <malecki at wustl.edu>
}

