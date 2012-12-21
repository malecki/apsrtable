\name{se.note}
\alias{notefunctions}
\alias{pval.note}
\alias{se.note}
\alias{stars.note}
\title{Table notes}
\usage{
  se.note(env)
}
\arguments{
  \item{env}{The environment of the \code{apsrtable()}
  call, because note functions may need to make use of some
  variables such as \code{lev} or \code{digits}.}
}
\value{
  A character string to place within the tabular
  environment in footnotesize beneath other output.
}
\description{
  Prepare notes about standard errors and statistical
  significance
}
\details{
  Table notes are part of the tabular environment and may
  be based on the content of the table itself. For example,
  the \code{stars} argument to \code{\link{apsrtable}}
  determines whether one or many levels of statistical
  significance are indicated in the output. The
  \code{stars.note} function creates text to place in such
  a note.

  By default the output uses the notation \eqn{ * p <.05}
  and the example below shows a replacement function that
  states, \dQuote{significant at \code{lev} percent.}.

  To access variables in the call to \code{apsrtable} from
  functions in \code{notes}, include the arugment
  \code{env} in any custom functions. This is the
  \code{apsrtable} call environment.

  Remember, to escape characters in Latex output,
  backslashes have to be doubled in R character strings.
}
\examples{
### Custom note function

signif.pct <- function(env) {
  paste("$^*$ significant at", evalq(lev,envir=env)*100, "percent")
}
### Continue the example from apsrtable
\dontrun{
apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="left",
          stars=1, lev=0.05, model.counter=0, order="rl",
          notes=list(se.note, signif.pct,
            "Plant weight data from the lm() example" )
	 )
}
}
\author{
  Michael Malecki <malecki at wustl.edu>
}

