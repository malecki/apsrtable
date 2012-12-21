\name{modelInfo}
\alias{modelInfo}
\alias{modelInfo,summary.clogit-method}
\alias{modelInfo,summary.coxph-method}
\alias{modelInfo,summary.gee-method}
\alias{modelInfo,summary.glm-method}
\alias{modelInfo,summary.lm-method}
\alias{modelInfo,summary.lrm-method}
\alias{modelInfo,summary.negbin-method}
\alias{modelInfo,summary.svyglm-method}
\alias{modelInfo,summary.tobit-method}
\title{Model fit and diagnostic functions for output}
\arguments{
  \item{x}{A \code{summary} object.}
}
\value{
  A list of named character objects representing the lines
  of model diagnostic information to be included for a
  given class of model. For example, the default for
  \code{\link[stats]{lm}} reports the \eqn{N, R^2},
  adjusted \eqn{R^2}, and residual \eqn{\sigma}. The
  default for \code{\link[stats]{glm}} includes the
  \eqn{N}, AIC, BIC, and log-likelihood. Common names
  across model classes in the same table -- e.g., the
  \eqn{N} -- are matched by name, exactly like the model
  coefficients (indeed, the same functions aggregate terms
  and order across models.)
}
\description{
  Model diagnostic / summary information to be included in
  apsrtable output.
}
\details{
  Returns a list containing model diagnostic information,
  with an interface described here to allow the user to
  change the information returned and thus presented. The
  method is called by \code{apsrtable} within an
  \code{lapply} on a list of model summaries. The modelInfo
  methods for a given model summary object simply return a
  list of arbitrary name-value pairs and give themselves
  the S3 class \code{modelInfo}. The modelInfo method
  dispach uses formal S4 classes, however.

  The example shows how one can change the summary for
  \code{lm} objects to include only the \eqn{N} and
  residual \eqn{\sigma}.

  If you register a \code{modelInfo} method and it appears
  not to work, try calling
  \code{\link[methods]{setOldClass}} in order to register
  new \code{modelInfo} methods for your model summary
  object. Method dispatch in R has some subtleties.
}
\examples{
setMethod("modelInfo", "summary.lm", function(x) {
  env <- sys.parent()
  digits <- evalq(digits, env)
  model.info <- list(
                     "$N$"=formatC(sum(x$df[1:2]),format="d"),
                     "Resid. sd" = formatC(x$sigma,format="f",digits=digits))
  class(model.info) <- "model.info"
  return(model.info)
} )

example(apsrtable)


### Switch back to the default
setMethod("modelInfo", "summary.lm", apsrtable:::modelInfo.summary.lm)
\dontrun{
example(apsrtable)
}
}
\author{
  Michael Malecki <malecki at wustl.edu>
}
\seealso{
  \link[base]{sys.frame}
}

