\name{apsrtable}
\alias{apsrtable}
\title{APSR-style latex tables with multiple models}
\usage{
  apsrtable(..., se = c("robust", "vcov", "both", "pval"),
    model.names = NULL, model.counter = 1, digits = 2,
    stars = 1, lev = 0.05,
    align = c("left", "center", "right"),
    order = c("lr", "rl", "longest"),
    notes = list(se.note, stars.note), omitcoef = NULL,
    coef.names = NULL, coef.rows = 2,
    multicolumn.align = c("center", "left", "right"),
    col.hspace = NULL, Sweave = FALSE, float = "table",
    Minionfig = FALSE, label = NULL, caption = NULL,
    caption.position = c("above", "below"))
}
\arguments{
  \item{...}{One or more fitted model objects of a
  supported class such as \code{lm} or \code{glm}. The
  model-object (a \code{list}) may also optionally contain
  an item named \code{se}: \code{model$se} may be a vector
  of standard errors, or a variance-covariance matrix, in
  which case the square root of the diagonal is used as the
  \dQuote{robust} standard errors in the output. See the
  \code{se} argument.}

  \item{se}{A character string controlling the quantities
  in parentheses, can take the following values: \describe{
  \item{list(list("robust"))}{Print (and calculate
  significance using) user-supplied errors in an \code{$se}
  element appended to the model object. }
  \item{list("vcov")}{Use the standard errors contained in
  the second column of \code{coef(summary(model))}. This
  behavior may be useful in writing
  \code{\link{apsrtableSummary}} methods to customize
  output or format new model classes.}
  \item{list("both")}{User-supplied errors are printed in
  (parentheses) and the default are printed in [square
  brackets.]} \item{list("pval")}{Prints the \eqn{p} value
  contained in the fourth column of
  \code{coef(summary(model))}. Quietly switches
  \code{\link{se.note}} to say \dQuote{(Robust) \eqn{p}
  values in parentheses.} Robust values are used (and so
  labeled contextually) whenever an \code{$se} element is
  present in any model.} } If any model in \code{...}
  contains an \code{se} element and \dQuote{robust} is
  chosen (the default), output is labeled as
  \dQuote{robust;} if no models have an \code{se} element
  (all use model vcov) but \code{se="robust"}, labeling is
  simply \dQuote{Standard errors in parentheses.} Default =
  "robust"}

  \item{model.names}{Optional vector of names to use as
  column headings in the table. If more models than names
  are supplied, unnamed models are numbered (starting at
  one more than the number of names).}

  \item{model.counter}{Change the number to start counting
  from when using automatically numbered models. Default =
  1.}

  \item{digits}{Number of decimal places to report. Default
  = 2}

  \item{stars}{Show statistical significance
  \dQuote{stars}, either \dQuote{1} or \dQuote{default}
  where \dQuote{default} is based on the output of
  \code{summary.lm}, except that a superscript dagger is
  used instead of a dot for \eqn{p < .10}. Here
  \dQuote{default} means \dQuote{the R default}, not to be
  confused with the function's (perhaps confusing)
  Default=1}

  \item{lev}{When \code{stars=1}, what level should be used
  for the test to reject statistical insignificance and
  bestow the glittering star? Disable decoration entirely
  by specifying \code{lev=0}. Default=.05.}

  \item{align}{How should columns be aligned in the output?
  Model summaries are always decimal-aligned using dcolumn
  (and therefore also set in math mode), but dcolumn also
  provides for decimal-point centering. Model names are set
  in \code{\multicolumn} spans with alignment given here,
  as are model terms (leftmost column of table). Default =
  \dQuote{left}.}

  \item{order}{Determines the order in which terms (rows)
  are included in the output when more than one model
  (column) is present. \dQuote{lr} and \dQuote{rl} take the
  order of terms from the first or last (leftmost or
  rightmost) model and appends new terms as they are
  encountered. \dQuote{longest} uses the order of terms in
  the model with the most terms.Default = \dQuote{lr}.}

  \item{notes}{A list to be evaluated and placed, one item
  per full-width (multicolumn) line, in footnote size. The
  default uses two functions, \code{\link{se.note}} and
  \code{\link{stars.note}} to generate notes about the
  standard errors and indicators of statistical
  significance. Other notes can be named function calls or
  simple character strings.}

  \item{omitcoef}{An optional integer or character vector
  of coefficient indices, or an \link{expression} involving
  \code{coefnames} that evaluates to integer or character,
  of rows to exclude from the output. See details.}

  \item{coef.names}{An optional vector of names for
  coefficients. It is recommended to establish the
  \code{omitcoef} and \code{order} settings with automatic
  symbolic naming before supplying a vector of
  \dQuote{pretty} variable names. If automatic symbolic
  naming is used, names are taken from the variables in the
  models and \dQuote{sanitized} for latex. If
  \code{coef.names} are supplied, they must be valid latex,
  with double-backslash escape characters.}

  \item{coef.rows}{The number of rows in the table given to
  each coefficient: by default each coefficient's standard
  error is printed in a row beneath it, but setting
  \code{coef.rows} to 1 places it in a new column to the
  right instead.}

  \item{multicolumn.align}{Alignment for the table's
  \code{multicolumn} spans: typically only the model names
  at the top, but, in the case of \code{coef.rows=1}, the
  \code{model.info} is also aligned beneath both columns.
  Default=\dQuote{center}}

  \item{col.hspace}{Optional \code{hspace} (number+tex
  units such as \code{em}) to insert between each model
  column(s). Intended mainly to separate models from each
  other when \code{coef.rows=1}. Default=NULL}

  \item{Sweave}{Toggle whether to include
  \code{\begin{table}...\end{table}}, label, and caption,
  or only the \code{\begin{tabular} ... \end{tabular}}.
  When called from within an \code{Sweave} document one
  would typically write such elements in the
  \dQuote{documentation} (latex-part) rather than inside
  the code chunk. When called from an \code{Sweave}
  document, make sure to set the code chunk option
  \code{results=tex}. Default = FALSE}

  \item{float}{if \code{Sweave} is false -- that is, if
  \emph{apsrtable} is supposed to wrap the output in the
  float environment, \code{float} allows you to specify an
  arbitrary custom float environment. Some useful ones
  include \dQuote{sidewaystable} (latex package
  \emph{rotating}), or \dQuote{longtable}. In the special
  case of \dQuote{longtable}, the header row of model names
  is included on all pages, and the \code{label} and
  \code{caption} arguments are included \emph{even when
  \code{Sweave=TRUE}} because of the structure of the latex
  environment.}

  \item{Minionfig}{Include latex command to change the
  figure style to \dQuote{tabular} and back to
  \dQuote{proportional}, specifically for the
  \code{MinionPro} latex package. Default = FALSE}

  \item{label}{A string to be used as the label in latex
  for the table. Used only when \code{Sweave=FALSE}, unless
  \code{float="longtable"}.}

  \item{caption}{A latex string to be used as the caption
  for the table (remember to use double backslashes for
  latex commands in R).  Used only when
  \code{Sweave=FALSE}, unless \code{float="longtable"}.}

  \item{caption.position}{Determines the placement of the
  caption and label with respect to the tabular
  environment. (Not thoroughly tested with alternative
  float environments, but should work with the standard
  \code{table/tabular} combination.)
  Default=\code{"above"}}
}
\value{
  A character vector containing lines of latex code. It can
  be written out using \code{writeLines} for inclusion via
  \code{\input{}} in latex documents.
}
\description{
  Produce well-formatted LaTeX tables of multiple models
  side-by-side.
}
\details{
  *** Requires \code{\usepackage{dcolumn}} in Latex/Sweave
  preamble. ***

  Given one or several model objects of various types,
  \code{apsrtable()} will produce side-by-side output in
  well-formatted LaTeX using either automated numbering or
  user-supplied model names and \code{dcolumn}
  decimal-aligned columns. Terms are matched across rows,
  with options for determining the order of terms. Nuisance
  terms (e.g. controls, or other quantities not of primary
  interest) may be omitted. Standard errors of parameter
  estimates are placed below estimates and in parentheses,
  with the option for the user to supply a replacement
  vector of standard errors or a replacement
  variance-covariance matrix, such as one estimated using
  the \code{sandwich} package. By default a single star
  denotes statistical significance at the .05 level, with
  the option to employ further decorations or specify
  another arbitrary level for the test. Finally, some model
  diagnostics are included along with a (somewhat) flexible
  means to program or include different items depending on
  model object class.

  The argument \code{omitcoef} suppresses the output of
  specific rows. It may be either a valid subscript index
  (integer or logical if opacity is desired, or character
  for transparency), or an \code{\link{expression}}, such
  as a \code{\link{grep}} expression to be evaluated with
  respect to \code{coefnames} (without a dot). The internal
  object\code{coefnames} is the union of all model terms,
  in the desired \code{order}. In the example below,
  \dQuote{(Intercept)} is excluded by a regular expression
  matching the parenthesis.

  To exclude multiple regular expressions, or a mix of
  expressions with other types, you may supply a
  \link{list}, but you must ensure that the result is a
  valid subscript list: all character, all numeric, or all
  logical. For example, if you refer to a specific
  coefficient by its character name, include the argument
  \code{value=TRUE} in any \code{\link{grep}} expressions
  in the list.

  Model diagnostic information (\dQuote{model info}) is
  handled by formal \code{\link{modelInfo}} methods defined
  for model summaries. These methods return lists of S3
  class \code{model.info}, named formatted (character)
  elements. To include fit (or other) information that is
  available from fitted model objects but \emph{not their
  summaries,} write an \code{\link{apsrtableSummary}}
  method to prepare a summary with the items needed for
  your own \code{modelInfo} method.

  Included are modelInfo functions for \code{lm},
  \code{glm}, and \code{ \link[AER]{tobit}},
  \code{\link[survival]{coxph}},
  \code{\link[survival]{clogit}}, and a skeleton
  (incomplete \code{modelInfo}) for \code{\link[gee]{gee}}
  and \code{\link[survey]{svyglm}} objects. Please email
  the author any \code{modelInfo} functions you write for
  different model objects for inclusion in future releases.
}
\examples{
## Use the example from lm() to show both models:
     ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
     ## Page 9: Plant Weight Data.
     ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
     trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
     group <- gl(2,10,20, labels=c("Ctl","Trt"))
     weight <- c(ctl, trt)
     lm.D9 <- lm(weight ~ group)
     glm.D9 <- glm(weight~group)
     lm.D90 <- lm(weight ~ group - 1) # omitting intercept
     apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="center",
               stars="default", model.counter=0, order="rl")
     \dontrun{
apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="l",
          stars=1, model.counter=0, order="rl",
          coef.rows=1, col.hspace="3em", float="sidewaystable")

## Omit rows by regular expressions
apsrtable(lm.D9, omitcoef=expression(grep("\\\\(",coefnames)))
apsrtable(lm.D90,lm.D9,
          omitcoef=list("groupCtl",
            expression(grep("\\\\(",coefnames,value=TRUE))
            )
          )
}
}
\author{
  Michael Malecki <malecki at wustl.edu>
}
\seealso{
  \code{\link{modelInfo}} for changing the model diagnostic
  summary information presented and how to include it for
  different classes of model objects;
  \code{\link{notefunctions}} for functions to produce
  dynamic \dQuote{notes} beneath tables; and
  \code{\link{apsrtableSummary}} for creating model
  summaries that produce results compatible with what
  \code{apsrtable} expects.
}

