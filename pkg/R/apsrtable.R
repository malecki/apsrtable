##' APSR-style latex tables with multiple models
##'
##' Produce well-formatted LaTeX tables of multiple models side-by-side.
##'
##' *** Requires \code{\usepackage{dcolumn}} in Latex/Sweave preamble. ***
##'
##' Given one or several model objects of various types, \code{apsrtable()}
##' will produce side-by-side output in well-formatted LaTeX using either
##' automated numbering or user-supplied model names and \code{dcolumn}
##' decimal-aligned columns. Terms are matched across rows, with options for
##' determining the order of terms. Nuisance terms (e.g. controls, or other
##' quantities not of primary interest) may be omitted. Standard errors of
##' parameter estimates are placed below estimates and in parentheses, with the
##' option for the user to supply a replacement vector of standard errors or a
##' replacement variance-covariance matrix, such as one estimated using the
##' \code{sandwich} package. By default a single star denotes statistical
##' significance at the .05 level, with the option to employ further
##' decorations or specify another arbitrary level for the test. Finally, some
##' model diagnostics are included along with a (somewhat) flexible means to
##' program or include different items depending on model object class.
##'
##' The argument \code{omitcoef} suppresses the output of specific rows. It may
##' be either a valid subscript index (integer or logical if opacity is
##' desired, or character for transparency), or an \code{\link{expression}},
##' such as a \code{\link{grep}} expression to be evaluated with respect to
##' \code{coefnames} (without a dot). The internal object\code{coefnames} is
##' the union of all model terms, in the desired \code{order}. In the example
##' below, \dQuote{(Intercept)} is excluded by a regular expression matching
##' the parenthesis.
##'
##' To exclude multiple regular expressions, or a mix of expressions with other
##' types, you may supply a \link{list}, but you must ensure that the result is
##' a valid subscript list: all character, all numeric, or all logical. For
##' example, if you refer to a specific coefficient by its character name,
##' include the argument \code{value=TRUE} in any \code{\link{grep}}
##' expressions in the list.
##'
##' Model diagnostic information (\dQuote{model info}) is handled by formal
##' \code{\link{modelInfo}} methods defined for model summaries. These methods
##' return lists of S3 class \code{model.info}, named formatted (character)
##' elements. To include fit (or other) information that is available from
##' fitted model objects but \emph{not their summaries,} write an
##' \code{\link{apsrtableSummary}} method to prepare a summary with the items
##' needed for your own \code{modelInfo} method.
##'
##' Included are modelInfo functions for \code{lm}, \code{glm}, and \code{
##' \link[AER]{tobit}}, \code{\link[survival]{coxph}},
##' \code{\link[survival]{clogit}}, and a skeleton (incomplete
##' \code{modelInfo}) for \code{\link[gee]{gee}} and
##' \code{\link[survey]{svyglm}} objects. Please email the author any
##' \code{modelInfo} functions you write for different model objects for
##' inclusion in future releases.
##'
##' @param ... One or more fitted model objects of a supported class such as
##' \code{lm} or \code{glm}. The model-object (a \code{list}) may also
##' optionally contain an item named \code{se}: \code{model$se} may be a vector
##' of standard errors, or a variance-covariance matrix, in which case the
##' square root of the diagonal is used as the \dQuote{robust} standard errors
##' in the output. See the \code{se} argument.
##' @param se A character string controlling the quantities in parentheses, can
##' take the following values: \describe{ \item{list(list("robust"))}{Print
##' (and calculate significance using) user-supplied errors in an \code{$se}
##' element appended to the model object. } \item{list("vcov")}{Use the
##' standard errors contained in the second column of
##' \code{coef(summary(model))}. This behavior may be useful in writing
##' \code{\link{apsrtableSummary}} methods to customize output or format new
##' model classes.} \item{list("both")}{User-supplied errors are printed in
##' (parentheses) and the default are printed in [square brackets.]}
##' \item{list("pval")}{Prints the \eqn{p} value contained in the fourth column
##' of \code{coef(summary(model))}. Quietly switches \code{\link{se.note}} to
##' say \dQuote{(Robust) \eqn{p} values in parentheses.} Robust values are used
##' (and so labeled contextually) whenever an \code{$se} element is present in
##' any model.} } If any model in \code{...} contains an \code{se} element and
##' \dQuote{robust} is chosen (the default), output is labeled as
##' \dQuote{robust;} if no models have an \code{se} element (all use model
##' vcov) but \code{se="robust"}, labeling is simply \dQuote{Standard errors in
##' parentheses.} Default = "robust"
##' @param model.names Optional vector of names to use as column headings in
##' the table. If more models than names are supplied, unnamed models are
##' numbered (starting at one more than the number of names).
##' @param model.counter Change the number to start counting from when using
##' automatically numbered models. Default = 1.
##' @param digits Number of decimal places to report. Default = 2
##' @param stars Show statistical significance \dQuote{stars}, either
##' \dQuote{1} or \dQuote{default} where \dQuote{default} is based on the
##' output of \code{summary.lm}, except that a superscript dagger is used
##' instead of a dot for \eqn{p < .10}. Here \dQuote{default} means \dQuote{the
##' R default}, not to be confused with the function's (perhaps confusing)
##' Default=1
##' @param lev When \code{stars=1}, what level should be used for the test to
##' reject statistical insignificance and bestow the glittering star? Disable
##' decoration entirely by specifying \code{lev=0}. Default=.05.
##' @param align How should columns be aligned in the output? Model summaries
##' are always decimal-aligned using dcolumn (and therefore also set in math
##' mode), but dcolumn also provides for decimal-point centering. Model names
##' are set in \code{\multicolumn} spans with alignment given here, as are
##' model terms (leftmost column of table). Default = \dQuote{left}.
##' @param order Determines the order in which terms (rows) are included in the
##' output when more than one model (column) is present. \dQuote{lr} and
##' \dQuote{rl} take the order of terms from the first or last (leftmost or
##' rightmost) model and appends new terms as they are encountered.
##' \dQuote{longest} uses the order of terms in the model with the most
##' terms.Default = \dQuote{lr}.
##' @param notes A list to be evaluated and placed, one item per full-width
##' (multicolumn) line, in footnote size. The default uses two functions,
##' \code{\link{se.note}} and \code{\link{stars.note}} to generate notes about
##' the standard errors and indicators of statistical significance. Other notes
##' can be named function calls or simple character strings.
##' @param omitcoef An optional integer or character vector of coefficient
##' indices, or an \link{expression} involving \code{coefnames} that evaluates
##' to integer or character, of rows to exclude from the output. See details.
##' @param coef.names An optional vector of names for coefficients. It is
##' recommended to establish the \code{omitcoef} and \code{order} settings with
##' automatic symbolic naming before supplying a vector of \dQuote{pretty}
##' variable names. If automatic symbolic naming is used, names are taken from
##' the variables in the models and \dQuote{sanitized} for latex. If
##' \code{coef.names} are supplied, they must be valid latex, with
##' double-backslash escape characters.
##' @param coef.rows The number of rows in the table given to each coefficient:
##' by default each coefficient's standard error is printed in a row beneath
##' it, but setting \code{coef.rows} to 1 places it in a new column to the
##' right instead.
##' @param multicolumn.align Alignment for the table's \code{multicolumn}
##' spans: typically only the model names at the top, but, in the case of
##' \code{coef.rows=1}, the \code{model.info} is also aligned beneath both
##' columns. Default=\dQuote{center}
##' @param col.hspace Optional \code{hspace} (number+tex units such as
##' \code{em}) to insert between each model column(s). Intended mainly to
##' separate models from each other when \code{coef.rows=1}. Default=NULL
##' @param Sweave Toggle whether to include \code{\begin{table}...\end{table}},
##' label, and caption, or only the \code{\begin{tabular} ... \end{tabular}}.
##' When called from within an \code{Sweave} document one would typically write
##' such elements in the \dQuote{documentation} (latex-part) rather than inside
##' the code chunk. When called from an \code{Sweave} document, make sure to
##' set the code chunk option \code{results=tex}. Default = FALSE
##' @param float if \code{Sweave} is false -- that is, if \emph{apsrtable} is
##' supposed to wrap the output in the float environment, \code{float} allows
##' you to specify an arbitrary custom float environment. Some useful ones
##' include \dQuote{sidewaystable} (latex package \emph{rotating}), or
##' \dQuote{longtable}. In the special case of \dQuote{longtable}, the header
##' row of model names is included on all pages, and the \code{label} and
##' \code{caption} arguments are included \emph{even when \code{Sweave=TRUE}}
##' because of the structure of the latex environment.
##' @param Minionfig Include latex command to change the figure style to
##' \dQuote{tabular} and back to \dQuote{proportional}, specifically for the
##' \code{MinionPro} latex package. Default = FALSE
##' @param label A string to be used as the label in latex for the table. Used
##' only when \code{Sweave=FALSE}, unless \code{float="longtable"}.
##' @param caption A latex string to be used as the caption for the table
##' (remember to use double backslashes for latex commands in R).  Used only
##' when \code{Sweave=FALSE}, unless \code{float="longtable"}.
##' @param caption.position Determines the placement of the caption and label
##' with respect to the tabular environment. (Not thoroughly tested with
##' alternative float environments, but should work with the standard
##' \code{table/tabular} combination.) Default=\code{"above"}
##' @return A character vector containing lines of latex code. It can be
##' written out using \code{writeLines} for inclusion via \code{\input{}} in
##' latex documents.
##' @author Michael Malecki <malecki at wustl.edu>
##' @seealso \code{\link{modelInfo}} for changing the model diagnostic summary
##' information presented and how to include it for different classes of model
##' objects; \code{\link{notefunctions}} for functions to produce dynamic
##' \dQuote{notes} beneath tables; and \code{\link{apsrtableSummary}} for
##' creating model summaries that produce results compatible with what
##' \code{apsrtable} expects.
##' @examples
##'
##'      ## Use the example from lm() to show both models:
##'      ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
##'      ## Page 9: Plant Weight Data.
##'      ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
##'      trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
##'      group <- gl(2,10,20, labels=c("Ctl","Trt"))
##'      weight <- c(ctl, trt)
##'      lm.D9 <- lm(weight ~ group)
##'      glm.D9 <- glm(weight~group)
##'      lm.D90 <- lm(weight ~ group - 1) # omitting intercept
##'      apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="center",
##'                stars="default", model.counter=0, order="rl")
##'      \dontrun{
##' apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="l",
##'           stars=1, model.counter=0, order="rl",
##'           coef.rows=1, col.hspace="3em", float="sidewaystable")
##'
##' ## Omit rows by regular expressions
##' apsrtable(lm.D9, omitcoef=expression(grep("\\(",coefnames)))
##' apsrtable(lm.D90,lm.D9,
##'           omitcoef=list("groupCtl",
##'             expression(grep("\\(",coefnames,value=TRUE))
##'             )
##'           )
##' }
##'
##' @export
apsrtable <- function (...,
                       se=c("robust","vcov","both","pval"),
                       # model.names can be shorter, others numbered;
                       # numbers start at value of model.counter
                       model.names=NULL, model.counter=1, digits=2,
                       # stars="default" prints R default
                       # this function default is one star at .05
                       stars=1,lev=.05,
                       align=c("left","center","right"),
                       order=c("lr","rl","longest"),
                       notes=list(se.note,stars.note),
                       omitcoef=NULL,coef.names=NULL,
                       coef.rows=2,
                       multicolumn.align=c("center","left","right"),
                       col.hspace=NULL,
                       Sweave=FALSE, float="table",
                       Minionfig=FALSE,
                       label=NULL,caption=NULL,
                       caption.position=c("above","below")
                       ) {
  x <- list()
  myenv <- new.env()
  opts <- match.call(expand.dots=FALSE)
  signif.stars <- TRUE

  order <- match.arg(order,
                     c("lr","rl","longest"))
  se <- match.arg(se,
                  c("robust","vcov","both","pval"))
  align <- match.arg(align,
                     c("left","center","right"))
  align <- substr(align,1,1)
  multicolumn.align <- match.arg(multicolumn.align,
                                 c("center","left","right"))
  multicolumn.align <- substr(multicolumn.align,1,1)
  adigits <- ifelse(align=="c",
                    -1,
                    digits)
  caption.position <- match.arg(caption.position,
                                c("above","below"))
  models <- list(...)
  nmodels <- length(models)

  ## used to multiply later for column counts
  coef.cols <- ifelse(coef.rows==2, 1, 2)

  ## Two default behaviors for col.hspace:
  ##  if in two-column-per-model mode, default to 2em
  ##  otherwise, empty. If not "", add some latex around it.
  if (is.null(col.hspace)) {
  col.hspace <- ifelse(coef.cols==1,"",
                       "2em")
  }
  if(col.hspace != "") {
    col.hspace <- paste("@{\\hspace{",col.hspace,"}}",sep="")
  }
  colspec <- paste("{",
                   align,
                   paste(rep(paste("D{.}{.}{",
                                   rep(adigits,coef.cols),
                                    "}",
                                   sep="",collapse=""),nmodels),
                         collapse=col.hspace)
                   ,"}",collapse="")
  if(float=="longtable") {
    long <- TRUE
    floatspec <- paste("\\begin{",float,"}",colspec,"\n",
                       ifelse(caption.position=="a",
                              paste("\\caption{",caption,"}\n\\label{",label,"}",sep=""),
                              ""),
                       sep="")
  } else
  {
    long <- FALSE
    floatspec <- paste(ifelse(!Sweave,
                              paste("\\begin{",float,"}[!ht]\n",
                                    ifelse(caption.position=="a",
                                           paste("\\caption{",caption,"}\n\\label{",label,"}",sep=""),
                                           ""),sep=""),
                              "" ),
                       paste("\n\\begin{tabular}",colspec,sep=""))
  }
  x <- paste(floatspec,
             ifelse(long,"\\\\",""),
             sep="")

  if(Minionfig){
    x <- c(x,"%Uncomment the following line and the end one to change figure versions\n%if you are using a full-featured family such as Minion Pro.\n\\figureversion{tabular}\n")
  }


  ## get the summaries for the objects
  model.summaries <- lapply(models, function(x) {
      ## If an apsrtableSummary exists, use it
      ## Otherwise, use summary.

      s <- try(apsrtableSummary(x), silent=TRUE)
      if (inherits(s, "try-error")) {
          s <- summary(x)
      }
      if(!is.null(customSE(x)) && se != "vcov") {
          est <- coef(x)
          if(class(customSE(x)) == "matrix") {
              x$se <- sqrt(diag(x$se))
          }
          s$coefficients[,3] <- tval <- est / customSE(x)
          e <- try(s$coefficients[,4] <-
                   2 * pt(abs(tval),
                          length(x$residuals) - x$rank,
                          lower.tail=FALSE),silent=TRUE)
          if(inherits(e,"try-error")){
              s$coefficients[, 4] <-
                  2*pnorm(abs(tval),lower.tail=FALSE)
          }
          s$se <- x$se
      }
      if(se == "pval") {
          s$coefficients[,2] <- s$coefficients[,4]

      }
      return(s)
  } )

  ## Quietly switch the se.note to the pval.note as needed
  if(se=="pval") { se.note <- pval.note }

  ## Set up the model names
  ## If there's a vector of names, use that, or as many as there are
  ## and either all or the remainder.
  ## Optionally, model.number.start allows you to resetcounter
  ## TO DO: allow model "name" attribute to be used
  ##        but overridden by vector here.
  if (is.null(model.names)) {
    m.first = model.counter; m.last=m.first+(nmodels-1)
    model.names=paste("Model", m.first:m.last)
  } else if (!is.null(model.names) && (length(model.names) < nmodels) ) {
    m.first = length(model.names)+1
    model.names=c(model.names, paste( "Model", m.first:nmodels))
  }

## get and order the coefficient names from all models
  coefnames <- orderCoef(model.summaries, order=order)

  ## mark those to omit from the output
  incl <- rep(TRUE,length(coefnames))
  names(incl) <- coefnames
  if(!is.null(omitcoef)) {
    ## Boris Shor <boris@bshor.com> asked how to omitcoef by regex
    ##  this line enables omitcoef=expression() 2010-03-17
    ##  OR if you want to mix modes or provide multiple expr
    ##  you can supply a list() eg list(expression(grep), 15)
    omitcoef <- unlist(sapply(omitcoef, eval, envir=myenv ))
    #print(omitcoef)
    incl[omitcoef] <- FALSE
  }
## now figure out position of each coef in each model
model.summaries <- coefPosition(model.summaries, coefnames)

  ## Now that the coef name matching is done, switch to pretty names
  ## if they are supplied.
    if(!is.null(coef.names)) {
      if(length(coef.names) != sum(incl)) {
        warning("Supplied coef.names not the same length as output. Check automatic names before supplying 'pretty' names.\n")
      }
      coefnames[incl] <- coef.names
    } else {
      coefnames[incl] <- sanitize(coefnames[incl])
    }


  out.table <- lapply(model.summaries, function(x){
    var.pos <- attr(x,"var.pos")
    model.out <- model.se.out <- star.out <- rep(NA,length(coefnames))
    model.out[var.pos] <- x$coefficients[,1]
    if(lev>0) {
        star.out[var.pos] <- apsrStars(x$coefficients,
                                       stars=stars,
                                       lev=lev,signif.stars=TRUE)
    } else {
        star.out <- rep("", length(coefnames))
    }
    model.out <- ifelse(!is.na(model.out),
                        paste(formatC(model.out,digits=digits,format="f"),
                              star.out),
                        "")



    model.se.out[var.pos] <- x$coefficients[,2]
    if( !is.null(customSE(x)) & se %in% c("robust","both") ) {
        model.se.out[var.pos] <- x$se
    }

    model.se.out <- ifelse(!is.na(model.se.out),
                           paste("(",
                                 formatC(model.se.out,
                                         digits=digits,
                                         format="f"),
                                 ")",sep=""),
                           "")
    if(se=="both" && !is.null(customSE(x))){
      model.se.out[var.pos] <- ifelse(model.se.out != "",
                             paste(model.se.out," [",
                                   formatC(x$coefficients[,2],
                                           digits=digits,
                                           format="f"),
                                   "]",sep=""),
                             "")
    }

    if(coef.rows==2) {
      ## Create two side by side columns and mesh them together
      model.out <- rep(model.out[incl], each=2)
      model.se.out <- rep(model.se.out[incl], each=2)
      pos.se <- (1:length(model.out))[(1:length(model.out) %% 2==0)]
      model.out[pos.se] <- model.se.out[pos.se]
      ## Add a new model info attribute to the model's output entry
      ## To change modelInfo for a given model, change the method for it
      ## see ?modelInfo, it is reasonably well documented.
    } else {
      ## two columns per model
      model.out <- model.out[incl]
      model.out <- cbind(model.out, model.se.out[incl])
    }
    attr(model.out,"model.info") <- modelInfo(x)
    return(model.out)
  })

  out.matrix <- matrix(unlist(out.table),
                       length(coefnames[incl])*coef.rows,
                       nmodels*coef.cols)

  out.matrix <- cbind(rep(coefnames[incl],each=coef.rows), out.matrix)
  if(coef.rows==2) {
    out.matrix[ (row(out.matrix)[,1] %% 2 ==0) , 1] <- ""
  }
  out.info <- lapply(out.table, attr, "model.info")
  info.names <- orderCoef(out.info)
  out.info <- coefPosition( out.info, orderCoef(out.info) )
  out.info <- lapply(out.info, function(x) {
    var.pos <- attr(x,"var.pos")
    model.out <- rep("",length(info.names))
    model.out[var.pos] <- coef(x)
    return(model.out)
  } )

  out.info <- matrix(unlist(out.info), length(info.names), nmodels)
  out.info <- cbind(as.character(info.names), out.info)

  if(coef.rows==2) {
    out.matrix <- rbind(c("%",model.names ),out.matrix)
  }
  outrows <- nrow(out.matrix)

  ## This does the pretty latex formatting, where commented model names
  ## line up with appropriately sized columns of numbers.

  ## Paul Johnson suggested a 'wide' or two column format for tables
  ## which then means model info needs to be underneath the two
  ## in a multicolumn span. But, for normal (two row, one column per coef)
  ## format, this is extraneous markup and hard to read.
  if(coef.cols==1) {
    out.matrix <- rbind(out.matrix,out.info)
    out.matrix[,-1] <- format(out.matrix[,-1])
    out.matrix[,1] <- format(out.matrix)[,1]
    out.matrix <- apply(out.matrix, 1, paste, collapse=" & ")
    out.info <- out.matrix[ (1+outrows) : length(out.matrix) ]
    out.matrix <- out.matrix[ 1:outrows ]
  } else {
    out.matrix <- format(out.matrix)
    out.matrix <- apply(out.matrix, 1, paste, collapse=" & ")
    ## now do the out.info as multicolumn blocks
    out.info[,-1] <- format(out.info[,-1])
    out.info[,-1] <- sapply(as.matrix(out.info[,-1]), function(x) {
      paste("\\multicolumn{",coef.cols,"}{",multicolumn.align,
            "}{",x,"}",sep="")
    })
    out.info[,1] <- format(out.info[,1])
    out.info <- apply(out.info, 1, paste, collapse=" & ")
  }

  headrow <- paste("\n\\hline \n",
                   paste(" &", paste("\\multicolumn{",coef.cols,"}{",
                               multicolumn.align,"}{",
                               model.names,"}", collapse=" & ")  ),
               "\\\\ \\hline\n")
  if(long) { headrow <- paste(headrow,"\\endhead\n",sep="") }
  x <- c(x, headrow)

  #x <- c(x,"")
  x <- c(x,paste(out.matrix, collapse="\\\\ \n"))
  x <- c(x,"\\\\\n")
  x <- c(x,paste(out.info, collapse="\\\\ \n"))

  ## Do notes
  ## Evaluate the notes list
  ## Switch the se to either robust or regular --
  ## Robust is the default, but if only vcov are given,
  ## quietly switch the argument.
  se <- ifelse((se != "vcov" &&
                sum(unlist(lapply(model.summaries, function(x) !is.null(customSE(x)))) >0 ) ) ,
               "robust","vcov")
  thenotes <- as.list(1:length(notes))
  thenotes[!sapply(notes,is.function)] <- notes[!sapply(notes,is.function)]
  thenotes[sapply(notes,is.function)] <- lapply(notes[sapply(notes,is.function)], do.call,
                                                args=list(env=myenv))

  x <- c(x,"\\\\ \\hline\n")
  notes <- lapply(thenotes, function(x) {
      ##eek! note coef cols was wrong
      ## fixed 2009-05-07 mjm
      paste("\\multicolumn{",(nmodels*coef.cols)+1,"}{l}{\\footnotesize{", x , "}}",sep="")
             } )
  x <- c(x, paste(notes, collapse="\\\\\n"))

  if(!long) { x <- c(x,"\n\\end{tabular}") }
  if(long) { x <- c(x,"\n\\end{longtable}") }
  if(caption.position=="b") {
      x <- c(x, paste("\n\\caption{",caption,"}\n\\label{",label,"}",sep=""))
  }
  x <- c(x,"\n")
  if(Minionfig) {x <- c(x,"\n\\figureversion{proportional}\n") }
  if(!Sweave & !long) { x <- c(x,paste("\\end{",float,"}\n",sep="")) }
  class(x) <- "apsrtable"
  return(x)
}









## A couple of test calls here for random features
## library(apsrtable);example(apsrtable);apsrtable(lm.D90, lm.D9, glm.D9, digits=1, align="center", stars="default", model.counter=0, order="rl", omitcoef="(Intercept)")
## library(apsrtable);example(apsrtable);apsrtable(lm.D90,coef.names=c("\\#1","\\#0"))

