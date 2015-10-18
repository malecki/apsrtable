##' Model fit and diagnostic functions for output
##'
##' Model diagnostic / summary information to be included in apsrtable output.
##'
##' Returns a list containing model diagnostic information, with an interface
##' described here to allow the user to change the information returned and
##' thus presented. The method is called by \code{apsrtable} within an
##' \code{lapply} on a list of model summaries. The modelInfo methods for a
##' given model summary object simply return a list of arbitrary name-value
##' pairs and give themselves the S3 class \code{modelInfo}. The modelInfo
##' method dispach uses formal S4 classes, however.
##'
##' The example shows how one can change the summary for \code{lm} objects to
##' include only the \eqn{N} and residual \eqn{\sigma}.
##'
##' If you register a \code{modelInfo} method and it appears not to work, try
##' calling \code{\link[methods]{setOldClass}} in order to register new
##' \code{modelInfo} methods for your model summary object. Method dispatch in
##' R has some subtleties.
##'
##' @aliases modelInfo modelInfo,summary.lm-method modelInfo,summary.glm-method
##' modelInfo,summary.svyglm-method modelInfo,summary.tobit-method
##' modelInfo,summary.gee-method modelInfo,summary.coxph-method
##' modelInfo,summary.clogit-method modelInfo,summary.negbin-method
##' modelInfo,summary.lrm-method
##' @param x A \code{summary} object.
##' @return A list of named character objects representing the lines of model
##' diagnostic information to be included for a given class of model. For
##' example, the default for \code{\link[stats]{lm}} reports the \eqn{N, R^2},
##' adjusted \eqn{R^2}, and residual \eqn{\sigma}. The default for
##' \code{\link[stats]{glm}} includes the \eqn{N}, AIC, BIC, and
##' log-likelihood. Common names across model classes in the same table --
##' e.g., the \eqn{N} -- are matched by name, exactly like the model
##' coefficients (indeed, the same functions aggregate terms and order across
##' models.)
##' @export
##' @docType methods
##' @rdname modelInfo-methods
##' @author Michael Malecki <malecki at wustl.edu>
##' @seealso \link[base]{sys.frame}
##' @examples
##'
##'\dontrun{
##' setMethod("modelInfo", "summary.lm", function(x) {
##'   env <- sys.parent()
##'   digits <- evalq(digits, env)
##'   model.info <- list(
##'                      "$N$"=formatC(sum(x$df[1:2]),format="d"),
##'                      "Resid. sd" = formatC(x$sigma,format="f",digits=digits))
##'   class(model.info) <- "model.info"
##'   return(model.info)
##' } )
##'
##' example(apsrtable)
##' ## Switch back to the default
##' setMethod("modelInfo", "summary.lm", apsrtable:::modelInfo.summary.lm)
##' example(apsrtable)
##'
##' }
setGeneric("modelInfo", function(x) {
             standardGeneric("modelInfo")
})


##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.lm,ANY-method
setMethod("modelInfo", "summary.lm", function(x) {
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
                     "$N$"=formatC(sum(x$df[1:2]),format="d"),
                     "$R^2$"=formatC(x$r.squared,format="f",digits=digits),
                     "adj. $R^2$"=formatC(x$adj.r.squared,format="f",digits=digits),
                     "Resid. sd" = formatC(x$sigma,format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
})

##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.glm,ANY-method
setMethod("modelInfo", "summary.glm", function(x) {
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
                       "$N$"=formatC(sum(x$df[1:2]),format="d"),

                       AIC=formatC(x$aic,format="f",digits=digits),
                       BIC= formatC(
                         ( (x$aic - 2*(length(x$coef)) ) +
                             log(sum(x$df[1:2]))*
                             length(coef(x)) ),
                         format="f",digits=digits),
                       "$\\log L$"=formatC( ((x$aic - 2*(length(x$coef))) / -2),
                         format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
})

##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.merMod,ANY-method
setMethod("modelInfo", "summary.merMod", function(x) {
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  GroupList <- vector("list", length(x$varcor) * 2)
  nams <- paste0("Group:", names(x$varcor))
  names(GroupList) <- paste0(rep(nams, times = 2), rep(c(" Effs.", " Var."), 
                                                       each = length(x$varcor)))
  for(i in 1:length(x$varcor)){
    GroupList[[i]] <- paste(names(attr(x$varcor[[i]], "stddev")), collapse = " | ")
    GroupList[[i + length(x$varcor)]] <- paste(round(attr(x$varcor[[i]], "stddev"), 
                                                           digits = digits), 
                                               collapse = " | ")
  }
  GroupList["Sigma"] <- formatC(as.numeric(attr(x$varcor, "sc"), digits = digits))
  if(x$isLmer == FALSE){
    GroupList["Sigma"] <- NULL
  }
  model.info <- list(
                "$N$"=formatC(as.numeric(x$devcomp$dims['n']),format="d"),
                "AIC"=formatC(as.numeric(x$AICtab)[1],
                format="f",digits=digits),
                "N Groups" = paste(as.numeric(x$ngrps), collapse = " | "),
                "Group Names" = paste(names(x$varcor), collapse = " | "))
  model.info <- append(model.info, GroupList)
  class(model.info) <- "model.info"
  invisible(model.info)
})


## 2009-02-25 mjm
## modelInfo request from Antonio Ramos for AER Tobit function
## Should be similar for 'survreg' objects, but without (necessarily)
## censoring info..
##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.tobit,ANY-method
setMethod("modelInfo", "summary.tobit", function(x) {
 env <- sys.parent()
 digits <- evalq(digits, envir=env)
 model.info <- list(
                    "Total $N$"=formatC(as.integer(x$n[1]),format="d"),
                    "Censored $N$"=formatC(sum(x$n[c(2,4)]),format="d"),
                    "$\\log L$" =
formatC(x$loglik[2],format="f",digits=digits),
                    "p(Wald)"=formatC(pchisq(x$wald,
                      sum(x$df) - x$idf,
                      lower.tail = FALSE),
                      digits=digits,format="f")
                    )
 class(model.info) <- "model.info"
 return(model.info)
})

##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.gee,ANY-method
setMethod("modelInfo", "summary.gee", function(x) {
 env <- sys.parent()
 digits <- evalq(digits, envir=env)
 model.info <- list(" " = ""
                    )
 class(model.info) <- "model.info"
 return(model.info)
})

##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.coxph,ANY-method
setMethod("modelInfo", "summary.coxph", function(x) {
       env <- sys.parent()
       digits <- evalq(digits, envir=env)
       model.info <- list()
       model.info[["$N$"]] <- x$n
       pv <- formatC(x$waldtest["pvalue"], format="f", digits=digits)
       rsq <- formatC(x$rsq["rsq"], format="f", digits=digits)
       maxrsq <- formatC(x$rsq["maxrsq"], format="f", digits=digits)
       model.info$Wald <- sprintf("%.0f on %.0f df, p = %s",
                                  x$waldtest["test"], x$waldtest["df"],
                                  pv)
       model.info[["$R^2$"]] <- sprintf("%s (Max %s)", rsq, maxrsq)
       class(model.info) <- "model.info"
       invisible(model.info)
})

##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.polr,ANY-method
setMethod("modelInfo", "summary.polr", function(x) {
    env <- sys.parent()
    digits<- evalq(digits, envir=env)

    model.info <- list(N= x$n,
                       AIC=format(x$deviance + 2 * x$edf, nsmall = 2L))
    class(model.info) <- "model.info"
    invisible(model.info)
})

##' @rdname modelInfo-methods
##' @aliases modelInfo,summary.lrm,ANY-method
setMethod("modelInfo", "summary.lrm", function(x) {
  env <- sys.parent()
  digits<- evalq(digits, envir=env)
  x <- as.numeric(x$modelinfo)
  ##         number of observations
  ##         used in the fit, maximum absolute value of first derivative
  ##         of log likelihood, model likelihood ratio chi-square, d.f.,
  ##         P-value, c index (area under ROC curve), Somers' D_{xy},
  ##         Goodman-Kruskal gamma, Kendall's tau-a rank correlations
  ##         between predicted probabilities and observed response, the
  ##         Nagelkerke R^2 index, and the Brier score
  model.info <-
      list(
           "$N$"=formatC(x[1],format="d"),
           "Max.Deriv."=formatC(x[2],format="f",digits=digits),
           "LR $\\chi^2$"=formatC(x[3],format="f",digits=digits),
           "d.f."=formatC(x[4],format="d"),
           "$P$"=formatC(x[5],format="f",digits=digits),
           "C-index"=formatC(x[6],format="f",digits=digits),
           "Somers $D_{xy}$"=formatC(x[7],format="f",digits=digits),
           ##"$\\gamma$"=formatC(x[8],format="f",digits=digits),
           ##"Kendall's tau-a"=formatC(x[9],format="f",digits=digits),
           "Nagelkerke $R^2$"=formatC(x[10],format="f",digits=digits),
           "Brier" = formatC(x[11],format="f",digits=digits))
  class(model.info) <- "model.info"
  invisible(model.info)
})