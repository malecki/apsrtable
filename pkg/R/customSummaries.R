## apsrtableSummary enables easy S3 method masking of summary functions
## where the model-package provides a summary not suitable for apsrtable,
## such as z scores instead of pnorms.



##' Custom summary functions for output tables
##'
##' Provide alternative model summaries specifically for use with
##' \code{apsrtable}
##'
##' When preparing model objects for output, \code{apsrtable} uses primarily
##' the representation of the model provided by its \code{summary} method.
##' However, some packages return summaries with information that can be
##' confusing to \code{apsrtable}.
##'
##' In such an event, you have two options: provide a custom
##' \code{apsrtableSummary} method,
##' or work with the package maintainers to
##' produce a suitable \code{summary} object.
##' Ideally, the former is a stopgap for the latter.
##'
##' @aliases apsrtableSummary apsrtableSummary.gee apsrtableSummary.lrm
##' @param x A model object to be summarized in a format suitable for
##' \code{apsrtable} output.
##' @return A \code{summary} representation of a model object,
##' probably derived  from the object's own \code{summary} method.
##' @author Michael Malecki <malecki at wustl.edu>
##' @import lme4
##' @examples
##'
##' ### summary.gee produces z scores but not Pr(z). This converts the relevant columns
##' ### to Pr(z) so that apsrstars() works on it, and places the vector of robust se's in
##' ### an $se position which apsrtable expects.
##'
##' apsrtableSummary.gee <- function(x) {
##'   s <- summary(x)
##'   newCoef <- coef(s)
##'   ## which columns have z scores? (two of them in robust case)
##'   zcols <- grep("z",colnames(newCoef))
##'   newCoef[,zcols] <- pnorm(abs(newCoef[,zcols]), lower.tail=FALSE)
##'   colnames(newCoef)[zcols] <- "Pr(z)"
##'   s$coefficients <- newCoef
##'   ## put the robust se in $se so that notefunction works automatically
##'   ## the se checker will overwrite [,4] with pt, but this doesn't matter
##'   ## because the last column Pr(z) is used by apsrstars() anyway
##'   ## and the se are pulled from $se.
##'   if( class(x) == "gee.robust") {
##'     s$se <- coef(s)[,4]
##'   }
##'   return(s)
##' }
##'

setGeneric("apsrtableSummary", function(object, ...) {
    standardGeneric("apsrtableSummary") })

"apsrtableSummary" <- function(object, ...) {
  UseMethod("apsrtableSummary") }


apsrtableSummary.mer <- function (object, ...) {
  fcoef <- coef(s)
  out <- list()
  useScale <- object@dims["useSc"]
  corF <- vcov(object)@factors$correlation
  coefs <- cbind(fcoef, corF@sd)
  browser()
  if (length (fcoef) > 0){
      if (!object@dims["useSc"]) {
        coefs <- coefs[, 1:2, drop = FALSE]
        out$z.value <- coefs[, 1]/coefs[, 2]
        out$p.value <- 2 * pnorm(abs(out$z.value), lower = FALSE)
        coefs <- cbind(coefs,
                       `z value` = out$z.value,
                       `Pr(>|z|)` = out$p.value)
      }
      else {
        out$t.value <- coefs[, 1]/coefs[, 2]
        coefs <- cbind(coefs, `t value` = out$t.value)
      }
    dimnames(coefs)[[2]][1:2] <- c("coef.est", "coef.se")
      if(detail){
        pfround (coefs, digits)
      }
      else{
        pfround(coefs[,1:2], digits)
    }
  }
  out$coef <- coefs[,"coef.est"]
  out$se <- coefs[,"coef.se"]
  vc <- as.matrix.VarCorr (VarCorr (object),
                           useScale=useScale, digits)
  vc[,1] <-
  print (vc[,c(1:2,4:ncol(vc))], quote=FALSE)

  out$ngrps <- lapply(object@flist, function(x) length(levels(x)))
  ## Model fit statistics.
  ll <- logLik(obj)[1]
  deviance <- deviance(obj)
  AIC <- AIC(obj)
  BIC <- BIC(obj)
  N <- as.numeric(smry@dims["n"])
  G <- as.numeric(smry@ngrps)
  sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC,
               BIC = BIC, N = N, Groups = G)

  ## Return model summary.
  list(coef = coef, sumstat = sumstat,
       contrasts = attr(model.matrix(obj), "contrasts"),
       xlevels = xlevels, call = obj@call)
}
setMethod("apsrtableSummary", "mer", apsrtableSummary.mer)
setMethod("summary","mer", selectMethod("summary","mer","package:lme4"))



## Req by Solomon Messing. This fxn is based on print.lrm, which seems
## to contain everything needed for table and modelinfo.
apsrtableSummary.lrm <- function (x) {
  digits <- 4
  strata.coefs <- FALSE
  sg <- function(x, d) {
    oldopt <- options(digits = d)
    on.exit(options(oldopt))
    format(x)
  }
  rn <- function(x, d) format(round(as.single(x), d))

  ##cat("\n")
    if (x$fail) {
      cat("Model Did Not Converge\n")
      return()
    }
  ##cat("Logistic Regression Model\n\n")
  ##sdput(x$call)
  ##cat("\n\nFrequencies of Responses\n")
  ##print(x$freq)
  if (length(x$sumwty)) {
    ##cat("\n\nSum of Weights by Response Category\n")
    ##print(x$sumwty)
  }
  ##cat("\n")
  if (!is.null(x$nmiss)) {
    ##cat("Frequencies of Missing Values Due to Each Variable\n")
    ##print(x$nmiss)
    ##cat("\n")
  }
  else if (!is.null(x$na.action))
    ##naprint(x$na.action)
  ns <- x$non.slopes
  nstrata <- x$nstrata
  if (!length(nstrata))
    nstrata <- 1
  pm <- x$penalty.matrix
  if (length(pm)) {
    psc <- if (length(pm) == 1)
      sqrt(pm)
    else sqrt(diag(pm))
    penalty.scale <- c(rep(0, ns), psc)
    cof <- matrix(x$coef[-(1:ns)], ncol = 1)
    ##cat("Penalty factors:\n\n")
    ##print(as.data.frame(x$penalty, row.names = ""))
    ##cat("\nFinal penalty on -2 log L:", rn(t(cof) %*% pm %*%
    ##    cof, 2), "\n\n")
  }
  vv <- diag(x$var)
    cof <- x$coef
    if (strata.coefs) {
        cof <- c(cof, x$strata.coef)
        vv <- c(vv, x$Varcov(x, which = "strata.var.diag"))
        if (length(pm))
            penalty.scale <- c(penalty.scale, rep(NA, x$nstrat -
                1))
    }
    score.there <- nstrata == 1 && (length(x$est) < length(x$coef) -
        ns)
    stats <- x$stats
    stats[2] <- signif(stats[2], 1)
    stats[3] <- round(stats[3], 2)
    stats[4] <- round(stats[4], 2)
    stats[5] <- round(stats[5], 4)
    stats[6] <- round(stats[6], 3)
    stats[7] <- round(stats[7], 3)
    if (nstrata == 1) {
        stats[8] <- round(stats[8], 3)
        stats[9] <- round(stats[9], 3)
        stats[10] <- round(stats[10], 3)
        if (length(stats) > 10) {
            stats[11] <- round(stats[11], 3)
            if (length(x$weights))
                stats[12] <- round(stats[12], 3)
        }
    }
    else stats <- c(stats, Strata = x$nstrat)

    res <- list()
    res$modelinfo <- stats

    z <- cof/sqrt(vv)
    stats <- cbind(cof,vv,cof/sqrt(vv) )
    stats <- cbind(stats, (1 - pchisq(z^2, 1)))
  ugh <- names(cof)
  names(cof) <- sub("Intercept","(Intercept)",ugh)
    dimnames(stats) <- list(names(cof), c("Coef", "S.E.", "Wald Z",
        "Pr(z)"))
    if (length(pm))
        stats <- cbind(stats, `Penalty Scale` = penalty.scale)
    ##print(stats, quote = FALSE)
    ##cat("\n")
    if (score.there) {
        q <- (1:length(cof))[-est.exp]
        if (length(q) == 1)
            vv <- x$var[q, q]
        else vv <- diag(x$var[q, q])
        z <- x$u[q]/sqrt(vv)
        stats <- cbind(z, (1 - pchisq(z^2, 1)))

        dimnames(stats) <- list(names(cof[q]), c("Score Z", "P"))
        ##printd(stats, quote = FALSE)
        ##cat("\n")
    }
  res$coefficients <- stats
  class(res) <- "summary.lrm"
  invisible(res)
}



## Added support for MASS::polr
## mjm 2012-05-20
"apsrtableSummary.polr" <- function (x) {
       s <- summary(x)
       newCoef <- coef(s)
       newCoef <- cbind(newCoef, pt(abs(coef(s)[,3]),
                         df=s$n-s$edf-1,
                         lower.tail=FALSE))
       colnames(newCoef) <- c("coef","se(coef)", "t value", "Pr(>|t|)")
       s$coefficients <- newCoef
       return(s)

}


"apsrtableSummary.gee" <- function(x) {
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
  if( class(x)[1] == "gee.robust") {
    s$se <- coef(s)[,4]
  }
  return(s)
}

"apsrtableSummary.clogit" <- apsrtableSummary.coxph <- function (x) {
       s <- summary(x)
       if("robust se" %in% colnames(coef(s))) s$se <- coef(s)[,"robust se"]
       s$coefficients <- coef(s)[,c("coef","se(coef)", "Pr(>|z|)")]
       return(s)
}
"apsrtableSummary.negbin" <- function (x) {
       s <- summary(x)
       coefs <- coef(s)
       theta <- matrix(c(s$theta, s$SE.theta,NA,NA),1,4)
       theta[,3] <- theta[,1]/theta[,2] ;
       theta[,4] <- pnorm(abs(theta[,3]),lower.tail=FALSE)
       rownames(theta) <- "$\\theta$"
       s$coefficients <- rbind(coefs,theta)
       return(s)
}

setOldClass("summary.lm")
setOldClass("summary.glm")
setOldClass("summary.tobit")
setOldClass("summary.gee")
setOldClass("summary.coxph")
setOldClass("summary.negbin")
setOldClass("summary.lrm")
setOldClass("summary.svyglm")
setOldClass("summary.polr")
