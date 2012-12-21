
## extractor for $se, returns NULL if it's an error
## (will be error for s4 objects like 'mer'
customSE <- function(x) {
    e <- try(x$se)
    if(inherits(e, "try-error")) NULL
    else e
}

fround <- function (x, digits) {
    format (round (x, digits), nsmall=digits)
}

pfround <- function (x, digits) {
    print (fround (x, digits), quote=FALSE)
}



"print.apsrtable" <- function(x,...) {
  cat(paste(x))
}

### This borrowed from package 'arm'
as.matrix.VarCorr <- function (varc, useScale, digits){
    ## VarCorr function for lmer objects, altered as follows:
    ##   1.  specify rounding
    ##   2.  print statement at end is removed
    ##   3.  reMat is returned
    ##   4.  last line kept in reMat even when there's no error term
    sc <- attr(varc, "sc")[[1]]
    if(is.na(sc)) sc <- 1
    recorr <- lapply(varc, function(el) attr(el, "correlation"))
    reStdDev <- c(lapply(varc, function(el) attr(el, "stddev")),
                  list(Residual = sc))
    reLens <- unlist(c(lapply(reStdDev, length)))
    reMat <- array('', c(sum(reLens), 4),
                   list(rep('', sum(reLens)),
                        c("Groups", "Name", "Variance", "Std.Dev.")))
    reMat[1+cumsum(reLens)-reLens, 1] <- names(reLens)
    reMat[,2] <- c(unlist(lapply(reStdDev, names)), "")
    reMat[,3] <- fround(unlist(reStdDev)^2, digits)
    reMat[,4] <- fround(unlist(reStdDev), digits)
    if (any(reLens > 1)) {
        maxlen <- max(reLens)
        corr <-
            do.call("rbind",
                    lapply(recorr,
                           function(x, maxlen) {
                               x <- as(x, "matrix")
                               cc <- fround (x, digits)
                               cc[!lower.tri(cc)] <- ""
                               nr <- dim(cc)[1]
                               if (nr >= maxlen) return(cc)
                               cbind(cc, matrix("", nr, maxlen-nr))
                           }, maxlen))
        colnames(corr) <- c("Corr", rep("", maxlen - 1))
        reMat <- cbind(reMat, rbind(corr, rep("", ncol(corr))))
    }
    if (useScale<0) reMat[nrow(reMat),] <-
        c ("No residual sd", rep("",ncol(reMat)-1))
    return (reMat)
}




## Given a list of model summaries (or anything with a coef method),
## and a master (unioned) list of coef names,
## Append an attribute to each element containing its
## coefs' position in the master coefficient list
"coefPosition" <- function(model.summaries, coefnames) {
  model.summaries <- lapply(model.summaries, function(x) {
    pos <- match(rownames(coef(x)), coefnames)
    attr(x,"var.pos") <- pos
    return(x)
  })
return(model.summaries)
}


"coef.model.info" <- function(object,...) {
  x <- as.matrix(unlist(object)); invisible(x)
}

## RULES: All according to longest model,
##        then left to right
## RESULT: union of all models' coefficient names in requested order.
orderCoef <- function(model.summaries,order="lr") {
  nmodels <- length(model.summaries)
  mlength <- sapply(model.summaries, function(x) length(coef(x)) )
  longest <- which.max(mlength) # longest model
  if(order=="rl") {
    modelorder <- nmodels:1 } else {
      modelorder <- 1:nmodels }
  if(order=="longest") {
    coefnames <-  rownames(coef(model.summaries[[longest]]))
  } else {
    coefnames <- rownames(coef(model.summaries[[modelorder[1]]])) }

  for(i in seq_along(model.summaries)) {
    matched <- match(rownames(coef(model.summaries[[i]])), coefnames, nomatch=0)
    unmatched <- which(is.na(matched) | matched==0)
    coefnames <- c(coefnames,
                   rownames(coef(model.summaries[[i]]))[unmatched]
                   )
  }
  return(coefnames)
}
