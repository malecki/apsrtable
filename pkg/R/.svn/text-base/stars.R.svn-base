
apsrStars <- function (x, digits = max(3, getOption("digits") - 2),
                       signif.stars = getOption("show.signif.stars"),
                       signif.legend = signif.stars,
                       dig.tst = max(1, min(5, digits - 1)), cs.ind = 1:k,
                       tst.ind = k + 1, zap.ind = integer(0),
                       P.values = NULL,
                       has.Pvalue =
                       nc >= 3 && # used to be 4
                       substr(colnames(x)[nc],
                                      1, 3) == "Pr(" ||
                       grepl("z",colnames(x)[nc]) == TRUE
                       ,
                       eps.Pvalue = .Machine$double.eps, na.print = "NA",
                       stars="default",lev=.05,
    ...)
{
    if (is.null(d <- dim(x)) || length(d) != 2)
        stop("'x' must be coefficient matrix/data frame")
    nc <- d[2]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue)
        stop("'P.values' is TRUE, but 'has.Pvalue' is not")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind))
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k)
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    if (length(cs.ind) > 0) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        if (any(is.finite(acs))) {
            digmin <- 1 + floor(log10(range(acs[acs != 0], na.rm = TRUE)))
            Cf[, cs.ind] <- format(round(coef.se, max(1, digits -
                digmin)), digits = digits)
        }
    }
    if (length(tst.ind) > 0)
        Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
            digits = digits)
    if (length(zap.ind) > 0)
        Cf[, zap.ind] <- format(zapsmall(xm[, zap.ind], digits = digits),
            digits = digits)
    if (any(r.ind <- !((1:nc) %in% c(cs.ind, tst.ind, zap.ind,
        if (has.Pvalue) nc))))
        Cf[, r.ind] <- format(xm[, r.ind], digits = digits)
    okP <- if (has.Pvalue)
        ok[, -nc]
    else ok
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if (dec != ".")
        x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1,
            digits - 1))
    }
    if (any(ina))
        Cf[ina] <- na.print

    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }

        if (any(okP <- ok[, nc])) {
          pv <- as.vector(xm[, nc])
          Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst,
                                     eps = eps.Pvalue)
          signif.stars <- signif.stars && any(pv[okP] < 0.1)
          Signif <- ""
          if (signif.stars && stars=="default") {
            Signif <- symnum(pv, corr = FALSE, na = FALSE,
                             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                             symbols = c("^{***}", "^{**}", "^*", "^\\dagger\ ", " "))
            Cf <- cbind(Cf, format(Signif))
          }
          else if (signif.stars && stars==1) {
           Signif <- symnum(pv, corr = FALSE, na = FALSE,
                             cutpoints = c(0,lev,1),
                             symbols = c("^*"," "))
          }
          return(Signif)
        }
      }

    return()
  }
